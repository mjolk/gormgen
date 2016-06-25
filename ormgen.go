package main

//go:generate go-bindata tmpl/

import (
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"strings"
	"text/template"
)

const (
	usageText = `ORMGEN
    Generate Go code to convert database rows into arbitrary structs.

USAGE
    ormgen [options] paths...

OPTIONS
    -o, -output
        Set the name of the generated file. Default is scans.go.

    -p, -package
        Set the package name for the generated file. Default is current
        directory name.

    -u, -unexport
        Generate unexported functions. Default is export all.

    -w, -whitelist
        Only include structs specified in case-sensitive, comma-delimited
        string.

    -v, -version
        Print version and exit.

    -h, -help
        Print help and exit.

EXAMPLES
    tables.go is a file that contains one or more struct declarations.

    Generate scan functions based on structs in tables.go.
        scaneo tables.go

    Generate scan functions and name the output file funcs.go
        scaneo -o funcs.go tables.go

    Generate scans.go with unexported functions.
        scaneo -u tables.go

    Generate scans.go with only struct Post and struct user.
        scaneo -w "Post,user" tables.go

NOTES
    Struct field names don't have to match database column names at all.
    However, the order of the types must match.

    Integrate this with go generate by adding this line to the top of your
    tables.go file.
        //go:generate scaneo $GOFILE
`
	COLUMN = "column"
	TABLE  = "table"
	FK     = "fk"
	LINK   = "link"
)

type fieldToken struct {
	Name      string
	Type      string
	Slice     bool
	Column    string
	Table     string
	Alias     string
	Fk        string
	Link      string
	LinkAlias string
	RelAlias  string
}

type structToken struct {
	IdColumn   string
	Name       string
	Fields     []*fieldToken
	Composite  bool
	OneToMany  []*relation
	ManyToOne  []*relation
	ManyToMany []*relation
	Table      string
	Alias      string
}

type relation struct {
	LinkRelationFields []*fieldToken
	LinkField          string
	LinkType           string
	Field              string
	Type               string
	Table              string
	LinkTable          string
	LinkAlias          string
	Alias              string
	From               string
	To                 string
	LinkFrom           string
	LinkTo             string
	IsAttribute        bool
	ManyToOne          []*relation
}

var structLookup map[string]*structToken

func main() {
	log.SetFlags(0)

	outFilename := flag.String("o", "ormgen.go", "")
	packName := flag.String("p", "current directory", "")
	unexport := flag.Bool("u", false, "")
	whitelist := flag.String("w", "", "")
	version := flag.Bool("v", false, "")
	help := flag.Bool("h", false, "")
	flag.StringVar(outFilename, "output", "ormgen.go", "")
	flag.StringVar(packName, "package", "current directory", "")
	flag.BoolVar(unexport, "unexport", false, "")
	flag.StringVar(whitelist, "whitelist", "", "")
	flag.BoolVar(version, "version", false, "")
	flag.BoolVar(help, "help", false, "")
	flag.Usage = func() { log.Println(usageText) } // call on flag error
	flag.Parse()

	if *help {
		// not an error, send to stdout
		// that way people can: scaneo -h | less
		fmt.Println(usageText)
		return
	}

	if *version {
		fmt.Println("scaneo version 1.2.0")
		return
	}

	if *packName == "current directory" {
		wd, err := os.Getwd()
		if err != nil {
			log.Fatal("couldn't get working directory:", err)
		}

		*packName = filepath.Base(wd)
	}

	files, err := findFiles(flag.Args())
	if err != nil {
		log.Println("couldn't find files:", err)
		log.Fatal(usageText)
	}

	structToks := make([]*structToken, 0)
	structLookup = make(map[string]*structToken)
	for _, file := range files {
		toks, err := parseCode(file, *whitelist)
		if err != nil {
			log.Println(`"syntax error" - parser probably`)
			log.Fatal(err)
		}

		structToks = append(structToks, toks...)
	}

	newStructToks := make([]*structToken, 0)

	//ugly shit
	//TODO refactor to recursive parser
	for _, structTk := range structToks {
		newStructTk := &structToken{
			Name:       structTk.Name,
			Fields:     make([]*fieldToken, 0),
			Composite:  false,
			Table:      structTk.Table,
			Alias:      structTk.Alias,
			IdColumn:   structTk.IdColumn,
			OneToMany:  make([]*relation, 0),
			ManyToOne:  make([]*relation, 0),
			ManyToMany: make([]*relation, 0),
		}
		for _, sf := range structTk.Fields {
			l2Rel := make([]*relation, 0)
			isSlice, embeddedStruct := isRelation(sf)
			if embeddedStruct == "" {
				newStructTk.Fields = append(newStructTk.Fields, sf)
				continue
			}
			newStructTk.Composite = true
			emb := structLookup[embeddedStruct]
			rel := &relation{Field: sf.Name, Table: emb.Table, Alias: sf.RelAlias, Type: embeddedStruct, IsAttribute: false}
			var rel3 *relation
			var rel2 *relation
			if embeddedStruct == "Attribute" {
				rel.IsAttribute = true
			}
			var embeddedIdColumn string = ""
			var embeddedIdColumn2 string = ""
			for _, field := range emb.Fields {
				// 1 st layer only manytoone
				isSlice2, embedded := isRelation(field)
				if isSlice2 {
					continue
				}
				if field.Name == "ID" {
					embeddedIdColumn = field.Column
				}
				baseName := sf.Name
				if isSlice {
					baseName = "proxy" + baseName
				}
				if embedded == "" {
					name := baseName + "." + field.Name
					nf := &fieldToken{
						Name:   name,
						Type:   field.Type,
						Slice:  isSlice,
						Column: field.Column,
						Table:  field.Table,
						Alias:  field.Alias,
						Fk:     sf.Fk,
					}
					newStructTk.Fields = append(newStructTk.Fields, nf)
					continue
				}
				emb2 := structLookup[embedded]
				if emb2 == emb {
					continue
				}
				rel2 = &relation{Field: field.Name, Table: emb2.Table, Alias: field.RelAlias, Type: embedded}
				for _, field2 := range emb2.Fields {
					_, embedded2 := isRelation(field2)
					if embedded2 != "" {
						continue
					}
					if field2.Name == "ID" {
						embeddedIdColumn2 = field2.Column
					}
					name2 := baseName + "." + field.Name + "." + field2.Name
					nf2 := &fieldToken{
						Name:   name2,
						Type:   field2.Type,
						Slice:  isSlice,
						Column: field2.Column,
						Table:  field2.Table,
						Alias:  field2.Alias,
						Fk:     field.Fk,
					}
					newStructTk.Fields = append(newStructTk.Fields, nf2)
				}
				rel2.From = field.Fk
				rel2.To = embeddedIdColumn2
				l2Rel = append(l2Rel, rel2)
			}
			if isSlice {
				//manytomaany
				if sf.Link != "" {
					rel.LinkAlias = sf.LinkAlias
					rel.From = newStructTk.IdColumn
					rel.LinkTo = embeddedIdColumn
					rel.LinkType = sf.Link
					rel.LinkField = sf.Link
					linkStruct := structLookup[sf.Link]
					rel.LinkTable = linkStruct.Table
					for _, field := range linkStruct.Fields {
						//adding relations to link table
						slc, linkEmbedded := isRelation(field)
						if slc {
							continue
						}
						if field.Name == "ID" {
							continue
						}
						lname := "proxy" + sf.Link + "." + field.Name
						if linkEmbedded == "" {
							if !strings.Contains(field.Name, "ID") {
								nf := &fieldToken{
									Name:   lname,
									Type:   field.Type,
									Column: field.Column,
									Table:  field.Table,
									Alias:  sf.LinkAlias,
								}
								newStructTk.Fields = append(newStructTk.Fields, nf)
							}
						} else {
							rel.LinkRelationFields = append(rel.LinkRelationFields, &fieldToken{Name: field.Name, Type: linkEmbedded})
							lemb := structLookup[linkEmbedded]
							rel3 = &relation{Field: field.Name, Table: lemb.Table, Alias: field.RelAlias, Type: linkEmbedded}
							for _, lfld := range lemb.Fields {
								if _, linkRelEmb := isRelation(lfld); linkRelEmb != "" {
									continue
								}
								if lfld.Name == "ID" {
									rel3.To = lfld.Column
								}
								name := lname + "." + lfld.Name
								nfl := &fieldToken{
									Name:   name,
									Type:   lfld.Type,
									Column: lfld.Column,
									Table:  lfld.Table,
									Alias:  lfld.Alias,
								}
								newStructTk.Fields = append(newStructTk.Fields, nfl)
							}
							rel3.From = field.Column
							rel.ManyToOne = append(rel.ManyToOne, rel3)
						}
						if field.Link == newStructTk.Name {
							rel.To = field.Column
						}
						if field.Link == emb.Name {
							rel.LinkFrom = field.Column
							rel.Alias = field.LinkAlias
						}
					}
					if len(l2Rel) > 0 {
						for _, r2 := range l2Rel {
							r2.From = rel.LinkTo
							rel.ManyToOne = append(rel.ManyToOne, r2)
						}
					}
					newStructTk.ManyToMany = addRelation(newStructTk.ManyToMany, rel)
				} else { //onetomany
					rel.From = newStructTk.IdColumn
					rel.To = sf.Fk
					if len(l2Rel) > 0 {
						for _, r2 := range l2Rel {
							//r2.From = sf.Column //sf.Fk
							rel.ManyToOne = append(rel.ManyToOne, r2)
						}
					}
					newStructTk.OneToMany = addRelation(newStructTk.OneToMany, rel)
				}
			} else {
				rel.From = sf.Fk
				rel.To = embeddedIdColumn
				if len(l2Rel) > 0 {
					rel.ManyToOne = append(rel.ManyToOne, l2Rel...)
				}
				newStructTk.ManyToOne = addRelation(newStructTk.ManyToOne, rel)
			}
		}
		checkDuplicateAlias(newStructTk)
		newStructToks = append(newStructToks, newStructTk)
	}

	if err := genFile(*outFilename, *packName, *unexport, newStructToks); err != nil {
		log.Fatal("couldn't generate file:", err)
	}
}

var aliases map[string]int

func checkDuplicateAlias(s *structToken) {
	aliases = make(map[string]int, 10)
	checkAlias(s.ManyToOne)
	checkAlias(s.ManyToMany)
	checkAlias(s.OneToMany)
	for _, morel := range s.ManyToOne {
		checkAlias(morel.ManyToOne)
	}
	for _, mmrel := range s.ManyToMany {
		checkAlias(mmrel.ManyToOne)
	}
	for _, omrel := range s.OneToMany {
		checkAlias(omrel.ManyToOne)
	}
}

func checkAlias(rels []*relation) {
	for _, rel := range rels {
		if _, ok := aliases[rel.Alias]; ok {
			//already exists
			rel.Alias = "dd" + rel.Alias
			continue
		}
		aliases[rel.Alias] = 1
	}
}

func addRelation(relations []*relation, tpe *relation) []*relation {
	if len(relations) > 0 {
		for _, rel := range relations {
			if rel.Field == tpe.Field {
				return relations
			}
		}
	}
	relations = append(relations, tpe)
	return relations
}

func isRelation(ft *fieldToken) (bool, string) {
	var embeddedStruct string = ""
	var isSlice bool = false
	//var isSlice bool = false
	if strings.HasPrefix(ft.Type, "*") {
		embeddedStruct = strings.TrimPrefix(ft.Type, "*")
	}
	if strings.HasPrefix(ft.Type, "[]*") {
		embeddedStruct = strings.TrimPrefix(ft.Type, "[]*")
		isSlice = true
	}
	return isSlice, embeddedStruct
}

func findFiles(paths []string) ([]string, error) {
	if len(paths) < 1 {
		return nil, errors.New("no starting paths")
	}

	// using map to prevent duplicate file path entries
	// in case the user accidently passes the same file path more than once
	// probably because of autocomplete
	files := make(map[string]struct{})

	for _, path := range paths {
		info, err := os.Stat(path)
		if err != nil {
			return nil, err
		}

		if !info.IsDir() {
			// add file path to files
			files[path] = struct{}{}
			continue
		}

		filepath.Walk(path, func(fp string, fi os.FileInfo, _ error) error {
			if fi.IsDir() {
				// will still enter directory
				return nil
			} else if fi.Name()[0] == '.' {
				return nil
			}

			// add file path to files
			files[fp] = struct{}{}
			return nil
		})
	}

	deduped := make([]string, 0, len(files))
	for f := range files {
		deduped = append(deduped, f)
	}

	return deduped, nil
}

func parseCode(source string, commaList string) ([]*structToken, error) {
	wlist := make(map[string]struct{})
	if commaList != "" {
		wSplits := strings.Split(commaList, ",")
		for _, s := range wSplits {
			wlist[s] = struct{}{}
		}
	}

	structToks := make([]*structToken, 0, 8)

	fset := token.NewFileSet()
	astf, err := parser.ParseFile(fset, source, nil, 0)
	if err != nil {
		return nil, err
	}

	var filter bool
	if len(wlist) > 0 {
		filter = true
	}

	//ast.Print(fset, astf)
	for _, decl := range astf.Decls {
		genDecl, isGeneralDeclaration := decl.(*ast.GenDecl)
		if !isGeneralDeclaration {
			continue
		}

		for _, spec := range genDecl.Specs {
			typeSpec, isTypeDeclaration := spec.(*ast.TypeSpec)
			if !isTypeDeclaration {
				continue
			}

			structType, isStructTypeDeclaration := typeSpec.Type.(*ast.StructType)
			if !isStructTypeDeclaration {
				continue
			}

			// found a struct in the source code!

			structTok := new(structToken)

			// filter logic
			if structName := typeSpec.Name.Name; !filter {
				// no filter, collect everything
				structTok.Name = structName
			} else if _, exists := wlist[structName]; filter && !exists {
				// if structName not in whitelist, continue
				continue
			} else if filter && exists {
				// structName exists in whitelist
				structTok.Name = structName
			}

			structTok.Fields = make([]*fieldToken, 0)

			// iterate through struct fields (1 line at a time)
			for _, fieldLine := range structType.Fields.List {
				column := columnName(fieldLine.Tag.Value)
				if column == "-" {
					continue
				}
				if table, tableAlias := tableName(fieldLine.Tag.Value); table != "" {
					structTok.Table = table
					structTok.Alias = tableAlias
				}
				fieldToks := make([]*fieldToken, len(fieldLine.Names))

				// get field name (or names because multiple vars can be declared in 1 line)
				for i, fieldName := range fieldLine.Names {
					fieldToks[i] = new(fieldToken)
					fieldToks[i].Name = parseIdent(fieldName)
				}

				var fieldType string

				// get field type
				switch typeToken := fieldLine.Type.(type) {
				case *ast.Ident:
					// simple types, e.g. bool, int
					fieldType = parseIdent(typeToken)
				case *ast.SelectorExpr:
					// struct fields, e.g. time.Time, sql.NullString
					fieldType = parseSelector(typeToken)
				case *ast.ArrayType:
					// arrays
					fieldType = parseArray(typeToken)
				case *ast.StarExpr:
					// pointers
					fieldType = parseStar(typeToken)
				}

				if fieldType == "" {
					continue
				}

				fk, relAlias := foreignKey(fieldLine.Tag.Value)
				link, linkAlias := linkTable(fieldLine.Tag.Value)
				// apply type to all variables declared in this line
				for i := range fieldToks {
					fieldToks[i].Type = fieldType
					fieldToks[i].Table = structTok.Table
					fieldToks[i].Alias = structTok.Alias
					if column != "" {
						fieldToks[i].Column = column
					}
					if fk != "" {
						fieldToks[i].Fk = fk
						fieldToks[i].Column = fk
						fieldToks[i].RelAlias = relAlias
					}
					if link != "" {
						fieldToks[i].Link = link
						fieldToks[i].LinkAlias = linkAlias
					}
					if fieldToks[i].Name == "ID" {
						structTok.IdColumn = fieldToks[i].Column
					}
				}

				structTok.Fields = append(structTok.Fields, fieldToks...)
			}

			structToks = append(structToks, structTok)
			structLookup[structTok.Name] = structTok
		}
	}

	return structToks, nil
}

func parseIdent(fieldType *ast.Ident) string {
	// return like byte, string, int
	return fieldType.Name
}

func parseSelector(fieldType *ast.SelectorExpr) string {
	// return like time.Time, sql.NullString
	ident, isIdent := fieldType.X.(*ast.Ident)
	if !isIdent {
		return ""
	}

	return fmt.Sprintf("%s.%s", parseIdent(ident), fieldType.Sel.Name)
}

func fieldTags(tag string) [][]string {
	prts := strings.Split(strings.Replace(strings.Trim(tag, "`\""), "\"", "", -1), " ")
	tags := make([][]string, 0)
	if len(prts) > 0 {
		for _, prt := range prts {
			sprts := strings.Split(prt, ":")
			tags = append(tags, sprts)
		}
	}

	return tags
}

//column has only one possible value
func columnName(tag string) string {
	values := tagValues(tag, COLUMN)
	if len(values) > 0 {
		return values[0]
	}
	return ""
}

//tabletag has 2 mandatory values eg table and alias
func tableName(tag string) (string, string) {
	values := tagValues(tag, TABLE)
	if len(values) > 1 {
		return values[0], values[1]
	}
	return "", ""
}

func foreignKey(tag string) (string, string) {
	values := tagValues(tag, FK)
	if len(values) > 1 {
		return values[0], values[1]

	}
	return "", ""
}

func linkTable(tag string) (string, string) {
	values := tagValues(tag, LINK)
	if len(values) > 1 {
		return values[0], values[1]
	}
	return "", ""
}

func tagValues(tag string, prop string) []string {
	tags := fieldTags(tag)
	for _, tag := range tags {
		if tag[0] == prop {
			values := strings.Split(tag[1], ",")
			return values
		}
	}
	return []string{}
}

func parseArray(fieldType *ast.ArrayType) string {
	// return like []byte, []time.Time, []*byte, []*sql.NullString
	var arrayType string

	switch typeToken := fieldType.Elt.(type) {
	case *ast.Ident:
		arrayType = parseIdent(typeToken)
	case *ast.SelectorExpr:
		arrayType = parseSelector(typeToken)
	case *ast.StarExpr:
		arrayType = parseStar(typeToken)
	}

	if arrayType == "" {
		return ""
	}

	return fmt.Sprintf("[]%s", arrayType)
}

func parseStar(fieldType *ast.StarExpr) string {
	// return like *bool, *time.Time, *[]byte, and other array stuff
	var starType string

	switch typeToken := fieldType.X.(type) {
	case *ast.Ident:
		starType = parseIdent(typeToken)
	case *ast.SelectorExpr:
		starType = parseSelector(typeToken)
	case *ast.ArrayType:
		starType = parseArray(typeToken)
	}

	if starType == "" {
		return ""
	}

	return fmt.Sprintf("*%s", starType)
}

func FieldOnly(prop string) string {
	propSeq := strings.Split(prop, ".")
	return propSeq[len(propSeq)-1]
}

func FieldShift(prop string) string {
	propSeq := strings.Split(prop, ".")
	return strings.Join(propSeq[1:], ".")
}

func ProxyType(prop string) string {
	propSeq := strings.Split(prop, ".")
	return strings.TrimPrefix(propSeq[0], "proxy")
}

func NativeField(field string) bool {
	if strings.Contains(field, ".") {
		return false
	}
	return true
}

func FilterNonNativeFieldsAndIDs(fields []*fieldToken) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		if NativeField(field.Name) && FieldOnly(field.Name) != "ID" {
			fts = append(fts, field)
		}
	}
	return fts
}

func FilterFieldsAndIDs(fields []*fieldToken, entityType string) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		if entityType == ProxyType(field.Name) && FieldOnly(field.Name) != "ID" {
			fts = append(fts, field)
		}
	}
	return fts
}

func FilterFields(fields []*fieldToken, entityType string) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		if entityType == ProxyType(field.Name) {
			fts = append(fts, field)
		}
	}
	return fts
}

func AttributeAlias(rels []*relation) string {
	for _, rel := range rels {
		if rel.Type == "Attribute" {
			return rel.Alias
		}
	}
	return ""
}

//lookup first part field Name (check '.') in relations referencing Field =>switch global alias for relation alias
func UpdateAlias(tok *structToken, field, alias string) string {
	if !strings.Contains(field, ".") {
		return alias
	}
	prts := strings.Split(field, ".")
	lookup := strings.TrimPrefix(prts[0], "proxy")
	var result string
	if result = lookUpAlias(tok.OneToMany, lookup, prts); result != "" {
		return result
	}
	if result = lookUpAlias(tok.ManyToOne, lookup, prts); result != "" {
		return result
	}
	if result = lookUpAlias(tok.ManyToMany, lookup, prts); result != "" {
		return result
	}
	return alias
}

func lookUpAlias(rels []*relation, lookup string, prts []string) string {
	for _, rel := range rels {
		if len(rel.ManyToOne) > 0 && len(prts) > 2 {
			for _, rel2 := range rel.ManyToOne {
				if prts[1] == rel2.Field {
					return rel2.Alias
				}
			}
		}
		if lookup == rel.Field {
			return rel.Alias
		}
	}
	return ""
}

func FilterSliceAndID(fields []*fieldToken) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		if field.Name == "ID" || strings.Contains(field.Name, "proxy") {
			continue
		}
		prts := strings.Split(field.Name, ".")
		if len(prts) > 1 {
			if prts[1] == "ID" {
				fts = append(fts, field)
				continue
			}
			continue
		}
		fts = append(fts, field)
	}
	return fts
}

func SwitchToFK(field *fieldToken) string {
	prts := strings.Split(field.Name, ".")
	if len(prts) > 1 {
		if prts[1] == "ID" {
			return field.Fk
		}
	}
	return field.Column
}

func OnlyIDFields(fields []*fieldToken) []*fieldToken {
	rets := make([]*fieldToken, 0)
	for _, fld := range fields {
		nm := FieldShift(fld.Name)
		if strings.Contains(nm, ".") {
			if FieldOnly(nm) == "ID" {
				rets = append(rets, fld)
			}
		} else if !strings.Contains(nm, "ID") {
			rets = append(rets, fld)
		}

	}
	return rets
}

func genFile(outFile, pkg string, unexport bool, toks []*structToken) error {
	if len(toks) < 1 {
		return errors.New("no structs found")
	}

	fout, err := os.Create(outFile)
	if err != nil {
		return err
	}
	defer fout.Close()

	data := struct {
		PackageName string
		Tokens      []*structToken
		Visibility  string
	}{
		PackageName: pkg,
		Visibility:  "L",
		Tokens:      toks,
	}

	if unexport {
		// func name will be scanFoo instead of ScanFoo
		data.Visibility = "l"
	}

	tmpl, err := Asset("tmpl/orm.tmpl")
	if err != nil {
		return err
	}

	fnMap := template.FuncMap{"title": strings.Title,
		"isint": func(tpe string) bool {
			if tpe == "int" {
				return true
			}
			return false
		},
		"isbool": func(tpe string) bool {
			if tpe == "bool" {
				return true
			}
			return false
		},
		"isstring": func(tpe string) bool {
			if tpe == "string" {
				return true
			}
			return false
		},
		"istime": func(tpe string) bool {
			if tpe == "time.Time" {
				return true
			}
			return false
		},
		"switch2fk":         SwitchToFK,
		"ffiltersliceandid": FilterSliceAndID,
		"updatealias":       UpdateAlias,
		"ffilternatid":      FilterNonNativeFieldsAndIDs,
		"ffilterid":         FilterFieldsAndIDs,
		"field":             FieldOnly,
		"fieldshift":        FieldShift,
		"proxy":             ProxyType,
		"ffilter":           FilterFields,
		"native":            NativeField,
		"attributealias":    AttributeAlias,
		"ffilterforids":     OnlyIDFields,
		"plus1": func(x int) int {
			return x + 1
		},
		"min1": func(x int) int {
			return x - 1
		}}
	scansTmpl, err := template.New("scans").Funcs(fnMap).Parse(string(tmpl))
	if err != nil {
		return err
	}

	if err := scansTmpl.Execute(fout, data); err != nil {
		return err
	}

	return nil
}
