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
        //go:generate ormgen $GOFILE
`
	COLUMN     = "column"
	TABLE      = "table"
	FK         = "fk"
	LINK       = "link"
	MANYTOMANY = "manytomany"
	ONETOMANY  = "onetomany"
	MANYTOONE  = "manytoone"
)

type fieldToken struct {
	Relation    *relation
	Name        string
	Type        string
	Slice       bool
	Column      string
	Table       string
	Alias       string
	Fk          string
	Link        string
	LinkAlias   string
	RelAlias    string
	IsLinkField bool
}

type structToken struct {
	IdColumn  string
	Name      string
	Fields    []*fieldToken
	Composite bool
	Relations []*relation
	Table     string
	Alias     string
}

type context struct {
	Name      string
	IsLink    bool
	IsSlice   bool
	LinkAlias string
	Level     int
	Link      string
	Alias     string
	Fk        string
}

type relation struct {
	RelationType   string
	FieldName      string
	Proxy          bool
	ProxyFieldName string
	ParentRelation *relation
	Owner          *structToken
	Source         string
	SourceType     string
	TargetType     string
	LinkField      string
	LinkType       string
	Field          string
	Type           string
	Table          string
	LinkTable      string
	LinkAlias      string
	Alias          string
	From           string
	To             string
	LinkFrom       string
	LinkTo         string
	IsAttribute    bool
	IsRoot         bool
	Fields         []*fieldToken
	LinkRelation   bool
}

func (r *relation) Parent() *relation {
	if r.ParentRelation == nil {
		return &relation{Alias: r.Owner.Alias, Type: r.Owner.Name, IsRoot: true}
	}
	return r.ParentRelation
}

func (r *relation) ParentName() string {
	return r.Parent().Type
}

func (r *relation) ParentAlias() string {
	if r.LinkRelation {
		return r.Parent().LinkAlias
	}
	return r.Parent().Alias
}

func (r *relation) IsManyToMany() bool {
	return r.RelationType == MANYTOMANY
}

func (r *relation) IsOneToMany() bool {
	return r.RelationType == ONETOMANY
}

func (r *relation) IsManyToOne() bool {
	return r.RelationType == MANYTOONE
}

func (r *relation) Equals(rel *relation) bool {
	if r.RelationType == rel.RelationType && r.Type == rel.Type && r.Field == rel.Field && r.Table == rel.Table && rel.FieldName == r.FieldName {
		return true
	}
	return false
}

var (
	inputLevel    int
	maxLevel      int
	structLookup  map[string]*structToken
	newStructToks []*structToken
	baseFileName  string
	packageName   string
	unExport      bool
)

func main() {
	log.SetFlags(0)

	max := flag.Int("l", 3, "max recursion level")
	outFilename := flag.String("o", "ormgen", "")
	packName := flag.String("p", "current directory", "")
	unexport := flag.Bool("u", false, "")
	whitelist := flag.String("w", "", "")
	version := flag.Bool("v", false, "")
	help := flag.Bool("h", false, "")
	flag.IntVar(max, "level", 3, "max recursion level")
	flag.StringVar(outFilename, "output", "ormgen", "")
	flag.StringVar(packName, "package", "current directory", "")
	flag.BoolVar(unexport, "unexport", false, "")
	flag.StringVar(whitelist, "whitelist", "", "")
	flag.BoolVar(version, "version", false, "")
	flag.BoolVar(help, "help", false, "")
	flag.Usage = func() { log.Println(usageText) } // call on flag error
	flag.Parse()

	baseFileName = *outFilename
	unExport = *unexport
	inputLevel = *max

	if *help {
		// not an error, send to stdout
		// that way people can: scaneo -h | less
		fmt.Println(usageText)
		return
	}

	if *version {
		fmt.Println("ormgen version 0.0.1")
		return
	}

	if *packName == "current directory" {
		wd, err := os.Getwd()
		if err != nil {
			log.Fatal("couldn't get working directory:", err)
		}

		*packName = filepath.Base(wd)
		packageName = *packName
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

	generate(inputLevel, structToks, "tmpl/orm.tmpl", "main")

	for i := 0; i <= inputLevel; i++ {
		generate(i, structToks, "tmpl/ormlevel.tmpl", "level")
	}
}

func generate(mLevel int, structs []*structToken, tmpl, tmplName string) {
	newStructToks = make([]*structToken, 0)
	maxLevel = mLevel

	for _, structTk := range structs {
		newStructTk := &structToken{
			Name:      structTk.Name,
			Fields:    make([]*fieldToken, 0),
			Composite: false,
			Table:     structTk.Table,
			Alias:     structTk.Alias,
			IdColumn:  structTk.IdColumn,
		}
		parse(newStructTk, structTk, nil, nil)
		checkDuplicateAlias(newStructTk)
		//log.Printf("============entity name : %s =================== \n", newStructTk.Name)
		//sep := ""
		/*for _, rel := range newStructTk.RootRelations() {
			printRelationTree(sep, rel, newStructTk.Relations)
		}*/
		proxyfy(newStructTk)
		newStructToks = append(newStructToks, newStructTk)
	}

	fileName := fmt.Sprintf("%s_%d.go", baseFileName, mLevel)

	if tmplName == "main" {
		fileName = fmt.Sprintf("%s_main.go", baseFileName)
	}

	if err := genFile(fileName, newStructToks, mLevel, tmpl, tmplName); err != nil {
		log.Fatal("couldn't generate file:", err)
	}
}

func printRelationTree(sep string, rel *relation, rels []*relation) {
	log.Printf("%s %s type:%s\n", sep, rel.RelationType, rel.Type)
	srels := rel.SubRelations(rels)
	sep = sep + "----"
	for _, subrel := range srels {
		printRelationTree(sep, subrel, rels)
	}
}

func (s *structToken) RootRelations() []*relation {
	res := make([]*relation, 0)
	for _, rel := range s.Relations {
		if rel.ParentRelation == nil {
			res = append(res, rel)
		}
	}
	return res
}

func (r *relation) Root() *relation {
	previous := r
	lr := r
	for lr.Parent() != nil {
		parent := lr.Parent()
		if parent.IsRoot {
			return previous
		}
		previous = lr
		lr = parent
	}
	return nil
}

func (r *relation) SubRelations(rels []*relation) []*relation {
	res := make([]*relation, 0)
	for _, rel := range rels {
		if r.Equals(rel.Parent()) {
			res = append(res, rel)
		}
	}
	return res
}

func (r *relation) AllSubRelations(rels []*relation) []*relation {
	lookup := r.SubRelations(rels)
	children := make([]*relation, len(lookup))
	copy(children, lookup)
	allSubRelations(rels, &children, lookup)
	return children
}

func allSubRelations(rels []*relation, children *[]*relation, lookup []*relation) {
	l2 := make([]*relation, 0)
	ch := *children
	for _, child := range lookup {
		newLookup := child.SubRelations(rels)
		ch = append(ch, newLookup...)
		l2 = append(l2, newLookup...)

	}
	if len(l2) > 0 {
		*children = ch
		allSubRelations(rels, children, l2)
	}
}

func findRelation(field string, rels []*relation) *relation {
	for _, rel := range rels {
		if rel.FieldName == field {
			return rel
		}
	}
	return nil
}

func proxyfyPath(path string, rels []*relation) string {
	prts := strings.Split(path, ".")
	edit := make([]string, len(prts))
	copy(edit, prts)
	prtsLen := len(prts)
	for i := prtsLen; i >= 0; i-- {
		var lookup string
		if i == 0 {
			lookup = prts[i]
		} else {
			lookup = strings.Join(prts[:i], ".")
		}
		fRel := findRelation(lookup, rels)
		if fRel.IsManyToMany() || fRel.IsOneToMany() {
			var st int
			if i == 0 {
				st = 0
			} else {
				st = i - 1
			}
			for j := st; j >= 0; j-- {
				if strings.HasPrefix(edit[j], "proxy") {
					continue
				}
				edit[j] = "proxy" + edit[j]
			}

		}
	}
	return strings.Join(edit, ".")

}

func proxyfy(src *structToken) {
	for s, rel := range src.Relations {
		if rel.IsManyToMany() || rel.IsOneToMany() {
			for _, field := range src.Fields {
				prts := strings.Split(field.Name, ".")
				lPrts := len(prts) - 1
				for i := lPrts; i >= 0; i-- {
					if prts[i] == rel.Field {
						for j := i; j >= 0; j-- {
							if strings.HasPrefix(prts[j], "proxy") {
								continue
							}
							prts[j] = "proxy" + prts[j]
						}
					}
				}
				field.Name = strings.Join(prts, ".")
			}
		}
		src.Relations[s].ProxyFieldName = proxyfyPath(src.Relations[s].FieldName, src.Relations)
		proxyPrts := strings.Split(src.Relations[s].ProxyFieldName, ".")
		if strings.HasPrefix(proxyPrts[len(proxyPrts)-1], "proxy") {
			src.Relations[s].Proxy = true
		}
	}
}

func parse(res *structToken, src *structToken, pRel *relation, ctx *context) {
	var baseName string
	if ctx != nil {
		ctx.Level++
		baseName = ctx.Name
	}
	for _, field := range src.Fields {
		isSlice, embeddedStruct := isRelation(field)
		name := field.Name
		fk := field.Fk
		alias := field.Alias
		lvl := 0
		if ctx != nil {
			alias = ctx.Alias
			name = baseName + "." + name
			lvl = ctx.Level
			if ctx.IsLink {
				//alias = ctx.LinkAlias
				if field.Link != "" && field.Link == pRel.SourceType {
					pRel.To = field.Column
				}
				if field.Link != "" && field.Link == pRel.TargetType {
					pRel.LinkFrom = field.Column
					pRel.Alias = field.LinkAlias
				}
			}
		}
		if embeddedStruct == "" {
			if ctx != nil && ctx.IsLink {
				if strings.Contains(field.Name, "ID") {
					continue
				}
			}
			nf := &fieldToken{
				Relation: pRel,
				Name:     name,
				Type:     field.Type,
				Slice:    isSlice,
				Column:   field.Column,
				Table:    field.Table,
				Alias:    alias,
				Fk:       fk,
			}
			if field.Name == "ID" && ctx != nil {
				nf.Alias = ctx.Alias
				nf.Fk = ctx.Fk
			}
			if ctx != nil && ctx.IsLink {
				nf.IsLinkField = true
			}
			res.Fields = append(res.Fields, nf)
			/*if pRel != nil {
				pRel.Fields = append(pRel.Fields, nf)
			}*/
			continue
		}
		//is a relation field
		res.Composite = true
		if ctx != nil {
			if ctx.Level >= maxLevel /*&& !ctx.IsLink*/ {
				continue
			}
		} else if maxLevel == 0 {
			continue
		}
		nCtx := &context{Name: name, Level: lvl, Fk: fk, Alias: field.RelAlias, IsLink: false}
		emb := structLookup[embeddedStruct]
		rel := &relation{
			Field:       field.Name,
			Table:       emb.Table,
			Alias:       field.RelAlias,
			Type:        embeddedStruct,
			IsAttribute: false,
			Owner:       res,
			FieldName:   name,
		}
		if pRel != nil {
			rel.ParentRelation = pRel
		}
		if embeddedStruct == "Attribute" {
			rel.IsAttribute = true
		}
		if ctx != nil && ctx.IsLink {
			rel.LinkRelation = true
		}
		if isSlice {
			//manytomany
			nCtx.IsSlice = true
			if field.Link != "" {
				//log.Printf("adding [[MANYTOMANY]]: %s \n", field.Name)
				lnCtx := new(context)
				lnCtx.Name = nCtx.Name
				lnCtx.Level = nCtx.Level
				lnCtx.IsLink = true
				lnCtx.Link = field.Link
				linkStruct := structLookup[field.Link]
				rel.RelationType = MANYTOMANY
				rel.LinkAlias = field.LinkAlias
				rel.From = src.IdColumn
				rel.LinkTo = emb.IdColumn
				rel.LinkType = field.Link
				rel.LinkField = field.Link
				rel.SourceType = src.Name
				rel.TargetType = emb.Name
				rel.LinkTable = linkStruct.Table
				lnCtx.LinkAlias = field.LinkAlias
				res.Relations = append(res.Relations, rel)
				parse(res, linkStruct, rel, lnCtx)
			} else { //onetomany
				//log.Printf("adding [[ONETOMANY]]: %s \n", field.Name)
				rel.From = src.IdColumn
				rel.To = fk
				rel.RelationType = ONETOMANY
				res.Relations = append(res.Relations, rel)
			}
		} else { // manytoone
			//log.Printf("adding [[MANYTOONE]]: %s \n", field.Name)
			rel.From = fk
			rel.To = emb.IdColumn
			rel.RelationType = MANYTOONE
			res.Relations = append(res.Relations, rel)
		}
		parse(res, emb, rel, nCtx)
	}
}

var aliases map[string]int

func checkDuplicateAlias(s *structToken) {
	aliases = make(map[string]int, len(s.Relations)+10)
	checkAlias(s.Relations)
}

func checkAlias(rels []*relation) {
	for _, rel := range rels {
		if rel.Alias == "" {
			//log.Printf("relation %s of type %s has no alias \n", rel.FieldName, rel.RelationType)
			continue
		}
		clearAlias(rel)
		clearLinkAlias(rel)
	}
}

func clearAlias(rel *relation) {
	if _, ok := aliases[rel.Alias]; ok {
		//already exists
		rel.Alias = "x" + rel.Alias
	} else {
		aliases[rel.Alias] = 1
		return
	}
	clearAlias(rel)
	aliases[rel.Alias] = 1
}

func clearLinkAlias(rel *relation) {
	if _, ok := aliases[rel.LinkAlias]; ok {
		//already exists
		rel.LinkAlias = "x" + rel.LinkAlias
	} else {
		aliases[rel.LinkAlias] = 1
		return
	}
	clearLinkAlias(rel)
	aliases[rel.LinkAlias] = 1
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

func genFile(outFile string, toks []*structToken, lvl int, tmpl, tmplName string) error {
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
		Level       int
		Levels      []int
		AtMaxLevel  bool
	}{
		PackageName: packageName,
		Visibility:  "L",
		Tokens:      toks,
		Level:       lvl,
		Levels:      make([]int, lvl+1),
		AtMaxLevel:  lvl == inputLevel,
	}

	if unExport {
		// func name will be scanFoo instead of ScanFoo
		data.Visibility = "l"
	}

	mTmpl, err := Asset(tmpl)
	if err != nil {
		return err
	}

	scansTmpl, err := template.New(tmplName).Funcs(fMap).Parse(string(mTmpl))
	if err != nil {
		return err
	}

	if err := scansTmpl.Execute(fout, data); err != nil {
		return err
	}

	return nil
}
