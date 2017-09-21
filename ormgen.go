package main

//go:generate go-bindata tmpl/

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"text/template"
)

const (
	usageText = `ORMGEN
    Generate Go code to access sql database, create config files in
    store/[schema]/
USAGE
    ormgen [options] paths...

OPTIONS
    -l 
       Set the depth to which relations can be fetched

    -t 
       Select entity type output: protobuf or regular go structs,
       pass either 'go' for regular structs or 'proto' for protobuf

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

NOTES
        //go:generate ormgen $GOFILE
`
	//COLUMN col
	COLUMN = "column"
	//TABLE table
	TABLE = "table"
	//FK foreign key
	FK = "fk"
	//LINK many 2 many next link
	LINK = "link"
	//MANYTOMANY relation type
	MANYTOMANY = "manytomany"
	//ONETOMANY realtion type
	ONETOMANY = "onetomany"
	//MANYTOONE relation type
	MANYTOONE = "manytoone"
)

type fieldToken struct {
	Relation    *relation
	Name        string `json:"name"`
	Type        string `json:"type"`
	Slice       bool
	Column      string `json:"column"`
	Table       string `json:"table"`
	Alias       string `json:"alias"`
	Fk          string `json:"fk"`
	Link        string `json:"link"`
	LinkAlias   string `json:"linkAlias"`
	RelAlias    string `json:"relAlias"`
	IsLinkField bool
	SQLType     string `json:"sqlType"`
	Primary     bool   `json:"primary"`
	NotNull     bool   `json:"notNull"`
	Unique      bool   `json:"unique"`
	Default     string `json:"default"`
	Delete      string `json:"delete"`
	IsInterface bool   `json:"interface"`
	Lazy        bool   `json:"lazy"`
	JSON        string `json:"json"`
}

type structToken struct {
	Index        int64         `json:"index"`
	IDColumn     string        `json:"idColumn"`
	Name         string        `json:"name"`
	Fields       []*fieldToken `json:"fields"`
	Composite    bool
	relations    []*relation
	Table        string `json:"table"`
	Alias        string `json:"alias"`
	LinkEntity   bool
	CompositeKey []string `json:"compositeKey"`
	Schema       string
	Query        bool
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
	SQLType        string
	Primary        bool
	NotNull        bool
	Unique         bool
	Default        string
	Delete         string
	Lazy           bool
	JSON           string
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
	structLookup  = make(map[string]*structToken, 0)
	linkStructs   = make(map[string]bool, 0)
	newStructToks []*structToken
	baseFileName  string
	packageName   string
	unExport      bool
	etype         = "go"
)

func main() {
	log.SetFlags(0)

	max := flag.Int("l", 3, "max recursion level")
	cfg := flag.String("c", "json", "configuration type")
	outFilename := flag.String("o", "gormgen", "")
	packName := flag.String("p", "current directory", "")
	unexport := flag.Bool("u", false, "")
	whitelist := flag.String("w", "", "")
	version := flag.Bool("v", false, "")
	help := flag.Bool("h", false, "")
	otype := flag.String("t", "go", "entity type")
	flag.IntVar(max, "level", 3, "max recursion level")
	flag.StringVar(otype, "etype", "go", "")
	flag.StringVar(outFilename, "output", "ormgen", "")
	flag.StringVar(packName, "package", "current directory", "")
	flag.BoolVar(unexport, "unexport", false, "")
	flag.StringVar(whitelist, "whitelist", "", "")
	flag.BoolVar(version, "version", false, "")
	flag.BoolVar(help, "help", false, "")
	flag.StringVar(cfg, "config", "json", "")
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
		fmt.Println("ormgen version 1.0.1")
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

	etype = *otype

	var structToks []*structToken
	var err error

	if *cfg == "json" {
		structToks, err = loadJSON()
		if err != nil {
			log.Fatalf("error loading json config: %s\n", err)
		}
	} else {
		structToks = loadSource(flag.Args(), *whitelist)
	}

	generate(inputLevel, structToks, "tmpl/ormchangeset.tmpl", "changeset")
	generate(inputLevel, filterQueries(structToks), "tmpl/orminit.tmpl", "init")
	generate(inputLevel, structToks, "tmpl/orm.tmpl", "main")
	if etype == "go" {
		generate(inputLevel, structToks, "tmpl/ormentity.tmpl", "entity")
	} else {
		generate(inputLevel, structToks, "tmpl/ormproto.tmpl", "proto")
	}

	for i := 0; i <= inputLevel; i++ {
		generate(i, structToks, "tmpl/ormlevel.tmpl", "level")
	}
}

func generateMetadata(src []*structToken) []*structToken {
	newStructToks = make([]*structToken, len(src))
	for i, structTk := range src {
		newStructTk := &structToken{
			Name:         structTk.Name,
			Fields:       make([]*fieldToken, 0),
			Composite:    false,
			Table:        structTk.Table,
			Alias:        structTk.Alias,
			IDColumn:     structTk.IDColumn,
			CompositeKey: structTk.CompositeKey,
			Schema:       structTk.Schema,
		}
		parse(newStructTk, structTk, nil, nil)
		checkDuplicateAlias(newStructTk)
		if ok, _ := linkStructs[newStructTk.Name]; ok {
			newStructTk.LinkEntity = true
		}
		//log.Printf("============entity name : %s =================== \n", newStructTk.Name)
		//sep := ""
		/*for _, rel := range newStructTk.RootRelations() {
			printRelationTree(sep, rel, newStructTk.Relations)
		}*/
		proxyfy(newStructTk)
		newStructToks[i] = newStructTk
	}
	return newStructToks

}

func generate(mLevel int, data []*structToken, tmpl, tmplName string) {
	maxLevel = mLevel
	ext := "go"

	if tmplName == "proto" {
		ext = "proto"
	}
	fileName := fmt.Sprintf("%s_%s_%d.%s", baseFileName, tmplName, mLevel, ext)

	switch tmplName {
	case "entity":
	case "changeset":
	case "proto":
	default:
		data = generateMetadata(data)
	}

	if err := genFile(fileName, data, mLevel, tmpl, tmplName); err != nil {
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

func (s *structToken) Relations() []*relation {
	return s.relations
}

func (s *structToken) EagerRelations() []*relation {
	return s.relations
}

func (s *structToken) RootRelations() []*relation {
	res := make([]*relation, 0)
	for _, rel := range s.relations {
		if rel.ParentRelation == nil {
			res = append(res, rel)
		}
	}
	return res
}

func (s *structToken) IDType() string {
	for _, f := range s.Fields {
		if f.Primary {
			return f.Type
		} else if f.Name == "ID" {
			return f.Type
		}
	}
	return ""
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

func (r *relation) HasChildIsList(rels []*relation) bool {
	children := r.AllSubRelations(rels)
	for _, child := range children {
		if child.IsManyToMany() || child.IsOneToMany() {
			return true
		}
	}
	return false
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
	for s, rel := range src.relations {
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
		src.relations[s].ProxyFieldName = proxyfyPath(src.relations[s].FieldName, src.relations)
		proxyPrts := strings.Split(src.relations[s].ProxyFieldName, ".")
		if strings.HasPrefix(proxyPrts[len(proxyPrts)-1], "proxy") {
			src.relations[s].Proxy = true
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
		if field.Column == "-" {
			continue
		}
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
					if field.Name == "ID" {
						name = baseName + ".LinkID"
					} else {
						continue
					}
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
				Link:     field.Link,
				SQLType:  field.SQLType,
				Primary:  field.Primary,
				NotNull:  field.NotNull,
				Unique:   field.Unique,
				Default:  field.Default,
				Delete:   field.Delete,
				Lazy:     field.Lazy,
				JSON:     field.JSON,
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
			SQLType:     field.SQLType,
			Primary:     field.Primary,
			NotNull:     field.NotNull,
			Unique:      field.Unique,
			Default:     field.Default,
			Delete:      field.Delete,
			Lazy:        field.Lazy,
			JSON:        field.JSON,
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
				linkStructs[linkStruct.Name] = true
				rel.RelationType = MANYTOMANY
				rel.LinkAlias = field.LinkAlias
				rel.From = src.IDColumn
				rel.LinkTo = emb.IDColumn
				rel.LinkType = field.Link
				rel.LinkField = field.Link
				rel.SourceType = src.Name
				rel.TargetType = emb.Name
				rel.LinkTable = linkStruct.Table
				lnCtx.LinkAlias = field.LinkAlias
				res.relations = append(res.relations, rel)
				parse(res, linkStruct, rel, lnCtx)
			} else { //onetomany
				//log.Printf("adding [[ONETOMANY]]: %s \n", field.Name)
				rel.From = src.IDColumn
				rel.To = fk
				rel.RelationType = ONETOMANY
				res.relations = append(res.relations, rel)
			}
		} else { // manytoone
			//log.Printf("adding [[MANYTOONE]]: %s \n", field.Name)
			rel.From = fk
			rel.To = emb.IDColumn
			rel.RelationType = MANYTOONE
			res.relations = append(res.relations, rel)
		}
		parse(res, emb, rel, nCtx)
	}
}

var aliases map[string]int

func checkDuplicateAlias(s *structToken) {
	aliases = make(map[string]int, len(s.relations)+10)
	checkAlias(s.relations)
}

func checkAlias(rels []*relation) {
	for _, rel := range rels {
		if rel.Alias == "" {
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
	var embeddedStruct string
	var isSlice = false
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
		Etype       string
	}{
		PackageName: packageName,
		Visibility:  "L",
		Tokens:      toks,
		Level:       lvl,
		Levels:      make([]int, lvl+1),
		AtMaxLevel:  lvl == inputLevel,
		Etype:       etype,
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
