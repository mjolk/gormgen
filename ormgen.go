/**
 * File   : ormgen.go
 * License: MIT/X11
 * Author : Dries Pauwels <2mjolk@gmail.com>
 * Date   : zo 24 feb 2019 01:37
 */
package main

//go:generate go-bindata tmpl/

import (
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"sort"
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

    -d 
      Select sql dialect, mysql, postgresql, cockroachdb

    -change
      Enable changefeed event extensions

    -attr
      Enable attribute extension

    -v, -version
        Print version and exit.

    -h, -help
        Print help and exit.
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

	POSTGRESQL = "postgresql"
	MYSQL      = "mysql"
	COCKROACH  = "csql"
)

type SQLIndex struct {
	Name    string   `json:"name"`
	Unique  bool     `json:"unique"`
	Columns []string `json:"columns"`
}

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
	Normalize   bool   `json:"normalize"`
	Embedded    bool   `json:"embedded"`
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
	CompositeKey []string   `json:"compositeKey"`
	Query        bool       `json:"query"`
	ChangeSet    []string   `json:"changeset"`
	SQLIndex     []SQLIndex `json:"indexes"`
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
	Embedded  bool
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
	Embedded       bool
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
	if r.RelationType == rel.RelationType &&
		r.Type == rel.Type &&
		r.Field == rel.Field &&
		r.Table == rel.Table &&
		rel.FieldName == r.FieldName {
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
	etype         = "go"
	changeFeed    = false
	dialect       = "postgresql"
	attributes    = false
	schema        string
)

func main() {
	log.SetFlags(0)

	max := flag.Int("l", 3, "max recursion level")
	outFilename := flag.String("o", "gormgen", "")
	version := flag.Bool("v", false, "")
	help := flag.Bool("h", false, "")
	otype := flag.String("t", "go", "entity type")
	ch := flag.Bool("change", false, "generate change event system")
	d := flag.String("d", POSTGRESQL, "select sql dialect")
	attr := flag.Bool("attr", false, "enable attributes")
	flag.IntVar(max, "level", 3, "max recursion level")
	flag.StringVar(otype, "etype", "go", "")
	flag.StringVar(outFilename, "output", "ormgen", "")
	flag.BoolVar(version, "version", false, "")
	flag.BoolVar(help, "help", false, "")
	flag.Usage = func() { log.Println(usageText) } // call on flag error
	flag.Parse()

	changeFeed = *ch
	dialect = *d
	attributes = *attr

	baseFileName = *outFilename
	inputLevel = *max

	if *help {
		fmt.Println(usageText)
		return
	}

	if *version {
		fmt.Println("ormgen version 1.0.1")
		return
	}

	wd, err := os.Getwd()
	if err != nil {
		log.Fatal("couldn't get working directory:", err)
	}

	packageName = filepath.Base(wd)

	etype = *otype

	var structToks []*structToken

	structToks, err = loadJSON()
	if err != nil {
		log.Fatalf("error loading json config: %s\n", err)
	}

	generate(0, []*structToken{}, "tmpl/database.tmpl", "driver")
	generate(0, []*structToken{}, "tmpl/initdb.tmpl", "dbinit")
	if changeFeed {
		generate(inputLevel, structToks, "tmpl/ormchangeset.tmpl", "changeset")
	}
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

type byDependency func(s1, s2 *structToken) bool

type depSorter struct {
	tokens []*structToken
	by     byDependency
}

func (by byDependency) Sort(tks []*structToken) {
	ds := &depSorter{
		tokens: tks,
		by:     by,
	}
	sort.Sort(ds)
}

func (d *depSorter) Len() int {
	return len(d.tokens)
}

func (d *depSorter) Swap(i, j int) {
	d.tokens[i], d.tokens[j] = d.tokens[j], d.tokens[i]
}

func (d *depSorter) Less(i, j int) bool {
	return d.by(d.tokens[i], d.tokens[j])
}

func isInRelations(s1, s2 *structToken) bool {
	for _, rel := range s2.RootRelations() {
		if s1.Name == rel.Type {
			return true
		}
	}
	return false
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
			SQLIndex:     structTk.SQLIndex,
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
	byDependency(isInRelations).Sort(newStructToks)
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
	case "dbinit":
	case "sqlcontext":
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
	prtsLen := len(prts)
	edit := make([]string, prtsLen)
	copy(edit, prts)
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
			if ctx != nil && ctx.IsLink { //preserve some fields linktable
				if strings.Contains(field.Name, "ID") {
					if field.Name == "ID" {
						name = baseName + ".LinkID"
					} else {
						continue
					}
				}
				switch field.Name {
				case "Updated":
					name = baseName + ".LinkUpdated"
				case "Version":
					name = baseName + ".LinkVersion"
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
			if ctx != nil {
				nf.Embedded = ctx.Embedded
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
		if (ctx != nil && ctx.Level >= maxLevel /*&& !ctx.IsLink*/) || maxLevel == 0 {
			continue
		}
		emb := structLookup[embeddedStruct]
		nCtx := &context{
			Name:     name,
			Level:    lvl,
			Fk:       fk,
			Alias:    field.RelAlias,
			IsLink:   false,
			Embedded: field.Embedded,
		}
		rel := &relation{
			Field:     field.Name,
			Table:     emb.Table,
			Alias:     field.RelAlias,
			Type:      embeddedStruct,
			Owner:     res,
			FieldName: name,
			SQLType:   field.SQLType,
			Primary:   field.Primary,
			NotNull:   field.NotNull,
			Unique:    field.Unique,
			Default:   field.Default,
			Delete:    field.Delete,
			Lazy:      field.Lazy,
			JSON:      field.JSON,
			Embedded:  field.Embedded,
		}
		if pRel != nil {
			rel.ParentRelation = pRel
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

type ByFieldNameLength []*relation

func (rs ByFieldNameLength) Len() int           { return len(rs) }
func (rs ByFieldNameLength) Swap(i, j int)      { rs[i], rs[j] = rs[j], rs[i] }
func (rs ByFieldNameLength) Less(i, j int) bool { return len(rs[i].FieldName) < len(rs[j].FieldName) }

var aliases map[string]int

func checkDuplicateAlias(s *structToken) {
	aliases = make(map[string]int, len(s.relations)+10)
	checkAlias(s.relations)
}

func checkAlias(rels []*relation) {
	cpRels := make([]*relation, len(rels))
	copy(cpRels, rels)
	sort.Sort(ByFieldNameLength(cpRels))
	for _, rel := range cpRels {
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

func genFile(outFile string, toks []*structToken, lvl int, tmpl, tmplName string) error {

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
		Dialect     string
		ChangeFeed  bool
		Schema      string
	}{
		PackageName: packageName,
		Visibility:  "L",
		Tokens:      toks,
		Level:       lvl,
		Levels:      make([]int, lvl+1),
		AtMaxLevel:  lvl == inputLevel,
		Etype:       etype,
		Dialect:     dialect,
		ChangeFeed:  changeFeed,
		Schema:      schema,
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
