package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"text/template"
)

var (
	fMap template.FuncMap = template.FuncMap{
		"title": strings.Title,
		"isint": func(tpe string) bool {
			if tpe == "int64" {
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
		"isbyte": func(tpe string) bool {
			if tpe == "[]byte" {
				return true
			}
			return false
		},
		"isuuid": func(tpe string) bool {
			if tpe == "uuid.UUID" {
				return true
			}
			return false
		},
		"typeprefix":        TypePrefix,
		"reproxyfy":         Reproxyfy,
		"tolower":           strings.ToLower,
		"linkprops":         LinkPropertiesFor,
		"linkrels":          LinkRelationsFor,
		"itoa":              strconv.Itoa,
		"structtok":         FindStructToken,
		"proxysubrels":      ProxyLinkRelations,
		"proxyshift":        FieldProxyShift,
		"changefields":      ChangeFields,
		"evdatafields":      FilterEventDataFields,
		"isobjectfield":     IsObjectField,
		"listfields":        Fields,
		"checkparent":       CheckParent,
		"norel":             FilterRelations,
		"tovar":             FieldToVariableName,
		"relarg":            RelationLevel,
		"switch2fk":         SwitchToFK,
		"ffiltersliceandid": FilterSliceAndID,
		"ffilterslice":      FilterSlice,
		"updatealias":       UpdateAlias2,
		"ffilternatid":      FilterNonNativeFieldsAndIDs,
		"ffilterid":         FilterFieldsAndIDs,
		"field":             FieldOnly,
		"fieldshift":        FieldShift,
		"proxy":             ProxyType,
		"ffilter":           FilterFields,
		"native":            OnlyNativeField,
		"attributealias":    AttributeAlias,
		"ffilterforids":     OnlyIDFields,
		"isreference":       FieldIsReference,
		"referenceonly":     ReferenceOnly,
		"deproxyfy":         DeProxyfyFieldName,
		"lookuplink":        LookupLink,
		"lookupidtype":      LookupIDType,
		"nopointer":         RemovePointer,
		"torepeat":          SliceToRepeat,
		"isidfield":         IsIDField,
		"versioned":         Versioned,
		"islinked":          LinkedEntity,
		"hassqlindexes":     HasSQLIndexes,
		"relationargs":      RelationArguments,
		"plus1": func(x int) int {
			return x + 1
		},
		"plus2": func(x int) int {
			return x + 2
		},
		"min1": func(x int) int {
			return x - 1
		},
		"psql":      IsPql,
		"msql":      IsMysql,
		"csql":      IsCockroach,
		"setschema": SetSchema,
		"efields":   EmbedFields,
		"embedid":   EmbedIdColumnType,
	}
)

func EmbedIdColumnType(sqlType string) string {
	if strings.Contains(sqlType, "serial") ||
		strings.Contains(sqlType, "identity") ||
		strings.Contains(sqlType, "auto") {
		return sqlType[0:strings.Index(sqlType, " ")]
	}
	return sqlType
}

type RelationArgs struct {
	Field   *fieldToken
	Dialect string
}

func RelationArguments(field *fieldToken, dialect string) RelationArgs {
	return RelationArgs{field, dialect}
}

type EFieldsArgs struct {
	Token *structToken
	Field *fieldToken
}

func EmbedFields(t *structToken, f *fieldToken) EFieldsArgs {
	return EFieldsArgs{t, f}
}

func SetSchema(dialect string) string {
	if dialect == "postgresql" {
		return "%[1]s."
	}
	return ""
}

func IsPql(dialect string) bool {
	return dialect == "postgresql"
}

func IsMysql(dialect string) bool {
	return dialect == "mysql"
}

func IsCockroach(dialect string) bool {
	return dialect == "cockroachdb"
}

func HasSQLIndexes(token *structToken) bool {
	return len(token.SQLIndex) > 0
}

func LinkedEntity(token *structToken) bool {
	for _, f := range token.Fields {
		if f.Name == "LinkID" {
			return true
		}
	}
	return false
}

func Versioned(token *structToken) bool {
	for _, f := range token.Fields {
		if f.Name == "Version" {
			return true
		}
	}
	return false
}

func FilterEventDataFields(fields []*fieldToken) []*fieldToken {
	evFields := make([]*fieldToken, 0)
	for _, field := range fields {
		if strings.Contains(field.Type, "[]*") {
			continue
		}
		evFields = append(evFields, field)
	}
	return evFields
}

func IsIDField(name string) bool {
	return strings.Contains(name, "ID")
}

func SliceToRepeat(tpe string) string {
	ret := strings.Replace(tpe, "[]", "repeated ", -1)
	return strings.Replace(ret, "*", "", -1)
}

func RemovePointer(tpe string) string {
	return strings.TrimPrefix(tpe, "*")
}

func IsObjectField(field *fieldToken) bool {
	if strings.Contains(field.Type, "*") {
		return true
	}
	return false
}

func ChangeFields(t *structToken) []*fieldToken {
	filtered := make([]*fieldToken, 0)
	for _, field := range t.Fields {
		for _, changeField := range t.ChangeSet {
			if field.Name == changeField {
				filtered = append(filtered, field)
			}
		}
	}
	return filtered
}

func filterQueries(toks []*structToken) []*structToken {
	filtered := make([]*structToken, 0)
	for _, tok := range toks {
		if tok.Query {
			continue
		}
		filtered = append(filtered, tok)
	}
	return filtered
}

func LookupIDType(e string) string {
	entity := strings.TrimPrefix(e, "*")
	l, ok := structLookup[entity]
	if !ok {
		panic("fatal error")
	}
	for _, field := range l.Fields {
		if field.Name == "ID" {
			return field.Type
		}
	}
	return "int64"
}

func LookupLink(link string) string {
	l, ok := structLookup[link]
	if !ok {
		return ""
	}
	return fmt.Sprintf(" references %%[1]s.%s(%s)", l.Table, l.IDColumn)
}

func TypePrefix(r *relation, baseType string) string {
	var parent *relation
	var skip int
	prefix := "new"
	lstType := r.Type
	if baseType != "" {
		if strings.Contains(baseType, ".") {
			basePrts := strings.Split(baseType, ".")
			lstType = basePrts[0]
		} else {
			lstType = baseType
		}
		skip++
	}
	lr := r
	for !lr.Parent().IsRoot {
		parent = lr.Parent()
		if parent.IsManyToMany() || parent.IsOneToMany() {
			if parent.Type == lstType {
				if skip > 0 {
					skip--
				} else {
					prefix = prefix + "_"
				}
			}
		}
		lr = parent
	}
	return prefix
}

func Reproxyfy(name string) string {
	prts := strings.Split(name, ".")
	for i, prt := range prts {
		if !strings.HasPrefix(prt, "proxy") {
			prts[i] = "proxy" + prts[i]
		}
	}
	return strings.Join(prts, ".")
}

func DeProxyfyFieldName(name string) string {
	return strings.Replace(name, "proxy", "", -1)
}

func FieldIsReference(ft *fieldToken) bool {
	if strings.Contains(ft.Name, ".") {
		return true
	}
	return false
}

func ReferenceOnly(ftName string) string {
	ftNamePrts := strings.Split(ftName, ".")
	return strings.Join(ftNamePrts[:len(ftNamePrts)-1], ".")
}

//special case with fields on linktable many2many, fields are not present on source
func LinkPropertiesFor(tpe string) []*fieldToken {
	fieldLookup := "proxy" + tpe + "s"
	fieldsFound := make(map[string]int, 0)
	addFields := make([]*fieldToken, 0)
	for _, tk := range newStructToks {
		for _, sfts := range tk.Fields {
			if sfts.IsLinkField {
				if strings.Contains(sfts.Name, fieldLookup) {
					fldPrts := strings.Split(sfts.Name, ".")
					actual := fldPrts[len(fldPrts)-1]
					if _, ok := fieldsFound[actual]; !ok {
						fieldsFound[actual] = 1
						addFields = append(addFields, sfts)
					}
				}
			}
		}
	}
	return addFields
}

func LinkRelationsFor(tpe string) []*relation {
	relLookup := tpe + "s"
	relsFound := make(map[string]int, 0)
	addRelations := make([]*relation, 0)
	for _, tk := range newStructToks {
		for _, relts := range tk.relations {
			if relts.LinkRelation {
				if strings.Contains(relts.FieldName, relLookup) {
					rlPrts := strings.Split(relts.FieldName, ".")
					actual := rlPrts[len(rlPrts)-1]
					if _, ok := relsFound[actual]; !ok {
						relsFound[actual] = 1
						addRelations = append(addRelations, relts)
					}
				}
			}
		}
	}
	return addRelations
}

func FindStructToken(tpe string) *structToken {
	for _, tok := range newStructToks {
		if tok.Name == tpe {
			return tok
		}
	}
	return nil
}

func ProxyLinkRelations(tpe string) []*relation {
	fts := make([]*relation, 0)
	for _, tok := range newStructToks {
		for _, rel := range tok.relations {
			if rel.IsManyToMany() {
				if rel.Type == tpe {
					for _, crel := range rel.SubRelations(tok.relations) {
						if crel.LinkRelation {
							dup := false
							for _, lrel := range fts {
								if lrel.Type == crel.Type {
									dup = true
								}
							}
							if !dup {
								fts = append(fts, crel)
							}
						}
					}
				}
			}
		}
	}
	return fts
}

func FieldProxyShift(prop string) string {
	propPrts := strings.Split(prop, ".")
	propLen := len(propPrts) - 1
	fieldPath := make([]string, 0)
	for i := propLen; i >= 0; i-- {
		if strings.Contains(propPrts[i], "proxy") {
			continue
		}
		fieldPath = append(fieldPath, propPrts[i])
	}
	return strings.Join(reverseList(fieldPath), ".")
}

func Fields(rel *relation) string {
	pth := make([]string, 0)
	if rel != nil {
		if rel.IsManyToOne() {
			pth = append(pth, rel.Field)
			hrel := rel
			for !hrel.Parent().IsRoot {
				if hrel.Parent().IsManyToMany() || hrel.Parent().IsOneToMany() {
					pth = append(pth, hrel.Parent().Type)
					return strings.Join(reverseList(pth), ".")
				}
				pth = append(pth, hrel.Parent().Field)
				hrel = hrel.Parent()
			}
		}
	}
	return ""
}

func CheckParent(rel *relation) bool {
	if rel != nil {
		if !rel.IsManyToOne() {
			return true
		}
	}
	return false
}

func FilterRelations(fields []*fieldToken, prefix string) []string {
	fts := make([]string, 0)
	prefixLen := len(strings.Split(prefix, "."))
	for _, field := range fields {
		fldPrts := strings.Split(field.Name, ".")
		fldLen := len(fldPrts)
		if strings.HasPrefix(field.Name, prefix) && !hasChildLists(prefixLen, fldPrts) {
			if !strings.HasPrefix(fldPrts[fldLen-2], "proxy") {
				add := strings.Join(fldPrts[:fldLen-1], ".")
				nw := true
				for _, added := range fts {
					if added == add {
						nw = false
					}
				}
				if nw {
					fts = append(fts, add)
				}
			} else {
				fts = append(fts, field.Name)
			}
		}
	}
	return fts
}

func hasChildLists(prefixLen int, field []string) bool {
	lField := len(field)
	if prefixLen < lField {
		for i := prefixLen; i < lField; i++ {
			if strings.Contains(field[i], "proxy") {
				return true
			}
		}
	}
	return false
}

func FieldToVariableName(field string) string {
	return strings.Replace(field, ".", "_", -1)
}

func RelationLevel(token *structToken, relation, parent, root *relation) RelationMerge {
	return RelationMerge{
		Token:          token,
		Relation:       relation,
		ParentRelation: parent,
		RootRelation:   root,
	}
}

func SwitchToFK(field *fieldToken) string {
	prts := strings.Split(field.Name, ".")
	if len(prts) > 1 {
		if prts[1] == "ID" {
			//log.Printf("switchtofk field: %s fk: %s\n", field.Name, field.Fk)
			return field.Fk
		}
	}
	return field.Column
}

func FilterSlice(fields []*fieldToken) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		fmt.Printf("field: %s embedded: %t\n", field.Name, field.Embedded)
		if strings.Contains(field.Name, "proxy") {
			continue
		}
		prts := strings.Split(field.Name, ".")
		if len(prts) > 1 {
			if prts[1] == "ID" || field.Embedded {
				fts = append(fts, field)
				continue
			}
			continue
		}
		fts = append(fts, field)
	}
	return fts
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

func UpdateAlias2(tok *structToken, field *fieldToken, alias string) string {
	fldPrts := strings.Split(field.Name, ".")
	fldPrtsLen := len(fldPrts)
	if fldPrtsLen > 1 {
		lookup := strings.Join(fldPrts[:fldPrtsLen-1], ".")
		for _, rel := range tok.relations {
			if lookup == rel.ProxyFieldName {
				if field.IsLinkField {
					return rel.LinkAlias
				}
				return rel.Alias
			}
		}
	}
	return alias
}

//lookup first part field Name (check '.') in relations referencing Field =>switch global alias for relation alias
func UpdateAlias(tok *structToken, field, alias string) string {
	log.Printf("update alias field %s original alias %s\n", field, alias)
	if !strings.Contains(field, ".") {
		return alias
	}
	prts := strings.Split(field, ".")
	lookup := strings.TrimPrefix(prts[0], "proxy")
	var result string
	if result = lookUpAlias(tok.relations, lookup, prts); result != "" {
		return result
	}
	return alias
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

func FilterFields(fields []*fieldToken, entityType string) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, field := range fields {
		if entityType == ProxyType(field.Name) {
			fts = append(fts, field)
		}
	}
	return fts
}

func OnlyNativeField(fields []*fieldToken) []*fieldToken {
	fts := make([]*fieldToken, 0)
	for _, fld := range fields {
		if NativeField(fld.Name) {
			fts = append(fts, fld)
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

func NativeField(field string) bool {
	if strings.Contains(field, ".") {
		return false
	}
	return true
}

func lookUpAlias(rels []*relation, lookup string, prts []string) string {
	for _, rel := range rels {
		if len(prts) > 2 {
			if prts[1] == rel.Field {
				return rel.Alias
			}
		}
		if lookup == rel.Field {
			return rel.Alias
		}
	}
	return ""
}

type RelationMerge struct {
	Token          *structToken
	Relation       *relation
	ParentRelation *relation
	RootRelation   *relation
}

func reverseList(rels []string) []string {
	ln := len(rels)
	nlst := make([]string, ln)
	k := 0
	for i := ln - 1; i >= 0; i-- {
		nlst[k] = rels[i]
		k++
	}
	return nlst
}
