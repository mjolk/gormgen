package main

import (
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"strings"
)

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

func loadSource(paths []string, wlist string) []*structToken {
	files, err := findFiles(paths)
	if err != nil {
		log.Println("couldn't find files:", err)
		log.Fatal(usageText)
	}

	var structToks []*structToken

	for _, file := range files {
		toks, err := parseCode(file, wlist)
		if err != nil {
			log.Println(`"syntax error" - parser probably`)
			log.Fatal(err)
		}

		structToks = append(structToks, toks...)
	}
	return structToks

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
						structTok.IDColumn = fieldToks[i].Column
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
