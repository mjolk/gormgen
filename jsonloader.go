package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"path/filepath"
	"sort"
	"strings"
)

type ByIndex []*structToken

func (a ByIndex) Len() int           { return len(a) }
func (a ByIndex) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByIndex) Less(i, j int) bool { return a[i].Index < a[j].Index }

func selectCfg() ([]string, error) {
	files, err := filepath.Glob("**/*.json")
	if err != nil {
		return []string{}, err
	}
	return files, nil
}

func loadFile(file string, cfgCh chan<- []*structToken, errCh chan<- error) {
	c, err := ioutil.ReadFile(file)
	if err != nil {
		errCh <- err
		return
	}
	cts := make([]*structToken, 0)
	err = json.Unmarshal(c, &cts)
	if err != nil {
		errCh <- fmt.Errorf("%s \nfor file: %s\n", err, file)
		return
	}
	prts := strings.Split(file, "/")
	for i := range cts {
		cts[i].Schema = prts[0]
	}
	cfgCh <- cts
}

func loadJSON() ([]*structToken, error) {
	cfgCh := make(chan []*structToken)
	errCh := make(chan error)
	structToks := make([]*structToken, 0)
	errs := make([]string, 0)

	files, err := selectCfg()
	if err != nil {
		return nil, err
	}
	nrFiles := len(files)
	if nrFiles == 0 {
		return nil, fmt.Errorf("No config files in dir %d", 0)
	}

	for _, file := range files {
		log.Printf("file: %s\n", file)
		go loadFile(file, cfgCh, errCh)
	}

	for i := 0; i < nrFiles; i++ {
		select {
		case parsed := <-cfgCh:
			structToks = append(structToks, parsed...)
			for _, t := range parsed {
				structLookup[t.Name] = t
			}
		case err := <-errCh:
			errs = append(errs, err.Error())
		}
	}

	if len(errs) != 0 {
		return nil, fmt.Errorf("errors occurred: %s\n",
			strings.Join(errs, "\n"))
	}

	sort.Sort(ByIndex(structToks))

	return structToks, nil
}
