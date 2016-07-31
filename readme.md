# ORMGEN

###Todo

1. Alter query template to support table prefix depending on user company schema property
2. Soft delete?

### Install
```
go generate
go install
```
templates are added to the executable in binary form so you don't get into trouble
with the template file locations
make sure your $GOPATH/bin is in your PATH

### Usage
```
ormgen [options] paths...
```

### Options
```
-l
    maximum depth to fetch relations, all levels from 0 to max level will be available

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
```

