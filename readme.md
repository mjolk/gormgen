# ORMGEN

## Usage
```
ormgen [options] paths...
```

### Options
```
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

