# Apex tools

This is not released (yet) since it's a work in progress. 

Building: First make sure you have the [Leiningen](https://leiningen.org/) build tool installed. 

## How to use: Example

Start a repl:

`lein repl`

`(use 'apextools.resolver)`
`(use 'apextools.package)`

This will parse the Apex class files and return a map with all ASTs:

`(def parsed-pkgs (load-packages "/path/to/dir/with/package-xml"))`

Calculate fully qualified names for types:

`(def resolved (resolve-all parsed-pkgs))`

Query the AST from "myclass" in "namespace" (all lowercase):

`(get-in resolved [:packages "namespace" :classes "myclass" :ast])`

## License

Distributed under the [EPL v1.0] \(same as Clojure).
Copyright &copy; 2015-2018 [Christian Tomoscheit].