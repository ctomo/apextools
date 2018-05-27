(ns apextools.ast-test
  (:import [org.antlr.v4.runtime.tree ParseTreeWalker]
           [org.antlr.v4.runtime ANTLRInputStream CommonTokenStream BaseErrorListener DefaultErrorStrategy ParserRuleContext]
           [apex.parser ApexLexer ApexParser ApexBaseListener])
  (:require [clojure.test :refer :all]
            [apextools.ast :as ast]))

;; helper functions to test the parser + listener

(defn- get-parser-from-string [str]
  (let [lexer (ApexLexer. (ANTLRInputStream. str))]
    (ApexParser. (CommonTokenStream. lexer))))

(defn- gen-ast [rule str]
  (with-redefs-fn {#'ast/make-node (fn [category node-data ctx] (assoc node-data :node-category category))}
    #(let [parser (get-parser-from-string str)
           method (-> parser .getClass (.getMethod rule (into-array java.lang.Class [])))
           tree (.invoke method parser (into-array Object []))
           listener (#'ast/return-listener)]
       (. ParseTreeWalker/DEFAULT walk listener tree)
       (.getRoot listener))))


(deftest test-class-declaration
  (testing "class declaration."
    (is (= {:node-category :type-declaration,
            :type :class,
            :identifier {:node-category :identifier, :value "A"},
            :modifiers nil,
            :type-parameters nil,
            :super-class nil,
            :interfaces nil,
            :body []}
           (gen-ast "classDeclaration" "class A {}")))

    (is (= {:type :class,
            :identifier {:value "Foobar", :node-category :identifier},
            :modifiers [{:value :public, :node-category :modifier}],
            :type-parameters nil,
            :super-class nil,
            :interfaces nil,
            :body
            [{:ctor true,
              :modifiers [{:value :public, :node-category :modifier}],
              :identifier {:value "Foobar", :node-category :identifier},
              :parameters nil,
              :statement-block
              {:type :block-statement,
               :statements [],
               :node-category :statement},
              :node-category :method-declaration}],
            :node-category :type-declaration}
           (gen-ast "classDeclaration"
                    (str "public class Foobar {\n"
                         "   public Foobar() {}\n"
                         "}"))))

    (is (= {:type :class,
            :identifier {:value "Foobar", :node-category :identifier},
            :modifiers
            [{:identifier {:value "IsTest", :node-category :identifier},
              :parameters nil,
              :node-category :annotation}
             {:value :private, :node-category :modifier}],
            :type-parameters nil,
            :super-class
            {:name
             {:identifiers [{:value "Qux", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type},
            :interfaces
            [{:name
              {:identifiers
               [{:value "Database", :node-category :identifier}
                {:value "Batchable", :node-category :identifier}],
               :node-category :qualified-name},
              :parameters
              [{:name
                {:identifiers [{:value "SObject", :node-category :identifier}],
                 :node-category :qualified-name},
                :parameters nil,
                :node-category :data-type}],
              :node-category :data-type}
             {:name
              {:identifiers
               [{:value "Database", :node-category :identifier}
                {:value "Stateful", :node-category :identifier}],
               :node-category :qualified-name},
              :parameters nil,
              :node-category :data-type}],
            :body [],
            :node-category :type-declaration}
           (gen-ast "classDeclaration" "@IsTest private class Foobar extends Qux implements Database.Batchable<SObject>, Database.Stateful {}")))))

(deftest test-interface-declaration
  (testing "interface declarations"
    (is (= {:node-category :type-declaration,
            :type :interface,
            :identifier {:node-category :identifier, :value "Foo"},
            :modifiers nil,
            :type-parameters nil,
            :super-class nil,
            :body []}
           (gen-ast "interfaceDeclaration" "interface Foo {}")))))

(deftest test-enum-declaration
  (testing "enum declarations"
    (is (= {:type :enum,
            :identifier {:value "FOO", :node-category :identifier},
            :modifiers nil,
            :constants
            [{:value "VAL_A", :node-category :identifier}
             {:value "VAL_B", :node-category :identifier}],
            :node-category :type-declaration}
           (gen-ast "enumDeclaration" "enum FOO {VAL_A, VAL_B}")))))

(deftest test-class-member-declaration
  (testing "member declarations"
    (is (= {:node-category :type-declaration,
            :type :enum,
            :identifier {:node-category :identifier, :value "BAR"},
            :modifiers nil,
            :constants nil}
           (gen-ast "classMember" "enum BAR {}")))
    (is (= {:node-category :type-declaration,
            :type :class,
            :identifier {:node-category :identifier, :value "Qux"},
            :modifiers nil,
            :type-parameters nil,
            :super-class nil,
            :interfaces nil,
            :body []}
           (gen-ast "classMember" "class Qux {}\n")))
    (is (= {:node-category :type-declaration,
            :type :interface,
            :identifier {:node-category :identifier, :value "Baz"},
            :modifiers nil,
            :type-parameters nil,
            :super-class nil,
            :body []}
           (gen-ast "classMember" "interface Baz {}\n")))
    (is (= {:node-category :block-statement,
            :static false,
            :block
            {:node-category :statement,
             :type :block-statement,
             :statements []}}
           (gen-ast "classMember" "{}\n")))
    (is (= {:node-category :block-statement,
            :static true,
            :block
            {:node-category :statement,
             :type :block-statement,
             :statements []}}
           (gen-ast "classMember" "static {}\n")))
    (is (= {:ctor true,
            :modifiers [{:value :public, :node-category :modifier}],
            :identifier {:value "Baz", :node-category :identifier},
            :parameters nil,
            :statement-block
            {:type :block-statement, :statements [], :node-category :statement},
            :node-category :method-declaration}
           (gen-ast "classMember" "public Baz() {}\n")))
    (is (= {:type :variable-declaration,
            :modifiers nil,
            :data-type
            {:name
             {:identifiers [{:value "Object", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type},
            :initializations
            [{:identifier {:value "obj", :node-category :identifier},
              :value nil,
              :node-category :variable-initialization}],
            :node-category :statement}
           (gen-ast "classMember" "Object obj;\n")))
    (is (= {:modifiers nil,
            :data-type
            {:name
             {:identifiers [{:value "Integer", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type},
            :identifier {:value "num", :node-category :identifier},
            :getter
            {:modifier nil,
             :statement-block nil,
             :node-category :property-getter},
            :setter
            {:modifier nil,
             :statement-block nil,
             :node-category :property-setter},
            :node-category :property}
           (gen-ast "classMember" "Integer num { get; set; }\n")))
    (is (= {:modifiers [{:value :abstract, :node-category :modifier}],
            :data-type
            {:name
             {:identifiers [{:value "void", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type},
            :identifier {:value "init", :node-category :identifier},
            :parameters nil,
            :statement-block nil,
            :node-category :method-declaration}
           (gen-ast "classMember" "abstract void init();\n")))
    (is (= {:modifiers nil,
            :data-type
            {:name
             {:identifiers [{:value "Object", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type},
            :identifier {:value "calc", :node-category :identifier},
            :parameters nil,
            :statement-block nil,
            :node-category :method-declaration}
           (gen-ast "classMember" "Object calc();\n")))))

(deftest test-expressions
  (is (= {:type :literal,
          :data-type :string,
          :value "'some text'",
          :node-category :expression}
         (gen-ast "expression" "'some text'")))

  (is (= {:type :literal,
          :data-type :integer,
          :value "10",
          :node-category :expression}
         (gen-ast "expression" "10")))

  (is (= {:type :literal,
          :data-type :decimal,
          :value "5.6",
          :node-category :expression}
         (gen-ast "expression" "5.6")))

  (is (= {:type :literal,
          :data-type :long,
          :value "30L",
          :node-category :expression}
         (gen-ast "expression" "30L")))

  (is (= {:type :literal,
          :data-type :double,
          :value "1.0d",
          :node-category :expression}
         (gen-ast "expression" "1.0d")))

  (is (= {:type :literal,
          :data-type :boolean,
          :value "true",
          :node-category :expression}
         (gen-ast "expression" "true")))

  (is (= {:type :binary,
          :operator :=,
          :operand-1
          {:type :reference,
           :value {:value "var", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :literal,
           :data-type :integer,
           :value "7",
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "var = 7")))

  (is (= {:type :binary,
          :operator :+=,
          :operand-1
          {:type :reference,
           :value {:value "var2", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :literal,
           :data-type :integer,
           :value "3",
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "var2 += 3")))

  (is (= {:type :conditional,
          :operand-1
          {:type :binary,
           :operator :==,
           :operand-1 {:type :null, :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "var", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "foo", :node-category :identifier},
           :node-category :expression},
          :operand-3
          {:type :reference,
           :value {:value "bar", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "null == var ? foo : bar")))

  (is (= {:type :binary,
          :operator :||,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :binary,
           :operator :&&,
           :operand-1
           {:type :reference,
            :value {:value "b", :node-category :identifier},
            :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "c", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a || b && c")))

  (is (= {:type :binary,
          :operator :||,
          :operand-1
          {:type :binary,
           :operator :&&,
           :operand-1
           {:type :reference,
            :value {:value "a", :node-category :identifier},
            :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "b", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "c", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a && b || c")))

  (is (= {:type :binary,
          :operator :|,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "b", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a | b")))

  (is (= {:type :binary,
          :operator :&,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "b", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a & b")))

  (is (= {:type :binary,
          :operator :<<,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "b", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a << b")))

  (is (= {:type :binary,
          :operator :>>,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "b", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a >> b")))

  (is (= {:type :unary,
          :operator :-,
          :operand-1
          {:type :literal,
           :data-type :integer,
           :value "5",
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "-5")))

  (is (= {:type :unary,
          :operator :!,
          :operand-1
          {:type :reference,
           :value {:value "foo", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "!foo")))

  (is (= {:type :postincrement,
          :operator :++,
          :operand-1
          {:type :reference,
           :value {:value "x", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "x++")))

  (is (= {:type :preincrement,
          :operator :--,
          :operand-1
          {:type :reference,
           :value {:value "y", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "--y")))

  (is (= {:type :binary,
          :operator :*,
          :operand-1
          {:type :parens,
           :value
           {:type :binary,
            :operator :+,
            :operand-1
            {:type :reference,
             :value {:value "a", :node-category :identifier},
             :node-category :expression},
            :operand-2
            {:type :reference,
             :value {:value "b", :node-category :identifier},
             :node-category :expression},
            :node-category :expression},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "c", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "(a + b) * c")))

  (is (= {:type :member-reference,
          :operand-1
          {:type :reference,
           :value {:value "obj", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "prop", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "obj.prop")))

  (is (= {:type :member-reference,
          :operand-1
          {:type :reference,
           :value {:value "obj", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :method-call,
           :identifier {:value "method", :node-category :identifier},
           :arguments nil,
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "obj.method()")))

  (is (= {:type :member-reference,
          :operand-1
          {:type :member-reference,
           :operand-1
           {:type :reference,
            :value {:value "x1", :node-category :identifier},
            :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "x2", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "x3", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "x1.x2.x3")))

  (is (= {:type :member-reference,
          :operand-1
          {:type :reference,
           :value {:value "array", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :index,
           :operand-1
           {:type :literal,
            :data-type :integer,
            :value "2",
            :node-category :expression},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "array[2]")))

  (is (= {:type :null, :node-category :expression}
         (gen-ast "expression" "null")))

  ;; (is (= {}
  ;;        (gen-ast "expression" "this")))

  ;; (is (= {}
  ;;        (gen-ast "expression" "super")))

  (is (= {:type :method-call,
          :identifier {:value "call", :node-category :identifier},
          :arguments
          [{:type :reference,
            :value {:value "a", :node-category :identifier},
            :node-category :expression}
           {:type :literal,
            :data-type :integer,
            :value "1",
            :node-category :expression}],
          :node-category :expression}
         (gen-ast "expression" "call(a, 1)")))

  (is (= {:type :class,
          :data-type
          {:name
           {:identifiers [{:value "Foo", :node-category :identifier}],
            :node-category :qualified-name},
           :parameters nil,
           :node-category :data-type},
          :node-category :expression}
         (gen-ast "expression" "Foo.class")))
  
  (is (= {:type :cast,
          :data-type
          {:name
           {:identifiers [{:value "DataType", :node-category :identifier}],
            :node-category :qualified-name},
           :parameters nil,
           :node-category :data-type},
          :operand-1
          {:type :reference,
           :value {:value "x", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "(DataType)x")))

  (is (= {:type :binary,
          :operator :+,
          :operand-1
          {:type :binary,
           :operator :*,
           :operand-1
           {:type :reference,
            :value {:value "a", :node-category :identifier},
            :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "b", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :operand-2
          {:type :reference,
           :value {:value "c", :node-category :identifier},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a * b + c")))

  (is (= {:type :binary,
          :operator :+,
          :operand-1
          {:type :reference,
           :value {:value "a", :node-category :identifier},
           :node-category :expression},
          :operand-2
          {:type :binary,
           :operator :*,
           :operand-1
           {:type :reference,
            :value {:value "b", :node-category :identifier},
            :node-category :expression},
           :operand-2
           {:type :reference,
            :value {:value "c", :node-category :identifier},
            :node-category :expression},
           :node-category :expression},
          :node-category :expression}
         (gen-ast "expression" "a + b * c")))


  (is (= {:type :new,
          :data-type
          {:name
           {:identifiers [{:value "Set", :node-category :identifier}],
            :node-category :qualified-name},
           :parameters
           [{:name
             {:identifiers [{:value "String", :node-category :identifier}],
              :node-category :qualified-name},
             :parameters nil,
             :node-category :data-type}],
           :node-category :data-type},
          :initialization {:values [], :node-category :list-initialization},
          :node-category :expression}
         (gen-ast "expression" "new Set<String>{}"))))

(deftest test-soql-query
  (is (= {:tables
          [{:name
            {:identifiers [{:value "Foobar", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :scope nil,
            :node-category :table}],
          :select-elements
          [{:value
            {:identifiers [{:value "Id", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :node-category :select-element}],
          :clauses nil,
          :node-category :object-query}
         (gen-ast "soqlQuery" "SELECT Id FROM Foobar")))

  (is (= {:tables
          [{:name
            {:identifiers [{:value "Lead", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :scope nil,
            :node-category :table}],
          :select-elements
          [{:value
            {:identifier {:value "toLabel", :node-category :identifier},
             :parameter
             {:identifiers
              [{:value "Recordtype", :node-category :identifier}
               {:value "Name", :node-category :identifier}],
              :node-category :qualified-name},
             :node-category :field-function},
            :alias nil,
            :node-category :select-element}],
          :clauses nil,
          :node-category :object-query}
         (gen-ast "soqlQuery" "SELECT toLabel(Recordtype.Name) FROM Lead")))

  (is (= {:tables
          [{:name
            {:identifiers [{:value "Account", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :scope nil,
            :node-category :table}],
          :select-elements
          [{:value
            {:identifiers [{:value "Id", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :node-category :select-element}],
          :clauses
          {:type :where,
           :condition
           {:type :binary,
            :operator :and,
            :operand-1
            {:type :compare,
             :operator :!=,
             :value
             {:identifiers [{:value "Id", :node-category :identifier}],
              :node-category :qualified-name},
             :expression {:type :null, :node-category :expression},
             :node-category :query-expression},
            :operand-2
            {:type :parens,
             :value
             {:type :not,
              :value
              {:type :compare,
               :operator :like,
               :value
               {:identifiers [{:value "Name", :node-category :identifier}],
                :node-category :qualified-name},
               :expression
               {:type :literal,
                :data-type :string,
                :value "'%A'",
                :node-category :expression},
               :node-category :query-expression},
              :node-category :query-expression},
             :node-category :query-expression},
            :node-category :query-expression},
           :node-category :query-clause},
          :node-category :object-query}
         (gen-ast "soqlQuery" "SELECT Id FROM Account WHERE Id != null AND (NOT Name LIKE '%A')")))

  (is (= {:tables
          [{:name
            {:identifiers [{:value "Account", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :scope nil,
            :node-category :table}],
          :select-elements
          [{:value
            {:identifiers
             [{:value "GroupingField", :node-category :identifier}],
             :node-category :qualified-name},
            :alias {:value "aliasName", :node-category :identifier},
            :node-category :select-element}],
          :clauses
          {:type :group-by,
           :fields
           [{:identifiers
             [{:value "GroupingField", :node-category :identifier}],
             :node-category :qualified-name}],
           :having nil,
           :node-category :query-clause},
          :node-category :object-query}
         (gen-ast "soqlQuery" "SELECT GroupingField aliasName FROM Account GROUP BY GroupingField")))

  (is (= {:tables
          [{:name
            {:identifiers [{:value "Foobar", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :scope nil,
            :node-category :table}],
          :select-elements
          [{:value
            {:identifiers [{:value "Id", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :node-category :select-element}
           {:value
            {:identifiers [{:value "Name", :node-category :identifier}],
             :node-category :qualified-name},
            :alias nil,
            :node-category :select-element}],
          :clauses
          {:type :where,
           :condition
           {:type :compare,
            :operator :like,
            :value
            {:identifiers [{:value "Name", :node-category :identifier}],
             :node-category :qualified-name},
            :expression
            {:type :literal,
             :data-type :string,
             :value "'FOO%'",
             :node-category :expression},
            :node-category :query-expression},
           :node-category :query-clause},
          :node-category :object-query}
         (gen-ast "soqlQuery" "SELECT Id, Name FROM Foobar WHERE Name LIKE 'FOO%'"))))


(deftest test-soql-field-or-expr
  (is (= {:type :binary,
          :operator :or,
          :operand-1
          {:type :compare,
           :operator :=,
           :value
           {:identifiers [{:value "Name", :node-category :identifier}],
            :node-category :qualified-name},
           :expression
           {:type :literal,
            :data-type :string,
            :value "'Test'",
            :node-category :expression},
           :node-category :query-expression},
          :operand-2
          {:type :compare,
           :operator :=,
           :value
           {:identifiers [{:value "Foo", :node-category :identifier}],
            :node-category :qualified-name},
           :expression
           {:type :literal,
            :data-type :string,
            :value "'Hello'",
            :node-category :expression},
           :node-category :query-expression},
          :node-category :query-expression}
         (gen-ast "soqlFieldOrExpr" "Name = 'Test' OR Foo = 'Hello'")))

  (is (= {:type :binary,
          :operator :or,
          :operand-1
          {:type :binary,
           :operator :or,
           :operand-1
           {:type :compare,
            :operator :=,
            :value
            {:identifiers [{:value "Name", :node-category :identifier}],
             :node-category :qualified-name},
            :expression
            {:type :literal,
             :data-type :string,
             :value "'Test'",
             :node-category :expression},
            :node-category :query-expression},
           :operand-2
           {:type :compare,
            :operator :=,
            :value
            {:identifiers [{:value "A", :node-category :identifier}],
             :node-category :qualified-name},
            :expression
            {:type :literal,
             :data-type :string,
             :value "'Bar'",
             :node-category :expression},
            :node-category :query-expression},
           :node-category :query-expression},
          :operand-2
          {:type :compare,
           :operator :=,
           :value
           {:identifiers [{:value "B", :node-category :identifier}],
            :node-category :qualified-name},
           :expression
           {:type :literal,
            :data-type :string,
            :value "'Baz'",
            :node-category :expression},
           :node-category :query-expression},
          :node-category :query-expression}
         (gen-ast "soqlFieldOrExpr" "Name = 'Test' OR A = 'Bar' OR B = 'Baz'"))))


(deftest test-sosl

  (is (= {:value
          {:type :literal,
           :data-type :string,
           :value "'xyz'",
           :node-category :expression},
          :clauses
          [{:type :returning,
            :tables
            [{:name
              {:identifiers
               [{:value "Object__c", :node-category :identifier}],
               :node-category :qualified-name},
              :fields
              [{:identifiers [{:value "Id", :node-category :identifier}],
                :node-category :qualified-name}],
              :clauses
              [{:type :where,
                :condition
                {:type :compare,
                 :operator :<,
                 :value
                 {:field
                  {:identifiers
                   [{:value "My_Location_Field__c",
                     :node-category :identifier}],
                   :node-category :qualified-name},
                  :location
                  {:latitude
                   {:type :literal,
                    :data-type :integer,
                    :value "37",
                    :node-category :expression},
                   :longitude
                   {:type :literal,
                    :data-type :integer,
                    :value "122",
                    :node-category :expression}},
                  :unit
                  {:type :literal,
                   :data-type :string,
                   :value "'mi'",
                   :node-category :expression},
                  :node-category :distance},
                 :expression
                 {:type :literal,
                  :data-type :integer,
                  :value "100",
                  :node-category :expression},
                 :node-category :query-expression},
                :node-category :query-clause}],
              :node-category :table}],
            :node-category :query-clause}],
          :node-category :object-search}
         (gen-ast "soslQuery" "FIND 'xyz' RETURNING Object__c (Id WHERE DISTANCE(My_Location_Field__c,GEOLOCATION(37,122),'mi') < 100)")))

  (is (= {:value
          {:type :literal,
           :data-type :string,
           :value "'Acme'",
           :node-category :expression},
          :clauses
          [{:type :search-group, :fields :all, :node-category :query-clause}
           {:type :returning,
            :tables
            [{:name
              {:identifiers [{:value "Account", :node-category :identifier}],
               :node-category :qualified-name},
              :fields
              [{:identifiers [{:value "Name", :node-category :identifier}],
                :node-category :qualified-name}],
              :clauses
              [{:type :scope,
                :scope :listview,
                :value {:value "MVPCustomers", :node-category :identifier},
                :node-category :query-clause}],
              :node-category :table}],
            :node-category :query-clause}],
          :node-category :object-search}
         (gen-ast "soslQuery" "FIND 'Acme' IN ALL FIELDS RETURNING Account(Name USING ListView = MVPCustomers)"))))

(deftest test-sosl-with

  (is (= {:type :with,
          :condition
          {:with {:value "HIGHLIGHT", :node-category :identifier},
           :value nil,
           :node-category :with},
          :node-category :query-clause}
         (gen-ast "soslWith" "WITH HIGHLIGHT")))  

  (is (= {:type :with,
          :condition
          {:with {:value "METADATA", :node-category :identifier},
           :value
           {:type :literal,
            :data-type :string,
            :value "'LABELS'",
            :node-category :expression},
           :node-category :with},
          :node-category :query-clause}
         (gen-ast "soslWith" "WITH METADATA='LABELS'")))  

  (is (= {:type :with,
          :condition
          {:with {:value "PricebookId", :node-category :identifier},
           :value
           {:type :literal,
            :data-type :string,
            :value "'01sxx0000002MffAAE'",
            :node-category :expression},
           :node-category :with},
          :node-category :query-clause}
         (gen-ast "soslWith" "WITH PricebookId = '01sxx0000002MffAAE'")))  

  (is (= {:type :with,
          :condition
          {:with {:value "SPELL_CORRECTION", :node-category :identifier},
           :value
           {:type :literal,
            :data-type :boolean,
            :value "true",
            :node-category :expression},
           :node-category :with},
          :node-category :query-clause}
         (gen-ast "soslWith" "WITH SPELL_CORRECTION = true")))  

  (is (= {:type :with,
          :condition
          {:with {:value "HIGHLIGHT", :node-category :identifier},
           :value nil,
           :node-category :with},
          :node-category :query-clause}
         (gen-ast "soslWith" "WITH HIGHLIGHT"))))

(deftest test-soql-number
  (is (= {:node-category :expression,
          :type :literal,
          :data-type :decimal,
          :value "-100.0"}
         (gen-ast "soqlNumber" "-100.0")))

  (is (= {:node-category :expression,
          :type :literal,
          :data-type :integer,
          :value "1"}
         (gen-ast "soqlNumber" "1")))

  (is (= {:node-category :expression,
          :type :literal,
          :data-type :decimal,
          :value "50.0"}
         (gen-ast "soqlNumber" "50.0"))))

