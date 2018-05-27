(ns apextools.ast
  (:require [clojure.data :as data]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as s])
  (:import [apex.parser ApexLexer ApexParser]
           apextools.ast.AstApexBaseListener
           [org.antlr.v4.runtime ANTLRInputStream BaseErrorListener CommonTokenStream DefaultErrorStrategy ParserRuleContext]
           org.antlr.v4.runtime.tree.ParseTreeWalker))

;; AST generation: helper functions & macros

(defn- location-data [ctx]
  (when ctx
    {:line (-> ctx .getStart .getLine)
     :column (-> ctx .getStart .getCharPositionInLine)
     :start (-> ctx .getStart .getStartIndex)
     :end (+ 1 (-> ctx .getStop .getStopIndex))}))

(defn make-node
  ([category node-data]
   (make-node category node-data nil))
  ([category node-data ctx]
   (-> node-data
       (assoc :node-category category)
       (cond-> ctx (assoc :location (location-data ctx))))))

(defn- child [ctx indices]
  (reduce #(.getChild %1 %2) ctx indices))

(defn- child-text [ctx indices]
  (-> (reduce #(.getChild %1 %2) ctx indices)
      (.getSymbol)
      (.getText)))

(defn- inner? [ctx]
  (instance? ParserRuleContext ctx))

(defmacro set-node
  "Helper macro for creating an AST node."
  [type node]
  `(.setTreeProperty ~'this ~'ctx
                     (make-node ~type ~node ~'ctx)))

(defmacro get-node [ctx]
  `(.getTreeProperty ~'this ~ctx))

(defmacro n->
  "Code to extract AST nodes which are mapped to ParserRuleContext nodes."
  [& args]
  (let [chain (map #(if (number? %1) `(.getChild ~%1) %1) args) ; rewrite number args
        nvar (gensym)] 
    `(when-let [~nvar (some-> ~'ctx ~@chain)]
       (if (instance? java.util.List ~nvar)
         (mapv #(.getTreeProperty ~'this %1) (filter inner? ~nvar))
         (.getTreeProperty ~'this ~nvar)))))

(defn- make-keyword [s]
  (keyword (string/lower-case (string/replace s #" " "-"))))

(defn convert-to-binary-expression
  [listener ctx]
  (let [children (.children ctx)
        rest (.subList children 1 (.size children))
        first-expr (.getTreeProperty listener (.getChild ctx 0))]
    (reduce (fn [expr p]
              (make-node :expression
                         {:type :binary
                          :operator (make-keyword (child-text p [0]))
                          :operand-1 expr
                          :operand-2 (.getTreeProperty listener (.getChild p 1))}
                         p))
            first-expr rest)))

(defn process-expression-node
  [listener ctx]
  (let [first (.getTreeProperty listener (.getChild ctx 0))]
    (if (= 1 (.getChildCount ctx))
      (.setTreeProperty listener ctx first)
      (.setTreeProperty listener ctx (convert-to-binary-expression listener ctx)))))


(defn process-condition-node
  [listener ctx nodes op]
  (let [condition-node
        (reduce (fn [e1 e2]
                  (if-not e1
                    e2
                    (make-node
                     :query-expression
                     {:type :binary,
                      :operator op,
                      :operand-1 e1,
                      :operand-2 e2}
                     nil)))
                nil nodes)]
    (.setTreeProperty listener ctx condition-node)))


;; Listener for AST generation

(defn- return-listener []
  (proxy [AstApexBaseListener] []

    ;; Declarations
    
    (exitClassDeclaration [ctx]
      (set-node :type-declaration {:type :class
                                   :identifier (n-> .identifier)
                                   :modifiers (n-> .classModifiers .children)
                                   :type-parameters (n-> .typeParameters .children)
                                   :super-class (n-> .inheritance .classOrInterfaceType)
                                   :interfaces (n-> .implementation .children)
                                   :body (n-> .classBlock .children)}))

    (exitInterfaceDeclaration [ctx]
      (set-node :type-declaration {:type :interface
                                   :identifier (n-> .identifier)
                                   :modifiers (n-> .classModifiers .children)
                                   :type-parameters (n-> .typeParameters .children)
                                   :super-class (n-> .inheritance .classOrInterfaceType)
                                   :body (n-> .interfaceBlock .children)}))
    
    (exitEnumDeclaration [ctx]
      (set-node :type-declaration {:type :enum
                                   :identifier (n-> .identifier)
                                   :modifiers (n-> .basicModifiers .children)
                                   :constants (n-> .enumList .children)}))

    (exitTriggerDeclaration [ctx]
      (set-node :trigger-declaration {:identifier (n-> 1)
                                      :sobject (n-> 3)
                                      :events (n-> .triggerEvents .children)
                                      :body (n-> .anonymousBlock .children)}))

    (exitTriggerEvent [ctx]
      (set-node :trigger-event {:execution-time (make-keyword (child-text ctx [0]))
                                :operation (make-keyword (child-text ctx [1]))}))
    
    (exitVariableDeclarationStatement [ctx]
      (set-node :statement {:type :variable-declaration
                            :modifiers (n-> .basicModifiers .children)
                            :data-type (n-> .variableDeclaration .type)
                            :initializations (n-> .variableDeclaration .variableInitializationList .children)}))

    (exitVariableInitialization [ctx]
      (set-node :variable-initialization {:identifier (n-> .identifier)
                                          :value (n-> .expression)}))

    (exitSetProperty [ctx]
      (set-node :property-setter {:modifier (n-> .accessLevel)
                                  :statement-block (n-> .statementBlock)}))


    (exitGetProperty [ctx]
      (set-node :property-getter {:modifier (n-> .accessLevel)
                                  :statement-block (n-> .statementBlock)}))
    
    (exitPropertyDeclaration [ctx]
      (let [property-methods (n-> .propertyBlock .properties .children)]
        (set-node :property {:modifiers (n-> .basicModifiers .children)
                             :data-type (n-> .type)
                             :identifier (n-> .identifier)
                             :getter (some #(when (= :property-getter (:node-category %1)) %1) property-methods)
                             :setter (some #(when (= :property-setter (:node-category %1)) %1) property-methods)})))
    
    (exitMethodParameter [ctx]
      (set-node :method-parameter {:final (not (nil? (.FINAL ctx)))
                                   :data-type (n-> .type)
                                   :identifier (n-> .identifier)}))

    (exitMethodDeclaration [ctx]
      (set-node :method-declaration {:modifiers (n-> .basicModifiers .children)
                                     :data-type (n-> .type)
                                     :identifier (n-> .identifier)
                                     :parameters (n-> .methodParameters .children)
                                     :statement-block (n-> .statementBlock)}))
    
    (exitInterfaceMethodDeclaration [ctx]
      (set-node :method-declaration {:interface true
                                     :data-type (n-> .type)
                                     :identifier (n-> .identifier)
                                     :parameters (n-> .methodParameters .children)}))
    
    (exitConstructorDeclaration [ctx]
      (set-node :method-declaration {:ctor true
                                     :modifiers (n-> .basicModifiers .children)
                                     :identifier (n-> .identifier)
                                     :parameters (n-> .methodParameters .children)
                                     :statement-block (n-> .statementBlock)}))

    (exitAnonymousConstructorDeclaration [ctx]
      (set-node :block-statement {:static (not (nil? (.STATIC ctx)))
                                  :block (n-> .statementBlock)}))

    (exitSharingModifier [ctx]
      (set-node :modifier {:value (make-keyword (str (child-text ctx [0]) " " (child-text ctx [1])))}))

    (exitModifier [ctx]
      (set-node :modifier {:value (make-keyword (child-text ctx [0]))}))

    (exitAccessLevel [ctx]
      (set-node :modifier {:value (make-keyword (child-text ctx [0]))}))
    
    (exitAnnotation [ctx]
      (set-node :annotation {:identifier (n-> .identifier)
                             :parameters (when (> (.getChildCount ctx) 2) (n-> 2 .children))}))
    
    (exitAnnotationParameter [ctx]
      (set-node :annotation-parameter {:identifier (n-> .identifier)
                                       :value (n-> .literal)}))
    
    (exitClassOrInterfaceType [ctx]
      (set-node :data-type {:name (n-> .qualifiedName)
                            :parameters (n-> .typeParameters .children)}))

    (exitArrayType [ctx]
      (set-node :array-type {:parameters [(n-> 0)]}))

    
    ;; Statements

    (exitStatementBlock [ctx]
      (set-node :statement {:type :block-statement
                            :statements (n-> .children)}))
    
    (exitBasicDbStatement [ctx]
      (set-node :statement {:type :db
                            :operation (make-keyword (child-text ctx [0 0]))
                            :expression (n-> .expression)}))
    
    (exitUpsertDbStatement [ctx]
      (set-node :statement {:type :db
                            :operation :upsert
                            :expression (n-> .expression)
                            :field (n-> .qualifiedName)}))
    
    (exitMergeDbStatement [ctx]
      (set-node :statement {:type :db
                            :operation :merge
                            :master (n-> 1)
                            :other (n-> 2)}))
    
    (exitIfStatement [ctx]
      (set-node :statement {:type :if
                            :condition (n-> .expression)
                            :then (n-> 4)
                            :else (n-> 6)}))
    
    (exitReturnStatement [ctx]
      (set-node :statement {:type :return
                            :value (n-> .expression)}))
    
    (exitTryCatchBlock [ctx]
      (set-node :statement {:type :try-catch
                            :statement-block (n-> .statementBlock)
                            :catch-blocks (n-> .catchPart)
                            :finally-block (n-> .finallyPart .statementBlock)}))
    
    (exitCatchPart [ctx]
      (set-node :catch-block {:data-type (n-> .type)
                              :identifier (n-> .identifier)
                              :statement-block (n-> .statementBlock)}))
    
    (exitWhileStatement [ctx]
      (set-node :statement {:type :while
                            :condition (n-> .expression)
                            :statement (n-> .statement)}))
    
    (exitDoWhileStatement [ctx]
      (set-node :statement {:type :do-while
                            :condition (n-> .expression)
                            :statement-block (n-> .statementBlock)}))

    (exitThrowStatement [ctx]
      (set-node :statement {:type :throw
                            :exception (n-> .expression)}))

    (exitRunasStatement [ctx]
      (set-node :statement {:type :run-as
                            :user (n-> .expression)
                            :statement-block (n-> .statementBlock)}))
    
    (exitExpressionStatement [ctx]
      (set-node :statement {:type :expression
                            :expression (n-> .expression)}))

    (exitBreakStatement [ctx]
      (set-node :statement {:type :break}))
    
    (exitContinueStatement [ctx]
      (set-node :statement {:type :continue}))

    
    ;; For Loop Statements
    
    (exitForStatement [ctx]
      (set-node :statement {:type :for
                            :head (n-> 2)
                            :statement (n-> .statement)}))
    
    (exitForLoopCollection [ctx]
      (set-node :foreach {:data-type (n-> .type)
                          :identifier (n-> .identifier)
                          :collection (n-> .expression)}))
    
    (exitForLoopControl [ctx]
      (set-node :for-control {:init (n-> .forLoopControlInit)
                              :condition (n-> .forLoopControlCondition)
                              :update (n-> .forLoopControlUpdate)}))
    
    (exitForLoopAssignments [ctx]
      (set-node :for-control-assignments {:assignments (n-> .children)}))
    
    (exitForLoopDeclarations [ctx]
      (set-node :statement {:type :variable-declaration
                            :data-type (n-> .type)
                            :initializations (apply vector
                                               (cons (make-node :variable-initialization
                                                       {:identifier (n-> .identifier)
                                                        :value (n-> .expression)}
                                                       ctx)
                                                 (n-> .variableInitializationList .children)))}))

    (exitSwitchStatement [ctx]
      (set-node :statement {:type :switch
                            :expression (n-> .expression)
                            :conditions (n-> .switches .children)}))

    (exitWhenCondition [ctx]
        (set-node :when {:match (if (n-> .matchType)
                                  (n-> .matchType)
                                  (n-> .expressionList .children))
                         :statement-block (n-> .statementBlock)}))

    (exitMatchType [ctx]
      (set-node :variable-declaration {:type :variable-declaration
                                       :data-type (n-> .type)
                                       :initializations [(n-> .matchVar)]}))

    (exitMatchVar [ctx]
      (set-node :variable-initialization {:identifier (n-> .identifier)
                                          :value nil}))

    (exitElseCondition [ctx]
      (set-node :when {:match :else
                       :statement-block (n-> .statementBlock)}))


    ;; Expressions
    
    (exitAssignmentExpression [ctx]
      (set-node :expression {:type :binary
                             :operator (make-keyword (child-text ctx [1 0]))
                             :operand-1 (n-> 0)
                             :operand-2 (n-> 2)}))

    (exitConditionalExpressionWrapper [ctx]
      (when (= 2 (.getChildCount ctx))
        (set-node :expression {:type :conditional
                               :operand-1 (n-> .shortCircuitOrExpressionWrapper)
                               :operand-2 (n-> .conditionalExpression .shortCircuitOrExpressionWrapper)
                               :operand-3 (n-> .conditionalExpression .conditionalExpressionWrapper)})))
    
    (exitShortCircuitOrExpressionWrapper [ctx]
      (process-expression-node this ctx))
    
    (exitShortCircuitAndExpressionWrapper [ctx]
      (process-expression-node this ctx))

    (exitLogicalExpressionWrapper [ctx]
      (when (= 2 (.getChildCount ctx))
        (set-node :expression {:type :binary
                               :operator (make-keyword (child-text ctx [1 0]))
                               :operand-1 (n-> .compareExpressionWrapper)
                               :operand-2 (n-> .logicalExpression .compareExpressionWrapper)})))
    
    (exitCompareExpressionWrapper [ctx]
      (when (= 2 (.getChildCount ctx))
        (if (.instanceofExpression ctx)
          (set-node :expression {:type :instance-of
                                 :data-type (n-> .instanceofExpression .type)
                                 :operand-1 (n-> .shiftExpressionWrapper)})
          (set-node :expression {:type :binary
                                 :operator (make-keyword (child-text ctx [1 0 0]))
                                 :operand-1 (n-> .shiftExpressionWrapper)
                                 :operand-2 (n-> .compareExpression .shiftExpressionWrapper)}))))
    
    (exitShiftExpressionWrapper [ctx]
      (when (= 2 (.getChildCount ctx))
        (set-node :expression {:type :binary
                               :operator (case (-> ctx .shiftExpression .shiftOperator .getChildCount)
                                           2 :>>
                                           3 :>>>
                                           :<<)
                               :operand-1 (n-> .additiveExpressionWrapper)
                               :operand-2 (n-> .shiftExpression .additiveExpressionWrapper)})))

    (exitAdditiveExpressionWrapper [ctx]
      (process-expression-node this ctx))

    (exitTermExpressionWrapper [ctx]
      (process-expression-node this ctx))
    
    (exitLiteral [ctx]
      (when-not (.booleanLiteral ctx)
        (set-node
         :expression
         {:type :literal,
          :data-type
          (let [type (-> ctx (.getChild 0) .getSymbol .getType)]
            (cond
              (= ApexParser/STRING type) :string
              (= ApexParser/LONG type) :long
              (= ApexParser/INTEGER type) :integer
              (= ApexParser/DOUBLE type) :double
              (= ApexParser/DECIMAL type) :decimal
              :else :unknown)),
          :value (child-text ctx [0])})))

    (exitBooleanLiteral [ctx]
      (set-node :expression {:type :literal
                             :data-type :boolean
                             :value (child-text ctx [0])}))

    (exitUnaryExpression [ctx]
      (set-node :expression {:type :unary
                             :operator (make-keyword (child-text ctx [0 0]))
                             :operand-1 (n-> .factorExpressionWrapper)}))
    
    (exitCastExpression [ctx]
      (set-node :expression {:type :cast
                             :data-type (n-> .type)
                             :operand-1 (n-> .factorExpressionWrapper)}))
    
    (exitPreIncrementExpression [ctx]
      (set-node :expression {:type :preincrement
                             :operator (make-keyword (child-text ctx [0]))
                             :operand-1 (n-> .factorExpressionWrapper)}))
    
    (exitPostIncrementExpression [ctx]
      (set-node :expression {:type :postincrement
                             :operator (make-keyword (child-text ctx [1]))
                             :operand-1 (n-> .memberReferenceExpression)}))
    
    (exitMemberReferenceExpression [ctx]
      (when (> (.getChildCount ctx) 1)
        (let [children (.children ctx)
              rest (.subList children 1 (.size children))
              first-expr (.getTreeProperty this (.getChild ctx 0))]
          (.setTreeProperty this ctx
                            (reduce (fn [expr member]
                                      (make-node :expression
                                                 {:type :member-reference
                                                  :operand-1 expr
                                                  :operand-2 (.getTreeProperty this (.getChild member (if (= 2 (.getChildCount member)) 1 0)))}
                                                 nil))
                                    first-expr rest)))))
    
    (exitMethodCall [ctx]
      (set-node :expression {:type :method-call
                             :identifier (n-> 0)
                             :arguments (n-> .expressionList .children)}))
    
    (exitGetIndex [ctx]
      (set-node :expression {:type :index
                             :operand-1 (n-> .expression)}))
    
    (exitParensExpression [ctx]
      (set-node :expression {:type :parens
                             :value (n-> .expression)}))
    
    (exitClassExpression [ctx]
      (set-node :expression {:type :class
                             :data-type (n-> .type)}))
    
    (exitReference [ctx] 
      (set-node :expression {:type :reference
                             :value (n-> 0)}))
    
    (exitReferenceAlt [ctx]
      (set-node :expression {:type :reference
                             :value (n-> 0)}))

    (exitReferenceThis [ctx]
      (set-node :expression {:type :this}))

    (exitReferenceSuper [ctx]
      (set-node :expression {:type :super}))
    
    (exitReferenceNull [ctx]
      (set-node :expression {:type :null}))
    
    (exitAllocateSimpleExpression [ctx]
      (set-node :expression {:type :new
                             :data-type (n-> .classOrInterfaceType)
                             :initialization (n-> .allocateInit)}))

    (exitAllocateArrayExpressionValues [ctx]
      (set-node :expression {:type :new
                             :data-type  (n-> .arrayType)
                             :initialization (make-node
                                              :list-initialization
                                              {:values (n-> .expressionList .children)} (.expressionList ctx))}))
    
    (exitAllocateArrayExpressionSize [ctx]
      (set-node :expression {:type :new
                             :data-type (make-node :array-type
                                                   {:parameters [(n-> .classOrInterfaceType)]}
                                                   nil)
                             :size (n-> .expression)}))
    
    (exitAllocateFields [ctx]
      (set-node :field-initializations {:assignments (n-> .sobjectInitList .children)}))

    (exitSobjectFieldAssignment [ctx]
      (set-node :field-initialization {:field (n-> .referenceAlt)
                                       :value (n-> .expression)}))
    
    (exitAllocateParameters [ctx]
      (set-node :parameters {:parameters (n-> .constructorParameters .children)}))
    
    (exitAllocateList [ctx]
      (set-node :list-initialization {:values (n-> .expressionList .children)}))
    
    (exitAllocateMap [ctx]
      (set-node :map-initialization {:values (n-> .keyValueList .children)}))

    (exitKeyValue [ctx]
      (set-node :key-value {:key (n-> 0)
                            :value (n-> 2)}))
    
    (exitAllocateEmpty [ctx]
      (set-node :list-initialization {:values []}))
    
    (exitIdentifier [ctx]
      (let [pos (if (inner? (.getChild ctx 0)) [0 0] [0])]
        (set-node :identifier {:value (child-text ctx pos)})))

    (exitIdentifierAll [ctx]
      (when (-> ctx .keyword)
        (set-node :identifier {:value (child-text ctx [0 0])})))
    
    (exitQueryExpression [ctx]
      (set-node :expression {:type :query
                             :value (n-> 1)}))


    ;;;; SOQL

    (exitSoqlQuery [ctx]
      (set-node :object-query {:tables (n-> .soqlFrom .soqlTableList .children)
                               :select-elements (n-> .soqlSelectList .children)
                               :clauses (n-> .soqlClauses)}))

    ;; SOQL select elements

    (exitSoqlSubQuery [ctx]
      (set-node :sub-query {:value (n-> .soqlQuery)}))

    (exitSoqlTable [ctx]
      (set-node :table {:name (n-> .qualifiedName)
                        :alias (n-> .soqlTableAlias .identifier)
                        :scope (n-> .soqlTableScope .identifier)}))

    (exitSoqlSelectElement [ctx]
      (set-node :select-element {:value (n-> 0)
                                 :alias (n-> .identifier)}))
    
    (exitSoqlTypeOf [ctx]
      (set-node :type-of {:field (n-> .qualifiedName)
                          :whens (n-> .soqlTypeOfWhen)
                          :else-fields (n-> .soqlTypeOfElse .soqlTypeOfFieldList .children)}))
    
    (exitSoqlTypeOfWhen [ctx]
      (set-node :type-of-when {:name (n-> .qualifiedName)
                               :fields (n-> .soqlTypeOfFieldList .children)}))
    
    (exitQualifiedName [ctx]
      (set-node :qualified-name {:identifiers (n-> .children)}))


    ;; SOQL clauses

    (exitSoqlWhere [ctx]
      (set-node :query-clause {:type :where
                               :condition (n-> .soqlConditionExpr)}))


    (exitSoqlWith [ctx]
      (set-node :query-clause {:type :with
                               :condition (n-> 1)}))
    
    (exitSoqlWithFilter [ctx]
      (set-node :with {:with :expression
                       :expression (n-> .soqlConditionExpr)}))

    (exitSoqlDataCategory [ctx]
      (set-node :with {:with :data-category
                       :categories (n-> .soqlCategoryFilter .children)}))

    (exitSoqlCategorySelection [ctx]
      (set-node :category-selection {:group-name (n-> .qualifiedName)
                                     :filter-selector (make-keyword (child-text ctx [1 0]))
                                     :names (n-> .soqlCategoryNames .children)}))

    (exitSoqlGroupBy [ctx]
      (if (> (-> ctx (.getChild 2) .getChildCount) 1)
        (set-node :query-clause {:type :qroup-by
                                 :sub-totals (n-> .soqlGroupByAddSubtotal .soqlRawFieldList .children)
                                 :action (make-keyword (child-text ctx [2 0]))
                                 :having (n-> .soqlHaving)})
        (set-node :query-clause {:type :group-by
                                 :fields (n-> .soqlGroupByFields .soqlRawFieldList .children)
                                 :having (n-> .soqlHaving)})))
    
    (exitSoqlHaving [ctx]
      (set-node :having {:value (n-> .soqlConditionExpr)}))


    (exitSoqlOrderBy [ctx]
      (set-node :query-clause {:type :order-by
                               :fields (n-> .soqlOrderByFields .children)}))
    
    (exitSoqlOrderByField [ctx]
      (set-node :order-by-field {:value (n-> .soqlFieldValueExpression)
                                 :orientation (when (.soqlOrderByOrientation ctx) (make-keyword (child-text (.soqlOrderByOrientation ctx) [0])))
                                 :nulls (when (.soqlOrderByNulls ctx) (make-keyword (child-text (.soqlOrderByNulls ctx) [1])))}))


    (exitSoqlLimit [ctx]
      (set-node :query-clause {:type :limit
                               :value (n-> .soqlValue)}))
    
    (exitSoqlFor [ctx]
      (set-node :query-clause {:type :for
                               :value (make-keyword (child-text ctx [1]))}))
    
    (exitSoqlForUpdate [ctx]
      (set-node :query-clause {:type :for-update}))
    
    (exitSoqlUpdate [ctx]
      (set-node :query-clause {:type :update
                               :actions (n-> .children)}))

    (exitSoqlUpdateAction [ctx]
      (.setTreeProperty this ctx (make-keyword (child-text ctx [0]))))


    (exitSoqlOffset [ctx]
      (set-node :query-clause {:type :offset
                               :value (n-> .soqlValue)}))
    
    (exitSoqlRows [ctx]
      (set-node :query-clause {:type :rows}))

    ;; SOQL expressions

    (exitSoqlFieldOrExpr [ctx]
      (process-condition-node this ctx (n-> .children) :or))
    
    (exitSoqlFieldAndExpr [ctx]
      (process-condition-node this ctx (n-> .children) :and))

    (exitSoqlFieldNotExpr [ctx]
      (set-node :query-expression {:type :not
                                   :value (n-> .soqlPrimExpr)}))
    
    (exitSoqlPrimExpr [ctx]
      (when (.soqlConditionExpr ctx)
        (set-node :query-expression {:type :parens
                                     :value (n-> .soqlConditionExpr)})))
    
    (exitSoqlFieldCmpExpr [ctx]
      (set-node :query-expression {:type :compare
                                   :operator (make-keyword (child-text ctx [1 0]))
                                   :value (n-> .soqlFieldValueExpression)
                                   :expression (n-> .soqlValue)}))
    
    (exitSoqlFieldCmpSetExpr [ctx]
      (set-node :query-expression {:type :compare
                                   :operator (if (= 1 (-> ctx (.getChild 1) .getChildCount))
                                               (make-keyword (child-text ctx [1 0]))
                                               (make-keyword (str (child-text ctx [1 0])
                                                                  "-"
                                                                  (child-text ctx [1 1]))))
                                   :value (n-> .soqlFieldValueExpression)
                                   :expression (n-> 2)}))
    
    (exitSoqlFieldJoinExpr [ctx]
      (set-node :query-expression {:type :join
                                   :anti (not (not (.NOT ctx)))
                                   :value (n-> .soqlFieldValueExpression)
                                   :query (n-> .soqlSubQuery)}))
    
    (exitSoqlValueList [ctx]
      (set-node :expression-collection {:values (n-> .children)}))
    
    (exitSoqlFieldFunction [ctx]
      (set-node :field-function {:identifier (n-> .identifier)
                                 :parameter (n-> .soqlFieldValueExpression)}))

    (exitSoqlDistanceFunction [ctx]
      (set-node :distance {:field (n-> .qualifiedName)
                           :location {:latitude (n-> 6)
                                      :longitude (n-> 8)}
                           :unit (n-> .soqlString)}))

    (exitSoqlDateLiteral [ctx]
      (set-node :query-expression {:type :literal
                                   :data-type :date
                                   :value (child-text ctx [0 0])
                                   :count (child-text ctx [2])}))
    
    (exitSoqlNumber [ctx]
      (set-node :expression
                {:type :literal
                 :data-type (let [type (-> (or (.INTEGER ctx) (.DECIMAL ctx)) .getSymbol .getType)]
                              (cond
                                (= ApexParser/INTEGER type) :integer
                                (= ApexParser/DECIMAL type) :decimal
                                :else :unknown))

                 :value (str (when (.ADD_OP ctx)
                               (child-text ctx [0]))
                             (child-text ctx [(if (.ADD_OP ctx) 1 0)]))}))

    (exitSoqlString [ctx]
      (set-node :expression {:type :literal
                             :data-type :string
                             :value (child-text ctx [0])}))

    (exitSoqlApexExpression [ctx]
      (set-node :expression {:type :apex-expression
                             :value (n-> .expression)}))

    (exitSoqlNullValue [ctx]
      (set-node :expression {:type :null}))


    ;; SOSL

    (exitSoslQuery [ctx]
      (set-node :object-search {:value (n-> .soqlValue)
                                :clauses (n-> .soslClauses .children)}))

    (exitSoslSearchGroup [ctx]
      (set-node :query-clause {:type :search-group
                               :fields (make-keyword (child-text ctx [1]))}))

    (exitSoslReturning [ctx]
      (set-node :query-clause {:type :returning
                               :tables (n-> .children)}))

    (exitSoslReturnObjectTypeName [ctx]
      (set-node :table {:name (n-> .qualifiedName)
                        :fields (n-> .soslReturningConditions .soqlRawFieldList .children)
                        :clauses (n-> .soslReturningConditions .soslReturningObjectClauses .children)}))

    (exitSoslUsingListView [ctx]
      (set-node :query-clause {:type :scope
                               :scope :listview
                               :value (n-> .identifier)}))
    
    (exitSoslWith [ctx]
      (set-node :query-clause {:type :with
                               :condition (n-> 1)}))

    (exitSoslDefaultWith [ctx]
      (set-node :with {:with (n-> .identifier)
                       :value (n-> .soslWithValue 1)}))

    (exitSoslWithSnippet [ctx]
      (set-node :with {:with :snippet
                       :value (n-> .soqlValue)}))))


;; Error handler  functions

(defn- current-rule [parser]
  (-> parser .getRuleInvocationStack (.get 0)))

(defn- get-apex-error-handler []
  (proxy [DefaultErrorStrategy] []
    (reportUnwantedToken [parser]
      (let [offending-token (-> parser .getCurrentToken)
            token-string (-> this (.getTokenErrorDisplay offending-token))]
        (do (-> this (.beginErrorCondition parser))
            (-> parser (.notifyErrorListeners offending-token (str "extraneous input " token-string " in parser rule '" (current-rule parser) "'.") nil)))))))

(defn- get-error-listener [errors]
  (proxy [BaseErrorListener] []
    (syntaxError [recognizer offending-symbol line column message ex]
      (swap! errors conj {:line line :column column :message message}))))

;; Parser functions

(defn- get-apex-parser [src error-listener error-handler]
  (let [lexer (ApexLexer.
               (ANTLRInputStream. src))
        parser (ApexParser.
                (CommonTokenStream. lexer))]
    (doto lexer
      (.removeErrorListeners)
      (.addErrorListener error-listener))
    (doto parser 
      (.removeErrorListeners)
      (.addErrorListener error-listener)
      (.setErrorHandler error-handler))))

(defn gen-ast
  ([rule src]
   (let [errors (atom [])
         parser (get-apex-parser src
                                 (get-error-listener errors)
                                 (get-apex-error-handler))
         method (-> parser .getClass (.getMethod rule (into-array java.lang.Class [])))
         tree (.invoke method parser (into-array Object []))
         ast (when (empty? @errors)
               (let [listener (return-listener)]
                 (. ParseTreeWalker/DEFAULT walk listener tree)
                 (.getRoot listener)))]
     {:ast ast
      :parse-errors @errors}))
  ([file-path]
   (with-open [xin (io/input-stream file-path)]
     (gen-ast "compilationUnit" xin))))

;;; === some print functions ================================================================================

(defmulti to-string :node-category)

(defmethod to-string :qualified-name [m]
  (string/join "." (s/select [:identifiers s/ALL :value] m)))

(defmethod to-string :data-type [m]
  (let [params (string/join ", " (map to-string (:parameters m)))]
    (str (to-string (:name m))
         (if (string/blank? params)
           ""
           (str "<" params ">")))))

(defmethod to-string nil [m] "")

(defmethod to-string :method-parameter [m]
  (str (to-string (:data-type m)) " " (get-in m [:identifier :value])))


(defn get-type
  [n]
  (to-string (:data-type n)))

(defn get-id
  [n]
  (s/select-one [:identifier :value] n))

(defn get-id-key
  [n]
  (string/lower-case (get-id n)))

(defn get-class-name [cls]
  (get-in cls [:ast :identifier :value]))

;; ==========================================================================================================

(defn extract-inner-classes
  "Return a map with inner class names as lowercase keys and maps as values which tell the names & member indices."
  [ast] 
  (let [indexed-types (->> (map-indexed (fn [i v] [i v]) (:body ast))
                           (filter (fn [[i v]] (= :type-declaration (:node-category v))))
                           (map (fn [[i v]] {:name (get-in v [:identifier :value]), :member-index i})))]
    (zipmap (map #(string/lower-case (:name %)) indexed-types)
            indexed-types)))

(def ast-walker
  (s/recursive-path [fn] p
                    (s/cond-path
                     map? (s/if-path (s/pred fn)
                                     (s/stay-then-continue s/MAP-VALS p)
                                     [s/MAP-VALS p])
                     vector? [s/ALL p])))

(def NO-SUPER
  (s/path [s/ALL (s/not-selected? (fn [[k v]] (= :super-class k))) s/LAST]))

(def ast-class-context-walker
  ;; Walk through AST but remember the class context, e.g. a selected element would be [n1 n2 n3 node] or [n1 node].
  (s/recursive-path [fn] p
                    (s/cond-path
                     #(= :type-declaration
                         (:node-category %)) (s/multi-path [:super-class p]
                                                           [(s/collect-one (s/view get-id-key)) NO-SUPER p]
                                                           (s/if-path (s/pred fn)
                                                                      s/STAY
                                                                      s/STOP))
                     map? (s/if-path (s/pred fn)
                                     (s/stay-then-continue s/MAP-VALS p)
                                     [s/MAP-VALS p])
                     vector? [s/ALL p])))

(defn has-modifier [node & modifiers]
  (some (fn [node]
          (and (= :modifier (:node-category node))
               (some #(= % (:value node)) modifiers)))
        (:modifiers node)))

(defn has-annotation [node annotation]
  (some #(and (= :annotation (:node-category %1))
              (= annotation (string/lower-case (get-in %1 [:identifier :value]))))
        (:modifiers node)))

(defn data-type-node?
  [node]
  (contains? #{:data-type :array-type} (:node-category node)))

(defn method-node?
  [node]
  (= :method-declaration (:node-category node)))

(defn variable-declaration-node?
  [node]
    (= :variable-declaration (:type node)))

(defn block-statement?
  [node]
  (and (= :statement (:node-category node))
    (= :block-statement (:type node))))

(defn property-node?
  [node]
  (= :property (:node-category node)))

(defn interface?
  [ast]
  (or (= :interface (:type ast))
      (:interface ast)))

(def return-this-signature
  {:type :return,
   :value
   {:type :reference,
    :value
    {:type :this,
     :node-category :expression},
    :node-category :expression},
   :node-category :statement})

(defn returns-this? [method]
  (let [last-statement (s/select-one [:statement-block :statements s/LAST] method)]
    (nil? (nth (data/diff last-statement return-this-signature) 1))))

(defn returns-void? [method]
  (.equalsIgnoreCase "void" (to-string (:data-type method))))

(defn returns-value?
  [method]
  (not (or (returns-void? method) (returns-this? method))))

(defn inside? [node position]
  (when-let [loc (:location node)]
    (and position
         (>= position (:start loc))
         (<= position (:end loc)))))

(def primitive-data-types
  #{"blob" "boolean" "date" "datetime" "decimal" "double" "id" "integer" "long" "string" "time"})

(defn primitive?
  "check if data type node is a primitive data type."
  [node]
  (contains? primitive-data-types (string/lower-case
                                   (get-in node [:name :identifiers 0 :value]))))

