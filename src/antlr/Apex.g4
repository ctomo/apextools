grammar Apex;

@lexer::members {
   public static final int COMMENTS = 1;
   public static final int WHITESPACE = 2;
}

compilationUnit : triggerDeclaration | classDeclaration | interfaceDeclaration | enumDeclaration;

enumDeclaration : basicModifiers ENUM identifier '{' enumList? '}';
enumList : identifier (',' identifier)*;

classDeclaration : classModifiers CLASS identifier typeParameters? inheritance? implementation? classBlock;
classBlock : '{' classMember* '}';
classMember : enumDeclaration | classDeclaration | interfaceDeclaration | anonymousConstructorDeclaration | variableDeclarationStatement | propertyDeclaration | constructorDeclaration | methodDeclaration;

interfaceDeclaration : classModifiers INTERFACE identifier typeParameters? inheritance? interfaceBlock;
interfaceBlock : '{' interfaceMethodDeclaration* '}';

inheritance : EXTENDS classOrInterfaceType;

implementation : IMPLEMENTS classOrInterfaceType (',' classOrInterfaceType)*;

classModifiers : (annotation | modifier | accessLevel | sharingModifier)*;
basicModifiers : (annotation | modifier | accessLevel)*;

modifier : ABSTRACT | STATIC | TESTMETHOD | VIRTUAL | OVERRIDE | FINAL | TRANSIENT | WEBSERVICE;
accessLevel : PUBLIC | GLOBAL | PROTECTED | PRIVATE;
sharingModifier : (WITH | WITHOUT | INHERITED) SHARING;

annotation : '@' identifier annotationParameters?;

annotationParameters : '(' annotationParameter+ ')';
annotationParameter : identifier EQ_OP literal;

triggerDeclaration : TRIGGER identifier ON identifier BULK? '(' triggerEvents ')' anonymousBlock;

triggerEvents : triggerEvent (',' triggerEvent)*;
triggerEvent : (BEFORE | AFTER) (INSERT | UPDATE | DELETE | UNDELETE);

type : arrayType | classOrInterfaceType;

arrayType : classOrInterfaceType '[' ']';

classOrInterfaceType : qualifiedName typeParameters?;

typeParameters : LDIAMOND type (',' type)* RDIAMOND;

anonymousBlock : '{' anonymousBlockElement* '}';
anonymousBlockElement : enumDeclaration | classDeclaration | interfaceDeclaration | statementBlock | propertyDeclaration |
                        methodDeclaration  | statement;

variableDeclarationStatement : basicModifiers variableDeclaration ';';
variableDeclaration : type variableInitializationList;
variableInitializationList : variableInitialization (',' variableInitialization)*;
variableInitialization : identifier (EQ_OP expression)?;

constructorDeclaration : basicModifiers identifier '(' methodParameters? ')' statementBlock; // Note: There is a bug in the parser from sf which allows for a qualified name instead of solely an identifier.

anonymousConstructorDeclaration : STATIC? statementBlock;

methodDeclaration : basicModifiers type identifier '(' methodParameters? ')' (statementBlock | ';');

interfaceMethodDeclaration : type identifier '(' methodParameters? ')' ';';

methodParameters : methodParameter (',' methodParameter)*;
methodParameter : FINAL? type identifier;

propertyDeclaration : basicModifiers type identifier propertyBlock;

propertyBlock : '{' properties '}';

properties : getProperty setProperty? | setProperty getProperty?;

getProperty : accessLevel? GET (';' | statementBlock);
setProperty : accessLevel? SET (';' | statementBlock);


// ==== Statements ====================================================================================================

statementBlock : '{' statement* '}';

statement : basicDbStatement
          | upsertDbStatement
          | mergeDbStatement
          | ifStatement
          | switchStatement
          | forStatement
          | whileStatement
          | doWhileStatement
          | tryCatchBlock
          | returnStatement
          | throwStatement
          | breakStatement
          | continueStatement
          | runasStatement
          | variableDeclarationStatement
          | expressionStatement
          | statementBlock
          ;


basicDbStatement : dbOperation expression ';';
dbOperation : INSERT | UPDATE | DELETE | UNDELETE;

upsertDbStatement : UPSERT expression qualifiedName? ';';
mergeDbStatement : MERGE expression expression ';';

// Note: dangling else is implicit solved (since the parse is greedy)
ifStatement : IF '(' expression ')' statement (ELSE statement)?;

returnStatement : RETURN expression? ';';

breakStatement : BREAK ';';

continueStatement : CONTINUE ';';

tryCatchBlock : TRY statementBlock catchPart* finallyPart?;
catchPart : CATCH '(' type identifier ')' statementBlock;
finallyPart : FINALLY statementBlock;

whileStatement : WHILE '(' expression ')' statement;

doWhileStatement : DO statementBlock WHILE '(' expression ')' ';';

throwStatement : THROW expression ';';

runasStatement : SYSTEM_RUNAS '(' expression ')' statementBlock;

expressionStatement : expression ';';


forStatement : FOR '(' (forLoopControl | forLoopCollection) ')' (';' | statement);

forLoopCollection : type identifier ':' expression;
forLoopControl : forLoopControlInit? ';' forLoopControlCondition? ';' forLoopControlUpdate?;

forLoopControlInit : forLoopDeclarations | forLoopAssignments;
forLoopDeclarations : type identifier EQ_OP expression (',' variableInitializationList)?;
forLoopAssignments : assignmentExpression (',' assignmentExpression)*;

forLoopControlCondition : expression;
forLoopControlUpdate : expression;

switchStatement : SWITCH ON expression '{' switches '}';
switches : (whenCondition | elseCondition)+;
whenCondition : WHEN (expressionList | matchType) statementBlock;
elseCondition : WHEN ELSE statementBlock;
matchType : type matchVar;
matchVar : identifier;

// ==== Expressions ===================================================================================================

expression : assignmentExpression | conditionalExpressionWrapper;

assignmentExpression : memberReferenceExpression assignOperator (assignmentExpression | conditionalExpressionWrapper);
assignOperator : ASSIGN_OP | EQ_OP;

conditionalExpressionWrapper : shortCircuitOrExpressionWrapper conditionalExpression?;
conditionalExpression : '?' shortCircuitOrExpressionWrapper ':' conditionalExpressionWrapper;

shortCircuitOrExpressionWrapper : shortCircuitAndExpressionWrapper shortCircuitOrExpression*;
shortCircuitOrExpression : '||' shortCircuitAndExpressionWrapper;

shortCircuitAndExpressionWrapper : logicalExpressionWrapper shortCircuitAndExpression*;
shortCircuitAndExpression : '&&' logicalExpressionWrapper;

logicalExpressionWrapper : compareExpressionWrapper logicalExpression?;
logicalExpression : LOGICAL_OP compareExpressionWrapper;

compareExpressionWrapper : shiftExpressionWrapper (compareExpression | instanceofExpression)?;

instanceofExpression : INSTANCEOF type;

compareExpression : compareOperator shiftExpressionWrapper;
compareOperator: APEX_COMPARE_OP | NEQ_OP | NEQ2_OP | LEQ_OP | GEQ_OP | RDIAMOND | LDIAMOND ;

shiftExpressionWrapper : additiveExpressionWrapper shiftExpression?;
shiftExpression : shiftOperator additiveExpressionWrapper;
shiftOperator : LSHIFT_OP | RDIAMOND RDIAMOND | RDIAMOND RDIAMOND RDIAMOND;

additiveExpressionWrapper : termExpressionWrapper additiveExpression*;
additiveExpression : ADD_OP termExpressionWrapper;

termExpressionWrapper : factorExpressionWrapper termExpression*;
termExpression : MUL_OP factorExpressionWrapper;

factorExpressionWrapper : unaryExpression | castExpression | incrementExpressionWrapper;

unaryExpression : unaryOperator factorExpressionWrapper;
unaryOperator : COMPLEMENT_OP | ADD_OP;

castExpression: '(' type ')' factorExpressionWrapper;

incrementExpressionWrapper : preIncrementExpression
                           | postIncrementExpression
                           | memberReferenceExpression
                           ;

preIncrementExpression : INC_OP factorExpressionWrapper;
postIncrementExpression : memberReferenceExpression INC_OP;


memberReferenceExpression : referenceNull | sourceExpression member* | referenceSuper member+;

sourceExpression : allocateExpression
                 | parensExpression
                 | classExpression
                 | methodCall
                 | reference
                 | literal
                 | queryExpression
                 ;

member : getIndex
       |  '.' methodCall
       |  '.' referenceAlt
       ;


methodCall : identifierAll '(' expressionList? ')';

parensExpression : '(' expression ')';

classExpression : type '.' CLASS;

getIndex : '[' expression ']';

reference : referenceThis | identifier;
referenceAlt : identifierAll;
referenceThis : THIS;
referenceNull : NULL;
referenceSuper : SUPER;

allocateExpression : allocateSimpleExpression | allocateArrayExpressionValues | allocateArrayExpressionSize;

allocateSimpleExpression : NEW classOrInterfaceType allocateInit;
allocateInit : allocateFields | allocateParameters | allocateList | allocateMap | allocateEmpty;

allocateArrayExpressionValues : NEW arrayType '{' expressionList? '}';
allocateArrayExpressionSize : NEW classOrInterfaceType '[' expression ']';

allocateFields : '(' sobjectInitList ')';
sobjectInitList : sobjectFieldAssignment (',' sobjectFieldAssignment)*;
sobjectFieldAssignment : referenceAlt EQ_OP expression;

allocateParameters : '(' constructorParameters? ')';
constructorParameters :  conditionalExpressionWrapper (',' conditionalExpressionWrapper)*;

allocateList : '{' expressionList '}';

allocateMap : '{' keyValueList '}';

allocateEmpty : '{' '}';

expressionList : expression (',' expression)*;
keyValueList : keyValue (',' keyValue)*;
keyValue : expression '=>' expression;

queryExpression : '[' (soqlQuery | soslQuery) ']';

literal : booleanLiteral | STRING | LONG | INTEGER | DOUBLE | DECIMAL;

booleanLiteral : TRUE | FALSE;


// ===== Query languages SOQL/SOSL ====================================================================================

soqlQuery : SELECT soqlSelectList soqlFrom soqlClauses;
soqlClauses : soqlWhere? soqlWith? soqlGroupBy? soqlOrderBy? soqlLimit? soqlOffset? soqlFor? soqlUpdate? soqlForUpdate? soqlRows?;
soqlSubQuery : '(' soqlQuery ')';

soqlSelectList : soqlSelectElement (',' soqlSelectElement)*;
soqlSelectElement : (soqlTypeOf | soqlFieldValueExpression | soqlSubQuery) identifier?;

soqlFieldValueExpression : qualifiedName | soqlFieldFunction | soqlDistanceFunction;

soqlFrom : FROM soqlTableList;
soqlTableList : soqlTable (',' soqlTable)*;
soqlTable : qualifiedName soqlTableAlias? soqlTableScope?;
soqlTableAlias : AS? identifier;
soqlTableScope : USING SCOPE? identifier;

soqlTypeOf : TYPEOF qualifiedName soqlTypeOfWhen+ soqlTypeOfElse? END;
soqlTypeOfWhen : WHEN qualifiedName THEN soqlTypeOfFieldList;
soqlTypeOfElse : ELSE soqlTypeOfFieldList;
soqlTypeOfFieldList : qualifiedName (',' qualifiedName)*;


// ==== SOQL clauses ====

soqlWhere : WHERE soqlConditionExpr;

soqlWith : WITH (soqlDataCategory | soqlWithFilter);

soqlWithFilter : soqlConditionExpr;

soqlDataCategory : DATA CATEGORY soqlCategoryFilter;
soqlCategoryFilter : soqlCategorySelection (AND soqlCategorySelection)*;

soqlCategorySelection : qualifiedName soqlCategoryFilterSelector soqlCategoryNames;
soqlCategoryFilterSelector : AT | ABOVE | BELOW | ABOVE_OR_BELOW;
soqlCategoryNames : qualifiedName | '(' qualifiedName (',' qualifiedName)* ')';

soqlGroupBy : GROUP BY (soqlGroupByAddSubtotal | soqlGroupByFields) soqlHaving?;
soqlGroupByFields : soqlRawFieldList;
soqlGroupByAddSubtotal : (ROLLUP | CUBE) '(' soqlRawFieldList ')';

soqlHaving : HAVING soqlConditionExpr;

soqlOrderBy : ORDER BY soqlOrderByFields;
soqlOrderByFields : soqlOrderByField (',' soqlOrderByField)*;
soqlOrderByField : soqlFieldValueExpression soqlOrderByOrientation? soqlOrderByNulls?;
soqlOrderByOrientation : ASC | DESC;
soqlOrderByNulls : NULLS FIRST | NULLS LAST;

soqlLimit : LIMIT soqlValue;

soqlFor : FOR (REFERENCE | VIEW);
soqlForUpdate : FOR UPDATE;

soqlOffset : OFFSET soqlValue;

soqlUpdate : UPDATE soqlUpdateAction (',' soqlUpdateAction)*;
soqlUpdateAction : (VIEWSTAT | TRACKING);

soqlRows : ALL ROWS;


// ==== SOQL expressions ====

soqlConditionExpr : soqlFieldNotExpr | soqlFieldAndExpr | soqlFieldOrExpr | soqlPrimExpr;

soqlFieldAndExpr : soqlPrimExpr (AND soqlPrimExpr)+;
soqlFieldOrExpr : soqlPrimExpr (OR soqlPrimExpr)+;
soqlFieldNotExpr : NOT soqlPrimExpr;

soqlPrimExpr : '(' soqlConditionExpr ')'
               | soqlFieldExpr
               ;

soqlFieldExpr : soqlFieldCmpExpr | soqlFieldJoinExpr | soqlFieldCmpSetExpr;

soqlFieldCmpExpr : soqlFieldValueExpression soqlCmpOp soqlValue;
soqlFieldCmpSetExpr : soqlFieldValueExpression soqlCmpSetOp (soqlValueList | soqlApexExpression);
soqlFieldJoinExpr : soqlFieldValueExpression NOT? IN soqlSubQuery;

soqlCmpOp : EQ_OP | NEQ_OP | NEQ2_OP | LDIAMOND | RDIAMOND | LEQ_OP | GEQ_OP | LIKE;
soqlCmpSetOp : NOT? IN | INCLUDES | EXCLUDES;

soqlFieldFunction : identifier '(' soqlFieldValueExpression? ')';
soqlDistanceFunction : DISTANCE '(' qualifiedName ',' GEOLOCATION '(' soqlNumber ',' soqlNumber ')' ',' soqlString ')' ;

soqlValueList : '(' soqlValue (',' soqlValue)* ')';
soqlValue : soqlNullValue | booleanLiteral | soqlDateLiteral | soqlDate | soqlDateTime | soqlString |  soqlNumber | soqlApexExpression;

soqlNumber : ADD_OP? (INTEGER | DECIMAL);
soqlNullValue : NULL;
soqlString : STRING;
soqlApexExpression : ':' expression;

soqlRawFieldList : soqlFieldValueExpression (',' soqlFieldValueExpression)*;

soqlDateLiteral : queryDateLiteral (':' INTEGER)?;

soqlDate : DATE;

soqlDateTime : DATETIME;

qualifiedName : identifier ('.' identifier)*;


// ==== SOSL ====

soslQuery : FIND soqlValue soslClauses;
soslClauses :  soslSearchGroup? soslReturning? soslWith? soqlLimit? soqlUpdate?;

soslSearchGroup : IN (ALL FIELDS | NAME FIELDS | EMAIL FIELDS | PHONE FIELDS | SIDEBAR FIELDS);

soslReturning : RETURNING soslReturnObjectTypeName (',' soslReturnObjectTypeName)*;
soslReturnObjectTypeName : qualifiedName soslReturningConditions?;
soslReturningConditions : '(' soqlRawFieldList soslReturningObjectClauses ')';
soslReturningObjectClauses : soslUsingListView? soqlWhere? soqlOrderBy? soqlLimit? soqlOffset?;
soslUsingListView : USING LISTVIEW EQ_OP identifier;

soslWith : WITH (soqlDataCategory | soslWithSnippet | soslDefaultWith);
soslDefaultWith : identifier soslWithValue?;
soslWithValue : EQ_OP soqlValue | IN soqlValueList;
soslWithSnippet : SNIPPET ('(' TARGET_LENGTH EQ_OP soqlValue ')')?;


// ==== L E X E R =====================================================================================================

identifierAll : identifier | keyword;

keyword : ABSTRACT | AND | AS | ASC | BREAK | BULK | BY | CATCH | CLASS | CONTINUE | DELETE | DESC | DO | ELSE | ENUM |
          EXTENDS | FALSE | FINAL | FINALLY | FOR | FROM | GLOBAL | IF | IMPLEMENTS | INSERT | INSTANCEOF | INTERFACE |
          LIKE | LIMIT | MERGE | NEW | NOT | NULL | NULLS | ON | OR | OVERRIDE | PRIVATE | PROTECTED | PUBLIC | RETURN |
          SELECT | STATIC | TESTMETHOD | THROW | TRIGGER | TRUE | TRY | UNDELETE | UPDATE | UPSERT | USING | VIRTUAL |
          WEBSERVICE | WHERE | WHILE | SYSTEM_RUNAS;

identifier : AFTER | BEFORE | DIVISION | END | GET | SET | WITH | WITHOUT | INHERITED | SHARING | TRANSIENT | THIS | SUPER
           | ABOVE | ABOVE_OR_BELOW | ALL | AT | BELOW | CATEGORY | CUBE | DATA | EMAIL
           | EXCLUDES | FIELDS | FIND | FIRST | GROUP | HAVING | IN | INCLUDES | LAST | LISTVIEW | NAME | NETWORK
           | OFFSET | ORDER | PHONE | REFERENCE | RETURNING | ROLLUP | ROWS | SIDEBAR | SCOPE | SNIPPET
           | SPELL_CORRECTION | TARGET_LENGTH | THEN | TRACKING | TYPEOF | VIEW | VIEWSTAT | WHEN | SWITCH
           | TRIGGER
           | queryDateLiteral
           | ID
           ;

queryDateLiteral : YESTERDAY | TODAY | TOMORROW | LAST_WEEK | NEXT_WEEK | LAST_MONTH | THIS_MONTH
                 | NEXT_MONTH | LAST_90_DAYS | NEXT_90_DAYS | LAST_N_DAYS | NEXT_N_DAYS | NEXT_N_WEEKS
                 | LAST_N_WEEKS | NEXT_N_MONTHS | LAST_N_MONTHS | THIS_QUARTER | LAST_QUARTER | NEXT_QUARTER
                 | NEXT_N_QUARTERS | LAST_N_QUARTERS | THIS_YEAR | LAST_YEAR | NEXT_YEAR | NEXT_N_YEARS
                 | LAST_N_YEARS | THIS_FISCAL_QUARTER | LAST_FISCAL_QUARTER | NEXT_FISCAL_QUARTER
                 | NEXT_N_FISCAL_QUARTERS | LAST_N_FISCAL_QUARTERS | THIS_FISCAL_YEAR | LAST_FISCAL_YEAR
                 | NEXT_FISCAL_YEAR | NEXT_N_FISCAL_YEARS | LAST_N_FISCAL_YEARS
                 ;

AFTER : A F T E R;
BEFORE : B E F O R E;
GET : G E T;
SET : S E T;
SHARING : S H A R I N G;
SUPER : S U P E R;
THIS : T H I S;
TRANSIENT : T R A N S I E N T;
WITH : W I T H;
WITHOUT : W I T H O U T;
INHERITED : I N H E R I T E D;

ABOVE_OR_BELOW : A B O V E '_' O R '_' B E L O W;
ABOVE : A B O V E;
ALL : A L L;
AT : A T;
BELOW : B E L O W;
CATEGORY : C A T E G O R Y;
CUBE : C U B E;
DATA : D A T A;
DISTANCE : D I S T A N C E;
DIVISION : D I V I S I O N;
EMAIL : E M A I L;
EXCLUDES : E X C L U D E S;
FIELDS : F I E L D S;
FIND : F I N D;
FIRST : F I R S T;
GEOLOCATION : G E O L O C A T I O N;
GROUP : G R O U P;
HAVING : H A V I N G;
IN : I N;
INCLUDES : I N C L U D E S;
LAST : L A S T;
LISTVIEW : L I S T V I E W;
NAME : N A M E;
NETWORK : N E T W O R K;
OFFSET : O F F S E T;
ORDER : O R D E R;
PHONE : P H O N E;
REFERENCE : R E F E R E N C E;
RETURNING : R E T U R N I N G;
ROLLUP : R O L L U P;
ROWS : R O W S;
SIDEBAR : S I D E B A R;
SNIPPET : S N I P P E T;
SCOPE : S C O P E;
SPELL_CORRECTION : S P E L L '_' C O R R E C T I O N;
TARGET_LENGTH : T A R G E T '_' L E N G T H;
TRACKING : T R A C K I N G;
TYPEOF : T Y P E O F;
VIEW : V I E W;
VIEWSTAT : V I E W S T A T;

YESTERDAY : Y E S T E R D A Y;
TODAY : T O D A Y;
TOMORROW : T O M O R R O W;
LAST_WEEK : L A S T '_' W E E K;
NEXT_WEEK : N E X T '_' W E E K;
LAST_MONTH : L A S T '_' M O N T H;
THIS_MONTH : T H I S '_' M O N T H;
NEXT_MONTH : N E X T '_' M O N T H;
LAST_90_DAYS : L A S T '_90_' D A Y S;
NEXT_90_DAYS : N E X T '_90_' D A Y S;
LAST_N_DAYS : L A S T '_' N '_' D A Y S;
NEXT_N_DAYS : N E X T '_' N '_' D A Y S;
NEXT_N_WEEKS : N E X T '_' N '_' W E E K S;

LAST_N_WEEKS : L A S T '_' N '_' W E E K S;
NEXT_N_MONTHS : N E X T '_' N '_' M O N T H S;
LAST_N_MONTHS : L A S T '_' N '_' M O N T H S;
THIS_QUARTER : T H I S '_' Q U A R T E R;
LAST_QUARTER : L A S T '_' Q U A R T E R;

NEXT_QUARTER : N E X T '_' Q U A R T E R;
NEXT_N_QUARTERS : N E X T '_' N '_' Q U A R T E R S;
LAST_N_QUARTERS : L A S T '_' N '_' Q U A R T E R S;
THIS_YEAR : T H I S '_' Y E A R;
LAST_YEAR : L A S T '_' Y E A R;
NEXT_YEAR : N E X T '_' Y E A R;

NEXT_N_YEARS : N E X T '_' N '_' Y E A R S;
LAST_N_YEARS : LAST '_' N '_' Y E A R S;
THIS_FISCAL_QUARTER : T H I S '_' F I S C A L '_' Q U A R T E R;
LAST_FISCAL_QUARTER : L A S T '_' F I S C A L '_' Q U A R T E R;
NEXT_FISCAL_QUARTER : N E X T '_' F I S C A L '_' Q U A R T E R;

NEXT_N_FISCAL_QUARTERS : N E X T '_' N '_' F I S C A L '_' Q U A R T E R S;
LAST_N_FISCAL_QUARTERS : L A S T '_' N '_' F I S C A L '_' Q U A R T E R S;
THIS_FISCAL_YEAR : T H I S '_' F I S C A L '_' Y E A R;
LAST_FISCAL_YEAR : L A S T '_' F I S C A L '_' Y E A R;

NEXT_FISCAL_YEAR : N E X T '_' F I S C A L '_' Y E A R;
NEXT_N_FISCAL_YEARS : N E X T '_' N '_' F I S C A L '_' Y E A R S;
LAST_N_FISCAL_YEARS : L A S T '_' N '_' F I S C A L '_' Y E A R S;

ABSTRACT : A B S T R A C T;
AND : A N D;
AS : A S;
ASC : A S C;
BREAK : B R E A K;
BULK : B U L K;
BY : B Y;
CATCH : C A T C H;
CLASS : C L A S S;
CONTINUE : C O N T I N U E;
DELETE : D E L E T E;
DESC : D E S C;
DO : D O;
END : E N D;
ELSE : E L S E;
ENUM : E N U M;
EXTENDS : E X T E N D S;
FALSE : F A L S E;
FINAL : F I N A L;
FINALLY : F I N A L L Y;
FOR : F O R;
FROM : F R O M;
GLOBAL : G L O B A L;
IF : I F;
IMPLEMENTS : I M P L E M E N T S;
INSERT : I N S E R T;
INSTANCEOF : I N S T A N C E O F;
INTERFACE : I N T E R F A C E;
LIKE : L I K E;
LIMIT : L I M I T;
MERGE : M E R G E;
NEW : N E W;
NOT : N O T;
NULL : N U L L;
NULLS : N U L L S;
ON : O N;
OR : O R;
OVERRIDE : O V E R R I D E;
PRIVATE : P R I V A T E;
PROTECTED : P R O T E C T E D;
PUBLIC : P U B L I C;
RETURN : R E T U R N;
SELECT : S E L E C T;
STATIC : S T A T I C;
SWITCH : S W I T C H;
TESTMETHOD : T E S T M E T H O D;
THEN : T H E N;
THROW : T H R O W;
TRIGGER : T R I G G E R;
TRUE : T R U E;
TRY : T R Y;
UNDELETE : U N D E L E T E;
UPDATE : U P D A T E;
UPSERT : U P S E R T;
USING : U S I N G;
VIRTUAL : V I R T U A L;
WEBSERVICE : W E B S E R V I C E;
WHEN : W H E N;
WHERE : W H E R E;
WHILE: W H I L E;

SYSTEM_RUNAS : S Y S T E M '.' R U N A S;

ASSIGN_OP : '+=' | '*=' | '-=' | '/=' | '|=' | '&=' | '^=' | '<<=' | '>>=' | '>>>=';

LOGICAL_OP : '&' | '|' | '^';

EQ_OP : '=';
NEQ_OP : '!=';
NEQ2_OP : '<>';
LEQ_OP : '<=';
GEQ_OP : '>=';
APEX_COMPARE_OP : '===' | '!==' | '==';

RDIAMOND : '>';
LDIAMOND : '<';
LSHIFT_OP : '<<';

INC_OP : '++' | '--';

ADD_OP : '+' | '-';

MUL_OP : '*' | '/';

COMPLEMENT_OP : '!';

fragment A : [Aa];
fragment B : [Bb];
fragment C : [Cc];
fragment D : [Dd];
fragment E : [Ee];
fragment F : [Ff];
fragment G : [Gg];
fragment H : [Hh];
fragment I : [Ii];
fragment J : [Jj];
fragment K : [Kk];
fragment L : [Ll];
fragment M : [Mm];
fragment N : [Nn];
fragment O : [Oo];
fragment P : [Pp];
fragment Q : [Qq];
fragment R : [Rr];
fragment S : [Ss];
fragment T : [Tt];
fragment U : [Uu];
fragment V : [Vv];
fragment W : [Ww];
fragment X : [Xx];
fragment Y : [Yy];
fragment Z : [Zz];


LINEFEED : ('\r' | '\n' | '\r\n') -> channel(2);
SPACE : [ \t\f]+ -> channel(2);

COMMENT : ('/*' COMMENT_TAIL | '//' ~[\r\n]* | '/*') -> channel(1);
fragment COMMENT_TAIL : .*? '*/' | .*? EOF; // allow open ended comment token

STRING : '\'' ( ESCAPE | ~[\r\n] )*? '\'';
fragment ESCAPE :  '\\b' | '\\t' | '\\n' | '\\f' | '\\r' | '\\\'' | '\\\\';

DOUBLE : DECIMAL [Dd];

DECIMAL : '.' INTEGER | INTEGER '.' INTEGER;

LONG : INTEGER [Ll];

DATETIME : YEAR '-' MONTH '-' DAY 'T' HOUR ':' MINSEC ':' MINSEC (MILLISEC? 'Z' | [+-] MINSEC ':' MINSEC);
fragment YEAR : [0-9][0-9][0-9][0-9];
fragment MONTH : ('0' [1-9]) | '11' | '12';
fragment DAY : ([0-2] [1-9]) | ( [1-3] '0') | '31';

fragment HOUR : ([0-1] [0-9]) | ('2' [0-3]);
fragment MINSEC : [0-5][0-9];

fragment MILLISEC : '.' [0-9]+;

// The DATE leads to weird behavior, since 2000-10-10 is now one token and
// Integer x = 2000-10-10; will throw an error. This is bad language design...
DATE : YEAR '-' MONTH '-' DAY;

INTEGER : [0-9]+;

ID : [a-zA-Z] [0-9a-zA-Z_]*;
