/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #include <ctype.h>
    // #define YYDEBUG 1
    int yydebug = 1;
    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }
    typedef struct Symbols {
        char* name;
        char* type;
        int line;
        char* etype;
        int addr;
    } Symbol;
    FILE *file;
    Symbol sym[10][40]; // [scope][index]
    int size[10];
    int scope;
    int addr;
    int isarray, is2arr;
    int isbool;
    int iserror;
    int undefinederr;
    char *type="a", *pretype="b";
    int isliteral, wrong_assign;
    int iout;
    float fout;
    int outaddr, storeaddr;
    int cmp0=0,cmp1=1;
    /* Symbol table function - you can add new function if needed. */
    static void create_symbol(/* ... */);
    static void insert_symbol(int scope, int index, int line, char* name, char* type, char* etype);
    static int lookup_symbol(int scope, char* name);
    static int lookup_symbol_type(int scope, char* name, char* typ);
    static void dump_symbol(int scope);
    static void cmp_bool();
    static void compare(char str[5]);
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token INT FLOAT BOOL STRING TRUE FALSE
%token SEMICOLON PRINT WHILE IF ELSE FOR
%token INC DEC 
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN ASSIGN
%token NOT 
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK 

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT IDENT 
%token <s_val> ADD SUB GTR LSS GEQ LEQ EQL NEQ MUL QUO REM AND OR
/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type DeclarationStmt

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;
Type 
    : INT { $$ = "int"; }
    | FLOAT { $$ = "float"; }
    | BOOL { $$ = "bool"; }
    | STRING { $$ = "string"; }
;
Expression 
    : AndExpr 
    | Expression OR AndExpr  
    {   if(strcmp(type,"float")==0 || strcmp(pretype,"float")==0){
            printf("error:%d: invalid operation: (operator OR not defined on float)\n", yylineno+1);
            iserror = 1;
        }
        if(strcmp(type,"int")==0 || strcmp(pretype,"int")==0){
            printf("error:%d: invalid operation: (operator OR not defined on int)\n", yylineno+1);
            iserror = 1;
        }
        printf("OR\n");  
        fprintf(file,"ior\n");
    }
;
AndExpr
    : ComparExpr
    | AndExpr AND ComparExpr 
    {   if(strcmp(type,"float")==0 || strcmp(pretype,"float")==0){
            printf("error:%d: invalid operation: (operator AND not defined on float)\n", yylineno+1);
            iserror = 1;
        }
        if(strcmp(type,"int")==0 || strcmp(pretype,"int")==0){
            printf("error:%d: invalid operation: (operator AND not defined on int)\n", yylineno+1);
            iserror = 1;
        }
        printf("AND\n"); 
        fprintf(file,"iand\n");
    }
;
ComparExpr
    : AddExpr
    | ComparExpr EQL AddExpr { isbool = 1; printf("EQL\n"); compare("EQL");}
    | ComparExpr NEQ AddExpr { isbool = 1; printf("NEQ\n"); compare("NEQ");}
    | ComparExpr GEQ AddExpr { isbool = 1; printf("GEQ\n"); compare("GEQ");}
    | ComparExpr LEQ AddExpr { isbool = 1; printf("LEQ\n"); compare("LEQ");}
    | ComparExpr GTR AddExpr { isbool = 1; printf("GTR\n"); compare("GTR");}
    | ComparExpr LSS AddExpr { isbool = 1; printf("LSS\n"); compare("LSS");}
;
AddExpr
    : MulExpr
    | AddExpr ADD MulExpr 
    {   if(strcmp(type,pretype)!=0){
            printf("error:%d: invalid operation: ADD (mismatched types %s and %s)\n", yylineno+1, pretype, type);
            iserror = 1;
        }
        printf("ADD\n"); 
        fprintf(file,"%cadd\n", type[0]); }
    | AddExpr SUB MulExpr 
    {   if(strcmp(type,pretype)!=0){
            printf("error:%d: invalid operation: SUB (mismatched types %s and %s)\n", yylineno+1, pretype, type);
            iserror = 1;
        }
        printf("SUB\n"); 
        fprintf(file,"%csub\n", type[0]); }
;
MulExpr  
    : UnaryExpr
    | MulExpr MUL UnaryExpr 
    {   if(strcmp(type,pretype)!=0){
            printf("error:%d: invalid operation: MUL (mismatched types %s and %s)\n", yylineno+1, pretype, type);
            iserror = 1;
        }
        printf("MUL\n"); 
        fprintf(file,"%cmul\n", type[0]);}
    | MulExpr QUO UnaryExpr 
    {   if(strcmp(type,pretype)!=0){
            printf("error:%d: invalid operation: QUO (mismatched types %s and %s)\n", yylineno+1, pretype, type);
            iserror = 1;
        }
        printf("QUO\n"); 
        fprintf(file,"%cdiv\n", type[0]);}
    | MulExpr REM UnaryExpr 
    {   if(strcmp(type,"float")==0 || strcmp(pretype,"float")==0){
            printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno+1);
            iserror = 1;
        }
        if(strcmp(type,"bool")==0 || strcmp(pretype,"bool")==0){
            printf("error:%d: invalid operation: (operator REM not defined on bool)\n", yylineno+1);
            iserror = 1;
        }
        printf("REM\n"); 
        fprintf(file,"%crem\n", type[0]);}
;
UnaryExpr
    : PrimaryExpr 
    { 
        if(isarray == 1){
            fprintf(file,"%caload\n", type[0]); 
            isarray = 0;
        }
    }
    | ADD UnaryExpr { printf("POS\n"); }
    | SUB UnaryExpr { printf("NEG\n"); fprintf(file,"%cneg\n", type[0]);}
    | NOT UnaryExpr 
    {   printf("NOT\n"); 
        fprintf(file, "iconst_1\n");
        fprintf(file, "ixor\n");}
;
PrimaryExpr
    : Operand {  isarray = 0; }
    | IndexExpr { isarray = 1; }
    | ConversionExpr
;
Operand 
    : Literal 
    | LPAREN Expression RPAREN
    | IDENT 
    {   
        outaddr = lookup_symbol(scope,$1);
        if(outaddr!=-1){
            printf("IDENT (name=%s, address=%d)\n", $1, outaddr);
            if(isarray == 1){
                isarray = 0;
                fprintf(file, "aload %d\n", outaddr);
            }
            else if(type[0]=='i')
                fprintf(file, "iload %d\n", outaddr);
            else if(type[0]=='f')
                fprintf(file, "fload %d\n", outaddr);
            else if(type[0]=='s')
                fprintf(file, "aload %d\n", outaddr);
        }
        else{
            iserror = 1;
            printf("error:%d: undefined: %s\n", yylineno+1, $1);
            undefinederr = 1;
        }
        isliteral = 0;
    }
    | TRUE { printf("TRUE\n"); pretype = type; type = "bool"; fprintf(file,"iconst_1\n");}
    | FALSE { printf("FALSE\n"); pretype = type; type = "bool"; fprintf(file,"iconst_0\n");}
;
Literal 
    : INT_LIT 
    { printf("INT_LIT %d\n", $1); 
       if(isarray != 1){
        pretype = type; 
        type = "int"; }
        isliteral = 1;
        fprintf(file,"ldc %d\n", $1);
    }
    | FLOAT_LIT { printf("FLOAT_LIT %f\n", $1); pretype = type; type = "float"; fprintf(file,"ldc %f\n", $1);}
    | STRING_LIT { printf("STRING_LIT %s\n", $1);  pretype = type; type = "string"; fprintf(file,"ldc \"%s\"\n", $1);}
;
IndexExpr 
    : PrimaryExpr Lb IndexBlock Rb { isarray=1; }
;
IndexBlock
    : Operand ADD Operand { printf("ADD\n"); fprintf(file,"iadd\n");}
    | Operand SUB Operand { printf("SUB\n"); fprintf(file,"isub\n");}
    | Operand
;
Lb 
    : LBRACK { isarray=1; }
;
Rb
    : RBRACK { isarray=1; }
;
ConversionExpr 
    : LPAREN Type RPAREN Expression 
    {   printf("%c to %c\n", toupper(type[0]), toupper($2[0])); 
        fprintf(file, "%c2%c\n", type[0], $2[0]);
        type = $2;
    }
;
Statement 
    : DeclarationStmt
    | AssignmentStmt
    | IncDecStmt
    | Block
    | IfStmt
    | WhileStmt
    | ForStmt
    | PrintStmt
    | ArithmeticStmt
;
DeclarationStmt
    : Type IDENT ASSIGN Expression SEMICOLON 
    {   int yline = lookup_symbol_type(scope, $2, $1);
        if(yline == -1){
            if(strcmp($1, "int")==0){
                fprintf(file,"istore %d\n",addr);
            }
            else if(strcmp($1, "float")==0){
                fprintf(file,"fstore %d\n",addr);
            }
            else if(strcmp($1, "string")==0){
                fprintf(file,"astore %d\n",addr);
            }
            insert_symbol(scope, size[scope], yylineno, $2, $1, "-");
        }
        else{
            iserror = 1;
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno+1, $2, yline+1);
        }
    }
    | Type IDENT Expression SEMICOLON
    {   int yline = lookup_symbol_type(scope, $2, $1);
        if(yline == -1){
            insert_symbol(scope, size[scope], yylineno, $2, $1, "-");
        }
        else{
            iserror = 1;
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno+1, $2, yline+1);
        }
    }
    | Type IDENT LBRACK Expression RBRACK SEMICOLON
    {   
        if(lookup_symbol_type(scope, $2, "array") == -1){
            if(strcmp($1, "int")==0)
                fprintf(file, "newarray int\n");
            else
                fprintf(file, "newarray float\n");
            fprintf(file, "astore %d\n", addr);
            insert_symbol(scope, size[scope], yylineno, $2, "array", $1);
        }
    }
    | Type IDENT SEMICOLON
    {   int yline = lookup_symbol_type(scope, $2, $1);
        if(yline == -1){
            if(strcmp($1, "int")==0){
                fprintf(file,"ldc 0\n");
                fprintf(file,"istore %d\n",addr);
            }
            else if(strcmp($1, "float")==0){
                fprintf(file,"ldc 0.0\n");
                fprintf(file,"fstore %d\n",addr);
            }
            else if(strcmp($1, "string")==0){
                fprintf(file,"ldc \"\"\n");
                fprintf(file,"astore %d\n",addr);
            }
            insert_symbol(scope, size[scope], yylineno, $2, $1, "-");
        }
        else{
            iserror = 1;
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno+1, $2, yline+1);
        }
    }
;
AssignmentExpr
    : PrimaryExpr { if(isarray==1) { isarray=0; is2arr=1; } storeaddr = outaddr;} ASSIGN Expression 
    { if(strcmp(type,pretype)!=0 && undefinederr!=1){
            iserror = 1;
            printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno+1, pretype, type);
            undefinederr = 0;
        }
      printf("ASSIGN\n"); 
      if(is2arr == 1){
          is2arr = 0;
          fprintf(file,"%castore\n", type[0]);
      }
      else if(strcmp(type,"string")==0)
        fprintf(file,"astore %d\n", storeaddr);
      else if(strcmp(type,"int")==0 || strcmp(type,"float")==0)
        fprintf(file,"%cstore %d\n", type[0], storeaddr);
    }
    | PrimaryExpr {if(isliteral == 1) wrong_assign = 1;} ADD_ASSIGN Expression 
    {   if(wrong_assign == 1){
            iserror = 1;
            printf("error:%d: cannot assign to %s\n", yylineno+1, pretype);
            wrong_assign = 0;
        }
        printf("ADD_ASSIGN\n"); 
        fprintf(file,"%cadd\n",type[0]);
        fprintf(file,"%cstore %d\n", type[0], outaddr);
    }
    | PrimaryExpr SUB_ASSIGN Expression 
    {   printf("SUB_ASSIGN\n"); 
        fprintf(file,"%csub\n",type[0]);
        fprintf(file,"%cstore %d\n", type[0], outaddr);
    }
    | PrimaryExpr MUL_ASSIGN Expression 
    {   printf("MUL_ASSIGN\n"); 
        fprintf(file,"%cmul\n",type[0]);
        fprintf(file,"%cstore %d\n", type[0], outaddr);
    }
    | PrimaryExpr REM_ASSIGN Expression 
    {   printf("REM_ASSIGN\n"); 
        fprintf(file,"%crem\n",type[0]);
        fprintf(file,"%cstore %d\n", type[0], outaddr);
    }
    | PrimaryExpr QUO_ASSIGN Expression 
    {   printf("QUO_ASSIGN\n");
        fprintf(file,"%cdiv\n",type[0]);
        fprintf(file,"%cstore %d\n", type[0], outaddr);
    }
;
AssignmentStmt 
    : AssignmentExpr SEMICOLON 
;
IncDecExpr 
    : Expression INC 
    {   printf("INC\n"); 
        if(strcmp(type,"int")==0)
            fprintf(file,"ldc 1\niadd\nistore %d\n",outaddr);
        else
            fprintf(file,"ldc 1.0\nfadd\nfstore %d\n",outaddr);
    }
    | Expression DEC 
    {   printf("DEC\n"); 
        if(strcmp(type,"int")==0)
            fprintf(file,"ldc 1\nisub\nistore %d\n",outaddr);
        else
            fprintf(file,"ldc 1.0\nfsub\nfstore %d\n",outaddr);
    }
;
IncDecStmt 
    : IncDecExpr SEMICOLON
;
ArithmeticStmt
    : Expression SEMICOLON
;
Block 
    : LBRACE StatementList RBRACE { dump_symbol(scope); scope--;}
;
StatementList 
    : Statement
    | StatementList Statement
;
IfStmt 
    : IF Condition Block 
    | IF Condition Block  Else2 IfStmt 
    | IF Condition Block  Else1 Block 
;
Else1
    : ELSE {scope++;}
;
Else2
    : ELSE
;
Condition 
    : Expression 
    {
        scope++;
        if(strcmp(type,"int")==0 && isbool != 1){
            printf("error:%d: non-bool (type int) used as for condition\n", yylineno+2);
            iserror = 1;
        }
        if(strcmp(type,"float")==0 && isbool != 1){
            printf("error:%d: non-bool (type float) used as for condition\n", yylineno+2);
            iserror = 1;
        }
        isbool = 0; 
    }
;
WhileStmt
    : WHILE LPAREN Condition RPAREN Block 
;
ForStmt 
    : FOR LPAREN ForClause RPAREN Block 
;
ForClause 
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt
;
InitStmt 
    : SimpleExpr
;
PostStmt 
    : SimpleExpr
;
SimpleExpr 
    : AssignmentExpr | Expression | IncDecExpr
;
PrintStmt 
    : PRINT LPAREN Expression RPAREN SEMICOLON
    { if(isarray == 1) {
        printf("PRINT %s\n", pretype);
        isarray = 0;
        }
        else{
            if(strcmp(type,"int")==0 || strcmp(type,"float")==0){
                printf("PRINT %s\n", type);
                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(file,"swap\n");
                fprintf(file,"invokevirtual java/io/PrintStream/print(%c)V\n",toupper(type[0]));
            }
            else if(strcmp(type,"bool")==0){
                cmp_bool();
                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(file,"swap\n");
                fprintf(file,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
            }
            else{
                fprintf(file,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(file,"swap\n");
                fprintf(file,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
            }
        }
    }
;

%%

/* C code section */
void create_symbol(){
    for(int i = 0; i < 10; i++){
        for(int j = 0; j < 40; j++){
            sym[i][j].name = "";
            sym[i][j].type = "";
            sym[i][j].etype = "";
            sym[i][j].line = -1;
            sym[i][j].addr = 0;
        }
        size[i] = 0;
    }
    addr=0;
}
void insert_symbol(int scope, int index, int line, char* name, char* type, char* etype){
    sym[scope][index].line = line;
    sym[scope][index].name = name;
    sym[scope][index].type = type;
    sym[scope][index].etype = etype;
    sym[scope][index].addr = addr;
    addr++;
    size[scope]++;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
}
int lookup_symbol(int scope, char* name){
    for(int i = scope; i >= 0; i--){
        for(int j = 0; j < size[i]; j++){
            if(strcmp(sym[i][j].name, name) == 0){
                pretype = type;
                if(strcmp(sym[i][j].type,"array")==0){
                    type = sym[i][j].etype;
                    isarray = 1;
                }
                else{
                    type = sym[i][j].type;
                    //fprintf(file, "%s : %s\n", name, type);
                }
                return sym[i][j].addr; //already exist
            }
        }
    }
    return -1; //not exist
}
int lookup_symbol_type(int scope, char* name, char* typ){
    //for(int i = 0; i <= scope; i++){
    int i = scope;
    for(int j = 0; j < size[i]; j++){
        //printf("%s %s\n", sym[scope][j].name, name);
        if(strcmp(sym[i][j].name, name) == 0){
            return sym[i][j].line; //already exist
        }
    }
    //}
    return -1; //not exist
}

void dump_symbol(int scope){
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno","Element type");
    for(int i = 0; i < size[scope]; i++){
        printf("%-10d%-10s%-10s%-10d%-10d%s\n", i, sym[scope][i].name, sym[scope][i].type,
         sym[scope][i].addr, sym[scope][i].line+1, sym[scope][i].etype);
    }
    for(int i = 0; i < size[scope]; i++){
        sym[scope][i].name = "";
        sym[scope][i].type = "";
        sym[scope][i].etype = "";
        sym[scope][i].line = -1;
        sym[scope][i].addr = 0;
    }
    size[scope] = 0;
}
void cmp_bool(){
    fprintf(file, "ifne L_cmp_%d\n", cmp0);
	fprintf(file, "ldc \"false\"\n");
	fprintf(file, "goto L_cmp_%d\n",cmp1);
	fprintf(file, "L_cmp_%d:\n",cmp0);
	fprintf(file, "ldc \"true\"\n");
	fprintf(file, "L_cmp_%d:\n",cmp1);
    cmp0 = cmp0 + 2;
    cmp1 = cmp1 + 2;
}
void compare(char str[5])	
{
    char ifstmt[5];
    if(strcmp(type, "int") == 0)
	    fprintf(file, "isub\n");
    else if(strcmp(type, "float") == 0)
	    fprintf(file, "fcmpl\n");

	if(strcmp(str,"EQL")==0)
		strcpy(ifstmt,"ifeq");
	else if(strcmp(str,"NEQ")==0)
		strcpy(ifstmt,"ifnq");
	else if(strcmp(str,"GTR")==0)
		strcpy(ifstmt,"ifgt");
	else if(strcmp(str,"LSS")==0)
		strcpy(ifstmt,"iflt");
	else if(strcmp(str,"GEQ")==0)
		strcpy(ifstmt,"ifge");
	else if(strcmp(str,"LEQ")==0)
		strcpy(ifstmt,"ifle");;

    fprintf(file, "%s L_cmp_%d\n",ifstmt, cmp0);
	fprintf(file, "iconst_0\n");
	fprintf(file, "goto L_cmp_%d\n",cmp1);
	fprintf(file, "L_cmp_%d:\n",cmp0);
	fprintf(file, "iconst_1\n");
	fprintf(file, "L_cmp_%d:\n",cmp1);
    cmp0 = cmp0 + 2;
    cmp1 = cmp1 + 2;
}
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    file = fopen("hw3.j", "w");
    fprintf(file, ".source hw3.j\n");
	fprintf(file, ".class public Main\n");
	fprintf(file, ".super java/lang/Object\n");
	fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
	fprintf(file, ".limit stack 100\n");
	fprintf(file, ".limit locals 100 ;\n");

    yylineno = 0;
    create_symbol();
    yyparse();
    dump_symbol(0);
	printf("Total lines: %d\n", yylineno+1);

    fprintf(file, "return\n");
	fprintf(file, ".end method\n");
    if (iserror) {
        remove("hw3.j");
    }
    fclose(file);
    fclose(yyin);

    return 0;
}



