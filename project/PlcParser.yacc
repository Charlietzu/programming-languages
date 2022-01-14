%%

%name PlcParser

%pos int

%term VARIAVEL | END | FUNCAOANONIMA | RECURSIVIDADE
    | IF | THEN | ELSE
    | MATCH | WITH
    | NEGACAO | E
    | HD | TL | ISE
    | PRINT
    | SOMA | SUBTRACAO | MULTIPLICACAO | DIVISAO
    | IGUAL | DIFERENTE | MENOR | MENOROUIGUAL
    | ADICIONAELEMENTOLISTA | DOISPONTOS | PONTOEVIRGULA | VIRGULA | SETA | BARRA | UNDERSCORE | SETADUPLA
    | LISTA | BOOLEANO | INTEIRO
    | TRUE | FALSE
    | PARENTESESESQUERDO | PARENTESESDIREITO | CHAVEESQUERDA | CHAVEDIREITA | COLCHETEESQUERDO | COLCHETEDIREITO
    | NOME of string | CINT of int | FUNCAONORMAL
    | EOF


%nonterm Prog of expr 
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list 
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list

%eop EOF

%right PONTOEVIRGULA SETA
%nonassoc IF
%left ELSE
%left E
%left IGUAL DIFERENTE
%left MENOR MENOROUIGUAL
%right ADICIONAELEMENTOLISTA
%left SOMA SUBTRACAO
%left MULTIPLICACAO DIVISAO
%nonassoc NEGACAO HD TL ISE PRINT NOME
%left COLCHETEESQUERDO


%noshift EOF

%start Prog

%%

Prog: Expr (Expr) 
    | Decl (Decl)

Decl: VARIAVEL NOME IGUAL Expr PONTOEVIRGULA Prog (Let(NOME, Expr, Prog))
    | FUNCAONORMAL NOME Args IGUAL Expr PONTOEVIRGULA Prog (Let(NOME, makeAnon(Args, Expr), Prog))
    | FUNCAONORMAL RECURSIVIDADE NOME Args DOISPONTOS Type IGUAL Expr PONTOEVIRGULA Prog (makeFun(NOME, Args, Type, Expr, Prog))

Expr: AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NEGACAO Expr (Prim1("!", Expr))
    | Expr E Expr (Prim2("&&", Expr1, Expr2))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr SOMA Expr (Prim2("+", Expr1, Expr2))
    | Expr SUBTRACAO Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTIPLICACAO Expr (Prim2("*", Expr1, Expr2))
    | Expr DIVISAO Expr (Prim2("/", Expr1, Expr2))
    | SUBTRACAO Expr (Prim1("-", Expr))
    | Expr IGUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr DIFERENTE Expr (Prim2("!=", Expr1, Expr2))
    | Expr MENOR Expr (Prim2("<", Expr1, Expr2))
    | Expr MENOROUIGUAL Expr (Prim2("<=", Expr1, Expr2))
    | Expr ADICIONAELEMENTOLISTA Expr (Prim2("::", Expr1, Expr2))
    | Expr PONTOEVIRGULA Expr (Prim2(";", Expr1, Expr2))
    | Expr COLCHETEESQUERDO CINT COLCHETEDIREITO (Item(CINT, Expr))

AtomExpr: Const (Const)
        | NOME (Var(NOME))
        | CHAVEESQUERDA Prog CHAVEDIREITA (Prog)
        | PARENTESESESQUERDO Comps PARENTESESDIREITO (List(Comps))
        | PARENTESESESQUERDO Expr PARENTESESDIREITO (Expr)
        | FUNCAOANONIMA Args SETADUPLA Expr END (makeAnon(Args, Expr))

AppExpr: AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
       | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const: TRUE (ConB true) | FALSE (ConB false)
     | CINT (ConI CINT)
     | PARENTESESESQUERDO PARENTESESDIREITO (List [])
     | PARENTESESESQUERDO Type COLCHETEESQUERDO COLCHETEDIREITO PARENTESESDIREITO (ESeq(Type))

Comps: Expr VIRGULA Expr (Expr1 :: Expr2 :: [])
     | Expr VIRGULA Comps (Expr :: Comps)

MatchExpr: END ([])
         | BARRA CondExpr SETA Expr MatchExpr ((CondExpr, Expr) :: MatchExpr)

CondExpr: Expr (SOME(Expr))
    | UNDERSCORE (NONE)
    
Args: PARENTESESESQUERDO PARENTESESDIREITO ([])
    | PARENTESESESQUERDO Params PARENTESESDIREITO (Params)
    
Params: TypedVar (TypedVar :: [])
      | TypedVar VIRGULA Params (TypedVar :: Params)

TypedVar: Type NOME ((Type, NOME))

Type: AtomType (AtomType)
    | PARENTESESESQUERDO Types PARENTESESDIREITO (ListT Types)
    | COLCHETEESQUERDO Type COLCHETEDIREITO (SeqT Type)
    | Type SETA Type (FunT (Type1, Type2))

AtomType: LISTA (ListT [])
        | BOOLEANO (BoolT)
        | INTEIRO (IntT)
        | PARENTESESESQUERDO Type PARENTESESDIREITO (Type)

Types: Type VIRGULA Type (Type1::Type2::[])
     | Type VIRGULA Types (Type::Types)
