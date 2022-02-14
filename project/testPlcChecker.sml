use "Plc.sml";
exception erroTestePlcChecker;

let
    val testeDiffBrTypes = teval(Let("b", Prim2("=", ConI 1, ConI 2),If(Var "b", Var "b", ConI 6)))[];
in 
    print("[ERRO] Esperada a exceção DiffBrTypes\n");
    raise erroTestePlcChecker
end handle DiffBrTypes => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeCallTypeMism = teval(Let("f",Anon (BoolT,"x",If (Var "x",ConI 11,ConI 22)),Call (Var "f",ConI 0)))[];
in
    print("[ERRO] Esperada a exceção CallTypeMisM\n");
    raise erroTestePlcChecker
end handle CallTypeMisM => print ("[SUCESSO] Exceção lançada com sucesso\n"); 

let 
    val testeWrongRetType = teval(Letrec("f",BoolT,"x",BoolT,If (Var "x",ConI 11,ConI 22), Call (Var "f",ConB true)))[];
in
    print("[ERRO] Esperada a exceção WrongRetType\n");
    raise erroTestePlcChecker
end handle WrongRetType => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeEmptySeq = teval (fromString "(Bool [])") [];
in
    print("[ERRO] Esperada a exceção EmptySeq\n");
    raise erroTestePlcChecker
end handle EmptySeq => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeUnknownType = teval (fromString "(1::2)") [("t", IntT)]
in
    print("[ERRO] Esperada a exceção UnknownType\n");
    raise erroTestePlcChecker
end handle UnknownType => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeNotEqTypes = teval (fromString "true != 1") [];
in
    print("[ERRO] Esperada a exceção NotEqTypes\n");
    raise erroTestePlcChecker
end handle NotEqTypes => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeWrongRetType = teval (fromString "fun rec f(Bool x):Int = if x != true then false else x; f(false)") [];
in
     print("[ERRO] Esperada a exceção WrongRetType\n");
    raise erroTestePlcChecker
end handle WrongRetType => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeDiffBrTypes = teval (fromString "fun f(Bool x) = if x != true then 0 else x; f(false)") []
in
    print("[ERRO] Esperada a exceção DiffBrTypes\n");
    raise erroTestePlcChecker
end handle DiffBrTypes => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeIfCondNotBool = teval (fromString "if () then true else false") []
in
    print("[ERRO] Esperada a exceção IfCondNotBool\n");
    raise erroTestePlcChecker
end handle IfCondNotBool => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeNoMatchResults = teval (fromString "match 3 with end") [];
in
    print("[ERRO] Esperada a exceção NoMatchResults\n");
    raise erroTestePlcChecker
end handle NoMatchResults => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeMatchResTypeDiff = teval (fromString "match x with | 1 -> 0 | _ -> false end") [("x", IntT)]
in
    print("[ERRO] Esperada a exceção MatchResTypeDiff\n");
    raise erroTestePlcChecker
end handle MatchResTypeDiff => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeMatchCondTypesDiff = teval (fromString "match x with | 1 -> true | 0 -> false end") [("x", BoolT)]
in
    print("[ERRO] Esperada a exceção MatchCondTypesDiff\n");
    raise erroTestePlcChecker
end handle MatchCondTypesDiff => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeCallTypeMisM = teval (fromString "fun f(Bool x) = if x != true then 0 else 1; f(6)") [];
in
    print("[ERRO] Esperada a exceção CallTypeMisM\n");
    raise erroTestePlcChecker
end handle CallTypeMisM => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeNotFunc = teval (fromString "var x = false; x(false)") []
in
    print("[ERRO] Esperada a exceção NotFunc\n");
    raise erroTestePlcChecker
end handle NotFunc => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeListOutOfRange = teval (fromString "(1,2,3,4)[5]") []
in
    print("[ERRO] Esperada a exceção testeListOutOfRange\n");
    raise erroTestePlcChecker
end handle ListOutOfRange => print ("[SUCESSO] Exceção lançada com sucesso\n");

let
    val testeOpNonList = teval (fromString "var x = false; x[1]") []
in
    print("[ERRO] Esperada a exceção OpNonList\n");
    raise erroTestePlcChecker
end handle OpNonList => print ("[SUCESSO] Exceção lançada com sucesso\n");

print("Testes do PlcChecker aprovados com sucesso\n")