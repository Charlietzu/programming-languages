use "Plc.sml";
exception testError;

let 
    val teste = eval (fromString "x = false") [("x", IntV 43)]
in
    print("[ERRO] Impossible deveria ser declarada!\n");
    raise testError
end handle Impossible => print ("[SUCESSO] Impossible foi declarada!\n");

let 
    val teste = eval (fromString "hd ([Int] [])") []
in
    print("[ERRO] HDEmptySeq deveria ser declarada!\n");
    raise testError
end handle HDEmptySeq => print ("[SUCESSO] HDEmptySeq foi declarada!\n");

let 
    val teste = eval (fromString "tl ([Bool] [])") []
in
    print("[ERRO] TLEmptySeq deveria ser declarada!\n");
    raise testError
end handle TLEmptySeq => print ("[SUCESSO] TLEmptySeq foi declarada!\n");

let 
    val teste = eval (fromString "match x with | true -> 1 end") [("x", BoolV false)]
in
    print("[ERRO] ValueNotFoundInMatch deveria ser declarada!\n");
    raise testError
end handle ValueNotFoundInMatch => print ("[SUCESSO] ValueNotFoundInMatch foi declarada!\n");

let 
    val teste = eval (fromString "var x = false; x(false)") []
in
    print("[ERRO] NotAFunc deveria ser declarada!\n");
    raise testError
end handle NotAFunc => print ("[SUCESSO] NotAFunc foi declarada!\n");


print("Testes do PlcInterp aprovados com sucesso\n")