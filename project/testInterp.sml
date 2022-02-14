use "Plc.sml";
exception testError;

let 
    val teste = eval (fromString "x = false") [("x", IntV 43)]
in
    print("Erro => Impossible deveria ser declarada!\n");
    raise testError
end handle Impossible => print ("Sucesso => Impossible foi declarada!\n");

let 
    val teste = eval (fromString "hd ([Int] [])") []
in
    print("Erro => HDEmptySeq deveria ser declarada!\n");
    raise testError
end handle HDEmptySeq => print ("Sucesso => HDEmptySeq foi declarada!\n");

let 
    val teste = eval (fromString "tl ([Bool] [])") []
in
    print("Erro => TLEmptySeq deveria ser declarada!\n");
    raise testError
end handle TLEmptySeq => print ("Sucesso => TLEmptySeq foi declarada!\n");

let 
    val teste = eval (fromString "match x with | true -> 1 end") [("x", BoolV false)]
in
    print("Erro => ValueNotFoundInMatch deveria ser declarada!\n");
    raise testError
end handle ValueNotFoundInMatch => print ("Sucesso => ValueNotFoundInMatch foi declarada!\n");

let 
    val teste = eval (fromString "var x = false; x(false)") []
in
    print("Erro => NotAFunc deveria ser declarada!\n");
    raise testError
end handle NotAFunc => print ("Sucesso => NotAFunc foi declarada!\n");


print("SUCCESS!\n")