use "Plc.sml";
exception testError;

let
    val teste = teval (fromString "x") [];
in
    print("[ERRO] SymbolNotFound não foi declarada!\n");
    raise testError
end handle SymbolNotFound => print ("[SUCESSO] SymbolNotFound foi declarada com sucesso!\n");

print("Testes de símbolos aprovados com sucesso!");