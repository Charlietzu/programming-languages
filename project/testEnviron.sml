use "Plc.sml";
exception testError;

let
    val teste = teval (fromString "x") [];
in
    print("Erro => SymbolNotFound não foi declarada!\n");
    raise testError
end handle SymbolNotFound => print ("APROVADO! => SymbolNotFound foi declarada com sucesso!\n");

print("Sucesso!\n");