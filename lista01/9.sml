datatype dinheiro  = Centavos of int | Real of real | Pessoa_Dinheiro of string * real;

fun amount (Centavos(valor)) = valor
    | amount (Real(valor)) = Real.floor(valor) * 100
    | amount (Pessoa_Dinheiro(nome, valor)) = Real.floor(valor) * 100;

amount(Real(2.0));

amount(Centavos(2));

amount(Pessoa_Dinheiro("Gene", 2.5));