fun pow(valor: int) = 
    let
        fun calculePow(valorCalcular: int) =
            valorCalcular * valorCalcular
    in calculePow(valor)
end;

pow(3);