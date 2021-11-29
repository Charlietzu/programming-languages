datatype area = RConst of real | AQuadrado of area | ACirculo of area | ARetangulo of area * area;

fun eval(RConst(valor)) = valor
    | eval(AQuadrado(valor)) = (eval valor) * (eval valor)
    | eval(ACirculo(valor)) = (eval valor) * (eval valor) * 3.14
    | eval(ARetangulo(valor1, valor2)) = (eval valor1) * (eval valor2);

eval(ACirculo(RConst(2.0)));