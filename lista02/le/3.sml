datatype perimetro = RConst of real | PQuadrado of perimetro | PCirculo of perimetro | PRetangulo of perimetro * perimetro
    | PTriangulo of perimetro * perimetro * perimetro;

fun eval(RConst(valor)) = valor
    | eval(PQuadrado(valor)) = (eval valor) * 4.0
    | eval(PCirculo(valor)) = 2.0 * 3.14 * (eval valor)
    | eval(PRetangulo(valor1, valor2)) = (eval valor1) + (eval valor1) + (eval valor2) + (eval valor2)
    | eval(PTriangulo(valor1, valor2, valor3)) = (eval valor1) + (eval valor2) + (eval valor3);

eval(PQuadrado(RConst(4.0)));