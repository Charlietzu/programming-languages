datatype UnOp = Not;
datatype BinOp = Add | Sub | Mul | Gt | Eq | Or;
datatype Sexpr = IConst of int | Op1 of UnOp * Sexpr | Op2 of BinOp * Sexpr
* Sexpr;

fun simplify (Op2(Add, IConst(valor1), IConst(valor2))) = if (valor1 = 0) then simplify(valor2) else if (valor2 = 0) then simplify(valor1) else simplify(valor1 + valor2)
    | simplify(IConst(valor)) = simplify(valor)
    | simplify(valor) = valor;

simplify(Op2(Add, IConst(2), IConst(0)));