datatype expr = IConst of int | Plus of expr * expr | Minus of expr * expr | Multi of expr * expr |
    Div of expr * expr | Max of expr * expr | Min of expr * expr | Eq of expr * expr | Gt of expr * expr;

fun eval (IConst(value)) = value
    | eval (Plus(value1, value2))  = (eval value1) + (eval value2)
    | eval (Minus(value1, value2)) = (eval value1) - (eval value2)
    | eval (Multi(value1, value2)) = (eval value1) * (eval value2)
    | eval (Div(value1, value2))   = if ((eval value1 = 0) orelse (eval value2 = 0)) then 0 
                                    else (eval value1) div (eval value2)
    | eval (Max(value1, value2))   = if (eval value1) > (eval value2) then (eval value1) 
                                    else (eval value2)
    | eval (Min(value1, value2))   = if (eval value1) < (eval value2) then (eval value1) 
                                    else (eval value2)
    | eval (Eq(value1, value2))    = if (eval value1) = (eval value2) then 1 else 0
    | eval (Gt(value1, value2))    = if (eval value1) > (eval value2) then 1 else 0;


eval(Max(IConst(3), Plus(IConst(2), IConst(3))));
eval(Div(Multi(IConst(5), IConst(4)), Minus(IConst(4), IConst(4))));