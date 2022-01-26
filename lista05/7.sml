exception ExcecaoValoresNegativos;

signature MATH = 
sig
    val fact: int -> int
    val halfPi: real
    val pow: int * int -> int
    val double: int -> int
end;

structure MyMathLib :> MATH = 
struct
    fun fact(x) =  if x < 0 then raise ExcecaoValoresNegativos else if x <= 1 then 1 else x * fact(x - 1);
    val halfPi = Math.pi/ 2.0;
    fun pow(x,y) = if y = 0 then 1 else if x < 0 then raise ExcecaoValoresNegativos else x * pow(x, y - 1);
    fun double (x) =  if x < 0 then raise ExcecaoValoresNegativos else x * 2;
end;

fun useMyMathLib(valor, operacao) = 
    let fun f valor "fact" = print(Int.toString(MyMathLib.fact(valor)))
        | f valor "pow" = print(Int.toString(MyMathLib.pow(valor, valor)))
        | f valor "double" = print(Int.toString(MyMathLib.double(valor)))
        | f valor "halfPi" = print(Real.toString(MyMathLib.halfPi))
    in
        f valor operacao
    end
    handle ExcecaoValoresNegativos => print("Não posso lidar com números negativos!");

useMyMathLib(2, "pow");
useMyMathLib(~3, "fact");