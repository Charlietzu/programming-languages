signature MATH = 
sig
    val fact: int -> int
    val halfPi: real
    val pow: int * int -> int
    val double: int -> int
end;

structure MyMathLib :> MATH = 
struct
    fun fact(x) =  if x <= 1 then 1 else x * fact(x - 1);
    val halfPi = Math.pi/ 2.0;
    fun pow(x,y) = if y = 0 then 1 else x * pow(x, y - 1);
    fun double (x) =  x * 2;
end;

MyMathLib.fact(5); 
MyMathLib.halfPi;
MyMathLib.pow(2,3);
MyMathLib.double(6);