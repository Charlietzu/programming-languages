fun sumValues(value1, value2) = value1 + value2;
fun powerSquare(anyValue) = anyValue * anyValue;
fun combiner(f,g,x,y) = f(g(x,y));

combiner(powerSquare, sumValues, 4, 5);