fun checkChar(c) = 
    Char.isSpace(c) orelse Char.isPunct(c);

fun split (f) = String.tokens checkChar(f);

split("Bom dia,pra-você");