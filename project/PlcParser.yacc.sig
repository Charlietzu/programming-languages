signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val FUNCAONORMAL:  'a * 'a -> (svalue,'a) token
val CINT: (int) *  'a * 'a -> (svalue,'a) token
val NOME: (string) *  'a * 'a -> (svalue,'a) token
val COLCHETEESQUERDO:  'a * 'a -> (svalue,'a) token
val COLCHETEDIREITO:  'a * 'a -> (svalue,'a) token
val CHAVEESQUERDA:  'a * 'a -> (svalue,'a) token
val CHAVEDIREITA:  'a * 'a -> (svalue,'a) token
val PARENTESESDIREITO:  'a * 'a -> (svalue,'a) token
val PARENTESESESQUERDO:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val INTEIRO:  'a * 'a -> (svalue,'a) token
val BOOLEANO:  'a * 'a -> (svalue,'a) token
val LISTA:  'a * 'a -> (svalue,'a) token
val SETADUPLA:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val BARRA:  'a * 'a -> (svalue,'a) token
val SETA:  'a * 'a -> (svalue,'a) token
val VIRGULA:  'a * 'a -> (svalue,'a) token
val PONTOEVIRGULA:  'a * 'a -> (svalue,'a) token
val DOISPONTOS:  'a * 'a -> (svalue,'a) token
val ADICIONAELEMENTOLISTA:  'a * 'a -> (svalue,'a) token
val MENOROUIGUAL:  'a * 'a -> (svalue,'a) token
val MENOR:  'a * 'a -> (svalue,'a) token
val DIFERENTE:  'a * 'a -> (svalue,'a) token
val IGUAL:  'a * 'a -> (svalue,'a) token
val DIVISAO:  'a * 'a -> (svalue,'a) token
val MULTIPLICACAO:  'a * 'a -> (svalue,'a) token
val SUBTRACAO:  'a * 'a -> (svalue,'a) token
val SOMA:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val ISE:  'a * 'a -> (svalue,'a) token
val TL:  'a * 'a -> (svalue,'a) token
val HD:  'a * 'a -> (svalue,'a) token
val E:  'a * 'a -> (svalue,'a) token
val NEGACAO:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val RECURSIVIDADE:  'a * 'a -> (svalue,'a) token
val FUNCAOANONIMA:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val VARIAVEL:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
