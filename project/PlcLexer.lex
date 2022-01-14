(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

fun converterStringParaInteiro s =
    case Int.fromString s of
    SOME i => i
    |  NONE => raise Fail ("Nao foi possivel converter de string para int")

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun obterPalavraReservada (palavra, esquerda, direita) =
    case palavra of 
        "var" => VARIAVEL (esquerda, direita)
        | "end" => END (esquerda, direita)
        | "fn" => FUNCAOANONIMA (esquerda, direita)
        | "rec" => RECURSIVIDADE (esquerda, direita)
        | "if" => IF (esquerda, direita)
        | "then" => THEN (esquerda, direita)
        | "else" => ELSE (esquerda, direita)
        | "match" => MATCH (esquerda, direita)
        | "with" => WITH (esquerda, direita)
        | "hd" => HD (esquerda, direita)
        | "tl" => TL (esquerda, direita)
        | "ise" => ISE (esquerda, direita)
        | "print" => PRINT (esquerda, direita)
        | "_" => UNDERSCORE (esquerda, direita)
        | "Nil" => LISTA (esquerda, direita)
        | "Int" => INTEIRO (esquerda, direita)
        | "Bool" => BOOLEANO (esquerda, direita)
        | "true" => TRUE (esquerda, direita)
        | "false" => FALSE (esquerda, direita)
        | "fun" => FUNCAONORMAL (esquerda, direita)
        | _ => NOME (palavra, esquerda, direita)

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
caracteresAlfa=[A-Za-z];
digitosNumericos=[0-9];
identificador=[a-zA-Z_][a-zA-Z_0-9]*;
espaco=[\ \t];
%s COMMENTARY;
%%

\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>"(*" => (YYBEGIN COMMENTARY; lex());
<COMMENTARY>"*)" => (YYBEGIN INITIAL; lex());
<COMMENTARY>. => (lex());
<INITIAL>{espaco}+ => (lex());
<INITIAL>{digitosNumericos}+ => (CINT(converterStringParaInteiro(yytext), yypos, yypos));
<INITIAL>{identificador} => (obterPalavraReservada(yytext, yypos, yypos));
<INITIAL>"!" => (NEGACAO(yypos, yypos));
<INITIAL>"&&" => (E(yypos, yypos));
<INITIAL>"+" => (SOMA(yypos, yypos));
<INITIAL>"-" => (SUBTRACAO(yypos, yypos));
<INITIAL>"*" => (MULTIPLICACAO(yypos, yypos));
<INITIAL>"/" => (DIVISAO(yypos, yypos));
<INITIAL>"=" => (IGUAL(yypos, yypos));
<INITIAL>"!=" => (DIFERENTE(yypos, yypos));
<INITIAL>"<" => (MENOR(yypos, yypos));
<INITIAL>"<=" => (MENOROUIGUAL(yypos, yypos));
<INITIAL>"::" => (ADICIONAELEMENTOLISTA(yypos, yypos));
<INITIAL>":" => (DOISPONTOS(yypos, yypos));
<INITIAL>";" => (PONTOEVIRGULA(yypos, yypos));
<INITIAL>"," => (VIRGULA(yypos, yypos));
<INITIAL>"->" => (SETA(yypos, yypos));
<INITIAL>"|" => (BARRA(yypos, yypos));
<INITIAL>"=>" => (SETADUPLA(yypos, yypos));
<INITIAL>"(" => (PARENTESESESQUERDO(yypos, yypos));
<INITIAL>")" => (PARENTESESDIREITO(yypos, yypos));
<INITIAL>"{" => (CHAVEESQUERDA(yypos, yypos));
<INITIAL>"}" => (CHAVEDIREITA(yypos, yypos));
<INITIAL>"[" => (COLCHETEESQUERDO(yypos, yypos));
<INITIAL>"]" => (COLCHETEDIREITO(yypos, yypos));
<INITIAL>. => (error("\n***Erro no lexer***\n");
                raise Fail("Erro no lexer" ^yytext));