(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run expressao =
        let
            val tipoExpressao = teval expressao []
            val resultadoExpressao = eval expressao []
        in
            val2string(resultadoExpressao) ^ " : " ^ type2string(tipoExpressao)
        end
        handle EmptySeq =>  "EmptySeq: não é possível haver sequencias sem tipo"
        | UnknownType => "UnknownType: tipo desconhecido"
        | NotEqTypes =>  "NotEqTypes: os tipos não são iguais"
        | WrongRetType =>  "WrongRetType: o tipo de retorno está diferente do esperado"
        | DiffBrTypes =>  "DiffBrTypes: os tipos apresentados no ramo condicional estão diferentes"
        | IfCondNotBool =>  "IfCondNotBool: a comparação dentro de um condicional deve ser booleano"
        | NoMatchResults =>  "NoMatchResults: não foram encontrados resultados para o match"
        | MatchResTypeDiff =>  "MatchResTypeDiff: os tipos do match estão diferentes"
        | MatchCondTypesDiff =>  "MatchCondTypesDiff: os tipos da condição de um match estão diferentes"
        | CallTypeMisM =>  "CallTypeMisM: os tipos dos argumentos passados para a função estão incorretos"
        | NotFunc =>  "NotFunc: isto não é uma função"
        | ListOutOfRange =>  "ListOutOfRange: o índice acessado não existe na lista"
        | OpNonList =>  "OpNonList: não é possível acessar um índice de algo que não seja uma lista"
        | Impossible => "Impossible: erro impossível"
        | HDEmptySeq =>  "HDEmptySeq: não é possível acessar a cabeça de uma sequencia vazia"
        | TLEmptySeq =>  "TLEmptySeq: não é possível acessar a calda de uma sequencia vazia"
        | ValueNotFoundInMatch =>  "ValueNotFoundInMatch: não foi possível realizar o match"
        | NotAFunc =>  "NotAFunc: isso não é uma função"
        | SymbolNotFound => "SymbolNotFound: símbolo não foi encontrado"
        | _ => "UnknownError: erro desconhecido"