
exception EmptySeq	
exception UnknownType	
exception NotEqTypes	
exception WrongRetType	
exception DiffBrTypes	
exception IfCondNotBool	
exception NoMatchResults	
exception MatchResTypeDiff	
exception MatchCondTypesDiff	
exception CallTypeMisM	
exception NotFunc	
exception ListOutOfRange	
exception OpNonList

fun teval (ConI _) _ = IntT
    | teval (ConB _) _ = BoolT
    | teval (Var variavelEval) (env:plcType env) = lookup env variavelEval
    | teval (List listaEval) (env:plcType env) =
        let
            fun verificarLista (cabeca::[]) = (teval cabeca env)::[]
                | verificarLista (cabeca::calda) = (teval cabeca env)::verificarLista calda
                | verificarLista _ = []
            val tipoLista = verificarLista listaEval
        in
            ListT tipoLista
        end
    | teval (ESeq sequencia) _ =
        let in
            case sequencia of
                SeqT tipoSequencia => SeqT tipoSequencia
            | _ => raise EmptySeq
        end
    | teval (Item (index, expressao)) (env:plcType env) =
        let
            fun verificarElemento (i, []) = raise ListOutOfRange
                | verificarElemento (i, (cabeca::[])) = if i = 1 then cabeca else raise ListOutOfRange
                | verificarElemento (i, (cabeca::calda)) = if i = 1 then cabeca else verificarElemento (i - 1, calda)
            val vType = teval expressao env
        in
            case vType of
                ListT tipoLista => verificarElemento(index, tipoLista)
            | _ => raise OpNonList
        end
    | teval (Let(var, expressao1, expressao2)) (env:plcType env) =
        let
            val tipoExpressao1 = teval expressao1 env
            val novoEnv = (var, tipoExpressao1) :: env
        in
            teval expressao2 novoEnv
        end
    | teval (Anon(t, argumento, expressao)) (env:plcType env) = 
        let
            val novoEnv = (argumento, t) :: env
            val tipoExpressao = teval expressao novoEnv
        in
            FunT (t, tipoExpressao)
        end
    | teval (Call(expressao2, expressao1)) (env:plcType env) =
        let
            val tipoExpressao1 = teval expressao1 env
            val tipoExpressao2 = teval expressao2 env
        in
            case tipoExpressao2 of FunT (tipoParametro, tipoResultado) => 
                    if tipoExpressao1 = tipoParametro then tipoResultado else raise CallTypeMisM
            | _ => raise NotFunc
        end
    | teval (If(condicao, expressao1, expressao2)) (env:plcType env) =
        let
            val tipoCondicao = teval condicao env
            val tipoExpressao1 = teval expressao1 env
            val tipoExpressao2 = teval expressao2 env
        in
            case tipoCondicao of
                BoolT => if tipoExpressao1 = tipoExpressao2 then tipoExpressao1 else raise DiffBrTypes
            | _ => raise IfCondNotBool
        end
    | teval (Prim1(operador, expressao)) (env:plcType env) =
        let
            val tipoExpressao = teval expressao env
        in
            case operador of "!" => if tipoExpressao = BoolT then BoolT else raise UnknownType
            | "-" => if tipoExpressao = IntT then IntT else raise UnknownType
            | "hd" => let in
                    case tipoExpressao of
                        SeqT tipoSequencia => tipoSequencia
                    | _ => raise UnknownType
                end
            | "tl" => let in
                    case tipoExpressao of
                        SeqT tipoSequencia => SeqT tipoSequencia
                    | _ => raise UnknownType
                end
            | "ise" => let in
                    case tipoExpressao of
                        SeqT tipoSequencia => BoolT
                    | _ => raise UnknownType
                end
            | "print" => ListT []
            | _ => raise UnknownType
        end
    | teval (Letrec(nomeFuncao, tipoParametro, parametro, tipoFuncao, expressao1, expressao2)) (env:plcType env) =
        let
            val recEnv = (nomeFuncao, FunT (tipoParametro, tipoFuncao))
            val parametroEnv = (parametro, tipoParametro)
            val tipoExpressao1 = teval expressao1 (recEnv :: parametroEnv :: env)
            val tipoExpressao2 = teval expressao2 (recEnv :: env)
        in
            if tipoExpressao1 = tipoFuncao then tipoExpressao2 else raise WrongRetType
        end
    | teval (Match(expressao1, expressao2)) (env:plcType env) =
        if null expressao2 then raise NoMatchResults else
            let
                val condicaoComeco = teval expressao1 env
                val padrao = (#2 (hd expressao2))
                val tipoPadrao = teval padrao env
                fun find (Match(expressao1, expressao2)) (env:plcType env) =
                        let in
                            case expressao2 of x::[] => let in
                                        case x of
                                            (SOME expressao2, expressao3) => 
                                                if (teval expressao3 env) = tipoPadrao then
                                                    if condicaoComeco = (teval expressao2 env) then 
                                                        teval expressao3 env 
                                                    else raise MatchCondTypesDiff
                                                else raise MatchResTypeDiff
                                        | (NONE, expressao3) => if (teval expressao3 env) = tipoPadrao then tipoPadrao else raise MatchResTypeDiff
                                    end
                            | x::xs => let in
                                    case x of (SOME expressao2, expressao3) => 
                                            if (teval expressao3 env) = tipoPadrao then
                                                if condicaoComeco = (teval expressao2 env) then
                                                    find (Match(expressao1, xs)) env 
                                                else raise MatchCondTypesDiff
                                            else raise MatchResTypeDiff
                                    | _ => raise UnknownType
                                end
                        end
                    | find _ _ = raise UnknownType
            in
                find (Match(expressao1, expressao2)) env
            end
    | teval (Prim2(operador, expressao1, expressao2)) (env:plcType env) =
        let
            val tipoExpressao1 = teval expressao1 env
            val tipoExpressao2 = teval expressao2 env
        in
            case operador of "&&" => if tipoExpressao1 = BoolT andalso tipoExpressao2 = BoolT then BoolT else raise UnknownType
            | "::" => let in
                    case (tipoExpressao1, tipoExpressao2) of (IntT, ListT []) => SeqT IntT
                    | (IntT, SeqT seqType) => if seqType = IntT then SeqT seqType else raise NotEqTypes
                    | (BoolT, ListT []) => SeqT BoolT
                    | (BoolT, SeqT seqType) => if seqType = BoolT then SeqT seqType else raise NotEqTypes
                    | (ListT t, ListT []) => SeqT (ListT t)
                    | (ListT t, SeqT seqType) => if seqType = ListT t then SeqT seqType else raise NotEqTypes
                    | _ => raise UnknownType
                end
            | "<" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then BoolT else raise UnknownType
            | "<=" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then BoolT else raise UnknownType
            | "=" => if tipoExpressao1 = tipoExpressao2 andalso (tipoExpressao1 = IntT orelse tipoExpressao1 = BoolT) then BoolT else raise NotEqTypes
            | "!=" => if tipoExpressao1 = tipoExpressao2 andalso (tipoExpressao1 = IntT orelse tipoExpressao1 = BoolT) then BoolT else raise NotEqTypes
            | "+" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then IntT else raise UnknownType
            | "-" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then IntT else raise UnknownType
            | "*" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then IntT else raise UnknownType
            | "/" => if tipoExpressao1 = IntT andalso tipoExpressao2 = IntT then IntT else raise UnknownType
            | ";" => tipoExpressao2
            | _ => raise UnknownType
        end

;