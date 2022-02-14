(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc


fun eval (ConI int) _ = IntV int
    | eval (ConB bool) _ = BoolV bool
    | eval (ESeq sequenciaVazia) _ = SeqV []
    | eval (Var v) (env:plcVal env) = lookup env v
    | eval (Item (index, expressao)) (env:plcVal env) =
        let
            fun getItemAtI (index, []) = raise Impossible
                | getItemAtI (index, (h::[])) = if index = 1 then h else raise Impossible
                | getItemAtI (index, (h::t)) = if index = 1 then h else getItemAtI (index - 1, t)
            val valor = eval expressao env
        in
            case valor of ListV list => getItemAtI (index, list)
            | SeqV sequencia => getItemAtI (index, sequencia)
            | _ => raise Impossible
        end
    | eval (Prim1 (operador, expressao)) (env:plcVal env) =
        let
            val valor = eval expressao env
        in
            case valor of IntV int => 
                    let in
                        case operador of
                            "-" => IntV (~ int)
                        | "print" => 
                            let 
                                val valor = IntV int
                                val ignore = print(val2string(valor) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                    end
            | BoolV bool =>
                let in
                    case operador of "!" => BoolV (not bool)
                    | "print" => 
                        let 
                            val valor = BoolV bool
                            val ignore = print(val2string(valor) ^ "\n")
                        in
                            ListV []
                        end
                    | _ => raise Impossible
                end
            | SeqV sequencia =>
                let in
                    case operador of "hd" => let in let in hd sequencia end handle Empty => raise HDEmptySeq end
                    | "tl" => let in let in SeqV (tl sequencia) end handle Empty => raise TLEmptySeq end
                    | "ise" =>
                        let in
                            case sequencia of [] => BoolV true
                            | _ => BoolV false
                        end
                    | "print" => 
                        let 
                            val ignore = print(list2string(val2string, sequencia) ^ "\n")
                        in
                            ListV []
                        end
                    | _ => raise Impossible
                end
            | ListV list =>
                let in
                    case operador of "print" => 
                            let 
                                val ignore = print(list2string(val2string, list) ^ "\n")
                            in
                                ListV []
                            end
                    | _ => raise Impossible
                end
            | _ => raise Impossible
        end
    | eval (Prim2 (operador, expressao1, expressao2)) (env:plcVal env) =
        if operador = ";" then
            let
                val ignore = eval expressao1 env
            in
                eval expressao2 env
            end
        else
            let
                val valor1 = eval expressao1 env
                val valor2 = eval expressao2 env
            in
                case (valor1, valor2) of
                    (IntV int1, IntV int2) => 
                        let in
                            case operador of "+" => IntV (int1 + int2)
                            | "-" => IntV (int1 - int2)
                            | "*" => IntV (int1 * int2)
                            | "/" => IntV (int1 div int2)
                            | "<" => BoolV (int1 < int2)
                            | "<=" => BoolV (int1 <= int2)
                            | "=" => BoolV (int1 = int2)
                            | "!=" => BoolV (int1 <> int2)
                            | _ => raise Impossible
                        end
                | (BoolV bool1, BoolV bool2) => 
                    let in
                        case operador of "&&" => BoolV (bool1 andalso bool2)
                        | "=" => BoolV (bool1 = bool2)
                        | "!=" => BoolV (bool1 <> bool2)
                        | _ => raise Impossible
                    end
                | (IntV int1, SeqV sequencia) => 
                    let in
                        case operador of
                            "::" => SeqV (IntV int1 :: sequencia)
                        | _ => raise Impossible
                    end
                | (BoolV bool1, SeqV sequencia) => 
                    let in
                        case operador of
                            "::" => SeqV (BoolV bool1 :: sequencia)
                        | _ => raise Impossible
                    end
                | (ListV list1, SeqV sequencia) => 
                    let in
                        case operador of
                            "::" => SeqV (ListV list1 :: sequencia)
                        | _ => raise Impossible
                    end
                | _ => raise Impossible
            end
    | eval (Match (expressao1, listaCombinacoes)) (env:plcVal env) = 
        let 
            val expressaoAvaliada = eval expressao1 env 
            fun resolveMatch (variavelAux, h::[]) env =
                    let in
                        case h of (SOME expressao2, expressao3) => if variavelAux = eval expressao2 env then expressao3 else raise ValueNotFoundInMatch
                        | (NONE, expressao3) => expressao3
                    end
                | resolveMatch (variavelAux, h::t) env =  let in
                        case h of (SOME expressao2, expressao3) => if variavelAux = eval expressao2 env then expressao3 else resolveMatch (variavelAux, t) env
                        | (NONE, expressao3) => raise Impossible
                    end
                | resolveMatch (variavelAux, _ ) env = raise Impossible
        in
            eval (resolveMatch (expressaoAvaliada, listaCombinacoes) env) env
        end
    | eval (Let (var, expressao1, expressao2)) (env:plcVal env) =
        let
            val newEnv = (var, eval expressao1 env) :: env
        in
            eval expressao2 newEnv
        end
    | eval (Anon (typ, param, expressao)) (env:plcVal env) = Clos ("", param, expressao, env) (* We need to check if var can be found in the env of Anon *)
    | eval (Call (expressao1, expressao2)) (env:plcVal env) = 
        let
            fun getParams (List (h::[])) = [eval h env]
                | getParams (List (h::t)) = [eval h env] @ getParams (List t)
                | getParams (expressao) = [eval expressao env]
            val newEnv = [("$list", ListV (getParams expressao2))] @ env
            val f = eval expressao1 env
        in
            case f of
                Clos(name, var, expressao, cEnv) =>
                    let
                        val ev = eval expressao2 newEnv
                        val fEnv = (var, ev)::(name, f)::cEnv
                    in
                        eval expressao fEnv
                    end
            | _ => raise NotAFunc
        end
    | eval (Letrec (funName, paramT, param, funT, expressao1, expressao2)) (env:plcVal env) =
        let
            val newEnv = (funName, Clos(funName, param, expressao1, env)) :: env
        in
            eval expressao2 newEnv
        end
    | eval (List []) (env:plcVal env) = ListV []
    | eval (List list) (env:plcVal env) = 
        let
            fun unroll (h::[]) = eval h env :: []
                | unroll (h::t) = eval h env :: unroll t
                | unroll _ = raise Impossible;
        in
            ListV (unroll list)
        end
    | eval (If (expressao1, expressao2, expressao3)) (env:plcVal env) = 
        let in
            case eval expressao1 env of 
                BoolV true => eval expressao2 env
            | BoolV false => eval expressao3 env
            | _ => raise Impossible
        end
;