fun split([]) = ([], [])
    | split([elemento]) = ([elemento], [])
    | split(cabeca::meio::restante) = 
    let
        val (esquerda, direita) = split(restante)
    in
        ((cabeca::esquerda), (meio::direita))
    end;

split([1,2,3,8,4,5]);