fun count_main(valorX: int) =
    let
        fun count (valorY: int) =
            if (valorY < valorX) then
                valorY::count(valorY + 1)
            else if (valorY = valorX) then
                valorX::[]
            else
                []
    in count(1)
    end;

count_main(5);