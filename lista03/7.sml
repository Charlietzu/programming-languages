fun good_max([]) = 0
| good_max (cabeca::[]) = cabeca
| good_max (cabeca::cauda) = 
    let
        val max_cauda = good_max(cauda)
        in
            if (cabeca > max_cauda) then 
                cabeca 
            else 
                max_cauda
    end;

good_max([5,67,216,112,234,753,31,4,7,5,6,0]);