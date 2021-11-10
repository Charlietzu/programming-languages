fun max (value::[]) = value 
    | max (headList::tailList) = 
        let
            val aux = max(tailList)
        in
            if (headList > aux) then headList else aux
        end;

max([2, 1, 7, 3]);