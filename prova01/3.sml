fun f ([]) = false
  | f (cabeca::calda) = 
    if cabeca mod 2 = 0 then
        let
          val aux = f(calda)
        in
          if aux = true then 
            false 
          else 
            true
        end
    else 
        f(calda);


f ([1,2,4]);