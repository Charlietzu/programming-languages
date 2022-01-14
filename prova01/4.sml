fun vconc (l: (string * string) list) =
case l of
nil => ("","")
| (h::t) =>
let val headValue1 = #1 h;
val headValue2 = #2 h;
val recValue = vconc t;
val recValue1 = #1 recValue;
val recValue2 = #2 recValue
in
(headValue1 ^ recValue1, headValue2 ^ recValue2)
end;
