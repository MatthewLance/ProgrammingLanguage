fun check_int(xs) =
  case xs of
       [] => false
     | x::t => List.exists(fn k => x = k) t orelse check_int(t)


fun is_variable(x : valu, y : pattern) =
  case y of
       Variable _ => true
     | _ => false


fun strap(x : pattern) =
  case x of
       Variable x => x
     | _ => ""

