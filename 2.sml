(*
fun apply(f: int*int -> int, x:int, y:int) =
  f(x, y);

fun plus(x:int, y:int)=
  x + y;

apply(plus, 10, 32);
*)

fun swap(pr: int*bool) =
  (#2 pr, #1 pr);

