(* Merge Lists *)
fun merge(xs: int list , ys: int list) =
  if null(xs)
  then ys
  else if null(ys)
  then xs
  else if hd(xs) < hd(ys)
  then hd(xs)::merge(tl(xs), ys)
  else hd(ys)::merge(xs, tl(ys))

(* Reverse List *)
fun reverse(xs : int list) =
let 
  fun f(os : int list, rs : int list) =
    if null(os)
    then rs
    else f(tl(os), hd(os) :: rs)
in
  f(xs, [])
end

(* Sigma Function *)
fun sigma(a : int, b : int, f : (int -> int)) = 
  if a = b
  then f(a)
  else f(a) + sigma(a + 1, b, f)

(* Digits Function *)
fun digits(d : int) = 
let 
  fun f(num : int, rs : int list) =
    if num = 0
    then rs
    else (num mod 10) :: f(num div 10, rs)
  val x = f(d, [])
in
  reverse(x)
(*  reverse(f(d, []))*)
end

(* Digital Roots and Additive Persistence *)
fun additivePersistence(x : int) = 
let
  fun sumList(xs : int list) =
    if null(xs)
    then 0 
    else hd(xs) + sumList(tl(xs))
  fun f(num : int, count : int) =
  let 
    val digitList = digits(num)
  in
    if null(tl(digitList)) 
    then count
    else f(sumList(digitList), count + 1)
  end
in
  f(x, 0)
end

fun digitalRoot(x : int) =
let 
  fun sumList(xs : int list) =
    if null(xs)
    then 0 
    else hd(xs) + sumList(tl(xs))
  fun f(num : int) =
  let 
    val digitList = digits(num)
  in
    if null(tl(digitList)) 
    then hd(digitList) 
    else f(sumList(digitList))
  end  
in
  f(x)
end
