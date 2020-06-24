fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs);

fun range(x : int) = 
  if x = 0
  then []
  else x :: range(x - 1);

fun append(xl : int list, yl : int list) =
  if null xl
  then yl
  else hd xl :: append(tl xl, yl);
 
fun sum_pair_list(xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs);

