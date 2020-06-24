datatype pattern = Wildcard 
                 | Variable of string 
                 | UnitP 
                 | ConstP of int 
                 | TupleP of pattern list 
                 | ConstructorP of string * pattern

datatype valu = Const of int 
              | Unit 
              | Tuple of valu list 
              | Constructor of string * valu 

fun check_pat(pat : pattern) : bool =
let
  fun get_string_list(p : pattern) = 
    case p of
         Variable s => [s]
       | TupleP (hp::tp) => (get_string_list(hp)@get_string_list(TupleP tp))
       | TupleP [] => []
       | ConstructorP (_, conp) => get_string_list(conp)
       | _ => []
  val ps = get_string_list(pat)
  fun find_dup(xs : string list) =
    case xs of
         [] => false 
       | x::t => List.exists(fn s => x = s) t orelse find_dup(t)
in
  not(find_dup(ps))
end

fun match(v : valu, pat : pattern) : (string * valu) list option =
let
  fun compare_tuple(ts, tps) =
    case (ts, tps) of
         (ht::tts, htps::ttps) => isSome(match(ht, htps)) andalso
         compare_tuple(tts, ttps)
       | ([], []) => true
       | _ => false

  fun is_variable(x : pattern, y : valu) = 
    case x of
         Variable _ => true
       | _ => false

  fun strap(xs : (pattern * valu) list) =
    case xs of
         (Variable s, x)::t => (s, x)::strap(t) 
       | _ => []

  fun tupling(ts : valu list, tps : pattern list) =
    strap(List.filter is_variable (ListPair.zip(tps, ts)))
in
  case (v, pat) of
       (_, Wildcard) => SOME []
     | (Unit, UnitP) => SOME []
     | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if compare_tuple(vs, ps) then SOME (tupling(vs,
     ps)) else NONE
     | (_, Variable x) => SOME [(x,v)] 
     | (Constructor(s1, p), ConstructorP(s2, v)) => if s1 = s2 then match(p, v)
                                                    else NONE
     | _ => NONE
end

type name = string
datatype RSP = ROCK
             | SCISSORS
             | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy) 
datatype tournament = PLAYER of name * (RSP strategy ref)   
                    | MATCH of tournament * tournament
                 
fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one)) 
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one)) 
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two,
three, one)) 


val r = onlyOne(ROCK) 
val s = onlyOne(SCISSORS) 
val p = onlyOne(PAPER) 
val rp = alterTwo(ROCK, PAPER) 
val sr = alterTwo(SCISSORS, ROCK) 
val ps = alterTwo(PAPER, SCISSORS) 
val srp = alterThree(SCISSORS, ROCK, PAPER) 


fun next(strategyRef) =     
let 
  val Cons(rsp, func) = !strategyRef 
in
  strategyRef := func();
  rsp    
end 

fun whosWinner(t) =
let
  fun fight(m1, m2) =
  let
    val PLAYER(name1, sr) = m1
    val hand1 = next(sr)
    val PLAYER(name2, sr) = m2
    val hand2 = next(sr)
  in
    case (hand1, hand2) of
         (ROCK, SCISSORS) => m1
       | (SCISSORS, PAPER) => m1
       | (PAPER, ROCK) => m1
       | (ROCK, PAPER) => m2
       | (SCISSORS, ROCK) => m2
       | (PAPER, SCISSORS) => m2
       | _ => fight(m1, m2)
  end
in
  case t of
       MATCH(m1, m2) => fight(whosWinner(m1), whosWinner(m2))
     | PLAYER _ => t
end



(*
val tp1 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "5", Variable "6", 
      TupleP([Variable "7", Variable "8"])
    ]
  )
]

val tp2 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "1",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "5", Variable "6", 
      TupleP([Variable "7", Variable "8"])
    ]
  )
]

val tp3 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "1", Variable "6", 
      TupleP([Variable "7", Variable "8"])
    ]
  )
]

val tp4 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "5", Variable "6", 
      TupleP([Variable "1", Variable "8"])
    ]
  )
]

val tp5 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "3", Variable "6", 
      TupleP([Variable "7", Variable "8"])
    ]
  )
]

val tp6 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "5", Variable "6", 
      TupleP([Variable "3", Variable "8"])
    ]
  )
]

val tp7 = TupleP [
  Variable "1", Variable "2",
  TupleP([Variable "3",Variable "4"]),
  ConstructorP("1", 
    TupleP [Variable "5", Variable "6", 
      TupleP([Variable "5", Variable "8"])
    ]
  )
]

val tuplep1 = TupleP [
  Variable "1",
  Variable "2",
  Variable "3",
  Variable "4"
                     ]

val tuplep2 = TupleP [
  Variable "1",
  Variable "2",
  Variable "3",
  Variable "4",
  ConstP  4
                     ]


val tuple1 = Tuple [
  Const 1,
  Const 2,
  Const 3,
  Const 4
                     ]

val tuple2 = Tuple [
  Const 2,
  Const 2,
  Const 3,
  Const 3,
  Const 4
                     ]

val tour1 = MATCH(PLAYER("1", ref srp), PLAYER("2", ref rp))
val tour2 = MATCH(tour1, PLAYER("3", ref sr))
val tour3 = MATCH(tour2, PLAYER("4", ref sr))
val tour4 = MATCH(PLAYER("5", ref p), tour3)
val tour5 = MATCH(PLAYER("6", ref rp), tour4)
*)  
