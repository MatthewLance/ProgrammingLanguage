fun zip3 lists = 
  case lists of
       ([], [], []) => []
     |  (x::xt, y::yt, z::zt) => (x,y,z)::zip3(xt, yt, zt)

fun unzip3 lists = 
  case lists of
       [] => ([], [], [])
     |  x::xt =>
         case x of
              (a, b, c) => 
              let 
                val (e, f, g) = unzip3(xt)
              in
                (a::e, b::f, c::g)
              end


