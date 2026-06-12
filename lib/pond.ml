

let hops (a, _) (b, _) = if a != b then 1. else 0.
let euc_dist_2D (_, (a,b)) (_,(c,d)) = Float.sqrt((a-.c)*.(a-.c)+.(b-.d)*.(b-.d))
let hops2 r (a, x) (b, y) = if a!=b && euc_dist_2D (a, x) (b, y) <= r then 1. else infinity