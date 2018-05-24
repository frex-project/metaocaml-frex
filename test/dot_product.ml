open Algebra
open Aux
open Commutative_rings

module Dot (A : Make(Cring_ops).Algebra) =
struct
  let (<|*|>)  xs ys =
    A.Op.(
      List.fold_right2
        (fun x y a ->
           (x <*> y) <+> a
        )
        xs ys zero
    )
end

let rec (--) a b =
  if b < a
  then []
  else a :: ((a+1) -- b)

module PS_Int = PS_csr (Int) (Int_code)


let helper  = PS_Int.(
           List.map
             sta
         )
let xs = helper (1 -- 10000)
let ys = xs

let main =
  let module D = Dot(PS_Int) in
    Printf.printf "%d\n" (Runcode.run @@ PS_Int.cd D.(xs <|*|> ys))
