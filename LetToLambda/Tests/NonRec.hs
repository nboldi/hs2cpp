module NonRec where

f =
  let g x y = x + 1
  in g 2 3 * (
    let h x = g 4 x * 6
    in h 5 - h 7)
