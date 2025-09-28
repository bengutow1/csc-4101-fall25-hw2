let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup ((a,b) : 'a * 'b) = (b, a);;
let rev_triple ((a, b, c) : 'a * 'b * 'c) = (c, b, a);;

let is_odd x =
        if x mod 2 = 0 then
                false
        else
                true
;;

let is_older (date1: int * int * int) (date2: int * int * int) =
        let (y1, m1, d1) = date1 in
        let (y2, m2, d2) = date2 in
        if y1 <= y2 then
                if y1 < y2 then
                        true
                else
                        if m1 <= m2 then
                                if m1 < m2 then
                                        true
                                else
                                        if d1 < d2 then
                                                true
                                        else
                                                false
                        else
                                false
        else
                false
;;

let to_us_format ((year, month, day): int * int * int) = (month, day, year);;
        
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = failwith "unimplemented"

let rec fac n = failwith "unimplemented"

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = failwith "unimplemented"

let larger lst1 lst2 = failwith "unimplemented"

let sum lst1 lst2 = failwith "unimplemented"
