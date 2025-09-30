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

let rec pow x p =
        if p = 0 then
                1
        else
                x * (pow x (p - 1))
;;


let rec fac n =
        if n = 0 then
                1
        else
                n * (fac (n - 1))
;;


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) =
        if lst = [] then        (* checking for empty list (no value to return) *)
                failwith "Error: Index out of bounds!"
        else
                if idx = 0 then
                        List.hd lst
                else
                        get_nth ((idx - 1), (List.tl lst))
;;


let rec larger lst1 lst2 =
        match lst1, lst2 with
        | [], [] -> []
        | [], _ -> lst2
        | _, [] -> lst1
        | x1 :: xs1, x2 :: xs2 ->
                        let tail = larger xs1 xs2 in
                        if tail == xs1 then lst1 else lst2      (* Checking for physical quality between tail and xs1 *)
                
;;

let rec sum lst1 lst2 =
        match lst1, lst2 with
        | [], [] -> 0
        | [], _ -> (List.hd lst2) + (sum lst1 (List.tl lst2))
        | _, [] -> (List.hd lst1) + (sum (List.tl lst1) lst2)
        | _, _ -> (List.hd lst1) + (List.hd lst2) + (sum (List.tl lst1) (List.tl lst2))

;;       
