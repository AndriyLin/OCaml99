
(** 99 Problems -- Working with lists *)
(** My testing workflow: (with a utop open by my side)
 * 1. Implement one question;
 * 2. Assign its test data;
 * 3. Reload this .ml in utop to see the values of latest test results.
 *)


(* 01 - Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | hd :: [] -> Some hd
  | _ :: tl -> last tl

let a_last_1 = last [ "a" ; "b" ; "c" ; "d" ];;
let a_last_2 = last [];;




(* 02 - Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: tl -> last_two tl

let a_last_two_1 = last_two [ "a" ; "b" ; "c" ; "d" ];;
let a_last_two_2 = last_two [];;



(* 03 - Find the k'th element of a list. (easy) *)
let rec at i = function
  | [] -> None
  | hd :: tl -> if i < 1 then None
                else if i = 1 then Some hd
                else at (i-1) tl

let a_at_1 = at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
let a_at_2 = at 3 [ "a" ] ;;




(* 04 - Find the number of elements of a list. (easy) *)
(*let rec length ls = match ls with
  | [] -> 0
  | _ :: tl -> (length tl) + 1
 *)
let length ls =
  let rec worker accu rest =
    match rest with
    | [] -> accu
    | _ :: tl -> worker (accu+1) tl
  in worker 0 ls

let a_length_1 = length [ "a" ; "b" ; "c"];;
let a_length_2 = length [];;




(* 05 - Reverse a list. (easy) *)
let rev ls =
  let rec aux accu = function
    | [] -> accu
    | hd :: tl -> aux (hd :: accu) tl
  in aux [] ls

let a_rev_1 = rev ["a" ; "b" ; "c"];;




(* 06 - Find out whether a list is a palindrome. (easy) *)
let is_palindrome ls =
  ls = rev ls

let a_is_palindrome_1 = is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
let a_is_palindrome_2 = not (is_palindrome [ "a" ; "b" ]);;



(* 07 - Flatten a nested list structure. (medium) *)
(* There is no nested list type in OCaml, so we need to define one first. A node
     of a nested list is either an element, or a list of nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | hd :: tl -> flatten_one_item hd @ flatten tl
and flatten_one_item = function
    | One s -> [s]
    | Many nn -> flatten nn

let a_flatten_1 = flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;


(* 08 - Eliminate consecutive duplicates of list elements. (medium) *)
let compress ls =
  let rec aux last accu = function
    | [] -> accu
    | hd :: tl -> if hd = last
                  then aux last accu tl
                  else aux hd (accu @ [hd]) tl
  in match ls with
     | [] -> []
     | hd :: tl -> aux hd [hd] tl

let a_compress_1 = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;



(* 09 - Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack ls =
  let aux_add_new i accu = match accu with
    | [] -> [[i]]
    | hds :: tls -> begin match hds with
                    | hd :: tl -> if hd = i
                                  then (i :: hds) :: tls
                                  else [i] :: accu
                    | _ -> failwith "impossible here"
                    end
  in let rec aux accu = function
       | [] -> accu
       | hd :: tl -> aux (aux_add_new hd accu) tl
     in rev (aux [] ls)

let a_pack_1 = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;




(* 10 - Run-length encoding of a list. (easy) *)
let encode ls =
  let packed = pack ls in
  let aux_compute ns = match ns with
    | hd :: _ -> (length ns, hd)
    | _ -> failwith "impossible here"
  in List.map aux_compute packed

let a_encode_1 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;




(* 11 - Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let modified_encode ls =
  let original = encode ls in
  let aux_map = function
    | (len, c) -> begin match len with
                  | 1 -> One c
                  | n -> Many (n, c)
                  end
  in List.map aux_map original

let a_modified_encode_1 = modified_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;



(* 12 - Decode a run-length encoded list. (medium) *)
let decode ns =
  let rec aux_nc n c = match n with
    | 0 -> []
    | n -> c :: (aux_nc (n-1) c)
  and aux_expand = function
    | One c -> [c]
    | Many (n, c) -> aux_nc n c
  and aux accu = function
    | [] -> accu
    | hd :: tl -> aux (accu @ (aux_expand hd)) tl
  in aux [] ns

let a_decode_1 = decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;



(* 13 - Run-length encoding of a list (direct solution). (medium) *)
(* Omited.. another way of previous question *)




(* 14 - Duplicate the elements of a list. (easy) *)
let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd :: hd :: (duplicate tl)

let a_duplicate_1 = duplicate ["a";"b";"c";"c";"d"];;




(* 15 - Replicate the elements of a list a given number of times. (medium) *)
let rec replicate ls n =
  let rec aux_nc n c = match n with
    | 0 -> []
    | n -> c :: (aux_nc (n-1) c)
  in match ls with
     | [] -> []
     | hd :: tl -> aux_nc n hd @ replicate tl n

let a_replicate_1 = replicate ["a";"b";"c"] 3;;




(* 16 - Drop every N'th element from a list. (medium) *)
let drop ls n =
  let rec aux i accu = function
    | [] -> accu
    | hd :: tl -> if i = 1
                  then aux n accu tl
                  else aux (i-1) (hd :: accu) tl
  in rev (aux n [] ls)

let a_drop_1 = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;



(* 17 - Split a list into two parts; the length of the first part is given. (easy) *)
let split ls n =
  let rec aux accu i = function
    | [] -> (accu, [])
    | hd :: tl -> if i = 0
                  then (accu, hd :: tl)
                  else aux (accu @ [hd]) (i-1) tl
  in aux [] n ls

let a_split_1 = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
let a_split_2 = split ["a";"b";"c";"d"] 5;;




(* 18 - Extract a slice from a list. (medium) *)
let slice ls i k =
  let rec aux_since_x x rest = match x with
    | 0 -> rest
    | n -> begin match rest with
           | [] -> []
           | hd :: tl -> aux_since_x (x-1) tl
           end
  in let first_i = aux_since_x i ls in
  let rec aux_first_y y accu = function
       | [] -> accu
       | hd :: tl -> if y < 0
                     then accu
                     else aux_first_y (y-1) (accu @ [hd]) tl
  in let newk = k - i
  in aux_first_y newk [] first_i

let a_slice_1 = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;




(* 19 - Rotate a list N places to the left. (medium) *)
let rotate ls n =
  let rec get_real_positive len k =
    if k < 0
    then get_real_positive len (k + len)
    else if k >= len
    then get_real_positive len (k - len)
    else k
  in
  let realn = get_real_positive (length ls) n in
  let (first, second) = split ls realn in
  second @ first

let a_rotate_1 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
let a_rotate_2 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;



(* 20 - Remove the K'th element from a list. (easy) *)
let rec remove_at i = function
  | [] -> []
  | hd :: tl -> if i = 0
                then tl
                else hd :: (remove_at (i-1) tl)

let a_remove_at_1 = remove_at 1 ["a";"b";"c";"d"];;




(* 21 - Insert an element at a given position into a list. (easy) *)
let rec insert_at s i ls =
  if i = 0
  then s :: ls
  else begin match ls with
       | [] -> failwith "i larger than size of ls"
       | hd :: tl -> hd :: insert_at s (i-1) tl
       end

let a_insert_at_1 = insert_at "alfa" 1 ["a";"b";"c";"d"];;
let a_insert_at_2 = insert_at "alfa" 3 ["a";"b";"c";"d"];;
let a_insert_at_3 = insert_at "alfa" 4 ["a";"b";"c";"d"];;



(* 22 - Create a list containing all integers within a given range. (easy) *)
let range i j =
  let rec range_up x y = begin
      if x = y
      then [x]
      else x :: range_up (x+1) y
    end
  in
  let rec range_down x y = begin
      if x = y
      then [x]
      else x :: range_down (x-1) y
    end
  in if i = j
     then [i]
     else if i < j
     then range_up i j
     else range_down i j

let a_range_1 = range 4 9;;
let a_range_2 = range 9 4;;




(* 23 - Extract a given number of randomly selected elements from a list. (medium) *)
let rec rand_select ls n =
  let aux_pick_one = function
    | [] -> (None, [])
    | ns -> let len = length ns in
            let r = Random.int len in
            (Some (List.nth ns r), remove_at r ns)
  in if n = 0
     then []
     else begin match aux_pick_one ls with
          | (None, _) -> []
          | (Some v, rest) -> v :: rand_select rest (n-1)
          end

let a_rand_select_1 = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;




(* 24 - Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let rec lotto_select n bound =
  if n = 0
  then []
  else (Random.int bound) :: lotto_select (n-1) bound

let a_lotto_select_1 = lotto_select 6 49;;




(* 25 - Generate a random permutation of the elements of a list. (easy) *)
let permutation ls =
  let aux_pick = function
    | [] -> (None, [])
    | ns -> let len = length ns in
            let r = Random.int len in
            (Some (List.nth ns r), remove_at r ns)
  in
  let rec aux accu = function
    | [] -> accu
    | ns -> begin match aux_pick ns with
            | (None, _) -> accu
            | (Some v, rest) -> aux (v :: accu) rest
            end
  in aux [] ls

let a_permutation_1 = permutation ["a"; "b"; "c"; "d"; "e"; "f"];;



(* 26 - Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
let extract n ls =
  (* Expected output for "1,2,3,4,5" with n_more = 2:
   * 1 & 2,3,4,5
   * 2 & 3,4,5
   * 3 & 4,5
   * 4 & 5
   * The latter is the potential scope for further picking.
   *)
  let rec aux_split_scope n_more ls = (* trusted *)
    if n_more <= 0
    then failwith "this should not happen"
    else
      match ls with
      | [] -> []
      | hd :: tl ->
         if List.length tl < n_more - 1 (* won't be able to pick the rest from tl *)
         then []
         else (hd, tl) :: aux_split_scope n_more tl
  in
  (* <picked, n_more, to_pick> is a data record,
   * aux_pick_one_more() expands each record to a list of records,
   * by picking one more from to_pick and save to picked
   *)
  let aux_pick_one_more (picked, n_more, to_pick) =
    if n_more = 0
    then [(picked, n_more, to_pick)]
    else if n_more > length to_pick
    then []
    else let scopes = aux_split_scope n_more to_pick in
         List.map (fun (cur, rest) -> (cur :: picked, n_more-1, rest)) scopes
  in
  (* Continue until every element in the list has no more to draw.
   * It needs to flatten in between.
   *)
  let rec aux accu =
    if List.exists (fun (_, n_more, _) -> n_more > 0) accu
    then let tmp = List.map aux_pick_one_more accu in
         aux (List.flatten tmp)
    else accu
  in
  let tmp = aux [ ([], n, ls) ] in
  (* Only maintain the picked part. *)
  List.map (fun (picked, _, _) -> List.rev picked) tmp

let a_extract_1 = extract 2 ["a";"b";"c";"d"];;


(* A much more succinct version is copied below.. But I don't quite understand.. *)
let extract_succinct k list =
  let rec aux k acc emit = function
    | [] -> acc
    | h :: t ->
       if k = 1
       then aux k (emit [h] acc) emit t
       else
         let new_emit x = emit (h :: x) in
         aux k (aux (k-1) acc new_emit t) emit t
  in
  let emit x acc = x :: acc in
  aux k [] emit list;;


(* STOP HERE TEMPORARILY.. I'll finish the rest later on *)
