type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)



let rec print t =
let rec print_tupple tlist = ( match tlist with
                                [] -> ()
                                |t1::[] -> print t1
                                |t1::ttls -> (print t1 ;print_string "," ;print_tupple ttls) )
in
let rec print_args tlist = ( match tlist with  [] -> () |  t1::ttls -> (print t1 ;print_string "->" ;print_args ttls) )in
match t with
  | Unit      -> print_string "unit"
  | Bool      -> print_string "bool"
  | Int       -> print_string "int"
  | Float       -> print_string "float"
  | Fun (tls,tt)  -> (print_string "fun " ; print_args tls ;  print tt)
  | Tuple tls     -> (print_string "("; print_tupple tls ; print_string ")")
  | Array tt    -> (print_string "Array " ; print tt)
  | Var tt      -> (match !tt with  Some ttt -> (print_string "Var"; print ttt) | None -> print_string "Var None")

