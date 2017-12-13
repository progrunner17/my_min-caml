type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
(* 定数 c *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
(* プリミティブ演算 op  *)
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }



let print t =
  let make_unit a = () in
  let rec p_i indent = if indent > 0 then (print_string "  " ; p_i (indent - 1)) else print_string "" in
  let rec print_vars vars = (match vars with
    | [] -> ()
    | (id,t)::varss -> (Id.print id ; print_string ":" ; Type.print t ; print_vars varss) )in
  let rec sub_print indent tt = (
      match tt with
      | Unit          -> (p_i indent ; Printf.printf "()"; print_newline ())
      | Bool b        -> (p_i indent ; Printf.printf "Bool(%B)" b; print_newline ())
      | Int  i        -> (p_i indent ; Printf.printf "Int(%d)" i;  print_newline ())
      | Float f     -> (p_i indent ; Printf.printf "Float(%f)" f;  print_newline ())
      | Not  e     -> (p_i indent ; print_string  "<Not>" ; print_newline ();
                                  sub_print (indent + 1)  e ;
                       p_i indent ; print_string "</Not>" ; print_newline ())
      | Neg  e     -> (p_i indent ; print_string  "<Neg>" ; print_newline ();
                                  sub_print (indent + 1)  e ;
                       p_i indent ; print_string "</Neg>" ; print_newline ())
      | Add  (e1,e2)     -> (p_i indent ;  print_string "<Add>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                            p_i indent ;  print_string "<+>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</Add>" ;print_newline ())

      | Sub  (e1,e2)     -> (p_i indent ;  print_string "<Sub>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "<->" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</Sub>" ;print_newline ())
      | FNeg  e     -> (p_i indent ; print_string  "<FNeg>" ; print_newline ();
                                  sub_print (indent + 1)  e ;
                       p_i indent ; print_string "</FNeg>" ; print_newline ())
      | FAdd  (e1,e2)     -> (p_i indent ;  print_string "<FAdd>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "<+.>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</FAdd>" ;print_newline ())
      | FSub  (e1,e2)     -> (p_i indent ;  print_string "<FSub>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "<-.>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</FSub>" ;print_newline ())
      | FMul  (e1,e2)     -> (p_i indent ;  print_string "<FMul>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "<*.>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</FMul>" ;print_newline ())
      | FDiv  (e1,e2)     -> (p_i indent ;  print_string "<FDiv>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "</.>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</FDiv>" ;print_newline ())
      | Eq  (e1,e2)     -> (p_i indent ;  print_string "<Eq>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "<==>" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</Eq>" ;print_newline ())
      | LE  (e1,e2)     -> (p_i indent ;  print_string "<LE>" ;print_newline ();
                                                    sub_print  (indent + 1) e1 ;
                             p_i indent ;  print_string "< < >" ;print_newline ();
                                                    sub_print  (indent + 1) e2 ;
                             p_i indent ;  print_string "</LE>" ;print_newline ())
      | If   (e1,e2,e3)   ->( p_i indent ; print_string "<If>"; print_newline ();
                            sub_print (indent + 1)  e1 ;
                            p_i indent ; print_string "<Then>"; print_newline ();
                            sub_print  (indent + 1) e2 ;
                            p_i indent ; print_string "<Else>"; print_newline ();
                            sub_print  (indent + 1) e3 ;
                            p_i indent ; print_string "</If>"; print_newline ())
      | Let  ((id,typ),e1,e2)    -> (p_i indent ; print_string "<Let " ; Id.print id ; print_string ":" ; Type.print typ ; print_string ">"; print_newline ();
                                    sub_print (indent + 1) e1;
                                    p_i indent ; print_string "<In>"; print_newline ();
                                    sub_print (indent + 1) e2;
                                    p_i indent ; print_string "</Let>"; print_newline ())
      | Var   id      ->( p_i indent ; print_string "<Var " ; Id.print id ;  print_string " >" ;print_newline ())
      | LetRec (f,ty)  -> (p_i indent ; print_string "<LetRec>" ;
                          p_i (indent + 1); Id.print (fst f.name) ; print_string ":(" ; Type.print (snd f.name) ; print_string ")   " ;  print_vars f.args; print_newline ();
                          p_i (indent + 1); print_string " = " ; print_newline ();
                                sub_print (indent + 1) f.body ;
                          p_i indent; print_string "<In>" ; print_newline ();
                                sub_print (indent + 1) ty;
                          p_i indent ; print_string "</LetRec>" ; print_newline ();)
      | App  (f,args) -> (p_i indent ; print_string "<App>" ;
                            sub_print (indent + 1) f ;
                          p_i indent ; print_string "<Args>"; print_newline ();
                            make_unit (List.map (sub_print (indent + 1)) args);
                           p_i indent ; print_string "</App>" ;print_newline ())
      | Tuple  tls   -> (p_i indent ;print_string "<Tuple>"; print_newline ();
                        make_unit (List.map (sub_print (indent + 1)) tls);
                        p_i indent ;print_string "</Tuple>"; print_newline ())
      | LetTuple (tys,e1,e2) -> (p_i indent ; print_string "<LetTuple>"; print_vars tys; print_newline ();
                              sub_print (indent + 1) e1;
                              print_string "In"; print_newline ();
                              sub_print (indent + 1) e2 )
      | Array  (t1 , t2)   -> (p_i indent ; print_string "<Array>"; print_newline ();
                      sub_print (indent + 1) t1;
                      sub_print (indent + 1) t2;
                      p_i indent ; print_string "</Array>"; print_newline ())
      | Get (t1,t2)      -> (p_i indent ; print_string "<Get>"; print_newline ();
                      sub_print (indent + 1) t1;
                      sub_print (indent + 1) t2;
                      p_i indent ; print_string "</Get>"; print_newline ())
      | Put  (t1,t2,t3)     ->  (p_i indent ; print_string "<Put>"; print_newline ();
                      sub_print (indent + 1) t1;
                      sub_print (indent + 1) t2;
                      sub_print (indent + 1) t3);
                      p_i indent ; print_string "</Put>"; print_newline ())
  in
      sub_print 0 t ; t
