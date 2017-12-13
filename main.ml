let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan (* out_channel -> Asm.prog -> unit *)
    (RegAlloc.f (*Asm.prog -> Asm.prog*)
    (Simm.f   (*Asm.prog -> Asm.prog*)
    (Virtual.f (*Closure.prog -> Asm.prog*)
    (Closure.f (*KNormal.t -> Closure.prog*)
    (iter !limit (* KNormal.t -> KNormal.t *)
    (Alpha.f (*KNormal.t -> KNnormal.t*)
    (KNormal.f  (*Syntax.t -> t KNormal.t*)
    (Syntax.print (*Syntax.t -> Syntax.t*)
    (Typing.f (* Syntax.t -> Syntax.t *)
    (Parser.exp Lexer.token l
    ))))))))))

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
(* lexbuf は 上で定義

 Lexing.from_channel inchan は入力チャンネル inchan を
 現在の読み込み位置から読み込む字句解析バッファを返します
*)
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]

    (fun s -> files := !files @ [s])

    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
