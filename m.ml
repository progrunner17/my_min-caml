(* customized version of Map *)

module M =
  Map.Make (*全順序な tを引数にとってマップ構造の実装を返す functor*)
    (struct
      type t = Id.t
      let compare = compare
    end)

(* M に関数を追加*)
include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys



(* つまり、M は Mapの関数
type key
表のキーの型。
type +'a t
key から 'a 型への表の型。
val empty : 'a t
空の表。
val is_empty : 'a t -> bool
表が空かどうか検査します。
val mem : key -> 'a t -> bool
mem x m は m が x に対する束縛を格納していたら true を返し、そうでなければ false を返します。
val add : key -> 'a -> 'a t -> 'a t
add x y m は m の全束縛に x から y への束縛を追加したものを格納する表を返します。 x が m で既に束縛されていた場合には、以前の束縛は置き換えられます。
val singleton : key -> 'a -> 'a t
singleton x y は x から y への束縛のみを含む 1 要素の表を返します。
Since 3.12.0
val remove : key -> 'a t -> 'a t
remove x m は m の全束縛から x に関する束縛を除いたものを格納した表を返します。
val merge : (key -> 'a option -> 'b option -> 'c option) ->
       'a t -> 'b t -> 'c t
merge f m1 m2 は、キーが m1 と m2 内のキーの部分集合であるような表を計算します。 束縛の存在、及び対応する値は、関数 f により決定されます。
Since 3.12.0
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
表同士の全順序関係。 最初の引数はふたつの表で同一のキーに対応づけられたデータの比較に使われます。
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
equal cmp m1 m2 は、表 m1 と m2 が等しいかどうか、すなわち、等しいキーを持ち、それに対応する値が等しいかどうかを検査します。 cmp は等しいキーに対応づけられたデータを比較する等価述語です。
val iter : (key -> 'a -> unit) -> 'a t -> unit
iter f m は f を表 m 中の全束縛に適用します。 f は第一引数としてキーを受け取り、第二引数として対応する値を受け取ります。 f に渡される束縛は、キー上の順序で昇順に渡されます。
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
fold f m a は (f kN dN ... (f k1 d1 a)...) を計算します。 ここで、 k1 ... kN は m 中の束縛におけるキーで（昇順）、 d1 ... dN はそれに対応する値です。
val for_all : (key -> 'a -> bool) -> 'a t -> bool
for_all p m は表中のすべての束縛が述語 p を満たすか検査します。
Since 3.12.0
val exists : (key -> 'a -> bool) -> 'a t -> bool
exists p m は表中の少なくともひとつの束縛が述語 p を満たすか検査します。
Since 3.12.0
val filter : (key -> 'a -> bool) -> 'a t -> 'a t
filter p m は m 中の束縛のうち、述語 p を満たす束縛だけから成る表を返します。
Since 3.12.0
val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
partition p m は表のペア (m1, m2) を返します。 ここで、 m1 は p の束縛のうち述語 p を満たすものだけを含み、 m2 は p の束縛のうち述語 p を満たさないものだけを含む表です。
Since 3.12.0
val cardinal : 'a t -> int
表に含まれる束縛の個数を返します。
Since 3.12.0
val bindings : 'a t -> (key * 'a) list
与えられた表に含まれるすべての束縛をリストにして返します。 結果のリストの要素は Ord.compare で与えられる順序について昇順に整列しています。 ここで Ord は Map.Make の引数に与えられた引数です。
Since 3.12.0
val min_binding : 'a t -> key * 'a
与えられた表の中で（Ord.compare の順序について）最小の束縛を返します。 表が空のときは Not_found 例外が発生します。
Since 3.12.0
val max_binding : 'a t -> key * 'a
Map.S.min_binding と同じですが、与えられた表の中で最大の束縛を返します。
Since 3.12.0
val choose : 'a t -> key * 'a
与えられた表から束縛をひとつ返します。 表が空の場合には Not_found 例外が発生します。 どの束縛が選択されるかは規定されていませんが、等しい表からは等しい束縛が選択されます。
Since 3.12.0
val split : key -> 'a t -> 'a t * 'a option * 'a t
split x m は 3 個組 (l, data, r) を返します。 ここで、 l は m 中の束縛で、キーが x よりも厳密に小さいものだけを含む表で、 r は m 中の束縛で、キーが x よりも厳密に大きいものだけを含む表です。 data は m に x に対する束縛がない場合は None で、 m が v を x に束縛していた場合には Some v になります。
Since 3.12.0
val find : key -> 'a t -> 'a
find x m は m における x の現在の束縛を返します。 そのような束縛がない場合には Not_found 例外が発生します。
val map : ('a -> 'b) -> 'a t -> 'b t
map f m は m と同じ定義域で（訳注: キーが同じで）、 m 中のすべての束縛の値 a が f を a に適用した結果で置き換えられた表を返します。 f に渡される束縛はキー上の順序について昇順で渡されます。
val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
Map.S.map と同じですが、表の各束縛について、関数 f は引数としてキーとそれに対応する値の両方を受け取ります。
と add_list add_list2を使える *)
