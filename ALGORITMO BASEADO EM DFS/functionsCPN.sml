(**********Auxiliary functions**********)
fun isolate [] = []
  | isolate (x::xs : int list) = x::isolate(List.filter (fn y => y <> x) xs);

fun constructGraph(G: (int*int) list) = 
let
    val vertices = isolate ((map #1 G) ^^ (map #2 G))

    fun helper ([],_) = []
    |helper (_,[]) = []
    |helper (G: (int*int) list,vertices: int list) =
    let
        val (v,_) = List.partition ((fn x => x = hd vertices) o #1) G
    in
        ((hd vertices), (map #2 v))::helper(G,tl vertices)
    end
in
    helper(G,vertices)
end;

fun getIndex (x, []) = raise Subscript
  | getIndex (x : int, (y,_)::ys) =
    if x = y then 0 else 1 + getIndex (x, ys);
(********************)

(**********Main Functions**********)
fun printAllPathsUtil (g:(int * int list)list) (d:int) (visited:bool array) (path: int list) (path_index: int) (u:int) =
let
    val it = Array.update(visited,getIndex(u,g),true)
    val path = path ^^ [u]
    val path_index = path_index+1
in
    if (u = d) then
        let
            val it = Array.update(visited,getIndex(u,g),false)
            val path_index = path_index-1
        in
            [path]
        end
    else
        let
            val (adj',_) = (List.partition ((fn x => x =u ) o #1) g); (*'*)
            val adj = List.concat (map #2 adj')
            val i = ref (length adj)
            val paths = ref []
        in
            while (!i <> 0) andalso (not (List.null(adj))) do (paths:= (if (Array.sub(visited,getIndex((List.nth(adj,!i-1)),g)) = false) then printAllPathsUtil g d visited path path_index (List.nth(adj,!i-1)) else [])::(!paths) ; i := !i -1);
            List.concat (!paths)
        end
end;

fun printAllPaths(nil,_,_) = nil
| printAllPaths(G:(int * int) list, s: int, d: int) = 
let
    val vertices = isolate ((map #1 G) ^^ (map #2 G));
    val v = length vertices;
    val g = constructGraph(G);
    val visited = Array.array(v,false)
    val path : int list = []
    val path_index = 0
in
    printAllPathsUtil g d visited path path_index s
end;

(*val allPaths= printAllPaths(g,<origem>,<destino>);*)

(*********************************************************************)

fun PossiblePaths([],_,_) = []
| PossiblePaths (_,_,[]) = []
| PossiblePaths (_,0,_) = []
|PossiblePaths ((G:(int * int) list), src:int, lst: int list) = 
let
    val vertices = isolate ((map #1 G) ^^ (map #2 G));
    val v = length vertices;
    val g = constructGraph(G);
    val i = ref (length lst)
    val p = ref []
in
    
    while (!i <> 0) do (p:=
    let
        val visited = Array.array(v,false)
        val path : int list = []
        val path_index = 0
    in
    if src < 0 then [] else
    printAllPathsUtil g (List.nth(lst,!i-1)) visited path path_index src end::(!p) ; i := !i -1);
    List.concat (!p)
end;