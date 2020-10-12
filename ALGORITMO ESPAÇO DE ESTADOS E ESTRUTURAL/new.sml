(**********Auxiliary functions**********)
fun isolate [] = []
  | isolate (x::xs : int list) = x::isolate(List.filter (fn y => y <> x) xs);

fun constructGraph(G: (int*int) list) = 
let
    val vertices = isolate ((map #1 G) @ (map #2 G))

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

fun GetPaths(nil,_,_) = nil
| GetPaths(G:(int*int) list, s:int, d:int) =
let
    val graph = constructGraph(G)
    val l = length(graph)
    val visited = Array.array(l,false)
        
    fun GetPathsAux(u: int, d: int, visited: bool array, path: int list) =
    let
        val u_index = getIndex(u,graph)
        val it = Array.update(visited,u_index,true)
        val path = path @ [u]
    in    
        if (u = d) then
            let
                val it = Array.update(visited,u_index,false)
            in
                [path]
            end
        else
            let
                val adj_startNode = #2 (List.nth(graph,u_index))
                val l_adj = length(adj_startNode)
                val Paths = ref []
                val i = ref 0
            in
             while !i <> l_adj do (
                Paths:= (!Paths) @
                [
                    let
                        val adj_i = List.nth(adj_startNode,!i)
                        val i_index = getIndex(adj_i,graph)
                    in
                    if not (Array.sub(visited,i_index)) then
                        let
                            val get = GetPathsAux(adj_i,d,visited,path)
                            val it = Array.update(visited,i_index,false)
                        in
                            get
                        end
                    else
                        let

                        in
                        []
                        end
                    end
                ]; i := !i + 1);
                List.concat (!Paths)
            end
    end
in
    GetPathsAux(s,d,visited,[])
end;
(*
val g = [(2,1),(2,0),(1,3),(0,2),(0,1),(0,3)];
val p = GetPaths(g,0,3);
length(p);
*)


fun GetAllPaths([],_,_) = []
| GetAllPaths (_,_,[]) = []
| GetAllPaths(g: (int*int) list,src: int,x::dstlst: int list) =
    GetPaths(g,src,x) @ GetAllPaths(g,src,dstlst);

(*val g=[(4,3),(1,3),(1,2),(2,5),(1,5),(3,5),(2,4)];*)
val g = [(1,2), (1,3), (1,4), (1,8), (1,10), (4,3), (2,5), (3,6), (5,6), (6,5), (6,7),(6,8), (6,9), (6,10), (6,11), (7,5), (8,8), (8,10), (9,11), (10,3)];
val p = GetPaths(g,1,11);
val routes =length(p);
(*GetAllPaths(g,1,[2,3,5]);*)