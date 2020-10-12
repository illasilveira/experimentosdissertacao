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


fun DepthSearch(_,_,[],_,_) = []
| DepthSearch(Node : int, DisabledNodes : bool array, Graph : (int*int list)list, G : int, PassedNodes : int list) =
let
    val Node_index = getIndex(Node,Graph)
    (*val it = Array.update(PassedNodes,Node_index,true)*)
    val it = Array.update(DisabledNodes,Node_index,true)
    val PassedNodes = PassedNodes ^^ [Node]
    val SubNodes = #2 (List.nth(Graph,Node_index))
    val len_SubNodes = length(SubNodes)
    val Result = false
    val Paths = ref []
    val i = ref 0
in
    while !i <> len_SubNodes do (
    Paths:= (!Paths) ^^ [
        let
            val SubNode_i = List.nth(SubNodes,!i)
            val SubNode_i_index = getIndex(SubNode_i,Graph)
        in
            if SubNode_i = G then
                let
                    val Result = true
                in
                    [PassedNodes ^^ [SubNode_i]]
                end
            else
                if not (Array.sub(DisabledNodes,SubNode_i_index)) orelse not (List.exists (fn x => x=SubNode_i) PassedNodes) then
                    let
                        val get = DepthSearch(SubNode_i,DisabledNodes,Graph,G,PassedNodes)
                        val it = Array.update(DisabledNodes,SubNode_i_index,false)
                    in
                        get
                    end
                else
                    if not Result then
                        let
                            val it = Array.update(DisabledNodes,Node_index,true)
                        in
                            []
                        end
                    else
                        []
        end
    ]; i := !i + 1);
    List.concat (!Paths)
end;
(****Fim APCO*******)
(******)
(******)
(**Funcoes Aux APCO*)
fun getSequence([]) = []
| getSequence(lst) = 
let
    val i = ref 1
    val seq = ref []
    val len = List.length(lst)
in
    while !i <= len do (
        seq:= (!seq) ^^ [
            [!i]
        ]; i:= !i +1);
    List.concat (!seq)
end;

fun getWeight([]) = []
| getWeight(lst) = 
let
    val i = ref 0
    val seq = ref []
    val len = List.length(lst)
in
    while !i <> len do (
        seq:= (!seq) ^^ [
        let
            val l = List.nth(lst,!i)
        in
            [List.length(l)]
        end
    ]; i:= !i +1);
    List.concat (!seq)
end;

fun computeScenarios([],_)  = []
| computeScenarios(_,[]) = []
| computeScenarios(S : int list , x::inter : (int*int) list) = 
let
    val i = #1 x
    val j = #2 x
in
    List.drop(List.take(S,j),i-1)::computeScenarios(S,inter)
end;

fun cScenarios([],_)  = []
| cScenarios(_,[]) = []
| cScenarios(G : (int * int) list , x::SBreaks : (int*int) list) = 
let
    val Graph = constructGraph(G)
    val lenGraph = length(Graph)
    val DisabledNodes = Array.array(lenGraph,false)
    val i = #1 x
    val j = #2 x
in
    DepthSearch(i,DisabledNodes,Graph,j,[]) ^^ cScenarios(G,SBreaks)
end;

fun mountScn(_,[]) = [] 
| mountScn(s : int list list,x::sb : (int * int) list) =
let
    fun eraseNil([]) = []
    | eraseNil(x::lst) =
        if List.null x then
            eraseNil(lst)
        else
            x::eraseNil(lst) 
    val i = #1 (x)
    val l = List.map (fn y => if (hd y = i) then y else [] ) s
in
    [eraseNil(l)] ^^ mountScn(s,sb)
end;
fun mapTestCases([],_) = []
| mapTestCases(_,[]) = []
| mapTestCases (x::lst1,y::lst2) = (x,y)::mapTestCases(lst1,lst2);
(**Fim Fun Aux APCO*)
(**Exec APCO*)
val testCases = cScenarios(G,ScenariosBreaks);
val subSequences = mountScn(testCases,ScenariosBreaks);
(**Fim Exec APCO*)