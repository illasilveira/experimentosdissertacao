fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end;


fun convertIntListList([]) = "" 
| convertIntListList(y::IntLstLst) = 
let
    val head = "["
    val tail = "]"
    fun convertIntList ([]) = ""
        | convertIntList (x::lst) =
    if lst <> [] then
        (Int.toString x ^ ",") ^ convertIntList(lst)
    else
         (Int.toString x ^ tail);
in
    if IntLstLst <> [] then
        head ^ convertIntList(y) ^ "," ^ convertIntListList(IntLstLst)
    else
        head ^ convertIntList(y)
end;

(*
val s_AllPaths = convertIntListList(AllPaths);
val s_numAllPaths = Int.toString numAllPaths;
writeFile "results_AllPaths.txt" s_AllPaths;
writeFile "results_numAllPaths.txt" s_numAllPaths;*)