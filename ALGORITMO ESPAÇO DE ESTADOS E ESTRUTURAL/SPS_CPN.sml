(**SPS*****)
fun intScn([]) = [] 
| intScn(lst) = 
let
    fun LenScn([]) = []
    | LenScn(x::lst) = 
        let
            fun helper ([]) = 0
            | helper (l) = 
                List.length(l);
        in
            helper(x)::LenScn(lst)
        end;
        
    val LenSc = LenScn(lst);
    val len = List.length(LenSc)
    val i = ref 0
    val intS = ref []
in
    while !i <> len do (
        intS:= (!intS) ^^ [
            if !i=0 then
                (1,List.nth(LenSc,!i))
            else
                let
                    fun sumIntList([]) = 0
                    | sumIntList(x::lst) = x+sumIntList(lst);
                    val l = List.take(LenSc,!i);
                    val ant = sumIntList(l)
                in
                    (ant+1,ant+List.nth(LenSc,!i))
                end
        ]; i:= !i + 1);
    (!intS)
end;

fun max([]) = 0 |max(x::[]) = x |max(x::xs) =
let
    val y = max(xs)
in
    if x>y then
        x
    else
        y
end;

fun getIdLast([],_) = raise Empty
| getIdLast (lst : int list, x : int) =
let
    val pos = List.length(lst)-1
    val y = List.last(lst)
    val isExists = List.exists (fn i => i=x) lst
    val lst = List.take(lst,pos)
in
    if isExists then
        if x = y then pos else getIdLast(lst,x)
    else
        raise Match
end;

fun MaxUnit(data1 : int list list, data2 : int list list, nScenarios : int) =
let
    val Sequences = ref []
    val i = ref 0
in
while !i <> nScenarios do (
    Sequences:= (!Sequences) ^^ [
        let
            val l_data1_i = List.nth(data1,!i)
            val data1_i = Array.fromList(l_data1_i)
            val l_data2_i = List.nth(data2,!i)
            val data2_i = Array.fromList(l_data2_i)
            val j = ref 0
            val Scenario_i = ref []
        in
            while !j <> Array.length(data1_i) do (
                Scenario_i:= (!Scenario_i) ^^ [
                    let
                        val l_data2_i = List.tabulate(Array.length(data2_i), fn i => Array.sub(data2_i, i))
                        val m = max(l_data2_i)
                        val id = getIdLast(l_data2_i,m)
                        (*val id = j*)
                        val ret = List.nth(l_data1_i,id)
                        val it = Array.update(data1_i,id,0)
                        val it = Array.update(data2_i,id,0)
                    in
                        [ret]
                    end
                ]; j := !j + 1);
            [List.concat (!Scenario_i)]
        end
    ]; i := !i + 1);
    List.concat (!Sequences)
end;

fun maxLenList([]) = 0 |maxLenList(x::[]) = List.length(x) |maxLenList(x::xs : int list list) =
let
    val len_y = if List.length(xs)=1 then List.length(hd xs) else maxLenList(xs)
    val len_x = List.length(x)
    
in
    if len_x>len_y then
        len_x
    else
        len_y
end;

fun joinSS([]) = []
| joinSS(SS) = 
let
    val Sequences = ref []
    val i = ref 0
    val n = maxLenList(SS)
in
    while !i <> n do (
        Sequences:= (!Sequences) ^^ [
        let
            val j = ref 0
            val m = List.length(SS)
            val Sequence = ref []
        in
            while !j <> m do (
                Sequence:= (!Sequence) ^^ [
                let
                    val l = (List.nth(SS,!j))
                    val ll = if List.length(l)-1 >= (!i) then [List.nth(l,!i)] else []
                in
                    ll
                end
            ]; j := !j + 1);
            [List.concat (!Sequence)]
        end
        ]; i := !i + 1);
    List.concat (!Sequences)
end;

fun insertSTS(STS, SS) = 
let
    val Seq = joinSS(SS)
    val len_Seq = List.length(Seq)
    val len_STS = List.length(STS)
in
    if len_Seq >= len_STS then
    let
        val i = ref 0
        val Sequences = ref []
    in
        while !i <> len_Seq do (
            Sequences:= (!Sequences) ^^ [
                if len_STS-1 >= (!i) then
                    [List.nth(STS,!i)::List.nth(Seq,!i)]
                else
                    [List.nth(STS,0)::List.nth(Seq,!i)]
            ]; i := !i + 1);
        List.concat (!Sequences)
    end
    else (*Quando STS maior joinSS(SS)*)
    let
        val j = ref 0
        val SequencesSTS = ref []
    in
        while !j <> len_STS do (
            SequencesSTS:= (!SequencesSTS) ^^ [
                if len_Seq-1 >= (!j) then
                    [List.nth(STS,!j)::List.nth(Seq,!j)]
                else
                    [List.nth(STS,!j)::List.nth(Seq,0)]
            ]; j := !j + 1);
        List.concat (!SequencesSTS)
    end
end;

fun insertETS([],_,_) = []
| insertETS(_,_,[]) = []
| insertETS(STS,SS,ETS) = 
let
    fun aux ([]) = [] | aux (h::lst) = [h]::aux(lst) 
    val Seq = if List.null(SS) then aux(STS) else insertSTS(STS,SS)
    val len_Seq = List.length(Seq)
    val len_ETS = List.length(ETS)
    fun helper ([],_) = []
    | helper(_,[]) = []
    | helper(x::lst1,y::lst2) = 
        if not (List.null(lst1)) then 
            (x^^[y])::helper(lst1,lst2)
        else 
            (x^^[y])::helper([x],lst2)
in
    if len_Seq >= len_ETS then
    let
        val i = ref 0
        val Sequences = ref []
    in
        while !i <> len_Seq do (
            Sequences:= (!Sequences) ^^ [
                if len_ETS-1 >= (!i) then
                    [List.nth(Seq,!i)^^[List.nth(ETS,!i)]]
                else
                    [List.nth(Seq,!i)^^[List.nth(ETS,0)]]
            ]; i := !i + 1);
        List.concat (!Sequences)
    end
    else helper(Seq,ETS)
end;
(**Fim SPS****)


(**Exec SPS*)
val Interval = intScn(subSequences);
val S = getSequence(testCases);
val R = getWeight(testCases);
val nScenarios = List.length(ScenariosBreaks);
val data1 = computeScenarios(S,Interval);
val data2 = computeScenarios(R,Interval);

val orderedTestCases = MaxUnit(data1,data2,nScenarios);
val STS = if List.length(orderedTestCases) >= 1 then hd orderedTestCases else [];
val ETS = if List.length(orderedTestCases) >= 2 then List.last(orderedTestCases) else [];
val SS = if List.length(orderedTestCases) >= 3  then tl (List.take(orderedTestCases, List.length(orderedTestCases)-1)) else [];

(*val testSequences = insertETS(STS,SS,ETS);
val lenTestSequences = List.length(testSequences);*)
(**Fim Exec SPS*)