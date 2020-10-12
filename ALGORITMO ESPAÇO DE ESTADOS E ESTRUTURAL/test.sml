fun helper ([],_) = []
| helper(_,[]) = []
| helper(x::lst1,y::lst2) = 
    if not (List.null(lst1)) then 
        (x@[y])::helper(lst1,lst2)
    else 
        (x@[y])::helper([x],lst2);

fun aux ([]) = [] | aux (h::lst) = [h]::aux(lst);
val l1= aux([1,2]);
val l2= [17,16,15];


helper(l1,l2);