let rec append lst1 lst2 = 
    match lst1 with 
    | [] -> lst2
    | fi :: res ->
        fi :: append res lst2;;

let rec filter f lst = 
    match lst with 
    | [] -> []
    | fi :: res ->
        if f fi then fi :: filter f res 
                else filter f res;;
