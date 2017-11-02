//week 6 second exercise:   fall 2013 first question


let rec inv = function 
        | [] -> true
        | (a,_)::b when not (List.exists (fun x -> fst x = a) b)-> true && inv b
        | head::tail -> false && inv tail;;


//Second question

let rec insert e n =function
                                | [] -> [(e,n)]
                                | a::b when (fst a = e) -> [(e,((snd a)+n))]@b  
                                | a::b -> a::(insert e n b);;



insert "a" 2 [("b",3);("a",5);("d",1)];;

//Third question
let rec numberOf e = function
    | [] -> 0
    | a::b when (fst a = e) -> snd a
    | a::b -> numberOf e b;;

numberOf "d" [("b",3);("a",5);("d",1)];;

// Fourth question

let rec delete e = function
        | [] -> []
        | a::b when (fst a = e)-> [(fst a, snd a-1)]@b
        | a::b -> a::(delete e b);;

delete "d" [("b",3);("a",5);("d",1)];;

