type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list

let riv3 = R("R3",8,[])
let riv = R("R", 10, [R("R3",8,[]);R("R1",5,[]);R("R2",15,[R("R4",2,[])])])
//Q2) --------------------------------------------------------------------------------------------------------------------
let rec contains n = function 
    | R(x,y,z) when n=x->true
    | R(_,_,[]) -> false
    | R(x,y,z::t) -> contains n z || contains n (R(x,y,t));;

contains "R12" riv;;

//Q3) --------------------------------------------------------------------------------------------------------------------
let rec allnames = function
    | R(x,_,[]) -> [x]
    | R(x,y,head::tail) -> (allnames head)@(allnames (R(x,y,tail)));;
allnames riv;;
//Q4) --------------------------------------------------------------------------------------------------------------------
let rec totalFlow = function
    | R(x,y,[]) -> y
    | R(x,y,head::tail) -> y+(totalFlow head) + (totalFlow (R(x,0,tail)));;
totalFlow riv;;
//Q5) --------------------------------------------------------------------------------------------------------------------

let rec listingFlows = function
    | R(x,y,[]) -> [(x,y)]
    | R(x,y,head::tail) -> (listingFlows head)@(listingFlows (R(x,y,tail)));;
listingFlows riv;;
let rec mainsource = function
    | R(x,y,[]) -> (x,y)
    | R(x,y,z) ->  ((List.maxBy snd (listingFlows (R(x,y,z)))) |> fst,(List.maxBy snd (listingFlows (R(x,y,z)))) |> snd)  ;;
                   
mainsource  riv;;
//Q6) --------------------------------------------------------------------------------------------------------------------
//I didnt understand this question.
let rec tryInsert n t r = match r with
    | 