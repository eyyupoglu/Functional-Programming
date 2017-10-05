//Mehmet Eyyupoglu s17448
//Gleb Lematsko s162105
//Abdelali Khatibi s164561
type no = int;;
type yb = int;;
type ths = string list;;
type description = (no * yb * ths )
type name = string 
type reg = ( name * description ) list

type extractInterested = (name * no) list
type p1 = bool
type p2 = bool
//helping function check if  it is a member
type ismember = bool

let reg = [( "Mike", (91932342,1994, ["jazz"] )); ( "Aaron", (91936542,1944, ["rock";"jazz";"basketball";"gamble";"soccer"] ));( "Jackson", (91936544,1997, ["jazz";"alcohol";"basketball";"soccer";"sex"] ))]

//type: 'a-> 'a list-> bool
//This function returns true if the first argument is matching with the head of second argument until the end of the list. Besides,
// these true or false predicates are connected with "or" logical operator because we know that if there is any, then it should return true
let rec ismember element = function
    | head::tail -> (head = element) || (ismember element tail)
    | [] -> false
//type: string list -> bool
//This function returns true if the argument(whose type is list) contains word of "soccer" and "jazz" then stops. Otherwise, it continues to do the same for
//the tail. And if it reaches the tail is actually an empty list, then it returns false and stops.
let rec p1  = function 
    | [] -> false
    | c when (ismember "soccer" c) && (ismember "jazz" c)-> true
    | x::y -> p1 y  ;;
//type: string list -> bool
//This function basically is doing the same thing with p1 only with difference of or betweenthe predicates of "when".
let rec p2  = function 
    | [] -> false
    | c when (ismember "soccer" c) || (ismember "jazz" c)-> true
    | x::y -> p1 y  ;;
//type: ('a -> bool) -> ('b * ('c * int *'a)) list -> ('b * 'c)list
//Takes two arguments of a predicate and a list. It checks if it is an empty list and then returns empty list and stops. If not it goes to second clause
// and checks pulls thms list from the tuple element of related list. After thms is pulled, it gives that thms list to p function and takes a truth value
// and connects that truth value with logical operator "and" to another one(which is our second condition being young, yb > 1984). If all these are correct then it continues 
//with the right side of the arrow and put the (name, no) tuple as the new element of a lise and calls the function again.
//if these clauses wont work then it just calls it self again because we need to look at the the other people's description too.
//Age of interest is specified here.
let rec extractInterested p = function
    | [] -> []
    | (name,(no,yb,thms))::tail when (p thms) && (yb > 1984) -> (name, no)::(extractInterested p tail)
    | _::b -> extractInterested p b;;

extractInterested p1 reg;;
extractInterested p2 reg;;
let test1 = extractInterested p1 reg = [("Jackson", 91936544)];;
let test2 = extractInterested p2 reg = [("Mike", 91932342); ("Jackson", 91936544)];;
let test_p1 = p1 ["rock";"jazz";"basketball";"gamble";"soccer"] = true;;
let test_p1_1= p1 [] = false;;
let test_p2 = p2 ["rock";"jazz";"basketball";"gamble"] = true;;
let test_p2_1= p2 [] = false;; 
let test_ismember = ismember "rock" ["rock";"jazz";"basketball";"gamble"] = true;;

let test_ismember2 = ismember "climbing" ["rock";"jazz";"basketball";"gamble"] = false;;