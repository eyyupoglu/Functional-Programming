//Mehmet Eyyupoglu s174448
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

let reg = [( "Mike", (91932342,1994, ["jazz"] ));( "Aaron", (91936542,1944, ["rock";"jazz";"basketball";"gamble";"soccer"] ));( "Jackson", (91936544,1997, ["jazz";"alcohol";"basketball";"soccer";"jumping"] ));( "Jacob", (91936543,1997, ["jazz";"alcohol";"basketball";"running";"jumping"] ));( "Ali", (91936544,1994, ["pop";"alcohol";"basketball";"soccer";"jumping"] ));( "Mehmet", (91936544,1453, ["asdqwe";"sdf";"basketball";"soccer";"jazz"] ))]

//type: 'a-> 'a list-> bool
//This function returns true if the first argument is matching with the head of second argument until the end of the list. Besides,
// these true or false predicates are connected with "or" logical operator because we know that if there is any, then it should return true
let rec ismember element = function
    | head::tail -> (head = element) || (ismember element tail)
    | [] -> false
//type: ('a * 'b * string list) -> bool
//This function returns true if the argument(whose type is tuple)'s last element contains word of "soccer" and "jazz" and second element of the tuple is greater than 1982, then stops.
//Otherwise it goes to the last clause and returns false
let p1  = function 
    | (_,_,[]) -> false
    | (_,yb,c) when (ismember "soccer" c) && (ismember "jazz" c) && (yb>1982)-> true
    | (_,_,y::z) -> false  ;;
//type: ('a * 'b * string list)-> bool
//This function basically is doing the same thing with p1 only with difference of or between the predicates of "when". This time it is "or"
let p2  = function 
    | (_,_,[]) -> false
    | (_,yb,c) when ((ismember "soccer" c) && (yb>1982)) || ((ismember "jazz" c)&& (yb>1982))-> true
    | (_,_,y::z) -> false  ;;
//type: (('a * int * 'b)-> bool) -> ('c * ('a * int *'b)) list -> ('c * 'a)list
//Takes two arguments, a predicate function and a list. It checks if it is an empty list and then returns empty list and stops. If not it goes to second clause
// and checks. It pulls thms list from the tuple element of related list. After thms is pulled, it passes that thms list to p function as argument and takes a truth value
// and connects that truth value with logical operator "and" to another one(which is our second condition "being young", "yb > 1984)". If all these are correct then it continues 
//with the right side of the arrow and put the (name, no) tuple as the new element of a list and calls the function again.
//if these first 2 clauses wont work then it just calls it self again because we need to look at the the other people's description too.
let rec extractInterested p = function
    | [] -> []
    | (name,(no,yb,thms))::tail when (p (no,yb,thms)) && (yb > 1984) -> (name, no)::(extractInterested p tail)
    | _::b -> extractInterested p b;;

extractInterested p1 reg;;
extractInterested p2 reg;;
let test1 = extractInterested p1 reg = [("Jackson", 91936544)];;
let test2 = extractInterested p2 reg =  [("Mike", 91932342); ("Jackson", 91936544); ("Jacob", 91936543);("Ali", 91936544)];;

let test_p1 = p1 (91932342,1980, ["jazz"] ) = false;;
let test_p1_0 = p1 (91932342,1994, ["jazz";"soccer"] ) =true;;
let test_p1_1= p1 (321 ,12,[]) = false;;
let test_p2 = p2 (91932342,1994, ["jazz"] ) = true;;
let test_p2_2 = p2 (91932342,1980, ["soccer"] ) = false;;
let test_p2_3 = p2 (91932342,1994, ["volleyball"] ) = false;;
let test_p2_4= p2 (123123,123123,[]) = false;; 
let test_ismember = ismember "rock" ["rock";"jazz";"basketball";"gamble"] = true;;

let test_ismember2 = ismember "climbing" ["rock";"jazz";"basketball";"gamble"] = false;;