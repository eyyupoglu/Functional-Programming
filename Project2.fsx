type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>
//----------------------------------------------------------------------------------------------------------------------------
//this function simply takes a single and return the boolean of if ects%5 = 0 nor not
let isValidCourseDesc (title,ects) = (ects % 5 =0) 
let test1a = ("cheese",22)
let test1b = isValidCourseDesc test1a = false;;
//----------------------------------------------------------------------------------------------------------------------------
//(Map<'a,('b*int)> -> bool))
(*Here forall method is used with two arguments, first being the predicate and second is the map. 
in the predicate, underscore is representing the key of the map and p is the value*)
let isValidCourseBase cb =  Map.forall (fun _ p -> isValidCourseDesc p) cb
let test2a = Map.ofList [(12,("cheese",20));(13,("herring",5));(14,("soft drink",5));(17,("herring",50))]
let test2b = isValidCourseBase test2a = true;;
//----------------------------------------------------------------------------------------------------------------------------
type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional
(*
Here, using the intersect method to see if there is anything in common and checking if it is an empty set or not.
If it is an empty set, then these the sets are disjoint and returning true.
*)
let disjoint s1 s2 = Set.intersect s1 s2 = set []
let test3a1 = Set.ofList [(12,("cheese",20));(13,("herring",5));(14,("soft drink",5));(17,("herring",50))]
let test3a2 = Set.ofList [(12,("cheese",21));(131,("herring",5))]
let test3b = disjoint test3a1 test3a2 = true;;
//----------------------------------------------------------------------------------------------------------------------------
(*Here, course base(map) is converted to list.(Because I felt more comfortable while iterating in lists). Then split into
('a,('b*int)) form. if the related 'no' is element of the course set then sums them up.
Since Set.remove function is giving us the removed set we can immidieately pass the result to the new sumECTS function again.*)
let rec sumECTS cs cb = match (Map.toList cb) with 
                        | []-> 0 
                        | (no,(_,ects))::tail -> if Set.contains no cs 
                                                  then ects + sumECTS (Set.remove no cs) (Map.ofList tail) 
                                                  else sumECTS (Set.remove no cs) (Map.ofList tail);;

                                       
let test4a1 = Set.ofList [12;14;17]
let test4a2 = Map.ofList [(12,("cheese",20));(13,("herring",5));(14,("soft drink",5));(17,("herring",50))]
let test4a3 = Set.ofList [11;18;19]

let test4b1 = sumECTS test4a1 test4a2 = 75
let test4b2 = sumECTS test4a1 Map.empty = 0
let test4b3 = sumECTS test4a3 test4a2 = 0;;
//----------------------------------------------------------------------------------------------------------------------------
(*Here right side of the function is simply connecting all 4 predicates asked in the question with AND logical operator.  *)
let isValidCourseGroup (man,opt) cb = disjoint man opt 
                                         && sumECTS man cb <= 45  
                                         && (if  sumECTS man cb =45 then opt =Set.empty else true)
                                         && sumECTS man cb + sumECTS opt cb >=45;;
let test5a1 = Set.ofList [12;14;17]
let test5a2 = Map.ofList [(12,("cheese",20));(13,("herring",5));(14,("soft drink",5));(17,("herring",20))]
let test5a3 = Set.ofList []

let test5b1 = isValidCourseGroup (test5a1,test5a3) test5a2 = true;;
//----------------------------------------------------------------------------------------------------------------------------

type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore
                * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>
(*Here, on the left side the flag model is decomposed into smaller pieces because they will be used on the right side.
In the first condition there could be another function which checks all 3 elements all together but in this case I found it more 
feasible to write liket this.  
In the second condition to check if any of them have some element in common, I simply united the whole sets and counted
their size and checked if this size is equal to the sum of the size of the sets individually.
In the third condition I again united the whole sets but this time I used forall method to check predicate*)
let isValid
  ((man, opt), (man2, opt2),
    (man3, opt3),es) cb =  isValidCourseGroup (man, opt) cb && isValidCourseGroup (man2, opt2) cb && isValidCourseGroup (man3, opt3) cb 
        //First condition is over, now the second condition
                            && Set.count (Set.union (Set.union (Set.union man opt) (Set.union man2 opt2)) 
                              (Set.union man3 opt3))  = (Set.count (Set.union man opt))+ (Set.count (Set.union man3 opt3))+(Set.count (Set.union man2 opt2) )
        //Second condition is over, now third
                            && Set.forall es (Set.union (Set.union (Set.union man opt) (Set.union man2 opt2)) 
                                        (Set.union man3 opt3));;

let man = Set.ofList [1;2;3]
let opt = Set.ofList [4;5;6]
let man2 = Set.ofList [7;8;9]
let opt2 = Set.ofList [10;11;12]
let man3 = Set.ofList [13;14;15]
let opt3 = Set.ofList [16;17;18]
//Indicating that electives cannot be say, Phd courses. "smaller than 50."
let es courseno = courseno < 50 

let coursebase =  Map.ofList [(1,("math1",20));(2,("math2",5));(3,("physics",5));(4,("chem1",20));
                              (5,("physics2",5));(6,("chem2",20));(7,("math3",20));(8,("math4",5));
                              (9,("physics3",5));(10,("chem3",20));(11,("math5",20));(12,("math6",5))
                              (13,("physics4",5));(14,("chem4",20));(15,("math7",20));(16,("math8",5))
                              (17,("physics5",5));(18,("chem5",20));(19,("math9",20));(20,("math10",5))]                               
let test6 = isValid ((man,opt),(man2,opt2),(man3,opt3),es) coursebase ;;

//----------------------------------------------------------------------------------------------------------------------------                                                          
let checkPlan cs (k1,k2,k3,_) cb = sumECTS cs cb = 180 && sumECTS k1 cb= 45 && sumECTS k2 cb = 45 && sumECTS k3 cb =45 ;;                                                                  