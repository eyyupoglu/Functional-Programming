type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

let isValidCourseDesc (title,ects) = (ects % 5 =0) ;;


let isValidCourseBase cb =  Map.forall (fun _ p -> isValidCourseDesc p) cb;;

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

let disjoint s1 s2 = not (Set.intersect s1 s2 = set []);;
let disjoint2 (s1,s2,s3,s4) = not (Set.intersect s1 s2 = set []) && not (Set.intersect s3 s4 = set [])
                                && not (Set.intersect s1 s3 = set [])&& not (Set.intersect s1 s4 = set [])
                                && not (Set.intersect s2 s3 = set []) && not (Set.intersect s2 s4 = set []);;

let boardMembers = Set.ofList [ "Alice"; "Bill"; "Ann"];;
let males = Set.ofList ["Bob"; "Bill"; "Ben"; "Bill"];;
 disjoint males boardMembers;;


let rec sumECTS cs cb = match (Map.toList cb) with | []-> 0 | (no,(_,ects))::tail -> if Set.contains no cs then ects + sumECTS (Set.remove no cs) (Map.ofList tail) else sumECTS (Set.remove no cs) (Map.ofList tail);;

let isValidCourseGroup (man,opt) cb = disjoint man opt 
                                         && sumECTS man cb <= 45  
                                         && (if  sumECTS man cb =45 then Set.count opt = 0 else true)
                                         && sumECTS man cb + sumECTS opt cb >=45;;
                                        
type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore
                * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

let isValid
  ((man, opt), (man2, opt2),
    (man3, opt3),es) cb =  isValidCourseGroup (man, opt) cb && isValidCourseGroup (man2, opt2) cb && isValidCourseGroup (man3, opt3) cb 
        //First condition is over, now the second condition
                            && Set.count (Set.union (Set.union (Set.union man opt) (Set.union man2 opt2)) 
                              (Set.union man3 opt3))  = (Set.count (Set.union man opt))+ (Set.count (Set.union man3 opt3))+(Set.count (Set.union man2 opt2) )
        //Second condition is over, now third
                            && Set.forall es (Set.union (Set.union (Set.union man opt) (Set.union man2 opt2)) 
                                        (Set.union man3 opt3));;
                                                          
 

let checkPlan cs (k1,k2,k3,_) cb = sumECTS cs cb = 180 && sumECTS k1 cb= 45 && sumECTS k2 cb = 45 && sumECTS k3 cb =45 ;;
                                                                  
                                                                 








let k = set [12;13;14;15];;


let reg1 = Map.ofList [(12,("cheese",22));(13,("herring",5));(14,("soft drink",5));(17,("herring",50))];;

sumECTS k reg1;;