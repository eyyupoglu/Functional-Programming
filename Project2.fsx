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


let boardMembers = Set.ofList [ "Alice"; "Bill"; "Ann"];;
let males = Set.ofList ["Bob"; "Bill"; "Ben"; "Bill"];;
 disjoint males boardMembers;;


let rec  sumECTS cs cb = match (Map.toList cb) with | []-> 0 | (no,(_,ects))::tail -> if Set.contains no cs then ects + sumECTS (Set.remove no cs) (Map.ofList tail) else sumECTS (Set.remove no cs) (Map.ofList tail);;




let k = set [12;13;14;15];;


let reg1 = Map.ofList [(12,("cheese",22));(13,("herring",5));(14,("soft drink",5));(17,("herring",50))];;

sumECTS k reg1;;