module CS220.Lists // This line declares Lists module.

/// Problem 1 : Find the last element of a list.
/// Example in F#: 
/// > myLast [1; 2; 3; 4];;
/// val it : int = 4
/// > myLast ['x';'y';'z'];;
/// val it : char = 'z'
let rec myLast = function
    | [] -> failwith "list is empty"
    | [last] -> last
    | _::tl -> myLast tl

/// Problem 2 : Find the last but one element of a list.
/// (Note that the Lisp transcription of this problem is incorrect.) 
///
/// Example in F#: 
/// myButLast [1; 2; 3; 4];;
/// val it : int = 3
/// > myButLast ['a'..'z'];;
/// val it : char = 'y'
let rec myButLast = function
    | [] | [_] -> failwith "list is empty or singleton"
    | [butLast; _] -> butLast
    | _::tl -> myButLast tl

/// Problem 3 : Find the K'th element of a list. The first element in the list is number 1.
/// Example: 
/// * (element-at '(a b c d e) 3)
/// c
/// 
/// Example in F#: 
/// > elementAt [1; 2; 3] 2;;
/// val it : int = 2
/// > elementAt (List.ofSeq "fsharp") 5;;
/// val it : char = 'r'
let rec elementAt list k =
    (**if k > List.length list then failwith "index out of bounds"
    else list.[k-1]**)
    match list, k with
    | [],_ -> failwith "empty list"
    | element::_, 1 -> element
    | _::tl, n -> elementAt tl (n-1)

/// Problem 4 : Find the number of elements of a list.
/// Example in F#: 
/// 
/// > myLength [123; 456; 789];;
/// val it : int = 3
/// > myLength <| List.ofSeq "Hello, world!"
/// val it : int = 13 
let myLength list =
    let rec loop acc = function
        | [] -> acc
        | _::tl -> loop (acc+1) tl
    loop 0 list

/// Problem 5 : Reverse a list.
/// Example in F#: 
///
/// > reverse <| List.ofSeq ("A man, a plan, a canal, panama!")
/// val it : char list =
///  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
///   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
///   'A']
/// > reverse [1,2,3,4];;
/// val it : int list = [4; 3; 2; 1]
let reverse list =
    let rec loop acc = function
        | [] -> acc
        | hd::tl -> loop (hd::acc) tl
    loop [] list

/// Problem 6 : Find out whether a list is a palindrome.
/// A palindrome can be read forward or backward; e.g. (x a m a x).
/// 
/// Example in F#: 
/// > isPalindrome [1;2;3];;
/// val it : bool = false
/// > isPalindrome <| List.ofSeq "madamimadam";;
/// val it : bool = true
/// > isPalindrome [1;2;4;8;16;8;4;2;1];;
/// val it : bool = true
let isPalindrome list =
    let rec loop lst1 lst2 =
        match lst1, lst2 with
        | [], [] -> true
        | hd1::tl1, hd2::tl2 when hd1 = hd2 -> loop tl1 tl2
        | _ -> false
    reverse list |> loop list

/// Problem 7 : Flatten a nested list structure.
/// Transform a list, possibly holding lists as elements into a `flat' list by replacing each 
/// list with its elements (recursively).
///  
/// Example: 
/// * (my-flatten '(a (b (c d) e)))
/// (A B C D E)
///  
/// Example in F#: 
/// 
type 'a NestedList = List of 'a NestedList list | Elem of 'a
///
/// > flatten (Elem 5);;
/// val it : int list = [5]
/// > flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
/// val it : int list = [1;2;3;4;5]
/// > flatten (List [] : int NestedList);;
/// val it : int list = []
let flatten nestedList =
    let rec loop acc = function
        | Elem el -> el::acc
        | List lst -> List.fold (fun acc x -> loop acc x) acc lst
    loop [] nestedList |> reverse

/// Problem 8 : Eliminate consecutive duplicates of list elements.
/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
///  
/// Example: 
/// * (compress '(a a a a b c c a a d e e e e))
/// (A B C A D E)
///  
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]
let compress list =
    (**let rec contains element = function
        | [] -> false
        | hd::_ when hd = element -> true
        | _::tl -> contains element tl**)

    let rec loop acc = function
        | [] -> acc
        | [lastElement] -> lastElement::acc
        | hd::conseq::tl when hd <> conseq -> loop (hd::acc) (conseq::tl)
        | _::tl -> loop acc tl

    loop [] list |> reverse

/// Problem 9 : Pack consecutive duplicates of list elements into sublists.
/// If a list contains repeated elements they should be placed 
/// in separate sublists.
///  
/// Example: 
/// * (pack '(a a a a b c c a a d e e e e))
/// ((A A A A) (B) (C C) (A A) (D) (E E E E))
///  
/// Example in F#: 
/// 
/// > pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 
///         'a'; 'd'; 'e'; 'e'; 'e'; 'e']
/// val it : char list list =
///  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d'];
///   ['e'; 'e'; 'e'; 'e']]
let pack list =
    let rec loop currentElement elementList acc = function
        | [] -> elementList::acc
        | hd::tl when hd = currentElement -> loop currentElement (hd::elementList) acc tl
        | hd::tl -> loop hd [] (elementList::acc) (hd::tl)

    match list with
    | [] -> []
    | hd::tl -> loop hd [] [] list |> reverse

/// Problem 10 : Run-length encoding of a list.
/// Use the result of problem P09 to implement the so-called run-length 
/// encoding data compression method. Consecutive duplicates of elements 
/// are encoded as lists (N E) where N is the number of duplicates of the element E.
///  
/// Example: 
/// * (encode '(a a a a b c c a a d e e e e))
/// ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
///  
/// Example in F#: 
/// 
/// encode <| List.ofSeq "aaaabccaadeeee"
/// val it : (int * char) list =
///   [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]
let encode list =
    let rec loop acc = function
        | [] -> acc
        | hd::tl -> loop ((List.head hd, myLength hd)::acc) tl

    let packed = pack list
    loop [] packed |> reverse

/// Problem 11 : Modified run-length encoding.
/// Modify the result of problem 10 in such a way that if an element has no duplicates it 
/// is simply copied into the result list. Only elements with duplicates are transferred as
/// (N E) lists.
///  
/// Example: 
/// * (encode-modified '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeModified <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]

type 'a Encoding = Multiple of int * 'a | Single of 'a
let encodeModified list =
    let rec loop acc = function
    | [] -> acc
    | hd::tl -> if myLength hd = 1 then loop (Single (hd.Head) :: acc) tl else loop (Multiple (myLength hd, hd.Head) :: acc) tl

    let packed = pack list
    loop [] packed |> reverse

/// Problem 12 : Decode a run-length encoded list.
/// Given a run-length code list generated as specified in problem 11. Construct its 
/// uncompressed version.
///  
/// Example in F#: 
/// 
/// > decodeModified 
///     [Multiple (4,'a');Single 'b';Multiple (2,'c');
///      Multiple (2,'a');Single 'd';Multiple (4,'e')];;
/// val it : char list =
///   ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
let decodeModified encoded =
    //let rec addMultipleValuesInFront count value list =
    //    if count = 0 then list
    //    else addMultipleValuesInFront (count-1) value (value::list)

    //let folder acc elem =
    //    match elem with
    //    | Single (v) -> v::acc
    //    | Multiple (times, v) -> addMultipleValuesInFront times v acc

    //List.fold folder [] encoded |> reverse
    let expand = function
        | Single x -> [x]
        | Multiple (n,x) -> List.replicate n x
    encoded |> List.collect expand

/// Problem 13 : Run-length encoding of a list (direct solution).
/// Implement the so-called run-length encoding data compression method directly. I.e. 
/// don't explicitly create the sublists containing the duplicates, as in problem 9, 
/// but only count them. As in problem P11, simplify the result list by replacing the 
/// singleton lists (1 X) by X.
///  
/// Example: 
/// * (encode-direct '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeDirect <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]
let encodeDirect list =
    let convertElementCount element count =
        if count = 1 then Single element
        else Multiple (count, element)

    let rec loop currentElement elementCount acc = function
        | [] -> (convertElementCount currentElement elementCount)::acc
        | hd::tl when hd = currentElement -> loop currentElement (elementCount+1) acc tl
        | hd::tl -> loop hd 0 ((convertElementCount currentElement elementCount)::acc) (hd::tl)

    match list with
    | [] -> []
    | hd::tl -> loop hd 0 [] list |> reverse

/// Problem 14 : Duplicate the elements of a list.
/// Example: 
/// * (dupli '(a b c c d))
/// (A A B B C C C C D D)
///  
/// Example in F#: 
/// 
/// > dupli [1; 2; 3]
/// [1;1;2;2;3;3]
let dupli list =
    let rec loop acc = function
        | [] -> acc
        | hd::tl -> loop (hd::hd::acc) tl

    loop [] list |> reverse

/// Problem 15 : Replicate the elements of a list a given number of times.
/// Example: 
/// * (repli '(a b c) 3)
/// (A A A B B B C C C)
///  
/// Example in F#: 
/// 
/// > repli (List.ofSeq "abc") 3
/// val it : char list = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c']
let repli list times = list |> List.collect (fun element -> List.replicate times element)

/// Problem 16 : Drop every N'th element from a list.
/// Example: 
/// * (drop '(a b c d e f g h i k) 3)
/// (A B D E G H K)
///  
/// Example in F#: 
/// 
/// > dropEvery (List.ofSeq "abcdefghik") 3;;
/// val it : char list = ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k']
let dropEvery list n =
    let rec loop count n acc = function
        | [] -> acc
        | _::tl when count = 1 -> loop n n acc tl
        | hd::tl -> loop (count-1) n (hd::acc) tl
    loop n n [] list |> reverse

/// Problem 17 : Split a list into two parts; the length of the first part is given.
/// Do not use any predefined predicates. 
/// 
/// Example: 
/// * (split '(a b c d e f g h i k) 3)
/// ( (A B C) (D E F G H I K))
///  
/// Example in F#: 
/// 
/// > split (List.ofSeq "abcdefghik") 3
/// val it : char list * char list =
///   (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k'])
let split list firstLength =
    let rec loop firstList count = function
        | [] -> firstList::[]::[]
        | hd::tl when count =  1 -> (hd::firstList |> reverse)::tl::[]
        | hd::tl -> loop (hd::firstList) (count-1) tl
    loop [] firstLength list

/// Problem 18 : Extract a slice from a list.
/// Given two indices, i and k, the slice is the list containing the elements between the 
/// i'th and k'th element of the original list (both limits included). Start counting the 
/// elements with 1.
///  
/// Example: 
/// * (slice '(a b c d e f g h i k) 3 7)
/// (C D E F G)
///  
/// Example in F#: 
/// 
/// > slice ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k'] 3 7;;
/// val it : char list = ['c'; 'd'; 'e'; 'f'; 'g']
let slice list i k =
    let rec loop acc currentIdx remainingList =
        if currentIdx > k then acc
        else
            match remainingList with
            | [] -> acc
            | hd::tl when currentIdx < i -> loop acc (currentIdx+1) tl
            | hd::tl -> loop (hd::acc) (currentIdx+1) tl
    loop [] 1 list |> reverse

/// Problem 19 : Rotate a list N places to the left.
/// Hint: Use the predefined functions length and (@) 
/// 
/// Examples: 
/// * (rotate '(a b c d e f g h) 3)
/// (D E F G H A B C)
/// 
/// * (rotate '(a b c d e f g h) -2)
/// (G H A B C D E F)
///  
/// Examples in F#: 
/// 
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] 3;;
/// val it : char list = ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c']
///  
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] (-2);;
/// val it : char list = ['g'; 'h'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f']
let rotate list n =
    let length = myLength list
    if n = 0 then list
    elif n > 0 then
        let totalShift = n % length
        match split list totalShift with
        | first::second::[] -> second @ first
        | _ -> []
    else
        let totalShift = (abs n) % length
        match split list (length-totalShift) with
        | first::second::[] -> second @ first
        | _ -> []

/// Problem 20 : Remove the K'th element from a list.
/// Example in Prolog: 
/// ?- remove_at(X,[a,b,c,d],2,R).
/// X = b
/// R = [a,c,d]
///  
/// Example in Lisp: 
/// * (remove-at '(a b c d) 2)
/// (A C D)
///  
/// (Note that this only returns the residue list, while the Prolog version also returns 
/// the deleted element.)
///  
/// Example in F#: 
/// 
/// > removeAt 1 <| List.ofSeq "abcd";;
/// val it : char * char list = ('b', ['a'; 'c'; 'd'])
let removeAt idx list =
    let rec loop acc currentIdx = function
        | [] -> failwith "index out of bounds"
        | hd::tl when currentIdx = idx -> (hd, (acc |> reverse) @ tl)
        | hd::tl -> loop (hd::acc) (currentIdx+1) tl
    loop [] 0 list

/// Problem 21 : Insert an element at a given position into a list.
/// Example: 
/// * (insert-at 'alfa '(a b c d) 2)
/// (A ALFA B C D)
///  
/// Example in F#: 
/// 
/// > insertAt 'X' (List.ofSeq "abcd") 2;;
/// val it : char list = ['a'; 'X'; 'b'; 'c'; 'd']
let rec insertAt element list position =
    match list, position with
    | [], 1 -> [element]
    | [], _ -> failwith "index out of bounds"
    | _, 1 -> element::list
    | hd::tl, _ -> hd::insertAt element tl (position-1)

/// Problem 22 : Create a list containing all integers within a given range.
/// Example: 
/// * (range 4 9)
/// (4 5 6 7 8 9)
///  
/// Example in F#: 
/// 
/// > range 4 9;;
/// val it : int list = [4; 5; 6; 7; 8; 9]
let range lower higher =
    //[lower..higher]
    let rec loop acc lower higher =
        if lower > higher then acc
        else
            loop (lower::acc) (lower+1) higher
    if lower > higher then failwith "wrong range"
    else
        loop [] lower higher |> reverse

/// Problem 23 : Extract a given number of randomly selected elements from a list.
/// Example: 
/// * (rnd-select '(a b c d e f g h) 3)
/// (E D A)
///  
/// Example in F#: 
/// 
/// > rnd_select (List.ofSeq "abcdefgh") 3;;
/// val it : seq<char> = seq ['e'; 'a'; 'h']
let rnd_select list num =
    let rnd = System.Random()
    let rec loop acc count list =
        if count = 0 then acc
        elif list = [] then acc
        else
            let randomIdx = rnd.Next(myLength list)
            let (element, remaining) = removeAt randomIdx list
            loop (element::acc) (count-1) remaining
    loop [] num list |> Seq.ofList

/// Problem 24 : Lotto: Draw N different random numbers from the set 1..M.
/// Example: 
/// * (rnd-select 6 49)
/// (23 1 17 33 21 37)
///  
/// Example in F#: 
/// 
/// > diff_select 6 49;;
/// val it : int list = [27; 20; 22; 9; 15; 29]

// using problem 23
let diff_select num M =
    rnd_select [1..M] num

/// Problem 25 : Generate a random permutation of the elements of a list.
/// Example: 
/// * (rnd-permu '(a b c d e f))
/// (B A D C E F)
///  
/// Example in F#: 
/// 
/// > rnd_permu <| List.ofSeq "abcdef";;
/// val it : char list = ['b'; 'c'; 'd'; 'f'; 'e'; 'a']
let rnd_permu list = myLength list |> rnd_select list

/// Problem 26 : Generate the combinations of K distinct objects chosen from the N elements of a list.
/// In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that 
/// there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For 
/// pure mathematicians, this result may be great. But we want to really generate all the 
/// possibilities in a list.
///  
/// Example: 
/// * (combinations 3 '(a b c d e f))
/// ((A B C) (A B D) (A B E) ... )
///  
/// Example in F#: 
/// 
/// > combinations 3 ['a' .. 'f'];;
/// val it : char list list =
///   [['a'; 'b'; 'c']; ['a'; 'b'; 'd']; ['a'; 'b'; 'e']; ['a'; 'b'; 'f'];
///    ['a'; 'c'; 'd']; ['a'; 'c'; 'e']; ['a'; 'c'; 'f']; ['a'; 'd'; 'e'];
///    ['a'; 'd'; 'f']; ['a'; 'e'; 'f']; ['b'; 'c'; 'd']; ['b'; 'c'; 'e'];
///    ['b'; 'c'; 'f']; ['b'; 'd'; 'e']; ['b'; 'd'; 'f']; ['b'; 'e'; 'f'];
///    ['c'; 'd'; 'e']; ['c'; 'd'; 'f']; ['c'; 'e'; 'f']; ['d'; 'e'; 'f']]

/// Problem 27 : Group the elements of a set into disjoint subsets.
/// a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 
/// and 4 persons? Write a function that generates all the possibilities and returns them 
/// in a list.
///  
/// Example: 
/// * (group3 '(aldo beat carla david evi flip gary hugo ida))
/// ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
/// ... )
///  
/// b) Generalize the above predicate in a way that we can specify a list of group sizes 
/// and the predicate will return a list of groups.
///  
/// Example: 
/// * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
/// ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
/// ... )
///  
/// Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) 
/// is the same solution as ((BEAT ALDO) ...). However, we make a difference between 
/// ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
///  
/// You may find more about this combinatorial problem in a good book on discrete 
/// mathematics under the term "multinomial coefficients".
///  
/// Example in F#: 
/// 
/// > group [2;3;4] ["aldo";"beat";"carla";"david";"evi";"flip";"gary";"hugo";"ida"];;
/// val it : string list list list =
///   [[["aldo"; "beat"]; ["carla"; "david"; "evi"];
///     ["flip"; "gary"; "hugo"; "ida"]];...]
/// (altogether 1260 solutions)
///  
/// > group [2;2;5] ["aldo";"beat";"carla";"david";"evi";"flip";"gary";"hugo";"ida"];;
/// val it : string list list list =
///   [[["aldo"; "beat"]; ["carla"; "david"];
///     ["evi"; "flip"; "gary"; "hugo"; "ida"]];...]
/// (altogether 756 solutions)

/// Problem 28 : Sorting a list of lists according to length of sublists
/// a) We suppose that a list contains elements that are lists themselves. The objective 
/// is to sort the elements of this list according to their length. E.g. short lists first,
/// longer lists later, or vice versa.
///  
/// Example: 
/// * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
/// ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
///  
/// Example in F#: 
/// 
/// > lsort ["abc";"de";"fgh";"de";"ijkl";"mn";"o"];;
/// val it : string list = ["o"; "de"; "de"; "mn"; "abc"; "fgh"; "ijkl"]
///
/// b) Again; we suppose that a list contains elements that are lists themselves. But this 
/// time the objective is to sort the elements of this list according to their length 
/// frequency; i.e.; in the default; where sorting is done ascendingly; lists with rare 
/// lengths are placed first; others with a more frequent length come later.
///  
/// Example: 
/// * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
/// ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
///  
/// Example in F#: 
/// 
/// > lfsort ["abc"; "de"; "fgh"; "de"; "ijkl"; "mn"; "o"];;
/// val it : string list = ["ijkl"; "o"; "abc"; "fgh"; "de"; "de"; "mn"]
