module CS220.Library // This line declares Library module.

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

