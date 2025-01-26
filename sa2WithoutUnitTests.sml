(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Nikoli Cooper *)
(* Time spent on SA2: 3-4 Hrs *)

(* Collaborators and references: ChatGPT 4o, Stack Exchange, 
   Documents provided in SA2 write-up *)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

(* mynull : 'a list -> bool *)
(* Checks if a list is empty *)
fun mynull []       = true
  | mynull (_::_)   = false

(**** Problem B ****)

(* firstVowel : char list -> bool *)
(* Checks if the first character in a list is a vowel *)
fun firstVowel [] = false
  | firstVowel (x::_) =
      let 
        val vowels = [#"a", #"e", #"i", #"o", #"u"]  (* List of vowel characters *)
      in 
        List.exists (fn v => v = x) vowels  (* Check if x is in vowel list *)
      end

(**** Problem C ****)

(* reverse : 'a list -> 'a list *)
(* Reverses a list using fold left *)
val empty = rev [] : int list  (* Gives empty list a type to avoid value polymorphism *)

fun reverse lst = foldl (fn (x, acc) => x :: acc) empty lst;

(**** Problem D ****)

exception EmptyList

(* minlist : int list -> int *)
(* Finds the minimum element in a list, raises exception if empty *)
fun minlist [] = raise EmptyList
  | minlist (x::xs) = foldl (fn (a, b) => if a < b then a else b) x xs

(**** Problem E ****)

exception Mismatch

(* zip : ('a list * 'b list) -> ('a * 'b) list *)
(* Combines two lists into a list of pairs, raises exception if lengths mismatch *)
fun zip ([],[]) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch

(**** Problem F ****)

(* concat : 'a list list -> 'a list *)
(* Flattens a list of lists into a single list *)
fun concat [] = []
  | concat (x::xs) = x @ concat xs

(**** Problem G ****)

(* isDigit : char -> bool *)
(* Checks if a character is a digit (0-9) *)
fun isDigit x = 
      case x of
          #"0" => true
        | #"1" => true
        | #"2" => true
        | #"3" => true
        | #"4" => true
        | #"5" => true
        | #"6" => true
        | #"7" => true
        | #"8" => true
        | #"9" => true
        | _ => false

(**** Problem H ****)

(* isAlpha : char -> bool *)
(* Checks if a character is an alphabetic letter *)
fun isAlpha c = 
      let 
        val ascii = Char.ord c  (* Get ASCII code of character *)
      in (ascii >= 65 andalso ascii <= 90) orelse (ascii >= 97 andalso ascii <= 122)
      end

(**** Problem I ****)

(* svgCircle : int * int * int * string -> string *)
(* Generates an SVG circle element string from parameters *)
fun svgCircle (cx, cy, r, color) = 
    let
      val xStr = Int.toString cx
      val yStr = Int.toString cy
      val rStr = Int.toString r
    in
      "<circle cx=\"" ^ xStr ^ "\" cy=\"" ^ yStr ^ "\" r=\"" ^ rStr ^ "\" fill=\"" ^ color ^ "\" />"
    end

(**** Problem J ****)

(* partition : ('a -> bool) -> 'a list -> ('a list * 'a list) *)
(* Splits a list into elements that satisfy a predicate and those that do not *)
fun partition p [] = ([],[])
  | partition p (x::xs) =
      let
        val (satisfy, notSatisfy) = partition p xs  (* Recursive partitioning of tail *)
      in
        if p x then (x::satisfy, notSatisfy)
        else (satisfy, x::notSatisfy)
      end


