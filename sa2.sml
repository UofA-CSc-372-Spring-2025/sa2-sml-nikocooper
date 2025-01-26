(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Nikoli Cooper *)
(* Time spent on SA2: 3-4 Hrs
*)

(* Collaborators and references: ChatGPT 4o, Stack Exchange, 
Documents provided in SA2 write-up *)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false
(*Unit Tests*)
(*val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true
val () = 
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false*)

(**** Problem B ****)

fun firstVowel [] = false
  | firstVowel (x::_) =
      let 
        val vowels = [#"a", #"e", #"i", #"o", #"u"]
      in 
        List.exists (fn v => v = x) vowels
      end

(*Unit Tests*)
(*val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true*)

(**** Problem C ****)

val empty = rev [] : int list (* Gives empty list a type to avoid value polymorphism *)

fun reverse lst = foldl (fn (x, acc) => x :: acc) empty lst;
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]*)

(**** Problem D ****)

exception EmptyList

fun minlist [] = raise EmptyList
  | minlist (x::xs) = foldl (fn (a, b) => if a < b then a else b) x xs
(*Unit Tests*)
(*val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0*)

(**** Problem E ****)

exception Mismatch

fun zip ([],[]) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch
(*Unit Tests*)
(* Test with equal length lists - expect correct result *)
(*val () =
  Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString (fn s => "\"" ^ s ^ "\"")))
  "zip ([1,2,3], [\"a\",\"b\",\"c\"]) should be [(1,\"a\"), (2,\"b\"), (3,\"c\")]"
  (fn () => zip ([1,2,3], ["a","b","c"]))
  [(1, "a"), (2, "b"), (3, "c")]

(* Test with empty lists - expect empty result *)
val () =
  Unit.checkExpectWith (Unit.listString (fn _ => ""))  (* Convert result to string *)
  "zip ([], []) should be []"
  (fn () => zip ([], []))
  []

(* Test with mismatched list lengths - expect exception *)
val () =
  Unit.checkExnWith (fn _ => "Mismatch")  (* Convert exception to string *)
  "zip ([1,2], [\"a\"]) should raise Mismatch exception"
  (fn () => zip ([1,2], ["a"]))*)

(**** Problem F ****)

fun concat [] = []
  | concat (x::xs) = x @ concat xs
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1,2,3,4,5,6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1,2,3,4,5,6]*)
(**** Problem G ****)

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
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith Bool.toString "isDigit #\"1\" should return true"
  (fn () => isDigit #"1")
  true

val () =
  Unit.checkExpectWith Bool.toString "isDigit #\"a\" should return false"
  (fn () => isDigit #"a")
  false*)
(**** Problem H ****)

fun isAlpha c = 
      let 
        val ascii = Char.ord c
      in (ascii >= 65 andalso ascii <= 90) orelse (ascii >= 97 andalso ascii <= 122)
      end
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith Bool.toString "isDigit #\"1\" should return true"
  (fn () => isAlpha #"1")
  false

val () =
  Unit.checkExpectWith Bool.toString "isDigit #\"a\" should return false"
  (fn () => isAlpha #"a")
  true

val () =
  Unit.checkExpectWith Bool.toString "isDigit #\"a\" should return false"
  (fn () => isAlpha #"F")
  true*)
(**** Problem I ****)

fun svgCircle (cx, cy, r, color) = 
    let
      val xStr = Int.toString cx
      val yStr = Int.toString cy
      val rStr = Int.toString r
    in
      "<circle cx=\"" ^ xStr ^ "\" cy=\"" ^ yStr ^ "\" r=\"" ^ rStr ^ "\" fill=\"" ^ color ^ "\" />"
    end
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";*)
(**** Problem J ****)

fun partition p [] = ([],[])
  | partition p (x::xs) =
      let
        val (satisfy, notSatisfy) = partition p xs
      in
        if p x then (x::satisfy, notSatisfy)
        else (satisfy, x::notSatisfy)
      end
(*Unit Tests*)
(*val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);*)


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)