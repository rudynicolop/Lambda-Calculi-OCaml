(*open Core*)
open Util
open FunUtil
open Syntax

let rec fold_term
    ~ctx:(o: 'o)
    ~f:(f: 'b -> 'o -> 'o)
    ~sort:(sf: sort -> 's)
    ~var:(v: 'o -> 'a -> 's)
    ~abs:(ab: 'o -> 'b -> 's -> 's -> 's)
    ~app:(ap: 's -> 's -> 's)
    ~pi:(pi: 'o -> 'b -> 's -> 's -> 's)
  : ('a,'b) term -> 's = function
  | Sort s -> sf s
  | Var a -> v o a
  | Abs (b,t1,t2) ->
    ab
      o b
      (fold_term
         ~ctx:o ~f:f
         ~sort:sf ~var:v ~abs:ab
         ~app:ap ~pi:pi t1)
    $ fold_term
      ~ctx:(f b o) ~f:f
      ~sort:sf ~var:v ~abs:ab
      ~app:ap ~pi:pi t2
  | App (t1,t2) ->
    ap
      (fold_term
         ~ctx:o ~f:f
         ~sort:sf ~var:v ~abs:ab
         ~app:ap ~pi:pi t1)
    $ fold_term
      ~ctx:o ~f:f
      ~sort:sf ~var:v ~abs:ab
      ~app:ap ~pi:pi t2
  | Pi (b,t1,t2) ->
    pi
      o b
      (fold_term
         ~ctx:o ~f:f
         ~sort:sf ~var:v ~abs:ab
         ~app:ap ~pi:pi t1)
    $ fold_term
      ~ctx:(f b o) ~f:f
      ~sort:sf ~var:v ~abs:ab
      ~app:ap ~pi:pi t2
(*
let term_scheme
    ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
*)
