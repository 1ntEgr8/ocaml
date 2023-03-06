(* Perceus reuse analysis on Lambda form *)

open Lambda

val insert_reuse_tokens : program -> program

val specialize_reuses : program -> program
