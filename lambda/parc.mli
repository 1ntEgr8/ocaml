(* Precise Automatic Reference Couting a la Perceus

   paper: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v4.pdf
 *)

exception ParcError

(* Runs the Perceus algorithm and inserts reference counting
   operations *)
val parc : Lambda.lambda -> Lambda.lambda

(* Runs Perceus algorithm at the program level *)
val parc_program : Lambda.program -> Lambda.program
