(* Precise Automatic Reference Couting a la Perceus

   paper: https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v4.pdf
 *)


(* Runs the Perceus algorithm and inserts reference counting
   operations *)
val parc : Clambda.ulambda -> Clambda.ulambda
