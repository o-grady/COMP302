(* Question 3.  *)
datatype exp = Nat of int | Plus of exp * exp | Minus of exp * exp |
Mult of exp * exp | If of exp * exp * exp | Bool of bool | 
And of exp * exp | Not of exp | Eq of exp * exp | Lte of exp * exp | 
Var of string | Let of exp * (string * exp) | Fun of string * string * exp |
Apply of exp * exp

(* An auxiliary function to remove all copies of an element from a list. *)
fun remove(s,[])    = []
  | remove(s, x::l) = if (s=x) then remove(s,l) else x::(remove(s,l))
(*returns true if a value is in a list*)
fun member (x,[]) = false
  | member (x,y::ys) = if (x=y) then true else member(x,ys)
fun free_list(n) = let 
    fun helper(And(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Apply(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Bool(_),unbound,bound) = (unbound,bound)
      | helper(Eq(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Fun(Name,param,body),unbound,bound) = (remove(Name,(remove(param,unbound)))@(#1 (helper(body,unbound,bound))),Name::param::bound@(#2 (helper(body,unbound,bound))))
      | helper(If(Condition,Then,Else),unbound,bound) = ((#1 (helper(Condition,unbound,bound)))@(#1 (helper(Then,unbound,bound)))@(#1 (helper(Else,unbound,bound))),(#2 (helper(Condition,unbound,bound)))@(#2 (helper(Then,unbound,bound)))@(#2 (helper(Else,unbound,bound))))
      | helper(Let(e1,(var,e2)),unbound,bound) = (remove(var,unbound)@(#1 (helper(e1,unbound,bound)))@(#1 (helper(e2,unbound,bound))),var::bound@(#2 (helper(e1,unbound,bound)))@(#2 (helper(e2,unbound,bound))))
      | helper(Lte(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Minus(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Mult(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound)))) 
      | helper(Nat(_),unbound,bound)= (unbound,bound)
      | helper(Not(Exp),unbound,bound) = ((#1 (helper(Exp,unbound,bound))),(#2 (helper(Exp,unbound,bound))))
      | helper(Plus(left,right),unbound,bound) = ((#1 (helper(left,unbound,bound)))@(#1 (helper(right,unbound,bound))),(#2 (helper(left,unbound,bound)))@(#2 (helper(right,unbound,bound))))
      | helper(Var(name),unbound,bound) = if (member(name,bound)) then
					      (unbound,bound) 
					  else
					      (name::unbound,bound)
in
let fun removelistfromlist ([],removeFrom) = removeFrom
      | removelistfromlist (toRemove,[]) = []
      | removelistfromlist (x::toRemove,removeFrom) = removelistfromlist(toRemove,remove(x,removeFrom))
in
removelistfromlist(#2 (helper(n,[],[])), #1 (helper(n,[],[])))
end
end

(* This function I wrote constructs the list of all free variables in an
expression.  It does not bother about removing duplicates. The remove
function above will remove duplicates when necessary. The variables are
stored as strings; we do not keep the Var constructor. *)

(* Examples for testing *)

val ex1 = Let(Nat(5),("a",(Plus(Var("a"),Nat(2)))))
val ex2 = Plus(Var("a"),Nat(2))
val ex3 = Let(Var("y"),("x",Plus(Var("x"),Var("z"))))
val ex4 = Let(Nat(3),("z",(Let(Nat(2),("y",ex3)))))
val ex5 = Fun("fac","n",If(Eq(Var("n"),Nat(0)),Nat(1), Mult(Var("n"),(Apply(Var("fac"),(Minus(Var("n"),Nat(1))))))))
val ex6 = Fun("f","n",Plus(Nat(3),Var("n")))
val ex7 = Fun("g","n",If(Eq(Var("n"),Nat(0)),Nat(0),Plus(Nat(1),Apply(Var("g"),(Minus(Var("n"),Nat(1)))))))
