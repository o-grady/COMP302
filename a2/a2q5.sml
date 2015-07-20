datatype Exptree = 
	 Const of int |
	 Var of string |
	 Add of Exptree * Exptree |
	 Mul of Exptree * Exptree
type Bindings = (string * int) list
fun lookup (s:string,(x::xs)) = let fun match(a:string,b:string * int ) = 
					   if (a = (#1 b)) then
					       SOME (#2 b)
					   else
					       NONE
				   in
				       if (isSome(match(s,x))) then
					   match(s,x)
				       else
					   lookup(s,xs)
					 end
  | lookup (s:string,[]) = NONE 
fun insert (s:string * int,[]) = [s]
  | insert (s:string * int , x::xs) =  if ((#1) s <= (#1)x) then
					  s::x::xs
				      else
					  x::insert(s,xs)
fun eval3 (Const z,x) = SOME z
  | eval3 (Var z,x) = lookup(z,x)
  | eval3 (Add (z1,z2),x) = if((isSome(eval3(z1,x))) andalso (isSome(eval3(z2,x)))) then
				   SOME (getOpt(eval3(z1,x),0)+getOpt(eval3(z2,x),0))
			       else
				   NONE
  | eval3 (Mul (z1,z2),x) = if((isSome(eval3(z1,x))) andalso (isSome(eval3(z2,x)))) then
				   SOME (getOpt(eval3(z1,x),0) * getOpt(eval3(z2,x),0))
			       else
				   NONE



