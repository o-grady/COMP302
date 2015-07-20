(* basic definitions and the "take" function. *)
datatype 'a susp = Susp of (unit -> 'a)

fun delay f = Susp f

fun force (Susp f) = f ()

datatype 'a str = Str of {hd: 'a, tl : ('a str) susp} 


 (* Inspect a stream up to n elements *)
fun take n (Str s) = case n of 
   0 => []
  | n => (#hd s) :: take (n-1) (force (#tl s))


(*Things needed to implement the primes stream by sieve of Erasthosthenes *)
fun find_hd p (Str s) = 
  if  p ((#hd s)) then ((#hd s), (#tl s))
  else find_hd p (force (#tl s))

fun filter p (x as Str s) = 
	let val (h,t) = find_hd p x in 
		Str{hd = h,
 			tl = Susp (fn () => filter p (force t))} end

fun nats_from n = Str{hd = n, tl = Susp (fn () => nats_from (n+1))}

(* A utility to print part of a stream; it prints the first n elements of s. *)
fun printIntStr s n = List.app (fn x => print ((Int.toString x)^" ")) (take n s)

fun infiniteDecimal a b = let val head = a div b;
			      val tail = 10 * (a mod b) in
			      Str{hd = head, tl = Susp(fn () => (infiniteDecimal tail b))}
end

fun notDivisible first second = if ((second mod first) = 0) then false else true
fun sieve (Str s) = let val divis = (notDivisible (#hd s)) in 
   Str{hd = (#hd s), tl = Susp (fn () => (sieve (filter divis (force( #tl s)))))} end
