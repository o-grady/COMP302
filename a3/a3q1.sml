(*COMP 302 Assignment 3 Question 1 Grady Weber 260513378*)
datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref)
fun insert (f,toInsert,x as (ref Empty)) = let val temp = !(x) in 
						x := RCons(toInsert,ref temp)
					    end
  | insert (f,toInsert, x as (ref (y as RCons(value, next)))) = if f(toInsert,value) then
							     let val temp = !(x) in 
								 x := RCons(toInsert,ref temp)
							     end
							 else
							     insert(f,toInsert,next)
fun getFirst (ref (RCons(value,next))) = value
fun getSecond (ref (RCons(value,next))) = next
fun less (x,y) = (x<y) 
fun show (ref Empty) = (print("\n"))
  | show (ref (RCons(h,rt))) = (print(Int.toString(h)^",");show(rt))
val bar = ref (RCons(1,ref (RCons(2, ref (RCons(6,ref Empty))))))
val _ = insert(less,3,bar)
val _ = show(bar)
