datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
fun flatten(x:'a tree)=let
    fun helper (Empty,l) = l
      | helper (Node(left,value,right),l) = l@[value]@helper(left,l)@helper(right,l)
    in
	helper(x,[])
    end
