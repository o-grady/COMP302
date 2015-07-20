datatype transactions = Withdraw of int | Deposit of int | Check_balance
exception wrongPassword
exception overDrawn of int
fun make_protected_account(opening_balance: int, password: string) = 
    let 
	val balance = ref opening_balance;
	val passwd = password
    in
	let 
	    val acc = fn (pass:string,Withdraw(x)) => if(pass=passwd) then
							  if(!balance > x) then
								(balance := !balance - x;print(Int.toString(!balance)^"\n"))  
							  else
							      raise overDrawn(!balance)
							     else
								 raise wrongPassword
			 |(pass:string,Deposit(x)) => if(pass=passwd) then
								(balance := !balance + x;print(Int.toString(!balance)^"\n"))   
							     else
								 raise wrongPassword
			 |(pass:string,Check_balance)  => if(pass=passwd) then
								print(Int.toString(!balance)^"\n")
							     else
								 raise wrongPassword

	in
	    fn (pw:string, t: transactions) =>
	       (acc(pw,t)
		   handle wrongPassword => print("wrong Password. \n")
		       | (overDrawn n) => 
			 print
			     ("Insufficient funds for this transaction. The balance is "^Int.toString(n)^"\n"))
	end
    end
