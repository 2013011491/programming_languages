datatype expr = NUM of int
|PLUS of expr * expr
|MINUS of expr * expr

datatype formula = TRUE
|FALSE
|NOT of formula
|ANDALSO of formula * formula
|ORELSE of formula * formula
|IMPLY of formula * formula
|LESS of expr * expr

fun eval e = 
	case e of
		TRUE =>true
		|FALSE =>false
		|NOT e1 => not (eval (e1))
		|ANDALSO(e1,e2) => if not (eval (e1)) then false else if eval e2 then true else false 
		|ORELSE(e1,e2) => if eval e1 then true else if eval e2 then true else false
		|IMPLY(e1,e2) => if not (eval (e1)) then true else if eval e2 then true else false
		|LESS(e1,e2) => let fun evall x =
			case x of
				NUM x1 => x1
				|PLUS(x1,x2) => evall x1 + evall x2
				|MINUS(x1,x2) => evall x1 - evall x2
			in if evall e1 < evall e2
				then true
				else false
			end