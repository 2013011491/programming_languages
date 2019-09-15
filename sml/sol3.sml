(*quetion1,2*)
datatype pattern = Wildcard
|Variable of string
|UnitP
|ConstP of int
|TupleP of pattern list
|ConstructorP of string * pattern

datatype valu = Const of int
|Unit
|Tuple of valu list
|Constructor of string * valu

(*quetion1*)
fun str_append(a,b) = 
	case a of
		Variable a1 => b @ [Variable a1]
		|TupleP a1 => b @ (List.foldl(str_append) [] a1)
		|ConstructorP (a1,a2) => str_append(a2,b)
		|UnitP => b @ [UnitP]
		|Wildcard => b @ [Wildcard]
		|ConstP a1 => b @ [ConstP a1]
fun check_list e = 
	if null e
	then true
	else (not (List.exists(fn (t) => ((hd e) = t)) (tl e))) andalso check_list(tl e)	

fun check_pat (p:pattern) = 
	case p of
		Variable p1 =>true
		|TupleP p1 => check_list(str_append(p,[]))
		|ConstructorP (p1,p2) => check_pat(p2)
		|_=>true

(*quetion2*)
fun match (v:valu,p:pattern) =
	case p of
		Wildcard => SOME []
		|Variable p1 => SOME [(p1,v)]
		|UnitP => if v=Unit then SOME [] else NONE
		|ConstP p1 => let fun concheck v = 
			case v of
				Const v1 => if(v1 = p1) then SOME[] else NONE
				|_ => NONE
			in concheck v 
			end
		|ConstructorP (p1,p2) => let fun conscheck v =
			case v of
				Constructor(v1,v2) => if p1 = v1 andalso isSome(match(v2,p2)) then match(v2,p2) else NONE
				|_=>NONE
			in conscheck v
			end
		|TupleP p1 =>
			case v of
				Tuple v1 => if null p1 andalso null v1 then SOME []
							else if List.length p1 = List.length v1
							then 
								let val first = match(hd v1,hd p1);val second = match(Tuple(tl v1),TupleP(tl p1))
								in 
									if isSome(first) andalso isSome(second)
									then SOME (valOf(first) @ valOf(second))
									else NONE
								end 
							else NONE  
				|_=>NONE




(*quetion3*)
type name = string

datatype RSP =
ROCK
|SCISSORS
|PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = 
PLAYER of name * (RSP strategy ref)
|MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one,fn()=>onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = Cons(one, fn()=>alterTwo(two,one))

fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one,fn()=> alterThree(two,three,one))

fun next(strategyRef) = 
	let val Cons(rsp,func) = !strategyRef in
		strategyRef :=func();
		rsp
	end

fun crirsp (rsp1:RSP,rsp2:RSP) = 
	case (rsp1,rsp2) of
		(ROCK,ROCK) => 0
		|(SCISSORS,SCISSORS) => 0
		|(PAPER,PAPER) => 0
		|(ROCK,SCISSORS) => 1
		|(SCISSORS,PAPER) => 1
		|(PAPER,ROCK) => 1
		|(ROCK,PAPER) => 2
		|(PAPER,SCISSORS) => 2
		|(SCISSORS,ROCK) => 2

fun whosWinner(t)  = 
	case t of
		PLAYER t1=> PLAYER t1
		|MATCH(t1,t2) =>let 
							val PLAYER(x1,funcc)= whosWinner(t1); val x=next(funcc)
						in 
							let
								val PLAYER(x2,funccc)= whosWinner(t2); val y=next(funccc)
							in
							if (crirsp(x,y) = 1)
							then PLAYER(x1,funcc)
							else if (crirsp(x,y) = 2)
							then PLAYER(x2,funccc)	
							else whosWinner(t)
						end
					end

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK,PAPER)
val sr = alterTwo(SCISSORS,ROCK)
val ps = alterTwo(PAPER,SCISSORS)
val srp = alterThree(SCISSORS,ROCK,PAPER)

val winner = whosWinner(MATCH(PLAYER("s",ref s),MATCH(PLAYER("rp",ref rp),PLAYER("r",ref r))))