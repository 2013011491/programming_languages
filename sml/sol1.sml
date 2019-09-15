fun merge(xs:int list, ys :int list)=
if null xs
then ys
else if null ys
then xs
else if hd(xs) < hd(ys)
then hd(xs)::merge(tl(xs),ys)
else hd(ys)::merge(xs,tl(ys))

fun reverse(xs:int list)=
if null xs
then []
else reverse(tl(xs))@[hd(xs)]

fun sigma(xs:int,ys:int,f:int->int)=
if xs=ys
then f(xs)
else f(xs)+sigma(xs+1,ys,f)

fun digits(xs:int)=
if xs<10
then [xs]
else digits(xs div 10)@[xs mod 10]

fun additivePersistence(xs:int)=
if xs<10
then 0
else let fun digit(ys:int)=
			if ys<10
 			then ys
 			else digit(ys div 10) + ys mod 10
 	 in 
 	 	let val sum=digit(xs)
 	 	in if sum >=10
 	 		then 1+additivePersistence(sum)
 	 		else 1
 	 	end
 	 end

fun digitalRoot(xs:int)=
	if xs<10
	then xs
	else let fun digit(ys:int)=
				if ys<10
				then ys
				else digit(ys div 10) + ys mod 10
		 in let val sum = digit(xs)
		 	in if sum >= 10
		 		then digitalRoot(sum)
		 		else sum
		 	end
		 end		