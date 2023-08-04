PART 1 --------------------------------------------------------------------------

Documentation for each line of code is written in LambdaParser.hs. This part is 
just a txt file to briefly clarify some parts.

Exercise 1:

<lambdaP> ::= <longLambdaP> | <shortLambdaP>
<longLambdaP> ::= <longPreBuild>
<shortLambdaP> ::= <shortPreBuild>
<longPreBuild> ::= "(" <longPreBuild ")" | 
                   <lambdaSymbol> <longParameter> <dot> <longPreBuild> |
                   <lambdaSymbol> <longParameter> <dot> <expression> |
                   "(" <lambdaSymbol> <longParameter> <dot> <expression> ")" <longPreBuild> |
                   <lambdaSymbol> <longParameter> <dot> <expression> <longPreBuild> |
                   "(" <lambdaSymbol> <longParameter> <dot> <expression> ")" <expression> |
                   "(" <lambdaSymbol> <longParameter> <dot> <expression> ")" <longPreBuild>
<lambdaSymbol> ::= "λ"
<dot> ::= "."
<variable> ::= [a-zA-Z]
<longParameter> ::= <variable>
<expression> ::= <variable> <spaces> <expression> |
		    <variable> <spaces> |
		    "(" <expression> ")" |
		    "(" <expression> ")" <expression>
<shortPreBuild> ::= <lambdaSymbol> <shortParameter> <dot> <expression> <shortPreBuild> |
		 	<lambdaSymbol> <shortParameter> <dot> <shortPreBuild> |
		  	<lambdaSymbol> <shortParameter> <dot> <expression> |
			"(" <shortPreBuild> ")" <shortPreBuild> |
			"(" <shortPreBuild> ")" |
			"(" <lambdaSymbol> <shortParameter> <dot> <expression> ")" <expression> |
			"(" <lambdaSymbol> <shortParameter> <dot> <expression> ")" <expression> <shortPreBuild>
<shortParameter> ::= <variable> <spaces> <shortParameter> |
			 "(" <shortParameter> ")" |
			 <variable> <spaces>
<spaces> ::= " " <spaces> | ""



p.s. there might be a slight different naming for the BNF created and the parser
in LambdaParser.hs. This is because some of them is compiled as 1. (For example, the
parser "(" <longPreBuild> ")" is compiled as recursiveBracket as these 3 are combined
together)


Exercise 2 & 3:

longPreBuild and shortPreBuild:
This method contains all the BNF translation of lambda calculus to the code. The
do command just wrapped a set of code that needs to be executed together. This method
is an equivalent to longLambdaP and shortLambdaP just with different type, where 
longLambdaP and shortLambdaP is a Parser Lambda and longPreBuild and shortPreBuild
are Parser Builder.



PART 2 --------------------------------------------------------------------------

Exercise 1:
The parser are created based on the order of precedece in which () -> not -> and 
-> or -> if.

Hence, the parser for if will be defined first and slowly goes to the bottom where
the base case will only be true or false.

Exercise 2:
Each ____block just indicates that ____ has a higher priority than the __P. In This
case, it will keep on going  until it reaches the base case which is the number.
Similar to exercise 1, parenthesisBlock handles addMinBlock again since there might
be similar computation inside the parenthesis although they have the highest 
precedence.

Exercise 3:

{- LOGIC GENERATED FROM EQ & LEQ
== -> EQ
!= -> not EQ
<= -> LEQ
< -> LEQ and (not EQ)
> -> not LEQ
>= -> (not LEQ) or EQ
-}

The reason why the code for complexCalcP and logicP are the same is because in the
parser on some bracketBlock (that is previously used by logicP), has another block 
being added, which is equalitiesBlock. 

This happens because inequalities are put in between the parser of logicP and arithmeticP. 
Since the maths needed to be computed first, arithmeticP will have the highest precedence,
hence exercise 2 does not have any modification. But inequalities happen before logical 
operation, so it will eventually be the same (since complexCalcP still needs the logicP
parser to do its computation but logicP has a lower precedence).


PART 3 --------------------------------------------------------------------------
Exercise 2:
Only partially done. Did factorial and try to parse string to numbers and a '!' symnol.