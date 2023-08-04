{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char
import qualified Numeric                       as N

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
--
longLambdaP :: Parser Lambda
-- using functor to compute the dunction to the parer builder input resulting to a parser lambda
longLambdaP = build <$> longPreBuild

longPreBuild :: Parser Builder
longPreBuild = recursiveBracket ||| multipleLambdas ||| simpleLambda ||| onlyBracket ||| expressionMerged ||| bracketThenExpression ||| expressionThenLambda
    where
        recursiveBracket = do
            -- check whether the string parse contain an open bracket, and if there is it will start computing this.
            operator '('
            -- calling the function recursively to check an existing same pattern within the string
            ll <- longPreBuild
            operator ')'
            -- returning the result of ll which was binded before (in this case, ll is a builder type from the recursion)
            pure ll
        multipleLambdas = do
            lambdaSymbol
            lp <- longParameter
            dot
            -- lambda takes longprebbuild as its second input and build a lambda expression
            lam lp <$> longPreBuild
        simpleLambda = do
            lambdaSymbol
            lp <- longParameter
            dot
            -- all the expressions that are reduced to a builder from a list of builder using apply and later
            -- combined with lam to build an entire lambda expression
            lam lp . foldl1 ap <$> expression
        onlyBracket = do
            operator '('
            lambdaSymbol
            lp <- longParameter
            dot
            ex <- expression
            operator ')'
            -- apply the equation in the bracket with another set of lambda equation after the bracket
            -- applied using a functor to wrap with the parser type
            ap (lam lp $ foldl1 ap ex) <$> longPreBuild
        expressionMerged = do
            lambdaSymbol
            lp <- longParameter
            dot
            ex <- expression
            -- when lambda calculus happens recursively, it will apply the entire expression
            -- within a lambda calculus before it is applied with another lambda
            ap (lam lp $ foldl1 ap ex) <$> longPreBuild
        bracketThenExpression = do
            operator '('
            lambdaSymbol
            lp <- longParameter
            dot
            ex1 <- expression
            operator ')'
            -- all the expressions inside are applied first together then it will be applied
            -- to the lambda calculus inside the bracket with the use of apply
            ap (lam lp (foldl1 ap ex1)) . foldl1 ap <$> expression
        expressionThenLambda = do
            operator '('
            lambdaSymbol
            lp <- longParameter
            dot
            ex1 <- expression
            operator ')'
            ex2 <- expression
            -- all the expressions before of a lambda calculus that happens before another lambda calculus
            -- will be applied together first then applied next with the next lambda 
            ap (ap (lam lp (foldl1 ap ex1)) (foldl1 ap ex2)) <$> longPreBuild


lambdaSymbol :: Parser Char
-- ignore the starting and trailing spaces and find the lambda symbol
lambdaSymbol = operator 'λ'

dot :: Parser Char
-- ignore the starting and trailing spaces and find the dot
dot = operator '.'

variable :: Parser Char
-- ignore the starting and trailing spaces and find any alphabet
variable = satisfy isAlpha


longParameter :: Parser Char
-- variable acccepted by lambda for longLambdaP
longParameter = variable


expression :: Parser [Builder]
expression = variableAndExpression ||| onlyVariable ||| braket ||| bracketThenExpression
    where
        variableAndExpression = do
            v <- variable
            spaces
            e <- expression
            -- a cons list putting the first variable to a list and do this recursively
            pure $ term v : e
        onlyVariable = do
            v <- variable
            spaces
            -- put a single alphabet to a list
            pure [term v]
        braket = do
            operator '('
            e <- expression
            operator ')'
            -- when there is bracket, combine the variables in the bracket first by foldr
            pure [foldl1 ap e]
        bracketThenExpression = do
            operator '('
            e1 <- expression
            operator ')'
            e2 <- expression
            -- prioritizing the variables in the bracket by applying them first then putting
            -- it to the list with the rest of the expression outside the bracket
            pure $ foldl1 ap e1 : e2




-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> shortPreBuild


shortPreBuild :: Parser Builder
shortPreBuild = expressionThenLambda ||| lambdaThenLambda ||| simpleExpression ||| bracketRecursive ||| onlyBracket ||| expressionOutside ||| expressionInOutLambda
    where
        expressionThenLambda = do
            lambdaSymbol
            sp <- shortParameter
            dot
            ex <- expression
            -- apply all the expression after the lambda symbol first (including the recursive ones)
            -- making it a single builder and then will be applied to the lambda
            sp . ap (foldl1 ap ex) <$> shortPreBuild
        lambdaThenLambda = do
            lambdaSymbol
            sp <- shortParameter
            dot
            -- apply the lambda function (that has been renamed as sp) together with the rest of the lambda
            -- expression
            sp <$> shortPreBuild
        simpleExpression = do
            lambdaSymbol
            sp <- shortParameter
            dot
            -- reduce all the expression first using apply then lambda function will take the builder as 
            -- one of the input to get the result
            sp . foldl1 ap <$> expression
        bracketRecursive = do
            operator '('
            sl <- shortPreBuild
            operator ')'
            -- when a string can be reduced to a smaller lambda expressions, compute them as an expression
            -- then merge everything with apply
            ap sl <$> shortPreBuild
        onlyBracket = do
            operator '('
            sl <- shortPreBuild
            operator ')'
            -- simply return the builder as it is after detecting bracket
            pure sl
        expressionOutside = do
            operator '('
            lambdaSymbol
            sp <- shortParameter
            dot
            ex1 <- expression
            operator ')'
            -- since there is another expression outside the bracket, compute the builder within
            -- the bracket first using lambda then using apply to merge the 2 builders together
            -- foldl is used since expression still comes in a form of a list
            foldl ap (sp (foldl1 ap ex1)) <$> expression
        expressionInOutLambda = do
            operator '('
            lambdaSymbol
            sp <- shortParameter
            dot
            ex1 <- expression
            operator ')'
            ex2 <- expression
            -- similar as the one above, but in this case, it is applied together with
            -- a recursive shortPreBuild. since they are not a list, foldl is not needed.
            -- but inside, since there is an expression, it should be reduced first together
            -- with the builder using apply
            ap (ap (sp (foldl1 ap ex1)) (foldl1 ap ex2)) <$> shortPreBuild


-- returns a function that takes a builder as an input 
shortParameter :: Parser (Builder -> Builder)
shortParameter = multipleVariable ||| bracket ||| simpleVariable
    where
        multipleVariable = do
            v <- variable
            spaces
            sp <- shortParameter
            -- taking shortParameter as builder input so it still return a function
            pure $ lam v <$> sp
        bracket = do
            operator '('
            sp <- shortParameter
            operator ')'
            -- just return the shortParameter after seperating it from the bracket
            pure sp
        simpleVariable = do
            v <- variable
            spaces
            -- returning a function where lam has taken a char input but not a builder input
            pure $ lam v

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λxyz"
-- UnexpectedChar '\955'
--
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '\955'
--
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True


{-
CODE TAKEN FROM TUTORIAL ------------------------------------------------
-}
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-------------------------------------------------------------------------

logicP :: Parser Lambda
logicP = build <$> ifBlock

ifBlock :: Parser Builder
-- it will check for any ifs and compute and if not found, will go to the higher precedence
ifBlock = ifp ||| orBlock

orBlock :: Parser Builder
-- chaining all the 'or' and the value returned after computation with and (which is higher in precedence)
orBlock = chain andBlock orChainP

andBlock :: Parser Builder
-- chaining all the 'and' and the value returned after computation with not (which is higher in precedence)
andBlock = chain notBlock andChainP

notBlock :: Parser Builder
notBlock = do
    nc <- notChain
    -- case when there is multiple nots (e.g. not not False)
    nc <$> notBlock
    |||
    -- when there is not, it will just straight up goes to the higher precedence
    bracketBlock
    |||
    do
    nc <- notChain
    -- not is computated together with the result from bracket
    nc <$> bracketBlock

bracketBlock :: Parser Builder
bracketBlock =
    do
        operator '('
        bb <- bracketBlock
        operator ')'
        -- just return the value inside the bracket 
        pure bb
    |||
    -- returning the base case (True / False)
    result
    |||
    do
        operator '('
        i <- ifBlock
        operator ')'
        -- in case there is another operation, goes back to the top (if) and do like what is done previously
        pure i
    |||
    -- check for equalities (used in exercise 3)
    equalitiesBlock

result :: Parser Builder
result = true ||| false

ifp :: Parser Builder
ifp = do
    strings "if"
    v1 <- ifBlock
    strings "then"
    v2 <- ifBlock
    strings "else"
    -- after parsing is done, go back to the ifBlock to check any more if parsing
    ifChain v1 v2 <$> ifBlock



ifb :: Builder
ifb = lam 'b' $ lam 't' $ lam 'f' $ ap (ap (term 'b') (term 't')) (term 'f')

ifChain :: Builder -> Builder -> Builder -> Builder
-- create a curried function based on the builder created
ifChain b t f = ap (ap (ap ifb b) t) f


------------------------------------------------------------------------

-- translate parser result to other type
fromParserResult :: ParseResult a -> a
fromParserResult (Result _ v) = v
fromParserResult _ = error ""

-------------------------------------------------------------------------

true :: Parser Builder
-- parsing true when detect the string true (same with false)
true = strings "True" >> pure trueb

trueb :: Builder
-- using the function defined in Builder.hs to get the builder for true (and false)
trueb = boolToLam True

false :: Parser Builder
false = strings "False" >> pure falseb

falseb :: Builder
falseb = boolToLam False
----------------------------------------------------------------------------
andb :: Builder
andb = lam 'x' . lam 'y' $ ap (ap (ap ifb (term 'x')) (term 'y')) falseb

andChain :: Builder -> Builder -> Builder
-- from the lambda expressions, create a function that takes 2 builder input
-- can be eta reduced but won't be doing that to avoid confusion
andChain b1 b2 = ap (ap andb b1) b2

andChainP :: Parser (Builder -> Builder -> Builder)
-- creating parser type for the function so that it can be used for chain (after check for string "and")
andChainP = strings "and" >> pure andChain
------------------------------------------------------------------------------
orb :: Builder
orb = lam 'x' . lam 'y' $ ap (ap (ap ifb (term 'x')) trueb) (term 'y')

orChain :: Builder -> Builder -> Builder
orChain b1 b2 = ap (ap orb b1) b2

orChainP :: Parser (Builder -> Builder -> Builder)
orChainP = strings "or" >> pure orChain
---------------------------------------------------------------------------------
notb :: Builder
notb = lam 'x' $ ap (ap (ap ifb (term 'x')) falseb) trueb

notChain :: Parser (Builder -> Builder)
-- apply not to the result when a string "not" is detected
notChain = strings "not" >> pure (ap notb)

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
--
basicArithmeticP :: Parser Lambda
-- only chain addP or minusP for basic (which function has been defined and can be written
-- this way since they have the same order of precedence)
basicArithmeticP = build <$> chain number (addP ||| minusP)


{-
DERIVED FROM CHURCH ENCODING GIVEN ABOVE------------------------------------------------------------------------------------------------------
-}
succL :: Builder
succL = lam 'n' $ lam 'f' $ lam 'x' $ ap (term 'f') $ ap (ap (term 'n') (term 'f')) (term 'x')

predL :: Builder
predL = lam 'n' $ lam 'f' $ lam 'x' $ ap (ap (ap (term 'n') (lam 'g' $ lam 'h' $ ap (term 'h') $ ap (term 'g') (term 'f'))) (lam 'u' $ term 'x')) (lam 'u' $ term 'u')

addL :: Builder
addL = lam 'x' $ lam 'y' $ ap (ap (term 'y') succL) (term 'x')

minusL :: Builder
minusL = lam 'x' $ lam 'y' $ ap (ap (term 'y') predL) (term 'x')

multiplyL :: Builder
multiplyL = lam 'x' $ lam 'y' $ lam 'f' $ ap (term 'x') (ap (term 'y') (term 'f'))

expL :: Builder
expL = lam 'x' $ lam 'y' $ ap (term 'y') (term 'x')

--------------------------------------------------------------------------------------------------------------------------------------------

{-
PARSER TAKEN FROM WEEK11 PROVIDED CODE--------------------------------------------------------------------------------------------------------
-}
tok :: Parser a -> Parser a
tok parser= do
  x <- parser
  spaces
  pure x

charTok :: Char -> Parser Char
charTok = tok.is

stringTok :: String -> Parser String
stringTok = tok . string

operator :: Char -> Parser Char -- parse a single char operator
operator c = spaces >> charTok c >> pure c

isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _         = False

readFloats :: (RealFrac a) => String -> Maybe (a, String)
readFloats str = case N.readSigned N.readFloat str of
  ((a, s) : _) -> Just (a, s)
  _            -> Nothing

readHex :: (Num a, Eq a) => String -> Maybe (a, String)
readHex str = case N.readHex str of
  ((a, s) : _) -> Just (a, s)
  _            -> Nothing

readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v
    Nothing        -> Error $ UnexpectedChar (head x)

-- modified from op to accept string
strings :: String -> Parser String
strings c = spaces >> stringTok c >> pure c

--------------------------------------------------------------------------------------------------------------------------------------------

add :: Builder -> Builder -> Builder
-- builder function created based on the lambda calculus where add takes
-- 2 input and compute it
add x y = ap (ap addL x) y

addP :: Parser (Builder -> Builder -> Builder)
-- ignore spaces and find '+' which will return add function (same goes to the other operator)
addP = operator '+' >> pure add

minus :: Builder -> Builder -> Builder
minus x y = ap (ap minusL x) y

minusP :: Parser (Builder -> Builder -> Builder)
minusP = operator '-' >> pure minus

multiply :: Builder -> Builder -> Builder
multiply x y = ap (ap multiplyL x) y

multiplyP :: Parser (Builder -> Builder -> Builder)
multiplyP = operator '*' >> pure multiply

exponential :: Builder -> Builder -> Builder
exponential x y = ap (ap expL x) y

expP :: Parser (Builder -> Builder -> Builder)
-- instead of operator, uses char since it has more than 1 length. but parsing
-- still works the same way
expP = strings "**" >> pure exponential


number :: Parser Builder
-- ignore the spaces and if detect a string digitm change to an int and then 
-- use intToLam to return a builder 
number = spaces >> intToLam <$> int


-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
--
arithmeticP :: Parser Lambda
arithmeticP = build <$> addMinBlock

addMinBlock :: Parser Builder
-- chain the result received from multiplyBlock to addP or minusP (left associative
-- since both have the same level of precedence and the ones encountered first
-- will be performed first)
addMinBlock = chain multiplyBlock (addP ||| minusP)

multiplyBlock :: Parser Builder
-- similar to add/minus but has lower precedence than exponential
multiplyBlock = chain expBlock multiplyP

expBlock :: Parser Builder
expBlock = chain parenthesisBlock expP

parenthesisBlock :: Parser Builder
parenthesisBlock =
    do
        operator '('
        bb <- parenthesisBlock
        operator ')'
        pure bb
    |||
    -- when only integer is there
    number
    |||
    do
        operator '('
        am <- addMinBlock
        operator ')'
        -- going back to the lowest priority
        pure am


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just True
--
complexCalcP :: Parser Lambda
-- has the same function as logicP as the parser used there are modified to make it more general
complexCalcP = build <$> ifBlock


{-
DERIVED FROM EQUATION GIVEN ABOVE
-}

isZero :: Builder
isZero = lam 'n' $ ap (ap (term 'n') (lam 'm' falseb)) trueb

lessThanEqualL :: Builder
lessThanEqualL = lam 'm' $ lam 'n' $ ap isZero ( ap (ap minusL (term 'm')) (term 'n') )

equalL :: Builder
equalL = lam 'm' $ lam 'n' $ ap (ap andb (ap (ap lessThanEqualL (term 'm')) (term 'n'))) (ap (ap lessThanEqualL (term 'n')) (term 'm'))

--------------------------------------------------------------------------------------------------------------------------------------

equal :: Builder -> Builder -> Builder
equal x y = ap (ap equalL x) y

equalP :: Parser Builder
equalP = do
    com1 <- addMinBlock
    strings "=="
    -- parse the equal string and takes 2 builder input to compare it equalities
    equal com1 <$> addMinBlock

lessThanEqual :: Builder -> Builder -> Builder
lessThanEqual x y = ap (ap lessThanEqualL x) y

lessThanEqualP :: Parser Builder
lessThanEqualP = do
    com1 <- addMinBlock
    strings "<="
    -- compare whether the first addMinBlock is LEQ than the second addMinBlock
    lessThanEqual com1 <$> addMinBlock


notEqualP :: Parser Builder
notEqualP = do
    com1 <- addMinBlock
    strings "!="
    -- based on the readme.txt, != is equivalent to not == so it takes 2 builder 
    -- input and compares it before applying notb to the builder result
    ap notb . equal com1 <$> addMinBlock

greaterThanP :: Parser Builder
greaterThanP = do
    com1 <- addMinBlock
    strings ">"
    -- similar to != but for LEQ since the opposite of <= is >
    ap notb . lessThanEqual com1 <$> addMinBlock

lessThanP :: Parser Builder
lessThanP = do
    com1 <- addMinBlock
    strings "<"
    com2 <- addMinBlock
    -- < indicates that is is <= and not == so it computes both (<=) and (not ==) individually
    -- then apply both together
    pure $ ap (ap (lessThanEqual com1 com2) andb) $ ap notb (equal com1 com2)


greaterThanEqualP :: Parser Builder
greaterThanEqualP = do
    com1 <- addMinBlock
    strings ">="
    com2 <- addMinBlock
    -- similar logic to lessThan. (not <=) and (==) is applied seperately then
    -- is combined by applying or first to one of them then combined with the rest
    pure $ ap (ap (equal com1 com2) orb) $ ap notb (lessThanEqual com1 com2)


equalitiesBlock :: Parser Builder
equalitiesBlock =
    -- the list of equalities comparison (same order of precedence for all comparison)
    equalP
    |||
    lessThanEqualP
    |||
    notEqualP
    |||
    lessThanP
    |||
    greaterThanP
    |||
    greaterThanEqualP


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--


{-
CREATING BUILDER FROM THE CHURCH ENCODING GIVEN
-}

nullB :: Builder
nullB = lam 'c' $ lam 'n' $ term 'n'

isNullB :: Builder
isNullB = lam 'l' $ ap (ap (term 'l') (lam 'h' $ lam 't' falseb)) trueb

consB :: Builder
consB = lam 'h' $ lam 't' $ lam 'c' $ lam 'n' $ ap (ap (term 'c') (term 'h')) (ap (ap (term 't') (term 'c')) (term 'n'))

headB :: Builder
headB = lam 'l' $ ap (ap (term 'l') (lam 'h' $ lam 't' (term 'h'))) falseb

tailB :: Builder
tailB = lam 'l' $ lam 'c' $ lam 'n' $ ap (ap (ap (term 'l') (lam 'h' $ lam 't' $ lam 'g' $ ap (ap (term 'g') (term 'h')) (ap (term 't') (term 'c')))) (lam 't' (term 'n'))) (lam 'h' $ lam 't' (term 't'))

{-
COPIED FROM PARSER.HS IN TUTORIAL 11 ---------------------------------------------------------------
-}
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 a s = do
  heads <- a
  rests <- list (s >> a)
  return (heads : rests)

sepby :: Parser a -> Parser s -> Parser [a]
sepby a s = sepby1 a s ||| pure []

array :: Parser a -> Parser [a]
array a = do
  charTok '['
  lst <- sepby a (charTok ',')
  charTok ']'
  pure lst

---------------------------------------------------------------------------------------------------
-- | Testing listP 
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
--
-- >>> parse listP "[1, 2]"
-- Result >< (\htcn.ch(tcn))(\f.f)((\htcn.ch(tcn))(\fx.f(fx))\cn.n)
--
-- >>> parse listP "[True and False, 5]"
-- Result >< (\htcn.ch(tcn))((\xy.(\btf.btf)xy\_f.f)(\t_.t)(\_f.f))((\htcn.ch(tcn))(\fx.f(f(f(f(fx)))))\cn.n)
--
-- >>> parse listP "[5 < 3]"
-- Result >< (\htcn.ch(tcn))((\mn.(\a.a(\b_f.f)\t_.t)((\xy.y(\afb.a(\gh.h(gf))(\u.b)\u.u)x)mn))(\fx.f(f(f(f(fx)))))(\fx.f(f(fx)))(\xy.(\btf.btf)xy\_f.f)((\x.(\btf.btf)x(\_f.f)\t_.t)((\mn.(\xy.(\btf.btf)xy\_f.f)((\ab.(\c.c(\d_f.f)\t_.t)((\xy.y(\cfd.c(\gh.h(gf))(\u.d)\u.u)x)ab))mn)((\ab.(\c.c(\d_f.f)\t_.t)((\xy.y(\cfd.c(\gh.h(gf))(\u.d)\u.u)x)ab))nm))(\fx.f(f(f(f(fx)))))(\fx.f(f(fx))))))\cn.n
--
listP :: Parser Lambda
listP = build <$> newList

newList :: Parser Builder
newList = do
-- detect the square bracket symbol to indicate list
  charTok '['
  -- parse all the contents (including the comma)
  lst <- contents
  charTok ']'
  -- reduce by apply all the input to [] 
  pure $ foldr ap nullB lst

contents :: Parser [Builder]
-- uses sepby to seperate all the commas that are found and put them in the list. Put in the list
-- by apply with consB (since consB is the lambda builder for (:)) to either complexCalcP or
-- arithmeticP (can be a mix of these in a list)
contents = sepby ((ap consB <$> (lamToBuilder <$> (complexCalcP ||| arithmeticP))) ||| newList) (operator ',')


-- | Testing for listOpP
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head tail [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
--
listOpP :: Parser Lambda
listOpP = build <$> compiledListOp

compiledListOp :: Parser Builder 
compiledListOp = do
    ops <- isNullP ||| consP ||| headP ||| tailP
    -- apply these recursively in case multiple compiledList are called
    ap ops <$> compiledListOp ||| newList


-- similar implementation are used in which the existance of string that
-- links to the lambda expression are checked and if it is, the builder
-- of that will be returned which will be used later on for applying
isNullP :: Parser Builder 
isNullP = do
    strings "isNull"
    pure isNullB

consP :: Parser Builder 
consP = do
    strings "cons"
    pure consB

headP :: Parser Builder 
headP = do
    strings "head"
    pure headB

tailP :: Parser Builder 
tailP = do
    strings "tail"
    pure tailB

-- | Exercise 2

-- | Implement your function(s) of choice below!

-- CHOICE 1 -> FACTORIAL
{-
To implement factorial, some functions and lambda notations from the previous questions (e.g. 
multiplication and subtraction) will be used. 1 and 0 will be translated to lambda immediately
based on the rules of factorial where 1! and 0! = 1
-}

-- | Checking Factorial
-- >>> lamToInt <$> parse factorialP " 0! "
-- Result >< Just 1
--
factorialP :: Parser Lambda 
factorialP = build <$> factorialB


factorialB :: Parser Builder
factorialB = 
    do
        operator '1'
        operator '!'
        pure $ intToLam 1
    |||
    do
        operator '0'
        operator '!'
        pure $ intToLam 1

    


