-----------------------
COS 441 Take-home Final
-----------------------

STUDENT NAME: Michael Bailey
STUDENT ID: mabailey
TIME THIS EXAM WAS DOWNLOADED: 9:00PM

Read the instructions below carefully.

You have 3 hours and 20 minutes from when you download this file to
email it back completed to BOTH dpw@cs.princeton.edu and
chris@monsan.to Put [COS 441 FINAL] in the subject of the email.  In
principal, the exam is 3 hours long, but you need some time to read
these instructions; you may need a restroom break and you may need a
minute or two after the exam is done to collect yourself and
officially press "send" on the email.  The extra 20 minutes is a grace
period in addition to the official 3 hours for the exam itself.  You
may use this grace period however you choose.

The work on the final must be your own work.  You must not communicate
with anyone else about the material on the final until Jan 19 at noon,
when all students have finished taking the exam.  If you have a
question about the exam material, make a reasonable assumption and
write down the assumption.  Then proceed to answer the question to the
best of your ability.  Partial solutions will be granted partial
credit.

You may refer to or use any of your notes from class, any material on
the course web site, the course textbooks and any code that you wrote
for your assignments.  You may refer to any documentation about
Haskell from the Haskell web site or from Hoogle.  In order to do
question 2 of the exam, you will also need the contents of this file:

http://www.cs.princeton.edu/~dpw/cos441-11/notes/exam-semantics.txt

You should download that file now.

This file must type check and compile when you hand it in.  Do not
submit solutions that do not compile and type check or you will lose
many points.  When writing code, feel free to add debugging code to
help you test your solution (that is a good idea!).  Be sure, however,
to make your solution clear using comments or otherwise.  Do not
submit multiple solutions to the same problem, hoping that one of them
is correct (such a strategy will not result in more points than
submitting one wrong solution).

There are 4 questions (each question has several subparts listed
(a), (b), (c), etc.):

1:  8  Points
2:  4  Points
3:  34 Points
4:  17 Points

   + 2 Points for writing out and signing the honour code below

Total:  65 Points

===================================


Here is the Princeton Honor Code:

I pledge my honor that I have not violated the Honor Code during this 
examination.

Please copy the Honor Code here:

I pledge my honor that I have not violated the Honor Code during this 
examination.


Type your name here to indicate you will abide by the honor code:

Michael Bailey


===================================

1.  [Total:  8 Points] 
In the following, use standard Haskell functions map, foldr, foldl and
++ to write equivalent functions that do not use explicit recursion.
You may add additional anonymous or auxiliary helper functions, but
none of the functions you write may be recursive (or mutually
recursive with eachother).  You may use library other than functions
map, foldr, foldl, or ++, provided those other functions are not
recursive.  For example, you may use functions fst and snd but you may
not use reverse (unless you define it yourself using map and fold).
You will receive partial credit if your program is correct on some
inputs but not others.  You will receive no credit if your program
does not type check and compile.

(a) [4 Points]

> thresholdR :: Int -> [Int] -> [Int]
> thresholdR n [] = []
> thresholdR n (x:xs) = 
>   if x > n then x : thresholdR n xs 
>            else 0 : thresholdR n xs

Write an equivalent function called "threshold" that is not recursive
itself but may call map, foldr, or foldl here:

> threshold :: Int -> [Int] -> [Int]
> threshold n xs = map (threshold_helper n) xs

> threshold_helper :: Int -> Int -> Int
> threshold_helper n x =
>   if x > n then x
>            else 0
                                                               
(b) [4 Points]

> prefixAddR :: [Int] -> [Int]
> prefixAddR xs = aux 0 xs
>   where
>     aux n [] = []
>     aux n (x:xs) = (n+x) : aux (n+x) xs

Write an equivalent function called "prefixAdd" that is not recursive
itself but does call map, foldr, or foldl here:

> prefixAdd :: [Int] -> [Int]
> prefixAdd xs = foldl (\acc x -> acc ++ [x+(addUntil x xs)]) [] xs

> sum :: [Int] -> Int
> sum xs = foldr (+) 0 xs

> antiThreshold n xs = map (\x -> if x < n then x else 0) xs

> addUntil n xs = foldr (+) 0 (antiThreshold n xs)

====================================

2.  [Total:  4 Points]  Is this a valid partial Hoare triple?

{true} 
x = 1; 
while (x > 0) { x = x + 1 }
{ x < -10 }

Yes or No:

No

If yes, write the loop invariant you would use to prove that it is valid:




====================================

3.  [Total:  34 Points] 
The syntax, operational semantics and typing rules for the lambda
calculus with booleans, functions and sum types is here:

http://www.cs.princeton.edu/~dpw/cos441-11/notes/exam-semantics.txt

(a)  [4 Points] Show that this program:

\x:Bool. if x then true else false 

is well-typed by building a proof using the typing rules from the file
above.  Proof (rewrite below):

         <...>
    ---------------------------------      --------------------------------------------
    <... build proof upwards here...>      <...your lines may be as wide as your like ...>
---------------------------------------------------------------------------------------------
|- \x:Bool. if x then True else False  : Bool -> Bool


(b)  [4 Points] Show that this program: 

\z:Bool + Bool. (case z of (Left x -> True) (Right y -> y)) 

is well-typed by building a proof using the rules from the file above.
Proof:
 
                  <...>
            -----------------------------     -----------------------------------------------
            <... build proof upwards here>    <your lines may be as wide as your like ...>
---------------------------------------------------------------------------------------------
|- \z:Bool + Bool. (case z of (Left x -> True) (Right y -> y)) : ???

(c) [2 Points] Explain (in general) what a "stuck expression" is:



(d) [2 Points] Give an example of a stuck expression that involves
either a "Left" "Right" or "Case" expression in some way:



(e) [2 Points] Explain what the canonical forms lemma should say about
a sum type t1 + t2:



(f) [10 Points] The preservation lemma says: 

If . |- e : t and e --> e' then . |- e' : t 

The proof is by induction on the derivation of e --> e'.  Below,
prove the cases corresponding to the rules (E-case1) and (E-case2).
In your proof, you may assume that standard Exchange, Weakening,
Canonical Forms, and Substitution lemmas have already been proven.  In
other words, you may use any of those lemmas in your proof without
proving them yourself.

case for rule (E-case1):

(p1) e --> e'
------------------------------------------------------- (E-case1)
case e of (Left x -> e2) (Right y -> e3) -->
case e' of (Left x -> e2) (Right y -> e3)
 
(1) |- case e of (Left x -> e2) (Right y -> e3) : t      (assumed)

...<fill in here>...
                  
                 
<end case>


case for rule (E-case2):
                  
----------------------------------------------------------- (E-case2)
case (Left v) of (Left x -> e2) (Right y -> e3) --> e2[v/x]

(1) |- case (Left v) of (Left x -> e2) (Right y -> e3) : t      (assumed)

...<fill in here>...


<end case>

(g) [5 Points] The progress lemma states: 

If |- e:t, then either: 
  (i) e is a value or  
  (ii) e -> e' 

The proof is by induction on the derivation of |- e:t.  In your proof,
you may assume that standard Exchange, Weakening, Canonical Forms,
Substitution lemmas have already been proven.  In other words, you may
use any of those lemmas in your proof without proving them
yourself. Prove the case that corresponds to the typing rule (T-Left).

case for rule (T-Left):

G |- e : t1
---------------------- (T-Left)
G |- Left e : t1 + t2 

...<fill in here>...

<end case>

(h) [5 Points]  Suppose we were to extend the language defined in this file:

http://www.cs.princeton.edu/~dpw/cos441-11/notes/exam-semantics.txt

with a new kind of expression "Middle e".  Suppose also that we added a new
typing rule for middle expressions:

G |- e : t2
------------------------------
G |- Middle e : t1 + (t2 + t3)

Suppose that "Middle v" counts as a value (ie:  Middle e is a value 
if e is a value v) and suppose that we add the following operational
rule for middle values:  

e --> e'
-----------------------
Middle e --> Middle e'

In this case, at least one of Progress or Preservation will not be true for
the new language.  Take your pick of either the Progress or the Preservation
theorem and explain why it is not true for the language containing Middle
expressions.  Your explanation should include a (small) concrete example program
that helps you demonstrate precisely why the theorem you chose does not hold.

Which one did you pick (Write Progress or Preservation here)?:   



Explanation:



===================================

4.  [Total:  17 Points]  
Consider the following BNF definition for "multi-expressions"
where n is any integer (0, 1, 2, 3,...,-1, -2, -3,...):

e ::= n | both(e1,e2) | neither | e1 + e2 

Multi-expressions evaluate in such a way that they have 0, 1, 2, ... or
many answers.  In other words, one multi-expression evaluates to a list
of values.  Here is an explanation of how evaluation should work:

-- integer n evaluates to the list containing just the integer [n]

-- if e1 evaluates to the list of values vs1 and e2 evaluates to the list 
of values vs2 then both(e1,e2) evaluates to the concatenation of lists vs1  
and vs2

-- neither evaluates to the empty list of values

-- if e1 evaluates to the list of values vs1 and e2 evaluates to the list 
of values vs2 then e1 + e2 evaluates to the list of values vs3 constructed
by adding each element of the list in vs1 to each element of the list in
vs2.  

NOTE:  You will not be judged on having the elements of the resulting
list in any particular order.  You just need to have the correct elements
(and the correct number of such elements).

For example:

both(2,3) + 7 = [9,10]
both(2,3) + both(4,5) = [6,7,7,8]
3 + neither = []
both(4,5) + neither = []

Here is a data type for representing multi-expressions:

> data Exp = Num Int | Both Exp Exp | Neither | Add Exp Exp

Here is a monad that will help you process lists of results:

> newtype Result a = R [a] deriving (Eq,Show)
>
> instance Monad Result where
>   return x = R [x]
>   (R xs) >>= f = R (squash f xs)
>
> squash :: (a -> Result b) -> [a] -> [b]
> squash f [] = []
> squash f (x:xs) = 
>   let R ys = f x in
>   ys ++ squash f xs
>
> many :: [a] -> Result a
> many xs = R xs

An example using bind and return explicitly:
(run this to find out what it does!)

> eg1 :: Result Int
> eg1 = many [3,4,5] >>= (\x -> return 1 >>= \y -> return (x-y))

(a) [2 Points] 
Write an equivalent computation to eg1 above but use do notation instead 
of using >>= explicitly.

> eg2 :: Result Int
> eg2 = error "fill me in using do notation"

(b) [10 Points]
Write a function eval that evaluates expressions and returns a list of
integers.  For full credit, use the monad defined above effectively
in your solution.  If you don't understand how the monad works, 
you may write the evaluator any way you choose and receive partial
credit for this question.  You may write any auxiliary functions or
definitions you choose.  

If you cannot figure out how to create an eval function that has the
type Exp -> [Int] then for partial credit, create your own alternative
eval function (perhaps call it "myeval") and give it a type that you
can make work. It is better to solve part of this problem but have
your code type check and compile than to try to solve it all but have
your code not compile.  Use comments to explain your code where
necessary.  (In particular, use comments if you solve part of the
problem to explain what you have and have not attempted to do.)
Your solution may judged on style and elegance as well as correctness.

> eval :: Exp -> [Int]
> eval e = error "implement me"

(c) [5 Points]  Consider any monad.

Assume that addition is associative and commutative. In other words,
assume these laws are true:

(Associativity) (x + y) + z = x + (y + z)
(Commutativity) x + y = y + x

Also, assume that the monad laws hold.  Here are the laws:

(L1)  return a >>= f  = f a
(L2)  m >>= (\x -> return x) = m
(L3)  (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)

Now, using the usual 2-column style from class, prove that the
following equation concerning monads is true for any integers x and y.
Justify each step of your proof using one of the laws above or by
substitution of equals for equals or calculation as we have done in
class.

return x >>= (\a -> return y >>= (\b -> return (a+b)))  
=     
return y >>= (\a -> return x >>= (\b -> return (a+b)))  

Proof:



