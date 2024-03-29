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


Context after T-fun:
let G = x:Bool


G(x) = Bool
---------(T-var)            ------------(T-True)      -----------(T-False)
G|- x:Bool                   G|-True:Bool              G|-False:Bool
-----------------------------------------------------------------(T-if)
G |- if x then True else False : Bool
---------------------------------------------------------------------------(T-fun)
|- \x:Bool. if x then True else False  : Bool -> Bool



(b)  [4 Points] Show that this program: 

\z:Bool + Bool. (case z of (Left x -> True) (Right y -> y)) 

is well-typed by building a proof using the rules from the file above.
Proof:
 


Context after T-fun:
Let G = z:Bool + Bool


G(z) = Bool+Bool                                    [G,y:t2](y) = t2
------------(T-var)   -----------------(T-True)    ------------(T-var)
G |- z:Bool+Bool       G,x:t1 |- True:Bool          G,y:t2 |- y:t2
----------------------------------------------------------------------(T-case)
            G |- case z of (Left x -> True) (Right y -> y) : Bool + t2 
---------------------------------------------------------------------------------------------(T-fun)
|- \z:Bool + Bool. (case z of (Left x -> True) (Right y -> y)) : Bool + t2




(c) [2 Points] Explain (in general) what a "stuck expression" is:

A "stuck expression" is an expression e where e is not a value and there
does not exist an e' such that e->e'.

  For us, this generally means that the expression has not completed its computation
  because it does not have a value to return and it cannot complete its computation
  because we have no more rules for being able to reduce it any further. This is generally
  when something somewhere was poorly typed or the language is not complete to the extent
  that this particular expression can be evaluated.



(d) [2 Points] Give an example of a stuck expression that involves
either a "Left" "Right" or "Case" expression in some way:


  case True of (Left True -> True) (Right True -> True)



(e) [2 Points] Explain what the canonical forms lemma should say about
a sum type t1 + t2:

  The canonical forms lemma says that knowing the type lets you know
  something about the value. A sum type would then indicate that if
  you know something belongs to the sum type, what you can deduce about
  the value belongs to the sum of things you could know about values
  belong to each type individually.

  For example, if we know that the type of something is Bool + Int,
  then we know the value must either be True, False, or a numerical
  value. This comes from the canonical forms lemma being applied to
  each type individually.


(f) [10 Points] The preservation lemma says: 

If . |- e : t and e --> e' then . |- e' : t 

The proof is by induction on the derivation of e --> e'.  Below,
prove the cases corresponding to the rules (E-case1) and (E-case2).
In your proof, you may assume that standard Exchange, Weakening,
Canonical Forms, and Substitution lemmas have already been proven.  In
other words, you may use any of those lemmas in your proof without
proving them yourself.


Proof by induction on the derivation of e --> e'

case for rule (E-case1):

(p1) e --> e'
------------------------------------------------------- (E-case1)
case e of (Left x -> e2) (Right y -> e3) -->
case e' of (Left x -> e2) (Right y -> e3)
 
(1) |- case e of (Left x -> e2) (Right y -> e3) : t      (assumed)
(2) |- e : t1 + t2                                       (by 1 and inversion of T-case typing rule)
(3) |- e2 : t                                            (by 1 and inversion of T-case typing rule)
(4) |- e3 : t                                            (by 1 and inversion of T-case typing rule)
(5) |- e' : t1 + t2                                      (by 1, 2, and IH)
(6) |- case e' of (Left x -> e2) (Right y -> e3) : t     (by 3,4,5)

Preservation holds because still type t after taking a step e->e'

<end case>



case for rule (E-case2):
                  
----------------------------------------------------------- (E-case2)
case (Left v) of (Left x -> e2) (Right y -> e3) --> e2[v/x]

(1) |- case (Left v) of (Left x -> e2) (Right y -> e3) : t          (assumed)
(2) |- v : t1+t2                                                    (by 1 and inversion of T-case typing rule)
(3) |- e2 : t                                                       (by 1 and inversion of T-case typing rule)
(4) |- x : t1                                                       (by 1 and inversion of T-case typing rule)
(5) |- e3 : t                                                       (by 1 and inversion of T-case typing rule)
(6) |- case (Left v) of (Left x -> e2) (Right y -> e3) --> e2[v/x]  (by 1 and E-case2)
(7) |- e2[v/x] : t                                                  (by 2, substitution lemma, 1, 3)

Preservation holds because we took a step from
  case (Left v) of (Left x -> e2) (Right y -> e3) --> e2[v/x]
to
  e2[v/x]
and maintained type t

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


Proof by induction on the derivation of |- e:t

case for rule (T-Left):

G |- e : t1
---------------------- (T-Left)
G |- Left e : t1 + t2 

By IH and that e : t1
(a) e is either a value, or
(b) e -> e'

subcase (a)
e is value v -->
Left v is a value (as stated by the language)

subcase (b)
|- e -> e'
----------(E-left, IH)
|- Left e -> Left e'

Left e has two cases. e is either a value and Left e is a value,
or e->e' so Left e->Left e' by T-Left. Progress lemma holds.

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

Progress

Explanation:

We would need a rule for E-case4 because currently
  case (Middle v) (Left x -> e2) (Right y -> e3)     [small example program]
type checks but cannot take a step.


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
> eg2 = do
>   x <- many [3,4,5]
>   y <- return 1
>   return (x-y)

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

Correctly does Add (Num 5) (Num 10) = 10

> eval :: Exp -> [Int]
> eval (Add e1 e2) = do
>   n1 <- eval e1
>   n2 <- eval e2
>   return (n1 + n2)

Correctly does 3 + [] = []

> eval (Neither) = []

Correctly does both(2,3)+7=[9,10]
Correctly does both(2,3)+both(4,5)=[6,7,7,8]
Correctly does both(4,5)+neither=[]

> eval (Both e1 e2) = do
>   n1 <- eval e1
>   n2 <- eval e2
>   (return n1) ++ (return n2)

Correctly returns the value from the Num

> eval (Num e1) = return e1

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

(I) return x >>= (\a -> return y >>= (\b -> return (a+b)))  
=     
(II) return y >>= (\a -> return x >>= (\b -> return (a+b)))  


Do notation:
(I)
do
a <- return y
b <- return x
return (a+b)
=
(II)
do
a <- return x
b <- return y
return (a+b)

Proof:
(1) return x >>= (\a -> return y >>= (\b -> return (a+b)))
(2) = (return x >>= return) >>= (\b -> return (a+b))          (by unfolding L3)
(3) = (return x) >>= (\b -> return (a+b))                     (by L1)
(4) = return x                                                (by L2)
...ran out of time


(1) return x >>= (\a -> return y >>= (\b -> return (a+b)))
(2) = (\a -> return y >>= (\b -> return (a+b))) x             (by L1)
(3) = (\a -> ((\b -> return (a+b)) y)) x                      (by L1)
(4) = 
...ran out of time

Simple explanation of proof...
Referring to do notation:
a+b=b+a (commutativity)
In (I), a <- return y
which is the same as b in the (II), b <- return y
and the same is true for b <- return x (I) and a <- return x (II)
thus we are essentially asking
a+b=b+a
which is true by commutativity

