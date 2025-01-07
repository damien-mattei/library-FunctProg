# library-FunctProg
Functional Programming library

some programs included here are:

start-λογικι-guile.scm a symbolic calculus program for logical expressions (Scheme Guile version)

start-λογικι.scm a symbolic calculus program for logical expressions

start-goodstein-recursive.scm : a symbolic calculus program for goodstein sequences

Scheme+ versions:
start-λογικι-guile+.scm
start-λογικι-racket+.scm

some examples:


Logical computation of the minimal disjonctive normal form of a logical expression:

```scheme

(infix-symb-min-dnf '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )

((¬a ∧ b ∧ d) ∨ (¬b ∧ ¬c) ∨ (c ∧ ¬d))


(cnf-infix-symb '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}})

((¬a ∨ ¬b ∨ c) ∧ (¬a ∨ ¬b ∨ ¬d) ∧ (¬a ∨ ¬c ∨ ¬d) ∧ (b ∨ ¬c ∨ ¬d) ∧ (¬b ∨ c ∨ d))


(infix-symb-bool-min-dnf  '(and (<=> (and p q) r) (<=> (not (and p q)) r)))

□


```



Goodstein sequences:


```scheme

;; goodstein-init-atomic-rec2 : start-up function
;;
;;    - set a few variables
;;    - define Goodstein function recursively
;;    - compute the start number in hereditary base
;;    - call the Goodstein recursive function with the start number as argument
;;        
;;        the Goodstein recursive function do:
;;            - check if we have reached zero 
;;            - display polynomial at each step
;;            - bump the base
;;            - decrement polynomial by calling symbolic-polynomial-1 function (also called h)
;;            - call (recursively) goodstein-rec2

;; >  (goodstein-init-atomic-rec2 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)
;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)
;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))
;; G(266)(5)=((6 ^ (6 ^ (6 + 1))) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; P(266)(5)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 5)
;; G(266)(6)=((7 ^ (7 ^ (7 + 1))) + (5 * (7 ^ 7)) + (5 * (7 ^ 5)) + (5 * (7 ^ 4)) + (5 * (7 ^ 3)) + (5 * (7 ^ 2)) + (5 * 7) + 4)
;; P(266)(6)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 4)
;; G(266)(7)=((8 ^ (8 ^ (8 + 1))) + (5 * (8 ^ 8)) + (5 * (8 ^ 5)) + (5 * (8 ^ 4)) + (5 * (8 ^ 3)) + (5 * (8 ^ 2)) + (5 * 8) + 3)
;; P(266)(7)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 3)
;; G(266)(8)=((9 ^ (9 ^ (9 + 1))) + (5 * (9 ^ 9)) + (5 * (9 ^ 5)) + (5 * (9 ^ 4)) + (5 * (9 ^ 3)) + (5 * (9 ^ 2)) + (5 * 9) + 2)
;; P(266)(8)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 2)
;; G(266)(9)=((10 ^ (10 ^ (10 + 1))) + (5 * (10 ^ 10)) + (5 * (10 ^ 5)) + (5 * (10 ^ 4)) + (5 * (10 ^ 3)) + (5 * (10 ^ 2)) + (5 * 10) + 1)
;; P(266)(9)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 1)
;; G(266)(10)=((11 ^ (11 ^ (11 + 1))) + (5 * (11 ^ 11)) + (5 * (11 ^ 5)) + (5 * (11 ^ 4)) + (5 * (11 ^ 3)) + (5 * (11 ^ 2)) + (5 * 11))
;; P(266)(10)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω))
;; G(266)(11)=((12 ^ (12 ^ (12 + 1))) + (5 * (12 ^ 12)) + (5 * (12 ^ 5)) + (5 * (12 ^ 4)) + (5 * (12 ^ 3)) + (5 * (12 ^ 2)) + (4 * 12) + 11)
;; P(266)(11)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 11)
;; G(266)(12)=((13 ^ (13 ^ (13 + 1))) + (5 * (13 ^ 13)) + (5 * (13 ^ 5)) + (5 * (13 ^ 4)) + (5 * (13 ^ 3)) + (5 * (13 ^ 2)) + (4 * 13) + 10)
;; P(266)(12)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 10)
;; G(266)(13)=((14 ^ (14 ^ (14 + 1))) + (5 * (14 ^ 14)) + (5 * (14 ^ 5)) + (5 * (14 ^ 4)) + (5 * (14 ^ 3)) + (5 * (14 ^ 2)) + (4 * 14) + 9)
;; P(266)(13)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 9)
;; G(266)(14)=((15 ^ (15 ^ (15 + 1))) + (5 * (15 ^ 15)) + (5 * (15 ^ 5)) + (5 * (15 ^ 4)) + (5 * (15 ^ 3)) + (5 * (15 ^ 2)) + (4 * 15) + 8)
;; P(266)(14)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 8)
;; G(266)(15)=((16 ^ (16 ^ (16 + 1))) + (5 * (16 ^ 16)) + (5 * (16 ^ 5)) + (5 * (16 ^ 4)) + (5 * (16 ^ 3)) + (5 * (16 ^ 2)) + (4 * 16) + 7)
;; P(266)(15)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 7)
;; G(266)(16)=((17 ^ (17 ^ (17 + 1))) + (5 * (17 ^ 17)) + (5 * (17 ^ 5)) + (5 * (17 ^ 4)) + (5 * (17 ^ 3)) + (5 * (17 ^ 2)) + (4 * 17) + 6)
;; P(266)(16)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 6)
;; G(266)(17)=((18 ^ (18 ^ (18 + 1))) + (5 * (18 ^ 18)) + (5 * (18 ^ 5)) + (5 * (18 ^ 4)) + (5 * (18 ^ 3)) + (5 * (18 ^ 2)) + (4 * 18) + 5)
;; P(266)(17)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 5)
;;

```

A math-logic problem:



I'm calculating a **logical expression** defined by a **recurrence relation**.  
  
I'm calculating boolean values $X_{k+1}$, where $X_{k+1}$ could be $True$ (noted $T$) or $False$ (noted $F$) but as i do the calculus symbolically you will almost never see $T$ or $F$ in the calculus result unless in the first steps of computations (for $k+1<5$). 
 
In the calculus $X_{k+1}$ depends of $X_k$ and of other independent boolean variables named $Y_k$ and $Y_{k-1}$. "Independent" meaning there is no existing or hidden relationship between $Y_k$ and $Y_{k-1}$, it is independent as tossing 2 different pieces of money and considering $T$ or $F$ as heads and tails.

The **recurrence relation** is :  

$X_{k+1}=Y_k \cdot Y_{k-1} \oplus X_k \cdot (Y_k \oplus Y_{k-1})$  

with $\cdot$ noting the logical *and* and $\oplus$ noting the logical *exclusive or*.  

the previous expression can be set in **minimal disjunctive normal form** ([computer calculated by my program][1]):

$X_{k+1}=(Y_k \land Y_{k-1}) \lor (Y_k \land X_k) \lor (Y_{k-1} \land X_k)$

The system is calculated with these **initial conditions**:  
$X_0=T$  
$Y_{-1}=F$  
$Y_0=T$

Then the computation give those results (manually computed for the first steps, computer calculated after):  
$X_1 = T$  
$X_2 = T$  
$X_3 = Y_1 \lor Y_2$  
$X_4 = Y_2 \lor (Y_1 \land Y_3)$  


Note that for a given $X_{n+1}$ the $Y_n$ are with $n \in ]-1,n-1[$ but concretely $n \in ]1,n-1[$ because the values of $n=-1$ or $n=0$ only occurring in the first steps of iteration,but not after as $X_1$ and $X_2$ are simply **tautologies** that do not depends anymore of variables when calculus is done.

$X_5 = (Y_1 \land Y_3) \lor (Y_2 \land Y_3) \lor (Y_2 \land Y_4) \lor (Y_3 \land Y_4)$  
$X_6 = (Y_1 \land Y_3 \land Y_5) \lor (Y_2 \land Y_3 \land Y_5) \lor (Y_2 \land Y_4) \lor (Y_3 \land Y_4) \lor (Y_4 \land Y_5)$  
$X_7 = (Y_1 \land Y_3 \land Y_5) \lor (Y_2 \land Y_3 \land Y_5) \lor (Y_2 \land Y_4 \land Y_6) \lor (Y_3 \land Y_4 \land Y_6) \lor (Y_4 \land Y_5) \lor (Y_5 \land Y_6)$  
$X_8 = (Y_1 \land Y_3 \land Y_5 \land Y_7) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7) \lor (Y_2 \land Y_4 \land Y_6) \lor (Y_3 \land Y_4 \land Y_6) \lor (Y_4 \land Y_5 \land Y_7) \lor (Y_5 \land Y_6) \lor (Y_6 \land Y_7)$  
$X_9 = (Y_1 \land Y_3 \land Y_5 \land Y_7) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8) \lor (Y_4 \land Y_5 \land Y_7) \lor (Y_5 \land Y_6 \land Y_8) \lor (Y_6 \land Y_7) \lor (Y_7 \land Y_8)$  
$X_{10} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9) \lor (Y_5 \land Y_6 \land Y_8) \lor (Y_6 \land Y_7 \land Y_9) \lor (Y_7 \land Y_8) \lor (Y_8 \land Y_9)$  
$X_{11} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_6 \land Y_7 \land Y_9) \lor (Y_7 \land Y_8 \land Y_{10}) \lor (Y_8 \land Y_9) \lor (Y_9 \land Y_{10})$  
$X_{12} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_6 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_7 \land Y_8 \land Y_{10}) \lor (Y_8 \land Y_9 \land Y_{11}) \lor (Y_9 \land Y_{10}) \lor (Y_{10} \land Y_{11})$  
$X_{13} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_6 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_7 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_8 \land Y_9 \land Y_{11}) \lor (Y_9 \land Y_{10} \land Y_{12}) \lor (Y_{10} \land Y_{11}) \lor (Y_{11} \land Y_{12})$  

Note: the above expressions have been verified with my Scheme program and the SymPy Python library to avoid any risk of error in the source code.(Mathematica software has also been used for some tests and verify a few calculus)

Note that the computer calculated calculus above use the [Quine–McCluskey algorithm][2] and the [Petrick's method][3] to generate *minimal disjunctive normal forms* and the time complexity of those algorithms are exponential relative to the number of variables.So the compute time grows fast and after $X_{13}$ it would be long (many hours) to compute on a personal computer.


But i find a way to pass from $X_k$ to $X_{k+1}$ without having to do the long computer calculus :  

Conjectured method to pass from  $X_n$ to $X_{n+1}$ : in the disjunctive expression of $X_n$ insert $Y_n$ in the conjunctive terms whose do not contains $Y_{n-1}$, and finally append a conjunctive term of expression $Y_{n-1} \land Y_n$ (in the global disjunctive expression).  

Examples:  

starting from $X_5$ (not working below 5)  

example 1 :  
n=5 , pass from $X_5 = (Y_1 \land Y_3) \lor (Y_2 \land Y_3) \lor (Y_2 \land Y_4) \lor (Y_3 \land Y_4)$ to $X_6$:  
 insert $Y_n = Y_5$ in the conjunctive terms whose do not contains $Y_{n-1} = Y_{5-1} = Y_4$
 we have then :  
$(Y_1 \land Y_3 \land Y_5) \lor (Y_2 \land Y_3 \land Y_5) \lor (Y_2 \land Y_4) \lor (Y_3 \land Y_4)$  
 now append a conjunctive term of expression $Y_{n-1} \land Y_n = Y_4 \land Y_5$ in the disjunctive expression, this give the result:  
 $X_6 = (Y_1 \land Y_3 \land Y_5) \lor (Y_2 \land Y_3 \land Y_5) \lor (Y_2 \land Y_4) \lor (Y_3 \land Y_4) \lor (Y_4 \land Y_5)$  
which is the same as the mathematically computed expression more above.  

example 2:  
n=12 , pass from $X_{12} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10}) \lor (Y_6 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_7 \land Y_8 \land Y_{10}) \lor (Y_8 \land Y_9 \land Y_{11}) \lor (Y_9 \land Y_{10}) \lor (Y_{10} \land Y_{11})$  
 to $X_{13}$:  
 insert $Y_n = Y_{12}$ in the conjunctive terms whose do not contains $Y_{n-1} = Y_{12-1} = Y_{11}$
 we have then :  
$(Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_6 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_7 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_8 \land Y_9 \land Y_{11}) \lor (Y_9 \land Y_{10} \land Y_{12}) \lor (Y_{10} \land Y_{11})$    
 now append a conjunctive term of expression $Y_{n-1} \land Y_n = Y_{11} \land Y_{12}$ in the disjunctive expression, this give the result:  
 $X_{13} = (Y_1 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_3 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_2 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_3 \land Y_4 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_4 \land Y_5 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_5 \land Y_6 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_6 \land Y_7 \land Y_9 \land Y_{11}) \lor (Y_7 \land Y_8 \land Y_{10} \land Y_{12}) \lor (Y_8 \land Y_9 \land Y_{11}) \lor (Y_9 \land Y_{10} \land Y_{12}) \lor (Y_{10} \land Y_{11}) \lor (Y_{11} \land Y_{12})$   
which is the same as the mathematically computed expression more above.  

As this worked from $X_5$ to $X_{12}$ i suppose it is not hasard and it will works for every $X_n$ with $n\geq5$.

How to prove what is conjectured above?



  [1]: https://github.com/damien-mattei/library-FunctProg/blob/master/racket/logiki%2B.rkt#L2697
  [2]: https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm
  [3]: https://en.wikipedia.org/wiki/Petrick%27s_method