---
title: Tactic Metaprogramming in Haskell
author: Reed Mullanix
date: Sep 18, 2018
tags: [haskell]
description: Adding tactics to haskell as a library
---

# Tactics in Haskell
One of the most interesting things about typed functional programing is the idea that a type can 
be seen as a proposition, and a program as a proof. If you haven't seen this before, stop reading 
this immediately, grab a nice hot bevarage, search "Curry Howard Isomorphism", and get ready to get 
sucked down a rabit hole that you probably will never come out of.

When viewing programing through this lens, we see that every program we write is indeed a proof of some
proposition. Type checking is proof verification, and type inference is finding the most general proposition
that some proof proves. But there is one interesting aspect of theorem proving that we are missing,
the holy grail of automated reasoning: proof synthesis. However, to solve this probleam, we need to take
a diversion into a bit of Proof Theory.

## The Structure of Proofs
Every proof starts with a theorem, and every theorem presents a goal, namely, to provide some sort of evidence that the theorem is actually true.
From there, we can use the rules available to us to either directly prove the goal, or to break down the goal into further subgoals. A proof is
complete when there are no more subgoals left to solve. From this perspective, we can see that a proof forms a tree. As an example, lets do a very
quick proof that $A \Rightarrow B \Rightarrow A \land B$.

Our initial goal is $A \Rightarrow B \Rightarrow A \land B$. To prove this, we
can assume that $A$ is true, and see if we can prove $B \Rightarrow A \land B$.
This gives us the subgoal.
$$A \vdash B \Rightarrow A \land B$$
As a quick notational note, you should read $H \vdash P$ as "Asssuming H is 
true, prove P". Continuing our proof, let us repeat the same step for $B$, 
giving us the subgoal:
$$A,B \vdash A \land B$$
To prove $A \land B$, we need to create 2 subgoals, one for $A$ and the other for $B$. Now our subgoals look like this:
$$A,B \vdash A$$
$$A,B \vdash B$$
In both of our subgoals, we have assumed the thing we are trying to prove is true, so the proof is complete! The full proof tree looks like this:

![](/tikz/proof-tree.png)

Putting on our magic Curry-Howard glasses, this proof tree looks suspiciously like an abstract syntax tree...

![](/tikz/syntax-tree.png)

If only we had some way to programatically construct syntax trees...

## Proof Synthesis as Metaprogramming
As it turns out, just as we can create a programming language that creates programs, we can create a programming language that actually creates proofs! 
This sort of language is what as known as a *Tactic Language*, and is a key component of most proof assistants.
<!-- The main difference between macro-based metaprograming and tactic metaprograming is that macros operate on ASTs, whereas -->
<!-- tactics operate on incomplete proofs. -->

Let's sketch out a toy tactic language for
simply typed lambda calculus with products, where the goals will be types along with a list of variables bound to types, and the proofs will be terms. 

$$
\begin{align*}
    t ::=&\: intro \\
        |&\: assumption \\
        |&\: id \\
        |&\: *t \\
        |&\: ?t \\
        |&\: t\: |\: t \\
        |&\: t; t \\
        |&\: [t_1, t_2, ..., t_n] \\
        |&\: \{t\}
\end{align*}
$$

<!-- The notation $(x:a) \vdash g \triangleright t$ should read as "Given that the variable $x$ has type $a$, prove the goal $g$, and when it is proved, return the term $t$". -->

<!-- With that in mind, the rules for the language are as follows: -->

<!-- - $\\{t\}$: Just acts as parentheses. -->
<!-- - $intro \: H \vdash a \rightarrow b$: Creates a subgoal $H,a \vdash b \triangleright t$, and returns a term $\lambda x.t$ -->
<!-- - $intro \: H \vdash a \times b$: Creates 2 subgoals  -->
<!-- $H \vdash a \triangleright l$ and -->
<!-- $H \vdash b \triangleright r$, and returns $(l,r)$. -->
<!-- - $assumption \: H,(x:a),H' \vdash a$: Creates no subgoals, and returns $x$. -->

<!-- If $intro$ is applied to any other type, or $\text{assumption}$ can't -->
<!-- find a varaible of the proper type, they will fail. -->

<!-- - $id \: H \vdash a$: Creates one subgoal $H \vdash a \triangleright t$,  -->
<!-- and returns $t$. At the moment, this seems super useless, but there is a very good reason for it (Try to guess!). -->
<!-- - $*\{t\}$: Runs a tactic repeatedly until it fails. For example:  -->
<!-- $*\{intro\} \: H \vdash a \rightarrow b \rightarrow c \rightarrow d$ -->
<!-- creates the subgoal $H,(x:a),(y:b),(z:c) \vdash d \triangleright h$ and  -->
<!-- returns $\lambda x. \lambda y. \lambda z. h$. -->

<!-- and will return a term $\lambda x.t$. -->

## Back to Haskell
I'm using the [tactic-haskell](https://github.com/totbwf/tactic-haskell) library here to handle the nitty gritty of proof refinement, so we
can focus on playing with the tactics side of things. I plan on writing a full explanation of the inner workings 

    tactic "t" [t| forall a b. a -> b -> (a,b) |] $ do
      forall
      intros ["x", "y"]
      split <..> [exact "x", exact "y"]


<!-- ```haskell -->
<!-- let t = do -->
<!--   many intro -->

<!-- $( -->
<!-- refine t [t| forall a b. a -> b -> (a,b)|] -->
<!-- ) -->

<!-- ``` -->

asdaasdfa
