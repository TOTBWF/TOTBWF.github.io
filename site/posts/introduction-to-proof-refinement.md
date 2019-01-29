---
title: An Introduction to Proof Refinement and Tactic Languages with Refinery
author: Reed Mullanix
date: Jan 23, 2018
tags: [haskell]
---

# An Introduction to Proof Refinement and Tactic Languages with Refinery
One of the most interesting things about typed functional programing is the idea that a type can 
be seen as a proposition, and a program as a proof. When we view
programming through this lens, we see that type checking is proof verification, and type
inference is finding the most general proposition that a proof proves. 
But there is one interesting aspect of theorem proving that we are missing,
the holy grail of automated reasoning: proof synthesis. Before diving
into this, we should first take a bit of a diversion into Proof Theory.

# The Structure of Proofs
Every proof starts with a theorem, and every theorem presents a goal, 
namely, to provide some sort of evidence that the theorem is actually true.
From there, we can use the rules available to us to either directly prove the
goal, or to break down the goal into further subgoals. A proof is
complete when there are no more subgoals left to solve. 
From this perspective, we can see that a proof forms a tree. As an example, lets do a very
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

![](/tikz/proof-tree.png){.centered}

Putting on our Curry-Howard glasses, this proof tree looks suspiciously like an abstract syntax tree...

![](/tikz/syntax-tree.png){.centered}

If only we had some way to programatically construct syntax trees...

## Proof Synthesis as Metaprogramming
As it turns out, just as we can create a programming language that creates programs, we can create a programming language that actually creates proofs! 
This sort of language is what as known as a *Tactic Language*, and is a key component of most proof assistants.

Let's sketch out a little toy language for simply typed lambda calculus with products.
```haskell
type Var = String

data Term
    = Hole -- We will get to this later...
    | Var Var
    | Lam Var Term
    | Pair Term Term
    deriving (Show) data Type
    = TVar Var
    | TArrow Type Type
    | TPair Type Type
    deriving (Show, Eq)
```

Now, let's design a tactic language! We will be using
[refinery](https://github.com/TOTBWF/refinery) to handle the nitty gritty bits,
so we can focus on playing with the tactics side of things.

The first thing we need to be able to do handle assumptions. To do this,
we shall define a new type `Judgement`, which carries a list of variables bound
to types, along with the goal we are trying to prove.
```haskell
data Judgement = Judgement [(Var, Type)] Type
  deriving (Show)
```

Now, on to the core type of `refinery`, `TacticT goal extract m a`.
