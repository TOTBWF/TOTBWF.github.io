---
title: Tactic Metaprogramming in Haskell
author: Reed Mullanix
date: Sep 18, 2018
tags: [haskell]
description: A quick demonstration of tactics in Haskell
---

# Tactical Haskell
Let's be entirely honest, metaprogramming is a really cool concept. Something
about the idea of writing programs to create more programs just really tickles
my fancy. The issues start to come in when you move from the idea stage to
actually doing it. Template Haskell, though useful at times, seems to run
counter to what exactly makes Haskell so elegant. There has to be a different way of doing things...

Here's a little hint of what is to come:

    let tac = do
      intro
      x <- intro
      y <- intro
      intro <..> [use x, use y]
    in $(refine tac [t| forall a b. a -> b -> (a,b) ])

## A Different Way of Doing Things
Many proof assistants have something called a _Tactic Language_, which is
essentially a language of commands that describe how to create a term. 
Let's see if we can't make something like this in Haskell. We will be using the [Refinery](https://github.com/TOTBWF/refinery) library, which provides a generic framework for doing refinement proofs.

First things first, we are going to need to use Template Haskell to actually generate/splice the results of our tactics, as well as some bits from `Refinery`:

    import Language.Haskell.TH
    import Refinery.MetaSubst
    import Refinery.Proof
    import Refinery.Telescope (Telescope(..), (@>))
    import qualified Refinery.Telescope as Tl
    import Refinery.Tactic (tactic, subgoal, solve, many, <..>)
    import qualified Refinery.Tactic as T
    
Now for the 1st bit of our implementation. We are going to need some

<!-- Now, I know I said Template Haskell can be a bit clunky and inelegant at times, but it turns out that  -->
