---
title: Tactic Metaprogramming in Haskell
author: Reed Mullanix
date: Sep 18, 2018
tags: [haskell]
description: A quick demonstration of tactics in Haskell
---
# Type-Directed Metaprogramming in Haskell

When I'm writing Haskell, quite often I come across type signatures that are
just a mishmash of types. For example, take the definitely not made up function `foo`:

```haskell
foo :: a -> b -> c -> (b, d -> c)
```

My go-to strategy for filling out the implementation of a function like this is to just start
by plopping down a hole, and refining down the solution. However, this process feels
incredibly mechanical. Almost so mechanical that it could be programmed...

Let's try to give our mechanical process a type. First, we
know it's going to be a function that starts when given a type. After all, what
is a process but a function?
```haskell
type MechanicalProcess goal = goal -> ???
```
Next, we know that at some points, we are going to need to make some subgoals.
For example, consider trying to fill in a hole `_h :: (a, b)`. The expression
you create will look something like `(_h1 :: a, _h2 :: b)`, which obviously
yields 2 subgoals.
```haskell
type MechanicalProcess goal = goal -> ([goal], ???)
```
Now, what do we do with our solutions? Well, all we need is a way to map the solutions
of all of the subgoals into a single solution. This gives us the final type:
```haskell
type MechanicalProcess g solution = g -> ([g], [solution] -> solution)
```
Finally, `MechanicalProcess` is a _terrible_ name, so lets give it a better one!
Let's call it a `Tactic`, because this seems essentially like a "tactic" for
filling out implementations for types!
```haskell
newtype Tactic g s = Tactic { unTactic :: g -> ([g], [s] -> s) }
```

## Some Simple Tactics
From here on out, we will be using the [refinery](https://github.com/TOTBWF/refinery)
library, because our `Tactic` type has a few issues that make it not super
friendly to work with. Don't worry though, the `refinery` version of `Tactic` is
morally equivalent, though it does have a few extra features. Let's take a look
at it.
```haskell
newtype TacticT g s m a = ...
```
It still has the `goal` and `solution` type parameters, but now it's been turned
into a monad transformer! This lets us handle things like errors/environments/etc
much easier. Also, the `a` parameter is pretty much just there to make the `Monad`
gods happy, and is set to `()` all of the time.

When discussing how to actually use these things, it will help to be able to
look at actual, concrete examples. To do this, let's define a super simple
subset of Haskell so that we can all be on the same page.

```haskell
import Control.Monad.Except
import Control.Monad.State
import Data.List (find)
import Refinery.Tactic

type Var = String
data Expr
    = Var Var
    | Hole      -- ^ An incomplete expression
    | Lambda Var Expr 
    | App Expr Expr
    | Pair Expr Expr
    | ProjL Expr -- ^ Equivalent to `fst`
    | ProjR Expr -- ^ Equivalent to `snd`
    deriving Show
    
data Type
    = TVar Var
    | Type :-> Type -- ^ Function type
    | Type :*: Type -- ^ Product/Pair type
    deriving (Show, Eq)

infixr :->
    
-- A Goal is a context containing variables bound to types, 
-- along with a type that we are trying to fill.
data Goal = Goal [(Var, Type)] Type
    deriving (Show)

-- We are gonna need errors
data TacticError
    = GoalMismatch String Type
    | UnsolvedSubgoals [Goal]
    deriving (Show)

type Tactic = TacticT Goal Expr (StateT Int (Except TacticError)) ()
-- To be discussed shortly
type Rule a = RuleT Goal Expr (StateT Int (Except TacticError)) a

-- Create a fresh variable
fresh :: Rule Var
fresh = do
    i <- get
    modify (+1)
    return $ "x" ++ show i

-- Sometimes, we need a monad to make holes, but in this case we don't
instance MonadExtract Expr Identity where
  hole = return $ Hole

runTactic :: Type -> Tactic -> Either TacticError Expr
runTactic ty tac = runExcept $ flip evalStateT 0 $ runTacticT tac (Goal [] ty) >>= \case
    (t, []) -> return t
    (_, sg) -> throwError $ UnsolvedSubgoals sg
```

Now, lets define a few super simple `Tactic`s so that we can play around a bit!
First, let's implement the `pair` tactic that takes a goal of type `(a,b)` and
yields 2 subgoals of type `a` and `b`. In `refinery`, we use the `rule`
function to create these sort of domain-specific tactics. It's type 
(when specialized to our case) is
```haskell
rule :: Goal -> Rule Expr -> Tactic
```
Now, here's the implementation of `pair`
```haskell
pair :: Tactic
pair = rule $ \(Goal ctx t) -> case t of
    (a :*: b) -> do
        l <- subgoal (Goal ctx a)
        r <- subgoal (Goal ctx b)
        return $ Pair l r
    t -> throwError $ GoalMismatch "pair" t
```
Pretty straightforward, isn't it! As an aside, all the `Rule/RuleT`
portion of `rule` does is give us access to the `subgoal :: Goal -> Rule Expr` 
function.

As a slightly more complex example, heres the `function` tactic:
```haskell
function :: Tactic
function = rule $ \(Goal ctx t) -> case t of
    (a :-> b) -> do
        x <- fresh
        body <- subgoal (Goal ((x,a):ctx) b)
        return $ Lambda x body
    t -> throwError $ GoalMismatch "function" t
```
This is also super simple! All this does is bind the fresh variable `x`
to the type `a` in the context, and create a subgoal of type `b`.

Now, for one last tactic: `assumption`.
```haskell
assumption :: Tactic
assumption = rule $ \(Goal ctx t) -> case find ((== t) . snd) ctx of
    Just (x, _) -> return $ Var x
    Nothing -> throwError $ GoalMismatch "assumption" t
```
All `assumption` does is see if any tactic has put anything
in the context that could match the goal, and if there is, it just 
creates an expression that references that variable, along with no
new subgoals.

## Composing Tactics and Tactic Combinators
Now that we've got our simple tactics, we can start to stick them together!
The simplest way to do this is to compose 2 `Tactic`s using `>>`.
Doing so creates a new `Tactic` that uses the 2nd `Tactic` to solve the subgoals
of the 1st. For example:
```haskell
pair >> pair
```
This will create a Tactic that, when presented with a goal of type `((a,b), (c,d))`,
will create 4 subgoals `a`, `b`, `c`, and `d`.

As useful as this is, what if we have a goal that looks like `((a,c), d
-> d)`? In this case, we can use:
```haskell
<@> :: Tactic -> [Tactic] -> Tactic
```
This combinator takes a tactic, and a list of tactics to apply to the subgoals
in order. In our example, the tactic we want would be:
```haskell
pair <@> [pair, function]
```

Now, this is great and all, but we've just traded in 1 way to write programs for 
a slightly more complicated way to write programs. What we really want is some
tactic `auto` that can just solve our problems for us. To accomplish this, we
are going to need to introduct 1 more combinator:
```
<!> :: Tactic -> Tactic -> Tactic
```
This combinator will try to run the 1st tactic, and, if it fails, will run the
2nd tactic. With this power, we can finally write our `auto` tactic:
```haskell
auto :: Tactic
auto = do
  function <!> pair <!> assumption
  auto
```
That's all there is to it! Let's try it out on our function `foo` from the very beginning!
```haskell
λ runTactic (TVar "a" :-> TVar "b" :-> TVar "c" :-> (TVar "b" :*: (TVar "d" :->
TVar "c"))) auto

Right (Lambda "x0" (Lambda "x1" (Lambda "x2" (Pair (Var "x1") (Lambda "x3" (Var "x2"))))))
```
Prettying that up a bit, we can see that `\x0 x1 x2 -> (x1, \x3 -> x2)` really does
have the type we want!

## Extending The Horizons
Obviously, there is a ton more work required to make this fully general.
However, I've started that work in
[tactic-haskell](https://github.com/TOTBWF/tactic-haskell),
which uses this approach to work with actual Haskell types, and
to generate actual Haskell code. In the future, I plan on writing a bit on the
actual guts of [refinery](https://github.com/TOTBWF/refinery) itself, and
perhaps a tutorial on [tactic-haskell](https://github.com/TOTBWF/tactic-haskell)
itself once it matures a bit more. Until then, happy hacking!


