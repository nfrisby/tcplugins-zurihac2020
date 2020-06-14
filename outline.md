# Introduction to type-checking plugins


* [What type checker plugins can do](#cando)
  - [Example tasks which are possible](#possible)
  - [Example type-related tasks which are not possible](#impossible)
* [Basics](#basics)
  - [Using a plugin](#usage)
  - [Defining a plugin](#define)
  - [What information the plugin has access to](#info)
  - [When is the plugin invoked?](#invocation)
* [Examples](#examples)
  - [Solving a nullary constraint](#nullary)
  - [Implementing magic typeclasses](#typeclasses)
  - [Implementing magic type families](#tyfams)
* [Background knowledge](#background)
  - [Constraints](#constraints)
    - [Constraint flavours](#flavours)
    - [Typeclass constraints](#classes)
    - [Equality constraints](#eqs)
    - [Quantified constraints](#qcs)
  - [Zonking](#zonking)
  - [Flattening](#flattening)

---

<a name="cando"></a>
# What type checker plugins can do

<a name="possible"></a>
## Example tasks which are possible

<a name="impossible"></a>
## Example type-related tasks which are not possible

---

<a name="basics"></a>
# Basics

<a name="usage"></a>
## Using a plugin

A type checker plugin is identified by a module name, e.g. `My.Plugin`. To enable `My.Plugin` in your module, use the `-fplugin` option.

```haskell
{-# OPTIONS_GHC -fplugin My.Plugin #-}
module My.Module where
...
```

Note that if the `OPTIONS_GHC` block is located after the `module` line or if `My.Plugin` is a valid module but not one which provides a type checker plugin, the line will be silently ignored.

It is possible to pass some `String` arguments to the plugin using the `-fplugin-opt` option. Since there can be more than one occurrence of `-fplugin`, each occurrence of `-fplugin-opt` must repeat the name of the plugin in order to clarify which of those plugins should receive the argument.

```haskell
{-# OPTIONS_GHC -fplugin My.Plugin
                -fplugin-opt=My.Plugin:arg1
                -fplugin-opt=My.Plugin:arg2
                -fplugin-opt=My.Plugin:arg3
  #-}
module My.Module where
...
```

<a name="define"></a>
## Defining a Plugin

To provide a type checker plugin, `My.Plugin` must expose a value `plugin :: Plugin` whose `tcPlugin :: Maybe TcPlugin` field is set to a `Just`.

```haskell
module My.Plugin (plugin) where

import Plugins (Plugin(..), defaultPlugin)
import TcPluginM (tcPluginIO)
import TcRnTypes (TcPlugin(..), TcPluginResult(TcPluginOk))

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \args -> Just $ TcPlugin
    { tcPluginInit = do
        tcPluginIO $ print args
    , tcPluginSolve = \_ _ _ _ -> do
        pure $ TcPluginOk [] []
    , tcPluginStop = \_ -> do
        pure ()
    }
  }
```

When compiling `My.Module`, `My.Plugin`'s `tcPluginInit` gets called, which prints `["arg1","arg2","arg3"]` to the console at compile-time.

Note that `My.Plugin` and `My.Module` must be part of different packages, or at least different targets, e.g. the `library` and `executable` sections of the same cabal file.

The expected types of `tcPluginInit`, `tcPluginSolve` and `tcPluginStop` are as follows:

```haskell
tcPluginInit  :: TcPluginM PluginState
tcPluginSolve :: PluginState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
tcPluginStop  :: PluginState -> TcPluginM ()
```

for some type `PluginState` that the plugin author can define as desired (in the above example, `()`).

The workhorse of a type-checker plugin is `tcPluginSolve`. Its task is to analyse the constraints it is provided with (`Ct` is GHC's type of constraints, which come in three [flavours](#flavours), in order: given, derived, wanted), before returning a `TcPluginResult`:

```haskell
data TcPluginResult
  = TcPluginContradiction [Ct] -- Report that these constraints constitute a contradiction.
  | TcPluginOk
      [(EvTerm,Ct)] -- Constraints solved by the plugin, with accompanying evidence terms.
      [Ct]          -- New constraints emitted by the plugin.
```

That is, the plugin can either report a contradiction (with the offending constraints), or solve some of the constraints (providing evidence for them) while also emitting new constraints. To do nothing, the plugin can return `TcPluginOk [] []`.


<a name="info"></a>
## What information the plugin has access to

### Constraints

### Evidence

### Definitions in other files

<a name="invocation"></a>
## When is the plugin invoked?

GHC's constraint simplifier/solver runs in a loop, simplifying constraints until no further simplifications are found.
Only then will plugins be given the chance to handle the constraints.
If the plugin solved any constraints, or emitted further constraints, this information will then be passed back to the main constraint solving loop. This continues until there are no more constraints to solve, or GHC reaches the maximum number of constraint solving iterations (customisable with `-fconstraint-solver-iterations=n`; `n=0` for no limit).

GHC keeps track of progress using the notion of __inert__ constraints, on which no further work is deemed possible. Each iteration of the loop attempts to uses the inert constraints to simplify the next work item, until this work item is also inert (maybe even solved). GHC then __kicks-out__ any formerly-inert constraints back onto the work list if the new inert constraint can simplify them.

Note also that GHC's constraint solver can be called for many different reasons, reasons which might not be immediately obvious from the program GHC is attempting to typecheck. One such example is GHC's [ambiguity check](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html).

---

<a name="examples"></a>
# Examples

<a name="nullary"></a>
## Solving a nullary constraint

<a name="typeclasses"></a>
## Implementing magic typeclasses

<a name="tyfams"></a>
## Implementing magic type families

Some type classes, like `Coercible` and `Typeable`, are "magic", in the sense that the compiler discharges those constraints without requiring you to write any instances. You can implement your own magic type classes by discharging those constraints inside a type checker plugin.

For example, suppose we define a nullary type class and a definition which requires it:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
module My.Nullary where

class MyNullary

requireMyNullary :: MyNullary => ()
requireMyNullary = ()
```

If we try to call `requireMyNullary` from a function which does not itself require `MyNullary`, we get a `No instance for MyNullary` error.

```haskell
{-# OPTIONS_GHC -fplugin My.Plugin
                -fplugin-opt=My.Plugin:Nullary
  #-}
module My.Module where

import My.Nullary (requireMyNullary)

unit :: ()
unit = requireMyNullary
```

We can get ghc to accept this code by writing a plugin which discharges the wanted `MyNullary` constraints it encounters:

```haskell
module My.Plugin (plugin) where

import Class (Class(className, classTyCon))
import FastString (mkFastString)
import GHC.TcPluginM.Extra (lookupModule, lookupName)
import MkCore (mkCoreConApps)
import Module (mkModuleName)
import Name (Name, mkTcOcc)
import Plugins (Plugin(pluginRecompile, tcPlugin), defaultPlugin, purePlugin)
import TcEvidence (EvTerm(EvExpr))
import TcPluginM (TcPluginM)
import TcRnTypes (Ct(CDictCan), TcPlugin(TcPlugin, tcPluginInit, tcPluginSolve, tcPluginStop) , TcPluginResult(TcPluginOk)
  )
import TyCon (tyConDataCons_maybe)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just $ TcPlugin
    { tcPluginInit  = pluginInit
    , tcPluginSolve = pluginSolve
    , tcPluginStop  = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }

pluginInit :: TcPluginM Name
pluginInit = do
  myNullaryModule <- lookupModule (mkModuleName "My.Nullary") (mkFastString "my-package")
  lookupName myNullaryModule (mkTcOcc "MyNullary")

pluginSolve :: Name -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve myNullaryName _ _ = go
  where
    go :: [Ct] -> TcPluginM TcPluginResult
    go [] = do
      pure $ TcPluginOk [] []
    go (ct@(CDictCan _ class_ _ _) : _) | className class_ == myNullaryName = do
      let myNullaryClass = class_
          dictTyCon = classTyCon myNullaryClass
          Just [dictDataCon] = tyConDataCons_maybe dictTyCon
      pure $ TcPluginOk [(EvExpr $ mkCoreConApps dictDataCon [], ct)] []
    go (_ : cts) = do
      go cts
```

In `pluginInit`, we first lookup `MyNullary` in the `My.Nullary` module in order to obtain its internal name. Later on, in `go`, we compare internal names in order to make sure we only discharge our `MyNullary` constraint, and not e.g. some other constraint which also happens to be called "MyNullary". We then discharge the constraint by returning it along with the relevant evidence: a dictionary providing an implementation for all the methods of the type class. Since `MyNullary` has zero methods, we can do this by using `mkCoreConApps` to apply the dictionary's data constructor to zero arguments.

We can ignore the rest of the `Ct`s; if they are still relevant after ghc has figured out all the consequences of the simplification we provided, we'll be given another chance to examine them.

## How to implement magic type families
## How to teach the solver about e.g. arithmetic facts

---

<a name="background"></a>
# Background knowledge

<a name="constraints"></a>
## Constraints

In source Haskell, constraints appear to the left of `=>` arrows, and correspond to information that is synthesized by the constraint solver and implicitly passed around. 

Examples include:
  
  - [typeclass constraints](#classes) such as `Eq a`,
  - [equality constraints](#eqs) such as `a ~ b`,
  - [quantified constraints](#qcs).

<a name="flavours"></a>
### Constraint flavours

Constraints come in three different __flavours__:

* \[__G__\] __Given__: we have evidence for this constraint,

* \[__W__\] __Wanted__: we want evidence for this constraint,

* \[__D__\] __Derived__: any solution must satisfy this constraint, but
  we don't need evidence for it.    
  Examples include:
  - superclasses of \[__W__\] class constraints,
  - equalities arising from functional dependencies or injective type families.


<a name="classes"></a>
### Typeclass constraints

Let's take the example of the `Eq` typeclass:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

The full type signatures of `Eq` and `(==)` are:

```haskell
Eq :: Type -> Constraint
(==) :: Eq a => a -> a -> Bool
```

Here `Constraint` is the surface-Haskell type of constraints. Consider what happens when a function requires the constraint `Eq a`:

```haskell
f :: forall a. Eq a => a -> a -> Bool
f x y = ( x == x ) && ( y == y )
```

The `Core` for this function is:

```haskell
f = \ @a ($dEq :: Eq a) (x :: a) (y :: a) -> ( (==) @a $dEq x x ) && ( (==) @a $dEq y y )
```

That is, the constraint `Eq a` is satisfied by passing an explicit _typeclass dictionary_ to `f`, which is here named `$dEq`. This dictionary is then handed to `(==)`, which simply accesses the field of the record `Eq a` corresponding to the typeclass method `(==)` of `Eq`.

In this case, we see that the __evidence__ for a typeclass constraint is a dictionary for this typeclass; that is, a record of the typeclass methods. For typeclasses with a single method, this record is moreover a _newtype_.

<a name="eqs"></a>
### Equality constraints

Haskell has two different notions of type equivalence:

  * __nominal equivalence__: two types are the same, e.g. `String` and `[Char]`.
  * __representational equivalence__: two types have the same representation,
    e.g. one is a newtype around the other (such as  `Int` and `Sum Int`).

In source Haskell, `a ~ b` is a nominal equivalence between `a` and `b`, whereas `Coercible a b` is a representational equivalence.

The __evidence__ for a type equality `a ~ b` is a coercion from `a` to `b`: the Haskell function

```haskell
f :: forall a b. ( a ~ b ) => a -> b
f x = x
```

results in the `Core`:

```haskell
f = \ @a @b ($d~ :: a ~ b) (x :: a) ->
      case eq_sel @Type @a @b $d~ of
        ( co :: a ~# b ) -> x `cast` ( Sub co :: a ~R# b )
```

Here `eq_sel` unwraps the coercion `$d~`, giving an unboxed nominal coercion `co :: a ~# b`. This coercion is downgraded to an unboxed representational coercion `Sub co :: a ~R# b`, which is then used to perform a type cast.

<a name="qcs"></a>
### Quantified constraints

<a name="zonking"></a>
## Zonking
  
<a name="flattening"></a>
## Flattening
