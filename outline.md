# Introduction to type-checking plugins


* [What typechecker plugins can do](#cando)
  - [Example tasks which are possible](#possible)
  - [Example type-related tasks which are not possible](#impossible)
  - [What information the plugin has access to](#info)
* [Basics](#basics)
  - [Invoking a plugin](#invoke)
  - [Defining a plugin](#define)
* [Examples](#examples)
  - [Solving a nullary constraint](#nullary)
  - [Implementing magic typeclasses](#typeclasses)
  - [Implementing magic type families](#tyfams)
* [Background knowledge](#background)
  - [Constraint flavours](#flavours)
  - [Typeclass constraints](#classes)
  - [Equality constraints](#eqs)
  - [Quantified constraints](#qcs)
  - [Evidence](#evidence)
  - [Zonking](#zonking)
  - [Flattening](#flattening)

---

<a name="cando"></a>
# What typechecker plugins can do

<a name="possible"></a>
## Example tasks which are possible

<a name="impossible"></a>
## Example type-related tasks which are not possible

<a name="info"></a>
## What information the plugin has access to

### Constraints

### Evidence

### Definitions in other files

---

<a name="basics"></a>
# Basics

<a name="invoke"></a>
## Invoking a plugin

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

<a name="examples"></a>
# Examples

<a name="nullary"></a>
## Solving a nullary constraint

<a name="typeclasses"></a>
## Implementing magic typeclasses

<a name="tyfams"></a>
## Implementing magic type families

---

<a name="background"></a>
# Background knowledge

<a name="flavours"></a>
## Constraint flavours

<a name="classes"></a>
## Typeclass constraints

<a name="eqs"></a>
## Equality constraints

<a name="qcs"></a>
## Quantified constraints

<a name="evidence"></a>
## Evidence

<a name="zonking"></a>
## Zonking
  
<a name="flattening"></a>
## Flattening
