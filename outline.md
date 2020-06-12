# What typechecker plugins can do
## Example tasks which are possible
## Example type-related tasks which are not
## What info the plugin has access to
### Constraints
### Evidence
### Definitions in other files
# Basics
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

## The Plugin type

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

# Examples
## How to teach the solver about e.g. arithmetic facts
## How to implement magic type families
## How to implement magic typeclasses
## How to implement magic nullary constraints
# Background knowledge
## Typeclass constraints
## Equality constraints
## Quantified constraints
## Evidence
## Wanted constraints
## Given constraints
## Derived constraints
## Zonking
  
