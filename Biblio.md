# Annotated Bibliography

## From GHC Wiki

* https://gitlab.haskell.org/ghc/ghc/-/wikis/plugins/type-checker

The current home

* https://gitlab.haskell.org/ghc/ghc/-/wikis/reading-list#types-and-type-inference

This looks like a nice list. (1) Is it out-of-date? (2) Is there a few paragraphs we could tell to orient all of those papers with respect to one another?

* https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/type-checker

Probably out of date. Not much narrative, mostly just terse high-level descriptions and "this source file defines that types/function". In particular, only says "The best place reading for the constraint solver is the paper Modular type inference with local assumptions".

* https://github.com/xnning/GHC-Core-Literature-Review/blob/master/doc/doc.pdf

A somewhat short list. Seems like it gives non-negligible notes about each. It's about features of the type system, but not the constraint solver. Looks like that is primarily just OutsideIn.pdf at this point (and maybe the QuantifiedContexts paper too, and Chak et al's coercions/fundeps/tyfams too I guess etc, but OutsideIn.pdf seems to cover that pretty well if I understand correctly).

* https://gitlab.haskell.org/ghc/ghc/-/tree/master/docs%2Fcore-spec

Important, but overwhelmingly dense! Excludes `data Ct`.

* https://gitlab.haskell.org/ghc/ghc/-/wikis/type-functions

Looks to be 11 years old. Probably inconsistent to varying degrees with current implementation. But large parts might still be relevant. Probably a good bridge from Chak et al's papers and OutsideIn.pdf to the code.

## Issues

  * https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=all&label_name[]=typechecker%20plugins
  * https://gitlab.haskell.org/ghc/ghc/issues/16639 not used in pattern checker
  * https://gitlab.haskell.org/ghc/ghc/issues/15248 RAE, coercions too floaty
  * https://gitlab.haskell.org/ghc/ghc/issues/15147 let plugin see Wanted CFunEqs
  * (can't remember issue number) GHC adds excessive parens when pretty-printing type operators

## Publications

  * 2015 Adam Gundry "A Typechecker Plugin for Units of Measure" [(GitHub)](https://github.com/adamgundry/uom-plugin)
  * 2015 Iavor Diatchki (yav) "Improving Haskell Types with SMT" [(GitHub)](https://github.com/yav/type-nat-solver)
  * 2016 Jan Bracker, Henrik Nilsson "Supermonads" [(GitHub)](https://github.com/jbracker/supermonad)
  * 2018 Divesh Otwani, Richard Eisenberg, (Ben Gamari) "The Thoralf Plugin: For Your Fancy Type Needs" [(GitHub)](https://github.com/bgamari/the-thoralf-plugin)
  * 2019 Luke Horvat/Sandy McGuire [(GitLab)](https://gitlab.com/LukaHorvat/simple-effects/-/blob/b5cf92cb7fb529453020f6564ba0aee2c2278791/src/Control/Effects/Plugin.hs) [(GitHub)](https://github.com/polysemy-research/polysemy/tree/4eece51af611e86fd24f2b1c50cfe352b61ff1f5/polysemy-plugin)

## Packages

  * (see publications above)
  * Christiaan Baaij
      * https://github.com/clash-lang/ghc-typelits-knownnat
      * https://github.com/clash-lang/ghc-typelits-natnormalise
      * https://github.com/clash-lang/ghc-typelits-extra
      * https://github.com/clash-lang/ghc-tcplugins-extra
  * 2018 Oleg Grenrus "regular expression of types" https://github.com/phadej/kleene-type
  * 2018 Joachim Breitner "synthesis of singletons" https://github.com/nomeata/ghc-justdoit
  * 2020 Samuel Gélineau (gelisam), "type-level rewrite rules" [(GitHub)](https://hackage.haskell.org/package/typelevel-rewrite-rules)

## Posts

  * (catalog) Matthew Pickering https://mpickering.github.io/plugins.html
  * 2016 Baaij Christiaan https://christiaanb.github.io/posts/type-checker-plugin/
  * 2019 Sandy Maguire https://reasonablypolymorphic.com/blog/faking-fundeps/

## Talks

  * 2018 Gabe Dijkstra https://skillsmatter.com/skillscasts/12388-write-your-own-ghc-type-checker-plugins
