For starters, here's a haphazard list of topics/questions I think we
should eventually cover. Feel free to add items and categories!

Missing/limited desiderata

  * [ ] use in pattern checker
  * [ ] custom equality evidence
  * [ ] Wanted CFunEqs
  * [ ] custom equality check
  * [ ] breadcrumb argument representing state flow (to allow sound caching)
  * [ ] list of in-scope forall-bound vars
  * [ ] easy access to reified inert substitution and/or flattening substitution
  * [ ] easy influence over flattening substitution (eg tv-tv equality orientation)
  * [ ] custom type formers (so tyfam abuse wouldn't be as necessary)
  * [ ] programmable plugins (eg user assertions, like Tritlo wants `{-# ANN type Max Symmetric #-}` which would be a bit nicer than passing -fplugin-opt eg)

Reference documentation

  * [ ] what is zonking?
  * [ ] when are things zonked/flattened/canonicalized/etc and when must they be so?
  * [ ] consistent terminology (eg skolem, univar, metavar, flexivar, mutvar, flatvar, tyvar, fsk, fmv, and so on)
  * [ ] basic constraint solver architecture
  * [ ] coercion invariants (eg does coercing coercions necessitate a lint error)
  * [ ] when and how does defaulting work? (including what happens to unfilled metavars)
  * [ ] Derived (when to emit them, how to rewrite/simplify them, when to split a WD)
  * [ ] Ct invariants (esp tv-tv eq orientation)
  * [ ] tyvar levels and touchability
  * [ ] floating Wanteds
  * [ ] various contexts in which the constraint solver and hence plugins are invoked (eg type checking vs type inferring vs ambiguity check vs pattern check vs kind checking etc)
  * [ ] Type vs TcType vs Xi vs Tau vs almost function-free vs etc
  * [ ] what is SigTyVar?
  * [ ] examples of source programs and the plugin invocations (ie implication constraints) they induce
  * [ ] how to create various flavors of evidence (eg how for a single-method class)
  * [ ] different flavors of Name (eg system)
  * [ ] do we need to discuss various TC monad environment and state (eg evbinds)?
  * [ ] inert substitution

Advice

  * [ ] minimal example plugins demonstrating a fundamentally sound/appropriate plugin architecture for a few primary use cases
  * [ ] generally how to avoid livelocks with GHC's constraint solver
  * [ ] advice regarding incompleteness trade-offs (eg "what to do instead of inferring an implication constraint")
  * [ ] cooperation with a core-to-core plugin?
  * [ ] cooperation with other constraint solver plugins?
  * [ ] how to introduce a minimally expressive constraint language
  * [ ] consolidated list of notable and/or maintained plugins
  * [ ] how to generate useful error messages (eg avoid 2 ~ 3, avoid system names, etc)
