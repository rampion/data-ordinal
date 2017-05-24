- [x] add to github
- [x] Define better Show instances
- [x] Remove LPred
- [x] Fix multiplication for `Expansion`
- [x] Fix exponentiation for `Expansion`
- [x] define a notation function for printing Infinity @ a 

          notation :: Typeable a => proxy a -> String

      and use that in the Show (Expansion a) instances
- [x] division, modulus
- [x] refactor Diff type
      RightDiff -> LessThanBy
      LeftDiff -> GreaterThanBy
      NoDiff -> EqualTo
- [ ] change `notation`?  is @ better than ::?
- [ ] Define lift operator?
- [ ] Test, Test, Test
- [ ] use hedgehog for property tests? https://hackage.haskell.org/package/hedgehog
- [ ] Document, Document, Document
- [ ] DocTests?
- [ ] move useful code from tests into library (e.g. helper functions to define NFData instances)
- [ ] refactor Infinity to Limit?
- [ ] replace Finite with GHC.Natural?
- [ ] replace "error" with "throw Undeflow"?
