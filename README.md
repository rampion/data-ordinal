This project defines the `data-ordinal` haskell package for calculations using [ordinal number][1].

The simplest ordinal numbers are the finite ordinals, aka the natural numbers:

    >>> import Prelude hiding ((^), quotRem)
    >>> import Data.Ordinal

		>>> 1 + 2 :: Finite
		3

We can expand on the finite ordinals to define ω, the smallest transfinite number

    >>> let w = ω :: Expansion Finite
    >>> w
    ω

which his greater than any finite ordinal:

    >>> w > 1
    True
    >>> w > 10000
    True

`Expansion Finite` includes not only `ω` and the finite ordinals, but
also their closure under addition, multiplication, and expnentiation:

    >>> 1 + w
    ω
    >>> w + 1
    ω + 1
    >>> 2 * (w + 1)
    ω + 2
    >>> (w + 1) * 2
    ω * 2 + 1
    >>> (w * 2 + 1) ^ 2
    ω ^ 2 * 2 + ω * 2 + 1
    >>> (w * 2 + 1) ^ (w * 2 + 1)
    ω ^ (ω * 2 + 1) * 2 + ω ^ (ω * 2)

Note that addition and multiplication, though still associative, are not
necessarily commutative.

Subtraction is defined as the inverse of left-addition by the subtrahend (e.g.
`α - β = γ` <=> `β + γ = α`):

    >>> w - 1
    ω
    >>> (w * 2 + 3) - (w + 5)
    ω + 3
    >>> (w + 5) + (w + 3)
    ω * 2 + 3

We can also find the quotient and remainder when dividing one ordinal by another,
where division is the inverse of left-multiplication by the divisor (i.e. 
`α ``quotRem`` β = (γ, δ)` <=> `α = β * γ + δ, δ < β`)

    >>> (w ^ (w + 3) + w * 5 + 7) `quotRem` (w + 1)
    (ω ^ (ω + 3) + 5,6)
    >>> (w + 1) * (w ^ (w + 3) + 5) + 6
    ω ^ (ω + 3) + ω * 5 + 7

When defining functions that take ordinal inputs, you can still use integer
patterns, but you can also use `Omega` to match ω, and [Cantor Normal Form][2]
to match arbitrary ordinals:

    >>> :{
      let f :: Expansion Finite -> String
          f 3 = "three"
          f Omega = "omega" 
          f (Expansion []) = "zero" 
          f (Expansion [(0,1)]) = "one" 
          f (Expansion [(0,_)]) = "some positive finite number" 
          f (Expansion [(1,_)]) = "a multiple of omega" 
          f (Expansion [(_,1)]) = "a power of omega" 
          f (Expansion [(3,2),(0,1)]) = "omega ^ 3 * 2 + 1" 
          f _ = "something else" 
      :}
    >>> Expansion [(3,2),(0,1)]
    ω ^ 3 * 2 + 1

The next transfinite number that can't be expressed by a finite number of
additions/multiplications/exponentiations of ω and the finite numbers is 
ε<sub>0</sub>:

    >>> let e_0 = ε SZero :: Expansion (Expansion Finite)
    >>> e_0
    ε_0

Then ε<sub>1</sub>, then ε<sub>2</sub>, and so forth:

    >>> let e_1 = ε (SSucc SZero) :: Expansion (Expansion Finite)
    >>> e_1
    ε_1
    >>> let e_2 = ε (SSucc (SSucc SZero)) :: Expansion (Expansion (Expansion Finite))
    >>> e_2
    ε_2

You can match ε<sub>i</sub> using either the `Epsilon` or the `Eps` patterns,
and use either CNF or the `Lifted` constructor to match transfinite values
from the base type:

    >>> :{
      let g :: Expansion (Expansion (Expansion Finite)) -> String
          g 4 = "four"
          g Omega = "omega"
          g (Epsilon SZero) = "epsilon-sub-0"
          g (Epsilon (SSucc SZero)) = "epsilon-sub-1"
          g (Eps 2) = "epsilon-sub-2"
          g (Expansion [(1,_)]) = "some multiple of epsilon-sub-2" 
          g (Expansion [(0, Expansion [(1,_)])]) = "some multiple of epsilon-sub-1" 
          g (Lifted (Lifted (Expansion [(3,2),(0,1)]))) = "omega ^ 3 * 2 + 1" 
          g _ = "something else"
      :}

The type `Kleene Expansion Finite` contains all countable `Expansion`s of
`Finite` (i.e., `Finite`, `Expansion Finite`, `Expansion (Expansion Finite)`,
`Expansion (Expansion (Expansion Finite))`, ...):

    >>> toEps 1000 :: Maybe (Kleene Expansion Finite)
    Just ε_1000

But we can go yet bigger, and get the first uncountable transfinite number:

    >>> OmegaOmega :: Expansion (Kleene Expansion Finite)
    ω_ω

And larger again... but the library doesn't have a pithy name for it

    >>> Infinity :: Expansion (Expansion (Kleene Expansion Finite))
    Infinity @(Expansion (Kleene Expansion Finite))
    >>> Infinity :: Expansion (Expansion (Expansion (Kleene Expansion (Kleene (Kleene (Kleene Expansion)) (Kleene Expansion (Expansion (Expansion Finite)))))))
    Infinity @(Expansion (Expansion (Kleene Expansion (Kleene (Kleene (Kleene Expansion)) Finite))))
