## Types (objects)

- We think of types as sets of terms.
- Types should include only valid terms.
- Sets can include finite or infinite sets of terms.
- finite sets of terms can have infinite values.

### There are three types of type composition:
- Sum composition
- Product composition
- ADT Algebraic data types (sum and product composition)

#### Product composition:
A: {a1, a2}
B: {b1}
A * B = { {a1, b1}, {a2, b1}}
Cartesian product of the two sets.
|A| = 2
|B| = 1
|A * B| = 2 * 1 = 3
A * B * C // three way product composition. Need three values to find a unique point in that space
A is an Axis, and B is an axis and then the product is the points on the graph (a, b)
In Scala:
  - Tuples
  - case Classes
Other notes
Unit = {()}
|Unit| = 1
We sometimes use 1 to represent Unit
A * 1 ~= A (contains no more information than A) we can move from one side to the other
Nothing = {}
|Nothing| = 0
A * 0 ~= 0

#### Sum composition:
A: {a1, a2}
B: {b1}
A + B = {Left_a1, Left_a2, Right_b1} // It keeps track of where they come from
|A + B| = |A| + |B| = 3
A + A = {Left_a1, Left_a2, Right_a1, Right_a2}
|A + A| = 4
Two ways:
 - Either
 - Sealed Trait
sum requires a finite number of terms (vs number of values which is ok to be infinite)


#### Notes:
- Using string to represent email/URL, etc. is not correct since it will have a lot of invalid values
- We need to tell the compiler what is valid and not valid, and in that case our code becomes a proof.

## Morphisms (functions)

- Functions map things from a domain to a co-domain. Sqr() function domain could be int, but the result set is square numbers only.

Functions have three properties:
1. Totality
2. Determinism
3. No Side Effects

## More on functions
- Higher order functions: Functions that takes a function as one of its parameters
- Functional combinator: Functions that only takes functions as its parameters

Mono-morphic functions drawbacks:
- Not easy to reuse (copy/paste)
- Many cards to win the game (lots of things can go wrong). This means you have to pay attention, and paying attention is not reliable, we need the compiler to provide the proof.

Polymorphic functions:
- Scala doesn't have polymorphic functions and thus we fake them using traits/objects or using functions instead


## Type Constructor
- `Tree[A]` is not a type, it is a **type constructor**
    * F(when given one type) => Another Type
    * `F(A) => Tree[A]`. That is a `[*] => *` (star to start) **kind**
    * `Either` is `[*, *] => *`
    * `* = {x: x is a type in the scala type system}`
- Partial type application
    * To Turn `[*, *] => *` to `[*] => *` We use `?` (which requires **type projector compiler plugin**)
    * `type newType[A] = Map[Int, A]`
    * `trait Foo[A[_, _, _], B, C[_,_]]`

## Type Classes: Enough but not too much
- Monomorphic functions know too much
- Polymorphic functions throw away too much (no more knowledge of comparability for an example)
- Type classes provide a way to regain structure but as little as possible
- Passing in a function is an ad-hock alternative
- We fake type classes in scala using traits or abstract classes

Type Classes should include:
1. Types
2. Operations on values of types
3. laws governing the behaviour of those operations

Every type should have a single instance of a type class. If we want different behaviour we need to define a new wrapper type and define a type class for it. The correctness of the program should not depend on which import is in scope.

We define type classes in the companion object of our type if it is coming from an external library or in the companion object of the type class if it is ours.


## Existential vs. Universal types
```scala
def foo[A] // Universal type

type Foo{
    type S // existential type
}
```

## Notes:
In Scala, types and values don't exist in the same universe
