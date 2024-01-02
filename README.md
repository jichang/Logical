The implementation of langauge is based on this [paper](http://www.cse.unt.edu/~tarau/research/2017/eng.pdf), but the syntax is based on S-expression

```lisp
(Relation (entity0 entity1) (goal0 goal1 goal2))
```

For example

```lisp
// assertion
(true)

// fact that a is male
(male a)
// fact that b is female
(female b)

// fact that a is c's father
(father (a c))
// fact that b is c's mother
(mother (b c))

// rule that A and A are couple if A is C's father and B is C's mother
(couple (A B)
  (
    (father (A C))
    (mother (B C))
  )
)
(couple
  (
    (father (A C))
    (mother (B C))
  )
)
```
