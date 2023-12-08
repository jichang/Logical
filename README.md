The implementation of langauge is based on this [paper](http://www.cse.unt.edu/~tarau/research/2017/eng.pdf), but the syntax is different

```lisp
(Relation (entity0 entity1) (goal0 goal1 goal2))
```

For example

```lisp
// fact that a is b's parent
(parent (a b))
// fact that b is c's parent
(parent (b c))

// X is Y's grandparent when X is Z's parent and Z is Y's parent
(grandparent (X Y) ((parent (X Z) (parent (Z Y)))))
```
