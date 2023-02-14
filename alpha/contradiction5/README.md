# Contradiction5

This test should create a contradiction.

## Setup

Statements:

```
[1] beetle a car  
[2] beetle a car -> (beetle is green) OR (beetle is blue) 
[3] beetle is green -> (beetle is nice) AND (beetle is X) -> (beetle is Y)
[4] beetle is blue -> (beetle is nice) AND (beetle is X) -> NOT(beetle is Y)
[5] beetle is nice -> beetle is X
[6] beetle is green -> NOT(beetle is Y)
[7] beetle is blue -> beetle is Y
```

## Challenge

The both implications [6] and [7] together, contradict together [3] and [4]. Any of the [6] and
[7] apart don't contradict [3] and [4].

Seems not solavable in with Latar algoritm that doesn't do Peirce iteration procedures.

By adding a `NOT(beetle is green)` or blue, X, Y an inconsistency can be spotted by the Latar algorithm if it would support deeply nested deiteration procedures. From [2] would follow `beetle is blue` from [7] would follow `beetle is Y`, this latter can be deiterated from [4] so that [5] and [4]-deiterated would result in `NOT(beetle is blue)`.

Note:
A theory (all surfaces without query or ask surface) is not inconsistent when all models are false. When a query is false for all models, then the excution must lead to an inconsistency. In this case it could mean that a query on `beetle is Y` must at least lead to an inconsistency. See also: https://skolemmachines.org/reports/SkolemMachines.pdf
