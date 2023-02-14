# Contradiction3

This test should create a contradiction.

## Setup

Statements:

```
[1] beetle a car  
[2] beetle a car -> (beetle is green) OR (beetle is blue) 
[3] beetle is green -> (beetle is nice) AND (beetle is pretty)
[4] beetle is pretty -> (beetle is pretty1) AND NOT(beetle is pretty2)
[5] beetle is nice -> (beetle is nice1) AND NOT(beetle is nice2)
[6] beetle is pretty1 -> (beetle is pretty11) AND NOT(beetle is pretty12)
[7] beetle is pretty2 -> (beetle is pretty21) AND NOT(beetle is pretty22)
[8] beetle is nice1 -> (beetle is nice11) AND NOT(beetle is nice12)
[9] beetle is nice2 -> (beetle is nice21) AND NOT(beetle is nice22)
[10] (beetle is nice11) OR (beetle is nice12) OR (beetle is nice21) OR (beetle os nice22) OR
     (beetle is pretty11) OR (beetle is pretty12) OR (beetle is pretty21) OR (beetle is pretty22) OR
     (beetle is blue) -> beetle is beautiful
[5] NOT(beetle is beautiful)
```

## Challenge

The [5] can be deiterated from [10], which should lead to a double cut of [10], which should
deiterate [9],[8],[7],[6],[5],[4],[3],[2] and lead to a contradiction that `beetle is green`
and `NOT(beetle is green)`.
