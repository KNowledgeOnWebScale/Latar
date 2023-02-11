# Contradiction4

This test should create a contradiction.

## Setup

Statements:

```
[1] beetle a car  
[2] beetle a car -> (beetle is green) OR (beetle is blue) 
[3] beetle is green -> (beetle is nice) AND (beetle is pretty)
[4] beetle is blue -> (beetle is nice) AND NOT(beetle is pretty)
[5] NOT(beetle is nice)
```

## Challenge

The `NOT(beetle is nice)` can only be deiterated by inserting of a double cut around `beetle is nice` in the conclusion of [4] and [5].
