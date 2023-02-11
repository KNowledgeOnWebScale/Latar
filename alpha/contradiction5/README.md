# Contradiction4

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