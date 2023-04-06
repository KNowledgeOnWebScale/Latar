# Contradiction6

This test should create a contradiction and should not produce `beetle is produced`.

## Setup

Statements:

```
[1] beetle a car  
[2] beetle a car -> beetle is produced                        # {Q}
[3] beetle a car -> (beetle is green) OR (beetle is blue)     # { A ∨ B }
[4] beetle a car -> (beetle is german) OR (beetle is mexican) # { X ∨ Y }
[5] beetle is green -> (beetle is cool) AND                   # C
     () (beetle is german) OR (beetle is mexican) ) -> NOT(beetle is produced) AND socrates is human
[6] beetle is blue -> (beetle is cool) AND  
     () (beetle is german) OR (beetle is mexican) ) -> NOT(beetle is produced) AND plato is human
```

## Challenge

The `(beetle is german) OR (beetle is mexican)` from [4] could be deiterated from the deeply nested [5] and [6]. It should procude `NOT(beetle is produced)` in all models.

Foramlized:

```
 Q ∧ (A ∨ B) ∧ (X ∨ Y) ∧ (A → (C ∧ ((X ∨ Y) → (¬Q ∧ R)))) ∧ (B → (C ∧ ((X ∨ Y) → (¬Q ∧ S))))
```

in https://www.erpelstolz.at/gateway/formular-uk-zentral.html