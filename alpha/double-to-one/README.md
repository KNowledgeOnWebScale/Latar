# Contradiction3

This test should  produce:

```
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#Alice>','<http://example.org/ns#Person>').
'<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'('<http://example.org/ns#Alice>','<http://example.org/ns#Human>').
```

## Setup

Statements:

```
[1] alice a person
[2] ~(alice a person AND ~(alice a human) AND ~(alice a human))
```

## Challenge

A repetition of `~(alice a human)` can be removed to that [2*] becomes:

```
[2] ~(alice a person AND ~(alice a human) )
```
