# lambda_calculus

[![pipeline status](https://gitlab.com/samlai/lambda_calculus/badges/master/pipeline.svg)](https://gitlab.com/samlai/lambda_calculus/-/commits/yaml)

Evaluator of untyped lambda calculus.

## Characteristic

When pretty printer recognizes that a given lambda structure is equivalent to some macro, it will be displayed.

## Examples

### Natural numbers

When you write a natural number in Church encoding, the pretty-printer will say what number is it:

```
|:\fx.f(fx)

2N
```

Letter `N` means "Natural".

### Booleans

It is similar to logical values:

```
|: \xy.x

TRUE
```

But if you write false you get:

```
|: \xy.y

[]
```

Because it is untyped lambda calculus, so the printer doesn't know it is a `FALSE` or empty list (see below).

### Pairs

Pairs are programs which, depending on the boolean values, return the first or the second value:
```
|: \x.x TRUE 12

[TRUE|12N]
```

It could be written equivalently like this:
```
|: <TRUE|12>

[TRUE|12N]
```

You can get the first element by applying the `TRUE`:

```
|: <TRUE|12> TRUE

TRUE
```

And the second element by applying `FALSE`:

```
|: <10|12> FALSE

12N
```

### Lists

If we have pairs, we can use them to represent lists:

```
|: FALSE

[]
|: <12|<5|<7|FALSE>>>

[12N,5N,7N]
```

### Tuples

We represent a tuple in a similar way to pairs:

```
|: \x.x 12 15 19

<12N|15N|19N>
```

To access the second element of above tuple we apply `l_x_.x`:

```
|: <12|15|19> l_x_.x

15N
```

Following this analogy, the identity function is an empty tuple:

```
|: \x.x

<>
```

### Enumarators

To get the second element of a 5-element tuple, write `\_x___.x`.

We have simplified this entry for `1E5`.
It means that it is getting second element (counting from zero) of a 5-element tuple.

So to get number `20` from tuple `<10|20|30|40|50>` you write:

```
|: <10|20|30|40|50> 1E5

20N
```

### Integers

Integers are constructed from pairs and natural numbers by [Grassman construction](https://en.wikipedia.org/wiki/Integer#Construction):

```
|: <5|0>

5Z
|: <0|5>

-5Z
|: <5|12>

-7Z5
|: <12|5>

7Z5
```

```
|: <5|0>

5Z
|: <0|5>

-5Z
|: <5|12>

-7Z5
|: <12|5>

7Z5
```

Number `7Z5` means that it is integer `7` with an "excess" `5`.

### Binary numbers

By using tuples and booleans you can write binary number $12_{(10)} = 1100_{(2)}$ like this:

```
|: <TRUE|TRUE|FALSE|FALSE>

12B4
```

Number `12B4` is binary number `12` in `4`-bit memory.

### Equals

The built-in operator `EQ` checks alpha equivalence between two lambda expressions:
```
|: EQ <> <>

TRUE
|: EQ 12 12

TRUE
|: EQ 13 []

[]
|: EQ TRUE FALSE

[]
```

## Syntax

The full syntax is shown below:

```yacc
Lam := whites Exp

Exp := Exp | Exp whites Exp_
Exp_ := L_Exp | Macro | Var | '(' Lam ')'

L_Exp := L L_Exp_
L_Exp_ := LVar L_Exp_
       |  '.' Exp
L := 'l' | 'λ' | '\''

Macro := [:upper:]+ | Tuple | List | Number_exp

Tuple := '<' Tuple_1
Tuple_1 := '>' | Lam Tuple_2
Tuple_2 := '>' | '|' Tuple_2

List := '[' List_1
List_1 := ']' | Lam List_2
List_2 := ']' | ',' List_2 | '|' Lam ']'

Number_exp := [:digit:]+
           | [:digit:]+ 'N'
           | [:digit:]+ 'Z' [:digit:]*
           | [:digit:]+ 'B' [:digit:]+

LVar := '_'
     |  Var
Var := [:lower:] # 'l'
```

## Required

 - swipl

## Compile

```
make
```

## Run

Default run:

```
./lambda_calculus
```

Run with showing intermediate states of calculations:

```
./lambda_calculus -d
```

## TODO
 - a choice of lazy and eager evaluation
 - other list representations
 - Fix bug with `lx.EQ x x`
