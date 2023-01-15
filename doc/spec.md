# Airda Specification

## Types

- Num

### Num

`Num` can be used to store numerical values. These can be integers, decimals etc.

## Variables

### Declaration

A typical variable declaration looks like the following:

``` airda
someVar : Num;
```

Variable declarations also allow to initialize the variable to a given value:

``` airda
someOtherVar : Num = 1;
```

If the declared variable is not initialized the type specific default value is used.

### Assignment

Assigning values to variables is done by using the '=' operator:

```
var = 1;
```

## Comments

Airda supports single line comments. Single line comments start with '#' and can be placed on individual lines or behind code on a line.

```
aOne : Num = 1;
# Separate line.

aTwo : Num = 2; # Same line as code
```