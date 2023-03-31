# **Symbolic Derivative**

Program written as a final project for Functional Programming course.

## Description
A program written in Haskell computing symbolic derivative of a given function and simplifying it. 

## Available Functions

`parse <String s>` <br>
Converts string *s* into object of type Maybe ExprThree. Returns Nothing if *s* doesn't represent a valid expression. Expression can contain:
- brackets
- spaces
- operations: `+`, `-`, `*`, `/`
- functions: `sin`, `cos`, `tg`, `exp`, `ln`
- variables: `x`, `y`, `z`
- constants as lowercase letters
- integer numeric constants

`derivative <ExprThree expr> <Var v>` <br>
Returns the partial derivative of a given expression *expr* with respect to the variable *v*. Possible values of *v* are `X`, `Y` or `Z`.

`allDerivatives <ExprThree expr>` <br>
Returns a tuple of partial derivatives with respect to variables x, y and z.

`simplify <ExprThree expr>` <br>
Simplifies the expression *expr*.

`simplifyM <Maybe ExprThree expr>`<br>
`derivativeM <Maybe ExprThree expr>` <br>
`allDerivativesM <Maybe ExprThree expr>` <br>
versions of the above functions taking Maybe ExprtThree as argument instead of ExprThree. It's more comfortable to use them after using `parse`.

## Usage

To use it in GHC interactive mode:
```
ghci
:load SymbolicDerivative
```
Example:
```
*SymbolicDerivative> x = parse "(x+y)*(y+z)"
*SymbolicDerivative> x
Just (x + y) * (y + z)
*SymbolicDerivative> derivativeM x X
Just y + z
*SymbolicDerivative> allDerivativesM x
Just (y + z,2.0 * y + z + x,x + y)
*SymbolicDerivative> y = parse "(x+1)*sin(x)*a + a*exp(2*x)*(1+x)"
*SymbolicDerivative> y
Just (x + 1.0) * sin(x) * a + a * exp(2.0 * x) * (1.0 + x)
*SymbolicDerivative> simplifyM y
Just (x + 1.0) * a * (sin(x) + exp(2.0 * x))

```