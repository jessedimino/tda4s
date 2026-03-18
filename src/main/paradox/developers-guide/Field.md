# Field

The Field object provides an implementation for field. Chains can be thought of as linear combinations of simplexes
drawing their coefficients from a Field, and the persistent homology algorithm is just a form of row reduction to find
the pivots in a boundary matrix, which contain information about the cycles in the complex.
The classical choice for the construction of the Vietoris-Rips Complex is $F_2$, but we provide a general implementation 
with support for addition and multiplication modulo the field characteristic $p$.
