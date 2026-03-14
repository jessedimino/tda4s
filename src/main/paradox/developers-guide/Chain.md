# Chain

Chains are objects that carry information about how simplexes are connected. They are linear combinations of
simplexes with coefficients drawn over some field. Chains can be linked together with boundary operators 
$\partial_k$, which express a $k$-dimensional chain as a formal linear combination of $k-1$-dimensional simplexes. 

Our implementation provides for chains to be a sorted map for simplexes and their corresponding coefficient, where sorting
should occur by filtration value in the simplex stream. We also provide a method for reduction. 

