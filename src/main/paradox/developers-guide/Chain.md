# Chain

Chains are objects that carry information about how simplexes are connected. They are linear combinations of
simplexes with coefficients drawn over a field. Chains are equipped with boundary operators 
$\partial_k$, which express a $k$ dimensional chain as a formal linear combination of $k-1$ dimensional simplexes, and these can be linked together to form a chain complex. 

We implement chains as a sorted map from simplexes to their corresponding coefficient. Sorting
should occur by filtration value in the simplex stream. We also provide a method for chain reduction. 

