
# CalculationTree
Scala University Solution of CalculationTree

Example 
Input:

    val calculus = Node('/',
			Node('+',
				Node('*', Leaf(6), Leaf(2)),
				Leaf(3),
				Leaf(4)),
			Node('-', Leaf(5))
			)
Output of *eval* function: -3.8

Output of *calcToString* function: (((6*2)+3+4)/(-5))
