# ENVX2001 Matrices
#A
A <- matrix(c(1,3,2,2,8,9),ncol=3)
A
# multiply by scalar
7*A

#B
B <- matrix(c(5,8,3,4,2,7),ncol=3)
B

# addition
A+B
A

# multiplication
A%*%B
# does not work as B does not have 3 rows
# other matrix
B1 <- matrix(c(5,8,4,2),ncol=2)
B1
# multiplication
B1%*%A
#no comformable
A%*%B1

# determinant
D <- matrix(c(5,3,9,6),nrow=2,ncol=2)
D
det(D)

# inverse
solve(D)

# cannot find for det = 0
E <- matrix(c(1,2,3,6),2,2)
E
det(E)
solve(E)

# almost identity matrix (rounding errors)
D%*%solve(D)

