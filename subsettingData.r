# make some random numbers, this generates 10 random numbers
x <- rnorm(10)
x
# do the same for two other vectors
y <- rnorm(10)
z <- rnorm(10)

# combine into a data.frame, the "x=x" means put x in a column named x
A <- data.frame(x=x,y=y,z=z)
A
# this works, as all values are numeric
as.matrix(A)
# you can now call elements of the data.frame, for example column x
# Two ways to do this
# 1. using the dollar sign
A$x; A$y
#2. using "square brackets"
A[,2]
# This means, from A take all rows (nothing before the comma) and column 2
# If I want just one value, I can define column and row
A[2,3]; A[1,1]
# If I want just a couple of rows:
A[1:5,]

# I can now use this to subset the data.frame
# for example, I want to drop column y:
B <- A[,-2]
B
# or I want to drop row 3 and 5
C <- A[-c(3,5),]
C

# finally a list:
L <- list(x=x,y=y,z=z)
L
# you can see the structure of the list
# again I can call the elements:
L$x
# Or using DOUBLE square brackets
L[[2]]