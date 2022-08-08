library("pracma")
# Span
## Span is a set of all possible linear combinations.
## In other words, a "vector" is in span of {vectors} means
## {vectors} have unique values of variables that can be equal to a "vector"
## Case 1: m(# of rows) = n (# of cols) 
## Case 2: m > n (debatable)
## Can't be in span when n>m in R^n space

## one to one: 1 input leads to 1 output
## onto: several input leads to the same onto with not all outputs are linked with inputs
## one to one and onto : all outputs covered with 1 to 1

## all rows in echelon form have pivots

# Independence
## All columns have pivots
## Unique solutions
## Not dependent on other variables

# Inverse
## AB = In
## Must be a square matrix
## If invertible, it is a nonsingular matrix.
## If A is invertible, AB = AD -> C = D
## If A isinvertible, Ac = 0 -> C = 0

## If A is nonsingular, Ax = b has unique solutions, where x= A-1b
## example: let's say y = B1x1 + B2x2
## From iris dataset, 
## this is Petal.Width = Sepal. Length + Sepal.Width

colnames(iris)
A <- cbind(rep(1,nrow(iris)),iris[,1:2]) # added 1s for B0, the intercept
A <- as.matrix(A)
Y <- iris[,4]
Y <- as.matrix(Y)


A.sq <- t(A)%*%A
Y.2 <- t(A)%*%Y
coeff <- solve(A.sq) %*% Y.2
coeff
# The mean is -2.5248
# B1 = 2.7756
# B2 = -0.3386

linear <- lm(Petal.Width ~Sepal.Length + Sepal.Width, data=iris)
summary(linear) 

# The reslut is same is R's lm function.


# LU factorization
## If A can be transformed to echelon form without interchanging rows then 
## A has LU- factorization
## this can enhance the speed of regression

# Markov Chain & Stochastic Matrix
## Stochastic Matrix -> Columns add to 1 and entries >=0
## Steady-state vector -> Stochastic Matrix that converges with Markov Chain
## Regular vector -> stochastic matrix with entries all positive; Markov Chain will always converge
## (A-I)x = 0 is when the steady vector is formed

## Markov Chain
## Scenario: Suppose 
## Pr[rain | rained yesterday] = 0.6
## Pr[no rain | didn't rain yesterday] = 0.8
## Pr of rain tomorrow?

A <- matrix(c(0.6,0.2,0.4,0.8),byrow=T,nrow=2)
x <- matrix(c(1,0),nrow=2)
diff <- 10
while(diff>0.01){
  x.1 <- A%*%x
  diff <- sum(abs(x-x.1))
  x <- x.1
  
}

diff
x # The probability of no rain tomorrow is 0.66 and rain is 0.34

# Subspace
## subset of a vector, where additions between elements of subset are in the vector.
## multiplied with scalars are also in the vector.
## 0 vector is always subspace
## Example, in 2-D space, a line through the origin is subspace.
## In 3-D space, a plane through the origin is subspace.

# Basis
## Set of independent elements in span.
## The basis can generate rest of the data.

## Finding Basis
A <- matrix(c(1,3,2,3,11,2,-1,-2,-4,2,11,-6), nrow=4, byrow=T)
rref(A) # only the first 2 columns have pivots
# so the basis are
A[,1:2]

# Additionally, in n-D space, there are n basis.
# So, in R^3 space, there are 3 basis.

# Rank & Nullity
## Rank = the max # of linearly independent column or row vectors
## Nullity = free parameters or general solutions of Ax = 0
## In n x m matrix, rank + nullity = m.

# Determinants
## The scale of how much area changes after the linear transformation
## It only exists in square matrix.
det(A[-4,])
# Det(A) == 0 means the transformation puts them in a line.
# neg det = flipping 

# Eigen value and vectors
## Eigen vectors = certain vectors that stay the same even after transformation
## eigen value = tells whether eigen vectors are streched, shrunk, or reversed
## Ax = lambda*x
## det(A-lamda*I) = 0
## solve for lamda = eigen value
## Plug lamda in A- lambdi I and its rref is the eigen value.

# Diagonalization
## nxn matrix is diagonalizable if there exists an nxn diagonal matrix d and an 
## invertible matrix p such that A = PDP^-1
## D = eigen values of nxn matrix
## P = columns of P are eigven vectors

## Benefit of Diagonalization 
A <- matrix(c(5,-4,6,-5), nrow=2,byrow=T)
eigen(A)
P <- eigen(A)$vectors
D <- diag(eigen(A)$values)
P%*%D%*%solve(P)

## A^100 = A%*%A...A for 100 times
## But, with diagonlization, its P(D%*%D..100times)P^-1 -> much faster.

A2 <- A%*%A
A2
A3 <- A%*%A%*%A
A3

(P%*%P)%*%(D%*%D)%*%(solve(P)%*%solve(P))

## it is mainly used for dimensionality reduction like PCA

# Dot product and Orthogonal Sets
u <- matrix(c(2,4,-1),ncol=1)
v <- matrix(c(5,0,2),ncol=1)

sum(u*v) # dot product
## or
as.vector(u)%*%as.vector(v)
## or
dot(u,v)

## some equations
## the dot product of a & b = length of a * length of b * cos(theta)
## cos(theta) = (dot(a,b) / length(a)*length(b))

## some plots
a <- matrix(c(2,1),nrow=1)
b <- matrix(c(3,5), nrow=1)
plot(x="",y="", xlim=c(0,5),ylim=c(0,10))

## The angle of a: cos(theta) = 11 / (sqrt(5) * sqrt(34)) -> theta = cos^-1(11 / (sqrt(5) * sqrt(34))) 
## for angles do * 180/pi

acos(0.8436) * 180/pi 

## when two vectors are orthogonal, the dot product wil lbe 0 because they have 90 degrees in the angle.
## more evidence for pca
## Furthermore, orthogonal vectors are independent. 
