

x <- rnorm(100,3,5)
y <- c(sample(x,50), rnorm(50,4,7))


cor(x,y)


iris

pca <- prcomp(iris[,1:4])

pca$sdev
pca$rotation
pca$x


eig <- eigen(cov(scale(iris[,1:4])))


t(t(eig$vectors) %*% t(scale(iris[,1:4])))


prcomp(iris[,1:4], scale. = T)$x

