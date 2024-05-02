# source: "https://bookdown.org/egarpor/NP-UC3M/kde-ii-mult.html"

library("ks")
n <- 200
set.seed(35233)
x <- mvtnorm::rmvnorm(n = n, mean = c(0, 0),
                      sigma = rbind(c(1.5, 0.25), c(0.25, 0.5)))

# Compute kde for a diagonal bandwidth matrix (trivially positive definite)

x

cov(rnorm(100),rpois(100,3))


var(iris)

H <- diag(c(1.25, 0.75))
kde <- ks::kde(x = x, H = H)

?kde


kde
# The eval.points slot contains the grids on x and y
str(kde$eval.points)
## List of 2
##  $ : num [1:151] -8.58 -8.47 -8.37 -8.26 -8.15 ...
##  $ : num [1:151] -5.1 -5.03 -4.96 -4.89 -4.82 ...

# The grids in kde$eval.points are crossed in order to compute a grid matrix
# where to evaluate the estimate
dim(kde$estimate)
## [1] 151 151

# Manual plotting using the kde object structure
image(kde$eval.points[[1]], kde$eval.points[[2]], kde$estimate,
      col = viridis::viridis(20), xlab = "x", ylab = "y")
points(kde$x,col="red") # The data is returned in $x


#### done by me

iris.kde  <- ks::kde(iris[,1:2], H=diag(c(var(iris$Sepal.Length),var(iris$Sepal.Width))))
image(iris.kde$eval.points[[1]], iris.kde$eval.points[[2]], iris.kde$estimate)
points(iris.kde$x)

# Don't conduct kde on actual points. 

# Do not confuse "gridsize" with "bgridsize". The latter controls the internal
# grid size for binning the data and speeding up the computations (compare
# with binned = FALSE for a large sample size), and is not recommended to
# modify unless you know what you are doing. The binning takes place if
# binned = TRUE or if "binned" is not specified and the sample size is large

# Evaluating the kde at specific points can be done with "eval.points"
kde_sample <- ks::kde(x = x, H = H, eval.points = x)
str(kde_sample$estimate)
##  num [1:200] 0.0803 0.0332 0.0274 0.0739 0.0411 ...

# Assign colors automatically from quantiles to have an idea the densities of
# each one
n_cols <- 20
quantiles <- quantile(kde_sample$estimate, probs = seq(0, 1, l = n_cols + 1))
col <- viridis::viridis(n_cols)[cut(kde_sample$estimate, breaks = quantiles)]
plot(x, col = col, pch = 19, xlab = "x", ylab = "y")



###############################################################################################################
# Density derivative estimation
# Simulated univariate data
n <- 1e3
set.seed(324178)
library("tidyverse")
library("nor1mix")
x <- nor1mix::rnorMix(n = n, obj = nor1mix::MW.nm8) #random points mixed distribution


# Location of relative extrema
dens <- function(x) nor1mix::dnorMix(x, obj = nor1mix::MW.nm8)
minus_dens <- function(x) -dens(x)
extrema <- c(nlm(f = minus_dens, p = 0)$estimate,
             nlm(f = dens, p = 0.75)$estimate,
             nlm(f = minus_dens, p = 1.5)$estimate)

extrema

# with iris
#iris in 1 column
iris.merge <- coalesce(iris[,1],iris[,2],iris[,3],iris[,4])
hist(iris.merge)
nor1mix::dnorMix(iris.merge, obj= nor1mix::MW.nm12)
?nlm

# In the link, p= # of predictor, r= degrees of derivative


# Plot target density
par(mfrow = c(2, 2))
plot(nor1mix::MW.nm8, p.norm = FALSE)
rug(x)
abline(v = extrema, col = c(3, 2, 3))

# Density estimation (automatically chosen bandwidth)
kdde_0 <- ks::kdde(x = x, deriv.order = 0)
?ks::kkde
plot(kdde_0, xlab = "x", main = "Density estimation")
abline(v = extrema, col = c(3, 2, 3))

# Density derivative estimation (automatically chosen bandwidth, but different
# from kdde_0!)
kdde_1 <- ks::kdde(x = x, deriv.order = 1)
plot(kdde_1, xlab = "x", main = "Density derivative estimation")
abline(v = extrema, col = c(3, 2, 3))

# Density second derivative estimation
kdde_2 <- ks::kdde(x = x, deriv.order = 2)
plot(kdde_2, xlab = "x", main = "Density second derivative estimation")
abline(v = extrema, col = c(3, 2, 3))


# Simulated bivariate data
n <- 1e3
mu_1 <- rep(1, 2)
mu_2 <- rep(-1.5, 2)
Sigma_1 <- matrix(c(1, -0.75, -0.75, 3), nrow = 2, ncol = 2)
Sigma_2 <- matrix(c(2, 0.75, 0.75, 3), nrow = 2, ncol = 2)
w <- 0.45
set.seed(324178)
x <- ks::rmvnorm.mixt(n = n, mus = rbind(mu_1, mu_2),
                      Sigmas = rbind(Sigma_1, Sigma_2), props = c(w, 1 - w))

# Density estimation
kdde_0 <- ks::kdde(x = x, deriv.order = 0)
plot(kdde_0, display = "filled.contour2", xlab = "x", ylab = "y")

# Density derivative estimation
kdde_1 <- ks::kdde(x = x, deriv.order = 1)
str(kdde_1$estimate)
## List of 2
##  $ : num [1:151, 1:151] -4.76e-19 4.18e-19 1.18e-19 -6.27e-20 -3.74e-19 ...
##  $ : num [1:151, 1:151] -7.66e-19 -1.18e-18 -4.19e-19 -6.70e-19 -8.30e-19 ...
# $estimate is now a list of two matrices with each of the derivatives

# Plot of the gradient field - arrows pointing towards the modes
plot(kdde_1, display = "quiver", xlab = "x", ylab = "y")






?bw.nrd
