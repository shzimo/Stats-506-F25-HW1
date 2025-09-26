x <- matrix(1:12, nrow = 6, ncol = 2)
x
dim(x)
dim(x) <- c(3, 2, 2)
class(x)
vec <- 101:124
dim(vec) <- c(4, 3, 2)
vec[5]

vec[3, 2, 1]
vec[1, 2]

vec[1:2, 1:2, 1:2]

vec

dim(vec) <- c(4, 6)
vec

vec[1: 2,]
vec[1,]
vec[1,,] #CONFUSED
vec[1, drop = FALSE]
vec[1,, drop = FALSE]

list(1, c("A", "b"), matrix())

mydat <- data.frame(id = 1:5, 
                    name = c("a", "b", "c", "d", "e"),
                    age = c(1, 2, 3, 4, 5))
mydat
str(mydat)
length(mydat) # three variables in there
class(mydat)
typeof(mydat)
mydat[,2]
mydat[1:2, 2]
mydat["name"]


c(3, 4, NA)
mean(c(3,4))
mean(c(3, 4, NA)) 
mean(c(3, 4, NA), na.rm = TRUE) 
mean(c(34, 4, NA), na.rm = TRUE) 
mean(c(3, 4, NA), na.rm = 4 == 2) 
mean(c(3, 4, NA), na.rm = 4 == 4) 

which(c(TRUE, FALSE, TRUE))
x <- c(4, 2, 6, 2, 3, 4)
x[x>3]
x[which(x>3)]
x > 3
which(x > 3)
y <- x 
mean(y, trim = 2)
mean(trim = 2, x = y)
mean(y, 0, FALSE)
y
mean(trim = 2, na.rm = TRUE, y) # dont do this
sum(1, 2, 3, FALSE)
sum(1, 2, NA, FALSE)
sum(1, 2, NA, na.rm = TRUE)

rescalar <- function(input) {
  output = (input - min(input) / (max(input) - min(input)))
  print("hi")
  return(output)
  print("bye")
}
rescalar(c(2, 5, 4, 2))
ls()

rescalar2 <- function(input) {
  output = (input - min(input) / (max(input) - min(input)))
  print("hi")
  print("bye")
  return(output)
}
rescalar2(c(2, 5, 4, 2))





rescaler_si <- function(input, na.rm = FALSE) 
# sanitize 'input'
{if(!is.numeric(input)) {
  stop("input must be a numeric vector")
}
# sanitize 'na.rm'
if (!is.logical(na.rm)) {
stop("na.rm must be logical")
}
if (is.na(na.rm)) {
  stop("na.rm must be TRUE or FALSE")
}
}


x <- c(2, 3, 4)
foo <- function(input) {
  z <- input^2
  return(z)
}

foo(x)



get100 <- function() {return(100)}

est_stat2 <- function(1000, rpois, median)

  xc <- runif(1, min = -1, max = 1)
yc <- runif(1, min = -1, max = 1)

sqrt(xc^2 + yc^2) <= 1

n <- 1000
xc <- runif(n, min = -1, max = 1)
yc <- runif(n, min = -1, max = 1)
pi <- 4*mean((sqrt(xc^2 + yc^2) <= 1))
pi

# slow version
system.time({
  reps <- 10000
  n <- 10000
  pi_dist <- c()
  for (i in seq_len(reps)) {
    xc <- runif(n, min = -1, max = 1)
    yc <- runif(n, min = -1, max = 1)
    pi_dist <- c(pi_dist, 4*mean((sqrt(xc^2 + yc^2) <= 1)))
  }
})
plot(density(pi_dist))

# faster version
system.time({
  reps <- 10000
  n <- 10000
  pi_dist <- rep(-999, reps)
  for (i in seq_len(reps)) {
    xc <- runif(n, min = -1, max = 1)
    yc <- runif(n, min = -1, max = 1)
    pi_dist[i] <- 4*mean((sqrt(xc^2 + yc^2) <= 1))
  }
})
plot(density(pi_dist))


pexp(.95)

n <- 10000
simdata <- rexp(n)
plot(density(simdata))


s2 <- sort(simdata)
s2[n*.05]
s2[n*.95]

mean(s2 <= 1.96)



reps <- 1000
n <- 15
lambda <- 4
covered <- rep(NA, times = reps)
lb <- rep(-999, times = reps)
ub <- rep(-999, times = reps)
for (i in seq_len(reps)) {
  sim <- rpois(n, lambda = lambda)
  
  m_sim <- mean(sim)
  sd_sim <- sd(sim)
  se_sim <- sd(sim)/sqrt(n-1)
  
  ci <- c(m_sim - 1.96*se_sim, m_sim + 1.96*se_sim)
  lb[i] <- ci[1]
  ub[i] <- ci[2]
  covered[i] <- ci[1] < lambda & ci[2] > lambda
}
mean(covered)


# version 2 - using apply
reps <- 1000
n <- 15
lambda <- 4
sim <- matrix(rpois(n*reps, lambda = lambda), ncol = reps)

mns <- apply(sim, 2, mean)
sds <- apply(sim, 2, sd)
se <- sds/sqrt(n-1)

lbs <- mns - 1.96*se
ubs <- mns + 1.96*se
mean(lbs < lambda & ubs > lambda)



# broadcasting

c(1, 2) + c(1, 2, 3, 4)
c(1, 0) * c(2, 5, 4, 2)



m <- matrix(c(5, 4, 2, 3, 4, 2), ncol = 3)
m

as.vector(m)


as.vector(m) * c(0, 1)
m * c(0, 1)

c(1, 2) * c(3, 4, 5)
m * c(0, 1, 100)


m
t(m)
t(t(m)*c(0, 1, 100))


apply(m, 2, mean)
colMeans(m)
rowMeans(m)

(m - rowMeans(m))/sd(m)
t((t(m) - rowMeans(t(m)))/sd(t(m)))


library(microbenchmark)
rows <- 50
cols <- 10000
x <- matrix(rnorm(rows*cols), nrow = rows)

v1 <- apply(x, 2, var)

# sample var: sum((x - xbar)^2)/(n - 1)

temp <- t(t(x) - colMeans(x))^2

v2 <- colSums(t(t(x) - colMeans(x))^2)/(rows - 1)

microbenchmark(apply = apply(x, 2, var),
               broadcast = colSums(t(t(x) - colMeans(x))^2)/(rows - 1))


# correlatrion between z and all x's
n <- 50
p <- 1000
z <- rnorm(n)
r <- .7
xmat <- matrix(rep(z, times = p), ncol = p)
xmat <- r*xmat + rnorm(n*p, sd = sqrt(1 - r^2))

wc <- cor(cbind(z, xmat))

microbenchmark(cor(cbind(z, xmat)))

corrs1 <- rep(-99, p)
for (i in seq_len(p)) {
  corrs1[i] <- cor(z, xmat[, i])
}

microbenchmark(forloop = {
  corrs1 <- rep(-99, p)
  for (i in seq_len(p)) {
    corrs1[i] <- cor(z, xmat[, i])
  }
})

corrs2 <- apply(xmat, 2, function(x) cor(x, z))
microbenchmark(apply = apply(xmat, 2, function(x) cor(x, z)))

colmns <- colMeans(xmat)
colsds <- apply(xmat, 2, sd)
xmat_std <- (xmat - rep(colmns, each = n))/rep(colsds, each = n)

zstd <- (z - mean(z))/sd(z)

c(1, 2) * c(3, 4)
c(1, 2) %*% c(3, 4)

corrs3 <- zstd %*% xmat_std / (n - 1)
corrs3[1]  
corrs1[1]


microbenchmark(linalg = {
  colmns <- colMeans(xmat)
  colsds <- apply(xmat, 2, sd)
  xmat_std <- (xmat - rep(colmns, each = n))/rep(colsds, each = n)
  zstd <- (z - mean(z))/sd(z)
  corrs3 <- zstd %*% xmat_std / (n - 1)
})


colmns <- colMeans(xmat)
xmat_c <- xmat - rep(colmns, each = n)
colsds <- sqrt(colSums(xmat_c^2)/(n-1))
xmat_std <- xmat_c / rep(colsds, each = n)
zstd <- (z - mean(z))/sd(z)
corrs4 <- zstd %*% xmat_std / (n - 1)
corrs4[1]

microbenchmark(apply = apply(xmat, 2, function(x) cor(x, z)),
               linalg2 = {
                 colmns <- colMeans(xmat)
                 xmat_c <- xmat - rep(colmns, each = n)
                 colsds <- sqrt(colSums(xmat_c^2)/(n-1))
                 xmat_std <- xmat_c / rep(colsds, each = n)
                 zstd <- (z - mean(z))/sd(z)
                 corrs4 <- zstd %*% xmat_std / (n - 1)
               })


y ~ x
y ~ x^2
mymod <- lm(wt ~., data = mtcars)
mymod
residuals(mymod)
predict(mymod)

lm(wt ~ mpg*mpg, mtcars)
lm(wt ~ mpg + mpg, mtcars)
I()
lm(wt ~ mpg + I(mpg*mpg), mtcars)
mymod <- lm(wt ~ mpg + I(mpg*mpg), mtcars)
mymod$coefficients["I(mpg * mpg)"]
lm(wt ~ mpg^2, mtcars)
lm(wt ~ mpg + mpgsq, data = mtcars)

poly()
mymod2 <- lm(wt ~ poly(mpg, degree = 2), mtcars)
summary(mymod2)

mymod3 <- lm(wt ~ poly(mpg, degree = 2, row = TRUE), mtcars)
summary(mymod3)

# doesn't work
# lm(wt ~ poly(mpg, degree = 3) - poly(...), data = mtcars)


lm(wt ~ mpg*gear, mtcars)
lm(wt ~ mpg*gear + mpg + mpg*gear, mtcars)

head(model.matrix(wt ~ mpg*gear, mtcars))

head(model.matrix(mymod, mtcars))

mymod4 <- lm(mpg ~ wt, data = mtcars, subset = gear > 4)
head(model.matrix(mpg ~ wt, data = mtcars))
head(model.matrix(mymod4, data = mtcars))



model.matrix(mpg ~ gear*wt, data = mtcars)
model.frame(mpg ~ gear*wt, data = mtcars)
model.response(model.frame(mpg ~ gear*wt, data = mtcars))


library(multcomp)
summary(mymod)
summary(glht(mymod, "(Intercept = 0)"))
summary(glht(mymod, "(Intercept + gear4 = 0)"))
summary(glht(mymod, "(Intercept + gear5 = 0)"))

mymod2 <- lm(mpg ~ wt + hp, data = mtcars)
mymod2
summary(glht(mymod2, "wt - hp = 0"))
summary(glht(mymod2, "wt - hp = -3.84"))

library(emmeans)
pairs(emmeans(mymod, "gear"), adjust = "none")

mymod <- lm(mpg ~ wt*hp, data = mtcars)
summary(emtrends(mymod, var = "wt"))

# Marginal effects
install.packages("marginaleffects")
install.packages("ggeffects")

emmip(mymod, hp ~ wt, at = list(hp = c(50, 150, 250)), wt = c(2, 3, 4))

# emmip(mymod, hp ~ wt, at = list(wt =
                                  
library(interactions)                          

myglm <- glm(vs ~ wt + gear, data = mtcars, family = binomial)
summary(myglm)

predict(myglm)
residuals(myglm)

emmeans(myglm, "gear")
names(mymod)
names(myglm)

library(lme4)
data(sleepstudy)
lm(Reaction ~ Days, data = sleepstudy)


