library(manipulate)
library(ggplot2)
k <- 1000
xvals <- seq(-5, 5, length=k)

myplot <- function(df) {
    d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                    x = xvals,
                    dist = factor(rep(c("Normal", "T"), c(k, k))))
    g <- ggplot(d, aes(x=x, y=y))
    g <- g + geom_line(size=2, aes(color=dist))
    g
}
manipulate(myplot(mu), mu = slider(1, 20, step=1))


pvals <- seq(0.5, 0.99, by=0.01)

myplot2 <- function(df) {
    d <- data.frame(n = qnorm(pvals), t = qt(pvals, df), p = pvals)
    g <- ggplot(d, aes(n, t))
    g <- g + geom_abline(size=2, color="blue")
    g <- g + geom_line(size=2, color="black")
    g <- g + geom_vline(xintercept=qnorm(0.975))
    g <- g + geom_hline(yintercept=qt(0.975, df))
    g
}
manipulate(myplot2(df), df = slider(1, 20, step=1))

###############################################################################
data(sleep)
head(sleep)


########################### HOMEWORK ###########################################
data(mtcars)
head(mtcars)
dim(mtcars)

mpgMean <- mean(mtcars$mpg)
mpgSD <- sd(mtcars$mpg)
qnorm(0.975, mean=mpgMean, sd=mpgSD/sqrt(dim(mtcars)[1])) # normal distr
qnorm(0.025, mean=mpgMean, sd=mpgSD/sqrt(dim(mtcars)[1])) # normal distr
t.test(mtcars$mpg) # t distr

round(qt(0.975, 8)/sqrt(9), 2)

# This is creating data frames and messing up t.test calculations
cyl4 <- subset(mtcars, cyl==4, select=mpg)
cyl6 <- subset(mtcars, cyl==6, select=mpg)

cyl4 <- mtcars$mpg[mtcars$cyl==4]
cyl6 <- mtcars$mpg[mtcars$cyl==6]

as.vector(t.test(cyl4, cyl6, var.equal=T)$conf.int)
mean(c(1.5^2, 1.8^2))

############################### QUIZ ###########################################
t.test()

round(2 * sqrt(9) / qt(0.975, 8), 2)

sp <- sqrt(mean(c(0.68, 0.6)))
round(3 - 5 +c(-1, 1) * qt(0.975, 18) * sp * (1/10 + 1/10)^0.5, 2)

sp <- sqrt(mean(c(1.5^2, 1.8^2)))
round(-3 - 1 +c(-1, 1) * qt(0.95, 16) * sp * (1/9 + 1/9)^0.5, 3)

############################### PROJECT ########################################

lambda <- 0.2
n <- 40
nosim <- 10000

#create the mean and standard deviation vectors
expMeans <- vector()
expSD <- vector()
for (i in 1:nosim) {
    expMeans[i] <- mean(rexp(n, lambda))
    expSD[i] <- sd(rexp(n, lambda))
}

# part 3: overlay the normal distribution curve
# and show that some of the known quantiles match
# hist((2*pi*5^2)^(-1/2) * exp(1)^((-(expMeans - 5)^2 / 2 * 5^2)), breaks=100)

rnormDist <- rnorm(nosim, 5, 5/sqrt(40))

par(mfrow=c(2, 1))
hist(expMeans, breaks=100, xlim=c(3,7))
hist(rnormDist, breaks=100, xlim=c(3,7))


length(expMeans[expMeans > 5-5/sqrt(40) & expMeans < 5+5/sqrt(40)])


# part 4
p2 <- 1.684 * expSD / sqrt(40)
p2 <- 1.96 * expSD / sqrt(40)

cov <- cbind(expMeans - p2, expMeans + p2)
dim(cov[cov[, 1]>5 | cov[, 2]<5, ])
dim(cov[cov[, 2]<5, ])
dim(cov[cov[, 1]>5,])

pt(1.684, 40)
2*pt(1.684, 40, lower.tail=F)
2*pt(1.96, 40, lower.tail=F)
