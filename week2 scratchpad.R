nosim <- 10000
n <- 10

means <- apply(matrix(rnorm(nosim * n), nosim), 1, mean)

sd(means)
1/sqrt(n)

#Thus, SD of enough number of means is 1/sqrt(number of samples) 


pbinom(2, size=5000, prob=0.001)
ppois(2, lambda=5000*0.001)

n <- 10000
means <- cumsum(rnorm(n))/(1:n)
plot(means)

binom.test(56, 100, conf.level=0.99)$conf.int


################## HOMEWORK ##################
pnorm(93, mean=100, sd=10)

round(qnorm(0.95, mean=100, sd=10), 3)

round(qnorm(0.95, mean=100, sd=2), 3)

0.5^6 * 7

round(pnorm(0.51, mean=0.5, sd=(sqrt(1/1200)), lower.tail=F), 3)

(2.5^2 + 1.5^2 + 0.5^2)/3

ppois(20, 2*16.5)

################## QUIZ ##################
round(pnorm(70, mean=80, sd=10) * 100, 0)
round(qnorm(0.95, mean=1100, sd=75), 0)
round(qnorm(0.95, mean=1100, sd=7.5), 0)
0.5^5 * 6
sqrt(1/1200)
ppois(10, 5*3)
