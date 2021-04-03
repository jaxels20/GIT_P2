library(distr)

n = 25   #Number of convolutions

lambda = 1

x = seq(from = 0, to = lambda*2, by = 1/(25/lambda))   #Interval for plot

pois = Pois(lambda)

Y = (1/n)*convpow(pois, n)

ks.test(p(Y)(x), pnorm, lambda, sd = sqrt(lambda)/sqrt(n))

plot(x, p(Y)(x))
lines(x, p(Norm(mean=lambda, sd=sqrt(lambda)/sqrt(n)))(x), col = "red")
