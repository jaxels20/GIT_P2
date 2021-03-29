library(fGarch)
library(distr)

#Unbiased and biased figure

x<-seq(-2.5, +2.5, by=0.001)
plot(x,
     fGarch::dsstd(x, mean = 0, sd = 1, nu = 4, xi = 1.8),
     type = "l",
     ylim=c(-0.1, 1.8), lty = 1,
     xlab="",
     ylab="",
     axes=FALSE,
     frame.plot=FALSE,
     main = "Biased and unbiased distribution")

lines(x,
      fGarch::dsstd(x, mean = -0.3, sd = 1, nu = 2.11, xi = 1.27),
      type = "l",
      ylim=c(0, 1.8))

lines(x,
      fGarch::dsstd(x, mean = 0, sd = 1, nu = 2.1, xi = 1),
      type = "l",
      ylim=c(0, 1.8))

abline(h = 0)
points(x=0, y=0, col="black", pch="|")
text(x=0, y=-0.1, labels=expression(paste(mu)), adj=0.5)

arrows(x0=-1, y0= 1.3, x1=-0.6, y1=1.3, length=0.08, angle=50)
text(x=-2.7, y=1.3, labels="Sampling distribution", adj=0, family="serif")
text(x=-2.7, y=1.18, labels="of sample median (biased)", adj=0, family="serif")

arrows(x0=0.6, y0= 1.3, x1=0.2, y1=1.3, length=0.08, angle=50)
text(x=0.7, y=1.3, labels="Sampling distribution of", adj=0, family="serif")
text(x=0.7, y=1.18, labels=expression(paste(bar(y), " (unbiased)")), adj=0, family="serif")

arrows(x0=1.2, y0= 0.4, x1=0.9, y1=0.2, length=0.08, angle=50)
text(x=1.1, y=0.46, labels="Population distribution", adj=0, family="serif")


#Empirical rule figure
x = seq(-4,4, length=200)
N = dnorm(x)
par(mar=(c(9, 4.1, 4.1, 2.1)))
plot(x, N, type="l", xaxt="n", yaxt="n", xlab="", ylab="", main = "The empirical rule")
axis(side=1, at=c(-3,-2,-1,0,1,2,3), labels=c(expression(mu-3*sigma),
                                              expression(mu-2*sigma),
                                              expression(mu-1*sigma),
                                              expression(mu),
                                              expression(mu+1*sigma),
                                              expression(mu+2*sigma),
                                              expression(mu+3*sigma)))
lines(x=c(0,0), y=c(-0.025,0.3988))
lines(x=c(-1,-1), y=c(-0.025,0.2427))
lines(x=c(-2,-2), y=c(-0.025,0.0534))
lines(x=c(-3,-3), y=c(-0.025,0.0039855305))
lines(x=c(1,1), y=c(-0.025,0.2427))
lines(x=c(2,2), y=c(-0.025,0.0534))
lines(x=c(3,3), y=c(-0.025,0.0039855305))

text(x=c(-0.5, 0.5, -1.5, 1.5, -2.5, 2.5, -3.5, 3.5),
     y=c(0.2, 0.2, 0.05, 0.05, -0.003, -0.003, 0.015, 0.015),
     labels=c("34%","34%","13.5%","13.5%","2.35%","2.35%","0.15%","0.15%"),
     adj=0.5, family="serif")

arrows(x0=-1, y0=-0.1, x1=1, y1=-0.1, length=0.1, angle=90, code=3, xpd=TRUE)
arrows(x0=-2, y0=-0.17, x1=2, y1=-0.17, length=0.1, angle=90, code=3, xpd=TRUE)
arrows(x0=-3, y0=-0.24, x1=3, y1=-0.24, length=0.1, angle=90, code=3, xpd=TRUE)

text(x=c(0,0,0), y=c(-0.12,-0.19,-0.26), labels=c("68%","95%","99.7%"),
     adj=0.5,family="serif", xpd=TRUE)


#Sum of Poisson distributions:
P1 = Pois(1)
plot(P1, to.draw.arg="d", ylab="Probability Mass",
     inner=FALSE,
     tmar=3)
title(main=expression(paste("Probability mass function of Poisson distribution, ", lambda, "=1")))

P2 = P1 + P1
plot(P2, to.draw.arg="d", ylab="Probability Mass",
     inner=FALSE,
     tmar=3)
title(main=expression(paste("Probability mass function of Poisson distribution, ", lambda, "=2")))


P3 = P2 + P1
plot(P3, to.draw.arg="d", ylab="Probability Mass",
     inner=FALSE,
     tmar=3)
title(main=expression(paste("Probability mass function of Poisson distribution, ", lambda, "=3")))

P5 = P3 + P2
plot(P5, to.draw.arg="d", ylab="Probability Mass",
     inner=FALSE,
     tmar=3)
title(main=expression(paste("Probability mass function of Poisson distribution, ", lambda, "=5")))