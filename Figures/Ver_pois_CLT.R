library(dgof)  

library(distr)

library(Publish) #CI for mean

library(TeachingDemos) #CI for variance

set.seed(12345)

#This is verifying the CLT for a poisson distribution


#Parameters
n = 50
lambda = 4


#Making the mean_sample vector, which contains the mean of each sample drawn
#from the population
means_sample = NULL
for (i in 1:1000) means_sample = c(means_sample, mean(rpois(n, lambda)))

#Tokes Formula of CLT (new_vector):
mean_difference = (means_sample - lambda)*sqrt(n)


#Calculating the theoretical and emperical parameters
theoretical_mean = 0 
theoretical_sd = sqrt(lambda)

emperical_mean = mean(mean_difference)
emperical_sd = sqrt(var(mean_difference))


#Plotting the sample means with theoretical mean

hist(mean_difference, col="grey", main = "Sample Mean Vs. Theoretical Mean", prob=T)
curve(dnorm(x,theoretical_mean, theoretical_sd),col="blue",lty=2, lwd=2,add=T)
abline(v = emperical_mean, col = "green", lwd = 2)
abline(v=theoretical_mean, col="red", lwd=2)


#ks_testing



normal_ecdf = rnorm(1000, 0, 2)



ks.test(mean_difference, pnorm)


#Plotting the ecdf:

plot(ecdf(normal_ecdf),  
     xlim = range(c(normal_ecdf, mean_difference)),  
     col = "blue") 
plot(ecdf(mean_difference),  
     add = TRUE,  
     lty = "dashed", 
     col = "red") 


#Calculating confidence interval

#CI_mean = ci.mean(mean_difference)


#Ci_var = sigma.test(mean_difference)






