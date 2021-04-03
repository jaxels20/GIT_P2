
# loading the required package
library(dgof) 
library(ggplot2)

set.seed(12345)


#Det her er prøve kode hvor to ens fordelinger bliver sammenlignet og deres p værdi bliver plottet.
ks_vector = NULL
Succes = 0
Failure = 0
number_of_tests = 100

for(i in 1:number_of_tests){
  Data = rnorm(30)
  y = rnorm(30)
  ks_full_result = ks.test(Data, y)
  if (ks_full_result$p.value > 0.05){
    Succes = Succes + 1 
  }
  else{
    Failure = Failure + 1
  }
}

barplot(c(Failure, Succes), names.arg=c("P < .05","P > .05"), ylim = c(0,number_of_tests))



#Det her er kode på vores poisson:
ks_vector = NULL
Succes = 0
Failure = 0
number_of_tests = 100
n = 30
lambda = 100

for(i in 1:number_of_tests){
  means_sample = NULL
  for (i in 1:1000) means_sample = c(means_sample, mean(rpois(n, lambda)))
  mean_difference = (means_sample - lambda)*sqrt(n)
  ks_full_result = ks.test(mean_difference, pnorm,0,sqrt(lambda))
  if (ks_full_result$p.value > 0.05){
    Succes = Succes + 1 
  }
  if(ks_full_result$p.value < 0.05){
    Failure = Failure + 1
  }
}

barplot(c(Failure, Succes), names.arg=c("P < .05","P > .05"), ylim = c(0,number_of_tests))





  