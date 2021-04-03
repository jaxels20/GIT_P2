library(distr)

library(boot)

#set.seed(12345)

n = 50
lambda = 4

ks_vector = NULL
for(i in 1:100){
  means_sample = NULL
  for (i in 1:1000) means_sample = c(means_sample, mean(rpois(n, lambda)))
  mean_difference = (means_sample - lambda)*sqrt(n)
  ks_full_result = ks.test(mean_difference, pnorm, 0,2)
  ks_p_value = ks_full_result$p.value
  ks_vector = c(ks_vector, ks_p_value)
}

hist(ks_vector, breaks = 16)








# her bliver der kørt 100ks test for at tjekke om det virker, der bliver 
#trukket Data der er normaltfordelt og vi forkaster alt for mange gange stadig.
ks_vector = NULL
for(i in 1:100){
  Data = rnorm(40, mean = 0, sd = 1)
  ks_full_result = ks.test(Data, pnorm, 0,2)
  ks_p_value = ks_full_result$p.value
  ks_vector = c(ks_vector, ks_p_value)
}

hist(ks_vector)











