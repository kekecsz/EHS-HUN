

library(tidyverse)
library(MASS) # for mvrnorm

simulation_function <- function(n, mean_scale_A, SD_scale_A, mean_scale_B, SD_scale_B, corr_A_B, varnames_sim_pre){
  
  # Simulating data
  
  data_sim_pre = as.data.frame(
    mvrnorm(n = n,
            mu = rep(0, length(varnames_sim_pre)),
            Sigma = matrix(c(  1,corr_A_B,
                               corr_A_B, 1), nrow = length(varnames_sim_pre)))
  )
  
  
  names(data_sim_pre) = varnames_sim_pre
  
  # Bringing data to realistic scale
  # Based on data from Kekecs et al. 2021 test-retest reliability study
  
  data_sim_pre[, "scale_A"] = data_sim_pre[, "scale_A"] * SD_scale_A + mean_scale_A
  data_sim_pre[, "scale_B"] = data_sim_pre[, "scale_B"] * SD_scale_B + mean_scale_B
  
  return(data_sim_pre)
}

test_function = function(n, mean_scale_A, SD_scale_A, mean_scale_B, SD_scale_B, corr_A_B, varnames_sim_pre){
  my_data = simulation_function(
    n = n,
    mean_scale_A = mean_scale_A,
    SD_scale_A = SD_scale_A,
    mean_scale_B = mean_scale_B,
    SD_scale_B = SD_scale_B,
    corr_A_B = corr_A_B, # simulated true correlation between the scales in the population
    varnames_sim_pre = varnames_sim_pre
  )
  
  cor_test_result = cor.test(my_data$scale_A, my_data$scale_B, conf.level = 0.90)
  CI_lb = cor_test_result$conf.int[1]
  r = cor_test_result$estimate
  
  return(c(CI_lb, r))
}

iter = 10000

res = replicate(iter,
          test_function(
            n = 80,
            mean_scale_A = 5.239,
            SD_scale_A = 2.922,
            mean_scale_B = 6.474,
            SD_scale_B = 2.525,
            corr_A_B = 0.7, # simulated true correlation between the scales in the population
            varnames_sim_pre = c("scale_A", "scale_B"))

)

sum(t(res)[,1]>0.5)/length(t(res)[,1])
sum(t(res)[,2]>0.65)/length(t(res)[,2])


res = replicate(iter,
                test_function(
                  n = 50,
                  mean_scale_A = 5.239,
                  SD_scale_A = 2.922,
                  mean_scale_B = 5.239,
                  SD_scale_B = 2.922,
                  corr_A_B = 0.75, # simulated true correlation between the scales in the population
                  varnames_sim_pre = c("scale_A", "scale_B"))
                
)

sum(t(res)[,1]>0.5)/length(t(res)[,1]) # power to reject null hypothesis
sum(t(res)[,2]>0.6)/length(t(res)[,2]) # power to detect acceptable correlation

res = replicate(iter,
                test_function(
                  n = 80,
                  mean_scale_A = 5.239,
                  SD_scale_A = 2.922,
                  mean_scale_B = 6.474,
                  SD_scale_B = 2.525,
                  corr_A_B = 0.5, # simulated true correlation between the scales in the population
                  varnames_sim_pre = c("scale_A", "scale_B"))
                
)

sum(t(res)[,1]>0.5)/length(t(res)[,1]) # alpha to falsely reject null hypothesis



res = replicate(iter,
                test_function(
                  n = 50,
                  mean_scale_A = 5.239,
                  SD_scale_A = 2.922,
                  mean_scale_B = 5.239,
                  SD_scale_B = 2.922,
                  corr_A_B = 0.5, # simulated true correlation between the scales in the population
                  varnames_sim_pre = c("scale_A", "scale_B"))
                
)

sum(t(res)[,1]>0.5)/length(t(res)[,1]) # alpha to falsely reject null hypothesis


