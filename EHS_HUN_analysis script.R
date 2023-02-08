
### This is the analysis script for the EHS-HUN project

### Load packages

library(psych) # for describe
library(car) # qqPlot
library(MASS) # for mvrnorm
library(tidyverse)

### Import data

## This is code to simulate data. This code should be replaced by the real data importing code when real data is available.
# ------------------------------------------------------------------------------------------
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

my_data =   simulation_function(
            n = 80,
            mean_scale_A = 5.239,
            SD_scale_A = 2.922,
            mean_scale_B = 6.474,
            SD_scale_B = 2.525,
            corr_A_B = 0.7, # simulated true correlation between the scales in the population
            varnames_sim_pre = c("scale_A", "scale_B"))
# ------------------------------------------------------------------------------------------

### Repeate the procedure below for both sutdy arms

# Check normality assumption (skew and kurtosis below -1 and above 1 indicate non-normality)

describe(my_data$scale_A)$skew
describe(my_data$scale_A)$kurtosis
qqPlot(my_data$scale_A)

describe(my_data$scale_B)$skew
describe(my_data$scale_B)$kurtosis
qqPlot(my_data$scale_B)

# compute test statistics

cor_test_result = cor.test(my_data$scale_A, my_data$scale_B, conf.level = 0.90)
CI_lb = cor_test_result$conf.int[1]
r = cor_test_result$estimate

# test_hypothesis

CI_lb > 0.5 

# if H0 is rejected, base conclusion on r and CI

r 
CI_lb
