---
title: "Log Normal Simulation"
author: "Brian Li"
date: "2018/11/18"
output: 
  html_document: 
    keep_md: yes
---




```r
# Assume we want to simulate 500 random variables (Y) that has a log normal distribution
# Y ~ LN(lognorm_mean, lognorm_sd)

# Specify mean of random variable
lognorm_mean <- 25

# Specify variance of random variable
lognorm_sd <- 16

# ln(Y) ~ N(norm_mean, norm_sd)

# Calculate 
norm_mean <- log(lognorm_mean/sqrt(1+lognorm_sd^2/lognorm_mean^2))
norm_sd <- sqrt(log(1+lognorm_sd^2/lognorm_mean^2))

x <- rlnorm(500, norm_mean, norm_sd)
upper <- ceiling(max(x))
hist(x, breaks = seq(0,upper, by = 1))
```

![](Log_Normal_Data_Simulation_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
mean(x)
```

```
## [1] 24.86802
```

```r
sd(x)
```

```
## [1] 16.9704
```