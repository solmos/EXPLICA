m = 14
v = 6^2
sig2 = log(v + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

x = exp(rnorm(100000, mu, sqrt(sig2)))
mean(x)
                                        # [1] 376.0851
var(x)
                                        # [1] 46050.79
sig2 = log(v + m^2) - 2*log(m)
mu = log(m) - sig2 / 2 

## https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/
location <- log(y_mean^2 / sqrt(y_sd^2 + y_mean^2))
shape <- sqrt(log(1 + (y_sd^2 / y_mean^2)))

sig2 <- log(y_sd^2 + m^2) - 2 * log(m)
mu = log(m) - sig2 / 2 
max_effect_log <- log(max_effect)
y_mean_log <- log(y_mean)
y_sd_log <- log(y_sd)
