library(tidyverse)
library(broom)
library(parallel)

source("functions.R")


sims_homocysteine <- simPvalues(
  n.sims = 1000,
  y_mean = 14,
  y_sd = 6.1,
  par_max_effect = seq(0.5, 2.5, by = 0.05),
  par_n = c(800, 900, 1000),
  par_k = 5:8,
  cores = 6
)
saveRDS(sims_homocysteine, "sims_homocysteine.rds")

sims_apob <- simPvalues(
  n.sims = 1000,
  y_mean = 110,
  y_sd = 31.5,
  par_max_effect = seq(2, 10, by = 0.05),
  par_n = c(800, 900, 1000),
  par_k = 5:8,
  cores = 6
)
saveRDS(sims_apob, "sims_apob.rds")

sims_hs <- simPvalues(
  n.sims = 1000,
  y_mean = 3.25,
  y_sd = 5.25,
  par_max_effect = seq(0, 2, by = 0.05),
  par_n = c(800, 900, 1000),
  par_k = 5:8,
  cores = 6
)
saveRDS(sims_hs, "sims_hs.rds")

calcPower(sims_hs) %>%
  plotPower()

## readRDS("sims.rds")

