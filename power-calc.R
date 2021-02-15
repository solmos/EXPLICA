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
  par_max_effect = seq(2, 5, by = 0.05),
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


## readRDS("sims.rds")

sims <- sims_apob
coefs <- map_df(sims, ~ .$coef)
p_values <- map_df(sims, ~ .$p_values)


power_df <- p_values %>%
  group_by(n, max_effect, ) %>%
  summarize(
    power = sum(p.value < 0.05) / n(),
    power_cat = case_when(
      power < 0.5 ~ 1,
      power >= 0.5 & power < 0.7 ~ 2,
      power >= 0.7 & power < 0.8 ~ 3,
      power > 0.8 ~ 4
    )
  )

power_df %>%
  ggplot(aes(max_effect, power, group = n, color = n)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey37") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_color_continuous(name = "Number of expotype categories") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  ) +
  labs(
    title = "Power for different number of expotype categories",
    x = "Maximum difference",
    y = "Power"
  )

