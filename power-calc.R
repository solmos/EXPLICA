library(tidyverse)
library(broom)
library(usethis)

source("functions.R")

param_grid_homo <- expand.grid(
  k = 5:8,
  n = c(1000, 900, 800),
  max_effect = seq(0.5, 2.5, by = 0.05)
) %>%
  as_tibble()

t <- Sys.time()
homocysteine_sims <- mcmapply(
  function(.x, .y) simModels(
                     n = 1000, k = .x, 14, 6, max.effect = .y, n.sims = 1000),
  param_grid$k,
  param_grid$max_effect,
  mc.cores = 6,
  SIMPLIFY = FALSE
)
Sys.time() - t

param_grid <- expand.grid(
  k = 5:8,
  max_effect = seq(0.5, 2.5, by = 0.05)
) %>%
  as_tibble()

t <- Sys.time()
homocysteine_sims <- map2(
  param_grid$k,
  param_grid$max_effect,
  ~ simModels(1000, .x, 14, 6, max.effect = .y, n.sims = 1000)
)
Sys.time() - t

## saveRDS(sims, "sims.rds")

readRDS("sims.rds")

sims <- homocysteine_sims
coefs <- map_df(sims, ~ .$coef)
p_values <- map_df(sims, ~ .$p_values)

coefs %>%
  filter(term != "(Intercept)") %>%
  group_by(sim)

power_df <- p_values %>%
  group_by(k, max_effect) %>%
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
  ggplot(aes(max_effect, power, group = k)) +
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

df %>%
  ggplot(aes(k, max_effect, fill = power_cat)) +
  geom_tile() +
  scale_fill_viridis_c()

sims <- simModels(
  n = 1000,
  k = 4,
  y.mean = 14,
  y.sd = 6,
  max.effect = 1,
  n.sims = 1000
)

m$model
max_effects
summary(m)
glance(m)

signif_pars <- sims %>%
  filter(term != "(Intercept)") %>%
  group_by(sim) %>%
  summarize(signif_params = sum(p.value < 0.05))

sum(signif_pars$signif_params > 0) / nrow(signif_pars)

bind_rows(sims, .id = "sim")
