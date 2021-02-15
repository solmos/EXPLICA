library(parallel)

simParModels <- function(n, k, y.mean, y.sd, max.effect, nsims, cores = 4) {

  ## n <- 1000
  ## k <- 4
  ## y.mean <- 14
  ## y.sd <- 6
  ## max.effect <- 2
  ## cores <- 4
  ## nsims <- 100

  sims <- mclapply(
    1:nsims,
    function(i) simExpotypeData(n, k, y.mean, y.sd, max.effect),
    mc.cores = cores
  )

  sims_data <- map(sims, ~ .$data)
  sims_betas <- map_dfr(sims, ~ c(betaA = y.mean, .$betas), .id = "dataset")

  models <- map(sims_data, ~ lm(y ~ expotype, .))

  model_summaries <- map_df(models, tidy, .id = "dataset") %>%
    mutate(
      n = n,
      k = k,
      max_effect = max.effect,
    )

  p_values <- map_dbl(models, ~ glance(.)$p.value)

  model_pvalues <- tibble(
    dataset = 1:nsims,
    p_value = p_values,
    n = n, k = k, max_effect = max.effect
  )

  list(
    pvalues = model_pvalues,
    coef_estimates = model_summaries,
    coef_true = sims_betas
  )


}

param_grid <- expand.grid(
  k = 5:8,
  max_effect = seq(0.5, 2.5, by = 0.05)
) %>%
  as_tibble()

t <- Sys.time()
homocysteine_sims <- mcmapply(
  function(.x, .y) simModels(1000, .x, 14, 6, max.effect = .y, n.sims = 1000),
  param_grid$k,
  param_grid$max_effect,
  mc.cores = 6,
  SIMPLIFY = FALSE
)
Sys.time() - t

