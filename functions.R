simExpotypeData <- function(n, k, y.mean, y.sd, max.effect) {
  id <- 1:n
  expotypes <- LETTERS[1:k] %>%
    setNames(LETTERS[1:k])
  expotype <- sample(expotypes, size = n, replace = TRUE)
  expotypes_dummy <- map_dfc(expotypes, ~ expotype == .) %>%
    setNames(paste0("expotype", expotypes)) %>%
    mutate(across(.fns = as.integer)) %>%
    select(-expotypeA)

  beta0 <-y.mean
  beta1 <- max.effect
  beta_other <- sample(c(0, max.effect / 2, - max.effect / 2, max.effect),
                       size = k - 2,
                       replace = TRUE)

  betas <- c(beta1, beta_other)
  names(betas) <- paste0("beta", expotypes[-1])

  mu_y <- as.matrix(expotypes_dummy) %*% betas %>%
    .[, 1] + beta0


  y <- rnorm(n, mu_y, sd = y.sd)

  df <- bind_cols(id = id, y = y, expotype = expotype, expotypes_dummy)

  list(data = df, betas = betas)
}

simModels <- function(n, k, y.mean, y.sd, max.effect, n.sims) {

  smod <- vector("list", length = n.sims)
  p_values <- vector("list", length = n.sims)
  for (i in 1:n.sims) {
    fake <- simExpotypeData(n, k, y.mean, y.sd, max.effect)
    true_coef = c(y.mean, fake$betas)
    lmod <- lm(y ~ expotype, data = fake$data)
    smod[[i]] <- tidy(lmod) %>%
      mutate(
        true_coef = true_coef,
        n = n,
        k = k,
        max_effect = max.effect
      )
    p_value <- glance(lmod)$p.value
    p_values[[i]] <- tibble(
      p.value = p_value,
      n = n,
      k = k,
      max_effect = max.effect
    )

  }

  list(
    p_values = bind_rows(p_values, .id = "sim"),
    coef = bind_rows(smod, .id = "sim")
  )
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Fit models to simulated datasets and obtain p-values
##' @param n.sims Integer scalar with the number of simulations for each parameter combination.
##' @param y_mean Double scalar with the mean of the outcome variable.
##' @param y_sd Double scalar with the standard deviations of the outcome variable.
##' @param par_max_effect Double vector with maximum differences in outcome
##'   across two expotype groups. Used to form the parameter grid.
##' @param par_n Double vector with number of observations per simulated dataset.
##'   Used to form the parameter grid.
##' @param par_k Double vector with number of expotype categories.
##' @param cores Integer scalar with the number of cores to be used.
##' @return A list with p-values of F-statistics and parameter estimates.
##' @author Sergio
simPvalues <- function(n.sims, y_mean, y_sd,
                      par_max_effect, par_n = c(1000, 900, 800), par_k = 5:8,
                      cores = 4) {

  ## browser()
  param_grid <- expand.grid(n = par_n, k = par_k, max_effect = par_max_effect) %>%
    as_tibble()

  sims <- mcmapply(
    function(.x, .y, .z) {
      simModels(
        n = .x,
        k = .y,
        y.mean = y_mean,
        y.sd = y_sd,
        max.effect = .z,
        n.sims = n.sims
      )
    },
    param_grid$n,
    param_grid$k,
    param_grid$max_effect,
    mc.cores = 6,
    SIMPLIFY = FALSE
  )

  sims
}


calcPower <- function(pvalues) {

  p_values <- map_df(pvalues, ~ .$p_values)
  p_values %>%
    group_by(n, max_effect) %>%
    summarize(
      power = sum(p.value < 0.05) / n(),
      power_cat = case_when(
        power < 0.5 ~ 1,
        power >= 0.5 & power < 0.7 ~ 2,
        power >= 0.7 & power < 0.8 ~ 3,
        power > 0.8 ~ 4
      )
    )

}

plotPower <- function(power) {

  power %>%
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

}
