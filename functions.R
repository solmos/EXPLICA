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
