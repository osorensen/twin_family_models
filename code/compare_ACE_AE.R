compare_ACE_AE <- function(mod, mod_AE){
  tibble(
    npar = length(c(getME(mod, "beta"), getME(mod, "theta"), getME(mod, "sigma")))
  ) %>% 
    mutate(npar = map(npar, ~ c(.x - 1, .x))) %>% 
    unnest(cols = npar) %>% 
    mutate(
      model = c("mod_AE", "mod"),
      logLik = as.numeric(c(logLik(mod_AE), logLik(mod))),
      AIC = - 2 * logLik + 2 * npar,
      BIC = - 2 * logLik + npar * log(2000),
      deviance = - 2 * logLik,
      Df = npar - lag(npar),
      Chisq = lag(deviance) - deviance,
      `Pr(>Chisq)` = 1 - map2_dbl(Chisq, Df, ~ pchisq(.x, df = .y))
    ) %>% 
    select(model, npar, AIC, BIC, logLik, deviance, Chisq, Df, `Pr(>Chisq)`)
}