

library(rstan)
library(bayesplot)
library(ggfortify)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sales_df_5 <- read.csv("5-7-1-sales-ts-5.csv")
sales_df_5$date <- as.POSIXct(sales_df_5$date)
head(sales_df_5, n = 3)

autoplot(ts(sales_df_5[, -1]))


data_list <- list(
  y = sales_df_5$sales, 
  T = nrow(sales_df_5)
)

autoregressive <- stan(
  file = "5-7-1-autoregressive.stan",
  data = data_list,
  seed = 1,
  control = list(max_treedepth = 15)
)

print(autoregressive, 
      par = c("s_w", "b_ar", "Intercept", "lp__"),
      probs = c(0.025, 0.5, 0.975))

mcmc_rhat(rhat(autoregressive))
check_hmc_diagnostics(autoregressive)

mcmc_sample <- rstan::extract(autoregressive, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_w", "b_ar", "Intercept", "lp__"))


