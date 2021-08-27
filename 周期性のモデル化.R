

library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("plotSSM.R", encoding="utf-8")


sales_df_4 <- read.csv("5-6-1-sales-ts-4.csv")
sales_df_4$date <- as.POSIXct(sales_df_4$date)
head(sales_df_4, n = 3)

autoplot(ts(sales_df_4[, -1]))


data_list <- list(
  y = sales_df_4$sales, 
  T = nrow(sales_df_4)
)

basic_structual <- stan(
  file = "5-6-1-basic-structual-time-series.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)

print(basic_structual, 
      par = c("s_z", "s_s", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))


mcmc_rhat(rhat(basic_structual))
check_hmc_diagnostics(basic_structual)

mcmc_sample <- rstan::extract(basic_structual, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_z", "s_s", "s_v", "lp__"))

options(max.print=100000)
print(basic_structual, probs = c(0.025, 0.5, 0.975))


mcmc_sample <- rstan::extract(basic_structual)

p_all <- plotSSM(mcmc_sample = mcmc_sample, 
                 time_vec = sales_df_4$date,
                 obs_vec = sales_df_4$sales,
                 state_name = "alpha", 
                 graph_title = "", 
                 y_label = "sales") 

p_trend <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_4$date,
        obs_vec = sales_df_4$sales,
        state_name = "mu", 
        graph_title = "", 
        y_label = "sales") 

p_cycle <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_4$date,
        state_name = "gamma", 
        graph_title = "", 
        y_label = "gamma") 

grid.arrange(p_all, p_trend, p_cycle)

