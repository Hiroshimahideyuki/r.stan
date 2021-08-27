

library(rstan)
library(bayesplot)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("plotSSM.R", encoding="utf-8")

sales_df_all <- read.csv("5-2-1-sales-ts-1.csv")
sales_df_all$date <- as.POSIXct(sales_df_all$date)

data_list_pred <- list(
  T = nrow(sales_df_all),
  y = sales_df_all$sales, 
  pred_term  = 20
)

local_level_pred <- stan(
  file = "5-3-1-local-level-pred.stan",
  data = data_list_pred,
  seed = 1
)

mcmc_rhat(rhat(local_level_pred))

print(local_level_pred,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


date_plot <- seq(
  from = as.POSIXct("2010-01-01"), 
  by = "days",
  len = 120)

seq(from = as.POSIXct("2010-01-01"), 
    by = 60*60*24,
    len = 120)

mcmc_sample_pred <- rstan::extract(local_level_pred)

plotSSM(mcmc_sample = mcmc_sample_pred, 
        time_vec = date_plot, 
        state_name = "mu_pred", 
        graph_title = "",
        y_label = "sales") 


sales_df_NA <- read.csv("5-3-1-sales-ts-1-NA.csv")

sales_df_NA$date <- as.POSIXct(sales_df_NA$date)


head(sales_df_NA, n = 3)


sales_df_omit_NA <- na.omit(sales_df_NA)
head(sales_df_omit_NA, n = 3)

nrow(sales_df_NA)

nrow(sales_df_omit_NA)

!is.na(sales_df_NA$sales)


which(c(TRUE, FALSE, TRUE))

which(!is.na(sales_df_NA$sales))


data_list_interpolation <- list(
  T       = nrow(sales_df_NA),
  len_obs = nrow(sales_df_omit_NA),
  y       = sales_df_omit_NA$sales, 
  obs_no  = which(!is.na(sales_df_NA$sales))
)

local_level_interpolation <- stan(
  file = "5-3-2-local-level-interpolation.stan",
  data = data_list_interpolation,
  seed = 1,
  iter = 4000
)

mcmc_rhat(rhat(local_level_interpolation))

print(local_level_interpolation,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


mcmc_sample_interpolation <- rstan::extract(
  local_level_interpolation)

plotSSM(mcmc_sample = mcmc_sample_interpolation, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "mu", 
        graph_title = "",
        y_label = "sales") 



local_level_prediction_interval <- stan(
  file = "5-3-3-local-level-interpolation-prediction-interval.stan",
  data = data_list_interpolation,
  seed = 1,
  iter = 4000
)

mcmc_rhat(rhat(local_level_prediction_interval))

print(local_level_prediction_interval,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))



mcmc_sample_prediction_interval <- rstan::extract(
  local_level_prediction_interval)


plotSSM(mcmc_sample = mcmc_sample_prediction_interval, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "y_pred", 
        graph_title = "",
        y_label = "sales") 



