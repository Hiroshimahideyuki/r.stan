
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("plotSSM.R", encoding="utf-8")


fish_ts <- read.csv("5-9-1-fish-num-ts.csv")
fish_ts$date <- as.POSIXct(fish_ts$date)
head(fish_ts, n = 3)

autoplot(ts(fish_ts[, -1]))

data_list <- list(
  y = fish_ts$fish_num, 
  ex = fish_ts$temperature, 
  T = nrow(fish_ts)
)

dglm_poisson <- stan(
  file = "5-9-1-dglm-poisson.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6, 
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)



print(dglm_poisson, 
      par =  c("s_z", "s_r", "b", "lp__"),
      probs = c(0.025, 0.5, 0.975))

mcmc_rhat(rhat(dglm_poisson))
check_hmc_diagnostics(dglm_poisson)

mcmc_sample <- rstan::extract(dglm_poisson, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_z", "s_r", "lp__"))

options(max.print=100000)
print(dglm_poisson, probs = c(0.025, 0.5, 0.975))


mcmc_sample <- rstan::extract(dglm_poisson)

p_all <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_exp", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 

p_smooth <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_smooth", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 

p_fix <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_smooth_fix", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 


grid.arrange(p_all, p_smooth, p_fix)







