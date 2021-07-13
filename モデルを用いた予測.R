library(rstan)
library(bayesplot)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")
sample_size<-nrow(file_beer_sales_2)

temperature_pred<-11:30
temperature_pred

data_list_pred<-list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature,
  N_pred=length(temperature_pred),
  temperature_pred=temperature_pred
)

mcmc_result_pred<-stan(
  file="3-3-1-simple-lm-pred.stan",
  data=data_list_pred,
  seed=1
)

print(mcmc_result_pred,probs=c(0.025,0.5,0.975))

mcmc_sample_pred<-rstan::extract(mcmc_result_pred,permuted=FALSE)

mcmc_intervals(
  mcmc_result_pred,
  regex_pars=c("sales_pred."),
  prbs=0.8,
  prob_outer=0.95
)

mcmc_intervals(
  mcmc_sample_pred,
  paars=c("mu_pred[1]","sales_pred[1]"),
  probs=0.8,
  probs_outer=0.95
)

mcmc_areas(
  mcmc_sample_pred,
  pars=c("sales_pred[1]","sales_pred[20]"),
  probs=0.6,
  prob_outer=0.99
)

