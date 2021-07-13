
library(rstan)
library(bayesplot)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")
head(file_beer_sales_2,n=3)

sample_size<- nrow(file_beer_sales_2)
sample_size

library(ggplot2)
ggplot(file_beer_sales_2,aes(x=temperature,y=sales))+
  geom_point()+
  labs(title="ビールと売り上げの関係")

data_list<-list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature
)

mcmc_result<-stan(
  file="3-2-2-simple-lm-vec.stan",
  data=data_list,
  seed=1
)

print(mcmc_result,probs=c(0.025,0.5,0.975))


mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)

mcmc_combo(
  mcmc_sample,
  pars=c("Intercept","beta","sigma")
)
