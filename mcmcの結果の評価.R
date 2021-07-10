


install.packages("rstan")
install(rstan)

pkgbuild::has_build_tools(debug=TRUE)
library(rstan)


mcmc_result<-stan(
  file="2-4-1-calc-mean-variance.stan", 
  data=data_list, 
  seed=1, 
  chains=4, 
  iter=2000, 
  warmup=1000, 
  thin=1 
)


mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)


class(mcmc_sample)


dim(mcmc_sample)

dimnames(mcmc_sample)

mcmc_sample[1,"chain:1","mu"]

mcmc_sample[,"chain:1","mu"]

length(mcmc_sample[,"chain:1","mu"])

length(mcmc_sample[,,"mu"])

dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])

mu_mcmc_vec<-as.vector(mcmc_sample[,,"mu"])

median(mu_mcmc_vec)
mean(mu_mcmc_vec)

quantile(mu_mcmc_vec,probs=c(0.025,0.975))

print(
  mcmc_result,
  probs=c(0.025,0.5,0.975)
)

install.packages("ggfortify")

library(ggfortify)
autoplot(ts(mcmc_sample[,,"mu"]),
         facets=F,
         ylab="mu",
         main="トレースプロット")

install.packages("ggplot2")

library("ggplot2")
mu_df<-data.frame(
  mu_mcmc_sample=mu_mcmc_vec
)
ggplot(data=mu_df,mapping=aes(x=mu_mcmc_sample))+
  geom_density(size=1.5)

install.packages("bayesplot")

library(bayesplot)

mcmc_hist(mcmc_sample,pars=c("mu","sigma"))

mcmc_dens(mcmc_sample,pars=c("mu","sigma"))

mcmc_trace(mcmc_sample,pars=c("mu","sigma"))

mcmc_combo(mcmc_sample,pars=c("mu","sigma"))

mcmc_intervals(
  mcmc_sample,pars=c("mu","sigma"),
  prob=0.8,
  prob_outer=0.95
)

mcmc_areas(mcmc_sample,pars=c("mu","sigma"),
           prob=0.6,
           prob_outer=0.99
           )

mcmc_acf_bar(mcmc_sample,pars=c("mu","sigma"))

animal_num<-read.csv("2-5-1-animal-num.csv")
head(animal_num,n=3)

sample_size<-nrow(animal_num)

data_list<-list(animal_num=animal_num$animal_num,N=sample_size)

library(rstan)
mcmc_normal<-stan(
  file="2-5-1-normal-dist.stan",
  data=data_list,
  seed=1
)

mcmc_poisson<-stan(
  file="2-5-2-poisson-dist.stan",
  data=data_list,
  seed=1
)

y_rep_normal<-rstan::extract(mcmc_normal)$pred
y_rep_poisson<-rstan::extract(mcmc_poisson)$pred

dim(y_rep_normal)
y_rep_normal[1,]
y_rep_poisson[1,]

hist(animal_num$animal_num)
hist(y_rep_normal[1,])
hist(y_rep_poisson[1,])

ppc_hist(y=animal_num$animal_num,
         yrep=y_rep_normal[1:5,])

ppc_hist(y=animal_num$animal_num,
         yrep=y_rep_poisson[1:5,])

