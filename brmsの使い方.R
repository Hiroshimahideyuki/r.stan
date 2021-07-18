install.packages("brms", dependencies = TRUE)

pkgbuild::find_rtools(debug = TRUE)
library(rstan)
library(brms)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")

simple_lm_brms<-brm(
  formula=sales~temperature,
  family=gaussian(link="identity"),
  data=file_beer_sales_2,
  seed=1
)

as.mcmc(simple_lm_brms,combine_chains=TRUE)

plot(simple_lm_brms)

simple_lm_formula<-bf(sales~temperature)

gaussian

binomial()

poisson()


simple_lm_brms_2<-brm(
  formula=simple_lm_formula,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  chains=4,
  iter=2000,
  warmup=1000,
  thin=1
)

prior_summary(simple_lm_brms)

get_prior

simple_lm_brms_3<-brm(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  prior=c(set_prior("",class="Intercept"),
          set_prior("",class="sigma"))
)

get_prior(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2
)

stancode(simple_lm_brms_3)
standata(simple_lm_brms_3)

standata_brms<-make_standata(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2
)
standata_brms

simple_lm_brms_stan<-stan(
  file="3-5-1-brms-stan-code.stan",
  data=stancoda_brms,
  seed=1
)

print(simple_lm_brms_stan,
      pars=c("b_Intercept","b[1]","sigma"),
      probs=c(0.025,0.5,0.975))

simple_lm_brms_3

mcmc_plot
mcmc_plot(simple_lm_brms,
         type="intervals",
         pars="^b_",
         prob=0.8,
         prob_outer=0.95)

new_data<-data.frame(temperature=20)
fitted(simple_lm_brms,new_data)

set.seed(1)
predict(simple_lm_brms,new_data)

mcmc_sample<-as.mcmc(simple_lm_brms,combine_chains=TRUE)
head(mcmc_sample,n=2)

mcmc_b_Intercept<-mcmc_sample[,"b_Intercept"]
mcmc_b_temperature<-mcmc_sample[,"b_temperature"]
mcmc_sigma<-mcmc_sample[,"sigma"]

saigen_fitted<-mcmc_b_Intercept+20*mcmc_b_temperature
mean(saigen_fitted)
quantile(saigen_fitted,probs=c(0.025,0.095))
fitted(simple_lm_brms,new_data)
         
set.seed(1)
saigen_predict<-do.call(
  rnorm,
  c(4000,list(mean=saigen_fitted,sd=mcmc_sigma))
)

mean(saigen_predict)
quantile(saigen_predict,probs=c(0.025,0.975))
set.seed(1)
predict(simple_lm_brms,data.frame(temperature=20))

eff<-marginal_effects(simple_lm_brms)
plot(eff,points=TRUE)

set.seed(1)
eff_pre<-marginal_effects(simple_lm_brms,method="predict")
plot(eff_pre,points=TRUE)

conditional_effects(simple_lm_brms,
                 effects="temperature")

marginal_effects(brms_model,
                 effects="x1:x2")

