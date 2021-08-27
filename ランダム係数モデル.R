
library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fish_num_climate_4 <- read.csv("4-3-1-fish-num-4.csv")
head(fish_num_climate_4, n = 3)


summary(fish_num_climate_4)

«
glm_pois_brms_interaction <- brm(
  formula = fish_num ~ temperature * human,
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)


glm_pois_brms_interaction

stanplot(glm_pois_brms_interaction, type = "rhat")



conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))


eff_1 <- marginal_effects(glm_pois_brms_interaction,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_1, points = TRUE)


glmm_pois_brms_keisu <- brm(
  formula = fish_num ~ temperature + (temperature||human),
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  iter = 6000,
  warmup = 5000,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)



glmm_pois_brms_keisu


plot(glmm_pois_brms_keisu)


prior_summary(glmm_pois_brms_keisu)

stanplot(glmm_pois_brms_keisu, type = "rhat")


conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))


eff_2 <- marginal_effects(glmm_pois_brms_keisu,
                          re_formula = NULL,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_2, points = TRUE)



