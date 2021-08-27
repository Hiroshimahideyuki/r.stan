¿
library(rstan)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


germination_dat <- read.csv("3-9-1-germination.csv")
head(germination_dat, n = 3)

summary(germination_dat)

ggplot(data = germination_dat, 
       mapping = aes(x = nutrition, y = germination, color = solar)) +
  geom_point() +
  labs(title = "")


glm_binom_brms <- brm(
  germination | trials(size) ~ solar + nutrition, 
  family = binomial(),
  data = germination_dat,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)

glm_binom_brms


newdata_1 <- data.frame(
  solar = c("shade", "sunshine", "sunshine"),
  nutrition = c(2,2,3),
  size = c(10,10,10)
)
newdata_1

linear_fit <- fitted(glm_binom_brms, newdata_1, scale = "linear")[,1]
fit <- 1 / (1 + exp(-linear_fit))
fit

odds_1 <- fit[1] / (1 - fit[1])
odds_2 <- fit[2] / (1 - fit[2])
odds_3 <- fit[3] / (1 - fit[3])

coef <- fixef(glm_binom_brms)[,1]
coef

odds_2 / odds_1
exp(coef["solarsunshine"])


odds_3 / odds_2
exp(coef["nutrition"])


eff <- marginal_effects(glm_binom_brms, 
                        effects = "nutrition:solar")

plot(eff, points = TRUE)



plot(glm_binom_brms, pars = "^b_")

stanplot(glm_binom_brms, type = "intervals", pars = "^b_")

set.seed(1)
eff_pre <- marginal_effects(glm_binom_brms, 
                            method = "predict",
                            effects = "nutrition:solar")
plot(eff_pre, points = TRUE)




solar_dummy <- as.numeric(germination_dat$solar == "sunshine")


data_list_1 <- list(
  N = nrow(germination_dat),
  germination = germination_dat$germination,
  binom_size = germination_dat$size,
  solar = solar_dummy,
  nutrition = germination_dat$nutrition
)
data_list_1


glm_binom_stan <- stan(
  file = "3-9-1-glm-binom-1.stan",
  data = data_list_1,
  seed = 1
)


print(glm_binom_stan,
      probs = c(0.025, 0.5, 0.975))


