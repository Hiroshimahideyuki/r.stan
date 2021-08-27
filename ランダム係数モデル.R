#ランダム係数モデル(モデルの係数がランダム効果によって増減する)の結果を交互作用を用いたモデルと比較
#過分散への対処・ランダム切片モデルと合わせて, ランダム効果のさまざまな組み込み方法について

#パッケージの読み込み
library(rstan)
library(brms)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
fish_num_climate_4 <- read.csv("4-3-1-fish-num-4.csv")
head(fish_num_climate_4, n = 3)

#データの要約
summary(fish_num_climate_4)

#交互作用を組み込んだポアソン回帰モデル
glm_pois_brms_interaction <- brm(
  formula = fish_num ~ temperature * human,
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)

#回帰直線を描く
#データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))
#図示
eff_1 <- marginal_effects(glm_pois_brms_interaction,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_1, points = TRUE)

#ランダム係数モデル
glmm_pois_brms_keisu <- brm(
  formula = fish_num ~ temperature + (temperature||human),
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  iter = 6000,
  warmup = 5000,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)

#データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))
#図示
eff_2 <- marginal_effects(glmm_pois_brms_keisu,
                          re_formula = NULL,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_2, points = TRUE)
