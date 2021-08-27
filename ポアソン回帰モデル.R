#ポアソン回帰モデルの推定
#確率分布とリンク関数の変更

#パッケージの読み込み
library(rstan)
library(brms)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
fish_num_climate <- read.csv("3-8-1-fish-num-1.csv")
head(fish_num_climate, 3)
#データの要約
summary(fish_num_climate)

#図示
ggplot(data = fish_num_climate, 
       mapping = aes(x = temperature, y = fish_num)) +
  geom_point(aes(color = weather)) +
  labs(title = "釣獲尾数と気温・天気の関係")

#ポアソン回帰モデルを作る
glm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature,
  family = poisson(),
  data = fish_num_climate,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)

#mcmcの結果確認
glm_pois_brms

#回帰曲線の図示(95%ベイズ信用区間付き)
eff <- marginal_effects(glm_pois_brms, 
                        effects = "temperature:weather")
plot(eff, points = TRUE)
#予測区間の図示
set.seed(1)
eff_pre <- marginal_effects(glm_pois_brms, 
                            method = "predict",
                            effects = "temperature:weather",
                            probs = c(0.005, 0.995))
plot(eff_pre, points = TRUE)

#デザイン行列の作成
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate)
design_mat


data_list_1 <- list(
  N = nrow(fish_num_climate),
  fish_num = fish_num_climate$fish_num,
  temp = fish_num_climate$temperature,
  sunny = as.numeric(design_mat[, "weathersunny"])
)
data_list_1


glm_pois_stan_exp <- stan(
  file = "3-8-1-glm-pois-1.stan",
  data = data_list_1,
  seed = 1
)


print(glm_pois_stan_exp,
      probs = c(0.025, 0.5, 0.975))



glm_pois_stan <- stan(
  file = "3-8-2-glm-pois-2.stan",
  data = data_list_1,
  seed = 1
)


print(glm_pois_stan,
      probs = c(0.025, 0.5, 0.975))



data_list_2 <- list(
  N = nrow(fish_num_climate),
  K = 3,
  Y = fish_num_climate$fish_num,
  X = design_mat
)
data_list_2


glm_pois_stan_design_mat <- stan(
  file = "3-8-3-glm-pois-design-matrix.stan",
  data = data_list_2,
  seed = 1
)


print(glm_pois_stan_design_mat,
      probs = c(0.025, 0.5, 0.975))



