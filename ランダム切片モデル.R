#ランダム効果の具体的な使い方のイメージ
#サンプリングの方法によってはデータの取得状況に何らかの階層構造に影響を与えることについて

#パッケージの読み込み
library(rstan)
library(brms)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
fish_num_climate_3 <- read.csv("4-2-1-fish-num-3.csv")
head(fish_num_climate_3, n = 3)
#データの要約
summary(fish_num_climate_3)

#brmによるGLMNの推定
glmm_pois_brms_human <- brm(
  formula = fish_num ~ weather + temperature + (1|human),
  family = poisson(),
  data = fish_num_climate_3,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sd"))
)
#参考:トレースプロットなど
plot(glmm_pois_brms_human)
#参考:収束の確認
stanplot(glmm_pois_brms_human, type = "rhat")

#結果の表示
glmm_pois_brms_human

#各々の調査者の大きさ
ranef(glmm_pois_brms_human)
#調査者ごとにグラフを分けて, 回帰直線を描く
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

eff_glmm_human <- marginal_effects(
  glmm_pois_brms_human,
  effects = "temperature:weather",
  re_formula = NULL,
  conditions = conditions)

plot(eff_glmm_human, points = TRUE)


