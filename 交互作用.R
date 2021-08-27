#線形予測子を複雑にして, 説明変数同士が交互に影響を与え合う構造をモデルで表現する(交互作用)
#交互作用は一般線形化モデル以外でも使用可能

#パッケージの読み込み
library(rstan)
library(brms)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#分析対象のデータ
interaction_1 <- read.csv("3-10-1-interaction-1.csv")
head(interaction_1, n = 3)
#データの要約
summary(interaction_1)

#デザイン行列の作成
model.matrix(sales ~ publicity * bargen, interaction_1)

#モデル化
interaction_brms_1 <- brm(
  formula = sales ~ publicity * bargen,
  family = gaussian(link = "identity"),
  data = interaction_1,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)


interaction_brms_1_2 <- brm(
  formula = sales ~ publicity + bargen + publicity:bargen,
  family = gaussian(link = "identity"),
  data = interaction_1,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)
interaction_brms_1
plot(interaction_brms_1)

#説明変数を作る
newdata_1 <- data.frame(
  publicity = rep(c("not", "to_implement"),2),
  bargen = rep(c("not", "to_implement"),each = 2)
)
newdata_1
#予測
round(fitted(interaction_brms_1, newdata_1), 2)

#モデルの図示
eff_1 <- marginal_effects(interaction_brms_1,
                          effects = "publicity:bargen")
plot(eff_1, points = T)

#分析対象のデータ
interaction_2 <- read.csv("3-10-2-interaction-2.csv")
head(interaction_2, n = 3)
#データの要約
summary(interaction_2)

#モデル化
model.matrix(sales ~ publicity * temperature, interaction_2)

#MCMCの結果の確認
interaction_brms_2 <- brm(
  formula = sales ~ publicity * temperature,
  family = gaussian(link = "identity"),
  data = interaction_2,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

interaction_brms_2
#図示
plot(interaction_brms_2)

#説明変数を作る
newdata_2 <- data.frame(
  publicity   = rep(c("not", "to_implement"), each = 2),
  temperature = c(0,10,0,10)
)
newdata_2
round(fitted(interaction_brms_2, newdata_2), 2)

#回帰直線の図示
eff_2 <- marginal_effects(interaction_brms_2,
                          effects = "temperature:publicity")
plot(eff_2, points = T)

#分析対象のデータ
interaction_3 <- read.csv("3-10-3-interaction-3.csv")
head(interaction_3, n = 3)
#データの要約
summary(interaction_3)

#データの図示
ggplot(data = interaction_3,
       aes(x = product, y = sales, color = factor(clerk)))+
  geom_point()

model.matrix(sales ~ product * clerk, interaction_3)

#モデル化
interaction_brms_3 <- brm(
  formula = sales ~ product * clerk,
  family = gaussian(link = "identity"),
  data = interaction_3,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

interaction_brms_3
#図示
plot(interaction_brms_3)

#説明変数を作る
newdata_3 <- data.frame(
  product = c(0,10,0,10),
  clerk   = c(0,0,10,10)
)
newdata_3
#予測
round(fitted(interaction_brms_3, newdata_3), 2)

int_conditions <- list(
  clerk = setNames(1:9, paste("clerk=", 1:9, sep=""))
)
int_conditions

eff_3 <- marginal_effects(interaction_brms_3,
                          effects = "product:clerk",
                          int_conditions = int_conditions)
plot(eff_3, points = TRUE)



conditions <- data.frame(clerk = 1:9)
conditions

eff_4 <- marginal_effects(interaction_brms_3,
                          effects = "product",
                          conditions = conditions)
plot(eff_4, points = FALSE)


