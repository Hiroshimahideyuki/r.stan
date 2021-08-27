#確定的トレンドや確率的トレンドの概念, および状態空間モデルによるトレンドの表現の方法について
#時系列分析におけるトレンドの扱いを理解する


#パッケージの読み込み
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()
#状態空間モデルの図示する関数の読み込み
source("plotSSM.R", encoding="utf-8")

#データの読み込み
sales_df_3 <- read.csv("5-5-1-sales-ts-3.csv")
sales_df_3$date <- as.POSIXct(sales_df_3$date)
head(sales_df_3, n = 3)
#図示
autoplot(ts(sales_df_3[, -1]))

#データの準備
data_list <- list(
  y = sales_df_3$sales, 
  T = nrow(sales_df_3)
)
#ローカルレベルモデルの推定
local_level <- stan(
  file = "5-2-1-local-level.stan",
  data = data_list,
  seed = 1
)
#ローカルレベルモデルの推定結果
print(local_level, 
      par = c("s_w", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))
        
#平滑化トレンドモデルの推定
smooth_trend <- stan(
  file = "5-5-1-smooth-trend.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

#平滑化トレンドモデルの推定結果
print(smooth_trend, 
      par = c("s_z", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))

#ローカル線形トレンドモデルの推定
local_linear_trend <- stan(
  file = "5-5-2-local-linear-trend.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
#ローカル線形トレンドモデルの推定
print(local_linear_trend, 
      par = c("s_w", "s_z", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))

#MCMCサンプルの取得
mcmc_sample_ll <- rstan::extract(local_level)
mcmc_sample_st <- rstan::extract(smooth_trend)
mcmc_sample_llt <- rstan::extract(local_linear_trend)

#関数plotSSMを用いて状態を図示        
#ローカルレベルモデル
p_ll <- plotSSM(mcmc_sample = mcmc_sample_ll, 
        time_vec = sales_df_3$date,
        obs_vec = sales_df_3$sales,
        state_name = "mu", 
        graph_title = "", 
        y_label = "sales") 
#平滑化トレンドモデル
p_st <- plotSSM(mcmc_sample = mcmc_sample_st, 
        time_vec = sales_df_3$date,
        obs_vec = sales_df_3$sales,
        state_name = "mu", 
        graph_title = "", 
        y_label = "sales") 
#ローカル線形トレンドモデル
p_llt <- plotSSM(mcmc_sample = mcmc_sample_llt, 
                time_vec = sales_df_3$date,
                obs_vec = sales_df_3$sales,
                state_name = "mu", 
                graph_title = "«", 
                y_label = "sales") 
grid.arrange(p_ll, p_st, p_llt)


#ドリフト成分の図示
plotSSM(mcmc_sample = mcmc_sample_llt, 
        time_vec = sales_df_3$date,
        state_name = "delta", 
        graph_title ="", 
        y_label = "delta")
        



