#ポアソン分布を仮定したDGLMの実装(過分散に対応するランダム効果)
#GLM, GLMN, DLM, DGLMの組み合わせ

#パッケージの読み込み
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)
#計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")

#データの読み込み
fish_ts <- read.csv("5-9-1-fish-num-ts.csv")
fish_ts$date <- as.POSIXct(fish_ts$date)
head(fish_ts, n = 3)
#図示
autoplot(ts(fish_ts[, -1]))

#データの準備
data_list <- list(
  y = fish_ts$fish_num, 
  ex = fish_ts$temperature, 
  T = nrow(fish_ts)
)
#モデルの推定
dglm_poisson <- stan(
  file = "5-9-1-dglm-poisson.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6, 
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

print(dglm_poisson, 
      par =  c("s_z", "s_r", "b", "lp__"),
      probs = c(0.025, 0.5, 0.975))

#MCMCサンプルの取得
mcmc_rhat(rhat(dglm_poisson))
check_hmc_diagnostics(dglm_poisson)

mcmc_sample <- rstan::extract(dglm_poisson, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_z", "s_r", "lp__"))

options(max.print=100000)
print(dglm_poisson, probs = c(0.025, 0.5, 0.975))


mcmc_sample <- rstan::extract(dglm_poisson)

#個別のグラフ作成
p_all <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_exp", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 

p_smooth <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_smooth", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 

p_fix <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = fish_ts$date,
        obs_vec = fish_ts$fish_num,
        state_name = "lambda_smooth_fix", 
        graph_title = "", 
        y_label = "",
        date_labels = "") 

#グラフの重ね合わせ
grid.arrange(p_all, p_smooth, p_fix)







