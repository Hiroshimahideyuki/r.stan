#単回帰モデルを用いた予測の方法　

#パッケージの読み込み
library(rstan)
library(bayesplot)
#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
#分析対象データ
file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")
#サンプルサイズ
sample_size<-nrow(file_beer_sales_2)

#気温を11度から30度までの変化させ,売り上げを予測
temperature_pred<-11:30

#listにまとめる
data_list_pred<-list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature,
  N_pred=length(temperature_pred),
  temperature_pred=temperature_pred
)
#平均:mu_pred,標準偏差:sigmaである正規分布から得られた予測値sales_predをmcmcサンプリングする.
#sales_predは平均値:mu_predとした正規分布から,さらに乱数を生成することでmcmcサンプルが得られる.

#mcmcの実行
mcmc_result_pred<-stan(
  file="3-3-1-simple-lm-pred.stan",
  data=data_list_pred,
  seed=1
)
#結果の表示
print(mcmc_result_pred,probs=c(0.025,0.5,0.975))

#mcmcサンプルの抽出
mcmc_sample_pred<-rstan::extract(mcmc_result_pred,permuted=FALSE)


mcmc_intervals(
  mcmc_result_pred,
  regex_pars=c("sales_pred."),#正規表現を用いて名称を指定
  prbs=0.8,#太い線の範囲
  prob_outer=0.95#細い線の範囲
)

#95%区間の比較
mcmc_intervals(
  mcmc_sample_pred,
  paars=c("mu_pred[1]","sales_pred[1]"),
  probs=0.8,#太い線の範囲
  probs_outer=0.95#細い線の範囲
)

mcmc_areas(
  mcmc_sample_pred,
  pars=c("sales_pred[1]","sales_pred[20]"),
  probs=0.6,#薄い青色で塗られた範囲
  prob_outer=0.99#細い線が描画される範囲
)

