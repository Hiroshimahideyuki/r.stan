#stanの基本
#stanの基本事項とR,stanの連携分析
#目的:stanを用いた分析を図式化

install.packages('rstan',repos='http://cloud.r-project.org/',dependencies = TRUE)

library(rstan)

#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

#分析対象のデータ
file_beer_sales_1<-read.csv("2-4-1-beer-sales-1.csv")
head(2-4-1-beer-sales-1.csv,n=3)

#分析対象のデータの最初から3までの数値
head(file_beer_sales_1,n=3)

#サンプルのサイズ
sample_size<-nrow(file_beer_sales_1)
sample_size

#listにまとめる
data_list<-list(sales=file_beer_sales_1$sales, N=sample_size)
data_list

#mcmcの実行
mcmc_result<-stan(
  file="2-4-1-calc-mean-variance.stan", #stanファイル
  data=data_list, #対象のデータ
  seed=1, #乱数の種
  chains=4, #チェーン数
  iter=2000, #乱数生成の反復数
  warmup=1000, #バーンイン期間
  thin=1 #間引き数(1なら間引き無し)
)

#結果の表示
print(
  mcmc_result, #MCMCサンプリングの結果
  probs=c(0.025,0.5,0.975) #中央値と95%信用区間を出力
)

#トレースプロット(バーンイン期間無し)
traceplot(mcmc_result)

#トレースプロット(バーンイン期間あり)
traceplot(mcmc_result,inc_warmup=T)

/
#サンプル:標本
#MCMCサンプリング:mcmcを用いて得られた乱数
#mcmc:マルコフ連鎖モンテカルロ法:求める確率分布をマルコフ連鎖を作成することでサンプリングを行うアルゴリズム
#バーンイン期間ありなし:mcmcサンプルの初期値(分布)が安定しないため収束させ,mcmcサンプルを用いる
