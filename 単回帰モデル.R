#単回帰モデルの推定方法
#RとStanを用いた一般化線形モデル推定, 使用法

#パッケージの読み込み
library(rstan)
library(bayesplot)
#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

#分析対象のデータ
file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")
head(file_beer_sales_2,n=3)
#サンプルサイズ
sample_size<- nrow(file_beer_sales_2)
sample_size

#図示
library(ggplot2)
ggplot(file_beer_sales_2,aes(x=temperature,y=sales))+
  geom_point()+
  labs(title="ビールの売り上げと気温の関係")

#listにまとめる
data_list<-list(
  N=sample_size,
  sales=file_beer_sales_2$sales,
  temperature=file_beer_sales_2$temperature
)

#乱数の生成
mcmc_result<-stan(
  file="3-2-2-simple-lm-vec.stan",
  data=data_list,
  seed=1
)

print(mcmc_result,probs=c(0.025,0.5,0.975))

#MCMCサンプルの抽出
mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)

#トレースプロットと事後分布
mcmc_combo(
  mcmc_sample,
  pars=c("Intercept","beta","sigma")
)
