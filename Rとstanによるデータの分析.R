#Stanの実装の方法について

#分析対象のデータの読み込み
file_beer_sales_ab<-read.csv("2-6-1-beer-sales-ab.csv")
head(file_beer_sales_ab, n=3)

#ビールの種類別のヒストグラム
library("ggplot2")
ggplot(data=file_beer_sales_ab,
       mapping=aes(x=sales,y=..density..,
                   color=beer_name,fill=beer_name))+
  geom_histogram(alpha=0.5,position="identity")+
  geom_density(alpha=0.5,size=0)
#2種類のグラフの重ね合わせ

#ビールの種類別にデータを分ける
sales_a<-file_beer_sales_ab$sales[1:100]
sales_b<-file_beer_sales_ab$sales[101:200]

data_list_ab<-list(
  sales_a=sales_a,
  sales_b=sales_b,
  N=100
)

library(rstan)
mcmc_result_6<-stan(
  file="2-6-5-difference-mean.stan",
  data=data_list_ab,
  seed=1
)

print(
  mcmc_result_6,
  probs=c(0.025,0.5,0.975)
)

library(ggplot2)
library(bayesplot)

#平均値の差の事後分布のグラフ
mcmc_sample<-rstan::extract(mcmc_result_6,permuted=FALSE)
mcmc_dens(mcmc_sample,pars="diff")
