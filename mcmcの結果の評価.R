#mcmcの結果の評価
#mcmcサンプルの取り扱いと事後予測、描画の方法
#目的:グラフを用いて結果の吟味、予測

install.packages("rstan")
install(rstan)

pkgbuild::has_build_tools(debug=TRUE)
library(rstan)

#mcmcの実行
mcmc_result<-stan(
  file="2-4-1-calc-mean-variance.stan",  #stanファイル
  data=data_list, #対象のデータ
  seed=1,  #乱数の種
  chains=4,  #チェーン数
  iter=2000,  #乱数生成の反復回数
  warmup=1000, #バーンイン期間
  thin=1  #間引き数
)

#mcmcサンプルの抽出
mcmc_sample<-rstan::extract(mcmc_result,permuted=FALSE)

#クラス
class(mcmc_sample) #各々1000,4,3の次元数と判明
#次元数
dim(mcmc_sample) #1000は繰り返し数,　4はチェイン数,　3は推定されたパラメータ
#各々の名称
dimnames(mcmc_sample)#今回は1000個のmcmcサンプルが1つのチェーンに存在する.　それが4つのチェーン数, 3つ(lp_を含むため, 実質muとsigmaの2つ)のパラメータとして存在する.

mcmc_sample[1,"chain:1","mu"] 
mcmc_sample[,"chain:1","mu"]

#パラメータmuの1回目のチェーンのmcmcサンプルの個数
length(mcmc_sample[,"chain:1","mu"])
#4つのチェーンすべてのMCMCサンプルの個数
length(mcmc_sample[,,"mu"]) #4000は1000行*4列の行列
#4つのチェーンがあるため、1000iter*4Chainのmatrix
dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])

#ベクトル化
mu_mcmc_vec<-as.vector(mcmc_sample[,,"mu"])

#事後中央値
median(mu_mcmc_vec) #median関数は中央値を得る関数
#事後期待値
mean(mu_mcmc_vec) #mean関数は平均値を得る
#95%信用区間
quantile(mu_mcmc_vec,probs=c(0.025,0.975)) #quatile関数によって2.5%と97.5%を得る
#ベイズ信用区間:95%の確率で区間内に真の値が存在するベイズ主義の考え

#参考
print(
  mcmc_result, #mcmcサンプリングの結果
  probs=c(0.025,0.5,0.975) #事後分布の四分位点を出力
)


install.packages("ggfortify")
library(ggfortify)

#トレースプロットの描画
autoplot(ts(mcmc_sample[,,"mu"]),
         facets=F, #4つのChianをまとめて1つのグラフへ
         ylab="mu", #y軸のラベル
         main="トレースプロット")
#トレースプロット:乱数が安定した変動を繰り返すことで定常分布へ収束したグラフのこと.

install.packages("ggplot2")
library("ggplot2")

#データの成形
mu_df<-data.frame(
  mu_mcmc_sample=mu_mcmc_vec
) #mcmcサンプルをすべてまとめてカーネル密度推定をすると、パラメータの事後分布のグラフが描画できる
#図示
ggplot(data=mu_df,mapping=aes(x=mu_mcmc_sample))+
  geom_density(size=1.5) #縦軸density, 横軸mu_mcmc_sample, density=カーネル密度推定

install.packages("bayesplot")
library(bayesplot)

#ヒストグラム
mcmc_hist(mcmc_sample,pars=c("mu","sigma"))
#ヒストグラム:度数分布(特定のデータを種類別の度数にまとめたもの)をグラフにしたもの

#カーネル密度推定
mcmc_dens(mcmc_sample,pars=c("mu","sigma"))
#カーネル密度推定:ヒストグラムの不連続性をなめらかなグラフとして扱う方法


#参考:トレースプロット
mcmc_trace(mcmc_sample,pars=c("mu","sigma"))
#事後分布とトレースプロットをまとめて図示
mcmc_combo(mcmc_sample,pars=c("mu","sigma"))
#事後分布:条件付き確率のこと.データが手に入った後の分布を指す.逆は事前分布

#事後分布の範囲を比較
mcmc_intervals(
  mcmc_sample,pars=c("mu","sigma"),
  prob=0.8, #太い線の範囲
  prob_outer=0.95　#細い線の範囲
)
#密度を合わせて描画
mcmc_areas(mcmc_sample,pars=c("mu","sigma"),
           prob=0.6, #薄い青色で塗られた範囲
           prob_outer=0.99 #細い線が描画される範囲
)

#MCMCサンプルのコレログラム
mcmc_acf_bar(mcmc_sample,pars=c("mu","sigma"))
#コレログラム:次数別の自己相関を可視化したもの.縦軸は自己相関係数,　横軸は次数.

#分析対象のデータ
animal_num<-read.csv("2-5-1-animal-num.csv")
head(animal_num,n=3)

#サンプルサイズ 
sample_size<-nrow(animal_num)
#listにまとめる
data_list<-list(animal_num=animal_num$animal_num,N=sample_size)
#mcmcの実行:正規分布仮定のモデル
mcmc_normal<-stan(
  file="2-5-1-normal-dist.stan",
  data=data_list,
  seed=1
)
#MCMCの実行:ポアソン仮定のモデル
mcmc_poisson<-stan(
  file="2-5-2-poisson-dist.stan",
  data=data_list,
  seed=1
)

#事後予測値のMCMCサンプルの取得
y_rep_normal<-rstan::extract(mcmc_normal)$pred
y_rep_poisson<-rstan::extract(mcmc_poisson)$pred

#サンプルサイズは200
#4000回分のMCMCサンプル
dim(y_rep_normal)

#正規分布を仮定したモデル
y_rep_normal[1,]
#ポアソン分布を仮定したモデル
y_rep_poisson[1,]
#ポアソン分布:離散確立分布の一つで、負ではない整数xを生成するための確率分布

#参考:観測データの分布と, 事後予測分布の比較
hist(animal_num$animal_num) #観測データの分布
hist(y_rep_normal[1,]) #正規分布を仮定した事後予測分布
hist(y_rep_poisson[1,]) #ポアソン分布を仮定した事後予測文王分布

#正規分布を仮定したモデル
ppc_hist(y=animal_num$animal_num,
         yrep=y_rep_normal[1:5,])
#ポアソン分布を仮定したモデル
ppc_hist(y=animal_num$animal_num,
         yrep=y_rep_poisson[1:5,])


/
