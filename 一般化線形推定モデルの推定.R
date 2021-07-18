#formula構文の活用,一般化線形モデルとの比較
#デザイン行列を用いることでStanファイルを書き換える必要がなくなる.よって様々なモデルで活用できる.

#パッケージの読み込み
install.packages("rstan")
library(rstan)
library(bayesplot)
#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores)
#分析対象のデータ
file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")
#サンプルサイズ
sample_size<-nrow(file_beer_sales_2)

#formulaの作成
formula_lm<-formula(sales~temperature)
#デザイン行列の作成
X<-model.matrix(formula_lm,file_beer_sales_2)
#formulaとmodel.matrixを用いたデザイン行列
head(X,n=5)
# formulaはデザイン行列を作成の記法.~(チルダ記号)の左側に応答変数,右側に説明変数を配置.
#応答変数はある特定の量.説明変数は応答変数に影響を与える量.

#サンプルサイズ
N<-nrow(file_beer_sales_2)
#nrow:行列を取得
#デザイン行列の列数(説明変数の数+1)
K<-2
#応答変数
Y<-file_beer_sales_2$sales
#listにまとめる
data_list_design<-list(N=N,K=K,Y=Y,X=X)

#mcmcの実行
mcmc_result_design<-stan(
  file="3-4-1-lm-design-matrix.stan",
  data=data_list_design,
  seed=1
)

print(mcmc_result_design,probs=c(0.025,0.5,0.975))
#b[1]は切片,b[2]が傾き(今回は気温の係数)を示す.
