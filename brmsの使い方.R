#brmを用いることで,ベイズ統計モデリングのハードルを下げる.brmsの結果をstanで表す.
#brmsとは:一般化線形モデルなどの回帰(2の変数の関係を表す式のうち,統計的に推計された式のこと)と呼ばれるモデルをstanを用いて推定するパッケージ.


#パッケージの読み込み
install.packages("brms", dependencies = TRUE)
library(rstan)
library(brms)
#計算の高速化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
#分析対象のデータ
file_beer_sales_2<-read.csv("3-2-1-beer-sales-2.csv")

#単回帰モデル(μ=β₀+β₁x)の作成
simple_lm_brms<-brm(
  formula=sales~temperature,#modelの構造を指定
  family=gaussian(link="identity"),#正規分布を使用
  data=file_beer_sales_2,#データ
  seed=1#乱数の種
)
#一般化線形モデルの場合,線形予測子(formula関数),確率分布(family関数),リンク関数を指定することでモデルの構造を決定する.familyは正規分布などを使用することができる.
as.mcmc(simple_lm_brms,combine_chains=TRUE)
#事後分布の図示(plot関数)
plot(simple_lm_brms)


#複雑なformulaはbf関数を使用
simple_lm_formula<-bf(sales~temperature)
#今回は説明変数が気温のみ.変数を増やす場合は+でつなげる.

#familyの指定
#正規分布
gaussian
#二項分布
binomial()
#ポアソン分布
poisson()

#brm関数:mcmcの実行(simple_lm_brmsと同じ結果)
simple_lm_brms_2<-brm(
  formula=simple_lm_formula,#bf関数で作成済みのformulaを指定
  family=gaussian(),#正規分布を使用
  data=file_beer_sales_2,#データ
  seed=1,#乱数の種
  chains=4,#チェーン数
  iter=2000,#乱数の繰り返し数
  warmup=1000,#バーンイン期間
  thin=1#間引き数
)

#prior_summary関数:事前分布の確認(推定された後のモデルに対して適用される関数)
prior_summary(simple_lm_brms)
#nu:自由度,mu:分布の中心位置,sigma:分布の裾の広さ,Intercept:切片,sigma:データのばらつきの大きさを表す.

#事前分布を無情報分布に変更
simple_lm_brms_3<-brm(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2,
  seed=1,
  prior=c(set_prior("",class="Intercept"),
          set_prior("",class="sigma"))
)
#引数pirorを指定することで任意の事前分布を指定できる.

get_prior(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2
)

#stanコードの抽出
stancode(simple_lm_brms_3)
#stanに渡すデータの抽出
standata(simple_lm_brms_3)


#standetaの作成(male_standeta関数)
standata_brms<-make_standata(
  formula=sales~temperature,
  family=gaussian(),
  data=file_beer_sales_2
)#結果の表示
standata_brms

#make_standeta関数の結果をmcmc
simple_lm_brms_stan<-stan(
  file="3-5-1-brms-stan-code.stan",
  data=stancoda_brms,
  seed=1
)
#rstanの実行結果
print(simple_lm_brms_stan,
      pars=c("b_Intercept","b[1]","sigma"),
      probs=c(0.025,0.5,0.975))
#brmsの実行結果
simple_lm_brms_3

#plot(simple_lm_brms):事後分布とトレースプロットが得られる.
#^b_は頭がb_から始まるパラメータという意味.type:グラフの種類の指定,pars:描画対象となるパラメータの指定
stanplot(simple_lm_brms,
         type="intervals",
         pars="^b_",
         prob=0.8,#太い線の範囲
         prob_outer=0.95)#細い線の範囲

#brmsを使うことで,特定の気温の時の売り上げを予測できる
#予測のための説明変数
new_data<-data.frame(temperature=20)

#fitted関数:モデルの予測値を回帰直線の95%ベイズ信用区間と合わせて出力できる.
#回帰直線の信用区間付きの予測値
fitted(simple_lm_brms,new_data)

#predict関数:モデルの予測値を95%ベイズ予測区間と合わせて出力できる.
#予測区間付きの予測値
set.seed(1)
predict(simple_lm_brms,new_data)

#mcmcサンプルを取り出す
mcmc_sample<-as.mcmc(simple_lm_brms,combine_chains=TRUE)
head(mcmc_sample,n=2)

#推定されたパラメータ別に保存しておく
mcmc_b_Intercept<-mcmc_sample[,"b_Intercept"]
mcmc_b_temperature<-mcmc_sample[,"b_temperature"]
mcmc_sigma<-mcmc_sample[,"sigma"]

saigen_fitted<-mcmc_b_Intercept+20*mcmc_b_temperature

#fittedの再現
mean(saigen_fitted)
quantile(saigen_fitted,probs=c(0.025,0.095))
fitted(simple_lm_brms,new_data)
         
#予測分布のmcmcサンプルを得る(平均値:saigen_effect,標準偏差:mcmc_sigmaである正規乱数)
#do.call関数:繰り返し計算をするための関数(引数に入れたrnorm関数を4000回繰り返す)
set.seed(1)
saigen_predict<-do.call(
  rnorm,
  c(4000,list(mean=saigen_fitted,sd=mcmc_sigma))
)
#predictの再現
#平均値:mu_pred
mean(saigen_predict)
quantile(saigen_predict,probs=c(0.025,0.975))
set.seed(1)
predict(simple_lm_brms,data.frame(temperature=20))

#回帰直線(説明変数によって目的変数が変化する数値)の95%ベイズ信用区間付きのグラフ
eff<-marginal_effects(simple_lm_brms)
plot(eff,points=TRUE)

#95%予測区間付きのグラフ
set.seed(1)
eff_pre<-marginal_effects(simple_lm_brms,method="predict")
plot(eff_pre,points=TRUE)

#参考:複数の説明変数があるときは、特定の要因だけを切り出せる
marginal_effects(simple_lm_brms,
                 effects="temperature")

marginal_effects(brms_model,
                 effects="x1:x2")

