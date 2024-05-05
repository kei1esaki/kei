#ディレクトリを検索
getwd()
setwd("/Users/esakikeiichi/Desktop/Applied_Data_Science/datasource")
base <- read.csv("Baseball_2016.csv")
head(base)
str(base)
summary(base)
table(base$球団)
base2 <- na.omit(base)
var(base2$打率)
sd(base2$打率)
hist(base$年俸)
boxplot(base$年俸)
boxplot(年俸 ~ リーグ, data = base)
install.packages("beeswrm")
library(beeswarm)
boxplot(年俸~リーグ,data=base, online=FALSE)
beeswarm(年俸~リーグ,data=base,add = T)
plot(base$年俸, base$得点)
base_num<-data.frame(c(16:42))
str(base_num)
cor(base_num)
cor_data<-cor(base_num,use = "complete.obs")
cor_df_data
base_num<-data.frame(base2)[c(16:42)] 
base_num<-data.frame(c(16:42))
str(base_num)
cor(base_num)
# 別の方法
base_num<-data.frame(base)[c(16:18,20:42)] 
# 契約解除フラグを外す
str(base_num) 
cor(base_num, use = "complete.obs") 
# 翌年年俸との相関の高い順にデータを出力
cor_data <-cor(base_num , use = "complete.obs") 
cor_df_data<-as.data.frame(cor_data) 
# 相関行列をデータフレームへ変換# 翌年年俸に対する相関関係が高い順に並べる
cor_df_sorted<-cor_df_data[order(-cor_df_data$翌年年俸),]
cor(cor_df_sorted)
# 年俸を安打で予測する単回帰分析、切片の符号をチェック、t・pを調査
reg_anda<-lm(base$翌年年俸~base$安打) 
# 回帰結果の表示
summary(reg_anda)
# 安打と得点と本塁打で翌年年俸を説明するモデル
mreg<-lm(base$翌年年俸~base$安打+base$得点+base$本塁打) 
# 回帰結果の表示
summary(mreg)
#相関行列
cor(base[,c("安打", "得点", "本塁打")] , use = "complete.obs")
# OLSSダウンロード
install.packages("olsrr")
library(olsrr) 
# VIFを求める
ols_vif_tol(mreg) 
# 変数増減法
step(mreg, direction="both")
# ステップワイズの結果を保存して出力
step <-step(mreg, direction="both") 
summary(step)
reg_anda2<-lm(base$翌年年俸~base$得点) 
summary(reg_anda2)
#演習
#⚫相関行列で翌年年俸との相関係数が0.7以上だった項目で重回帰分析を実施
#⚫VIFを計算し多重共線性をチェック
#⚫変数選択を実施して一番良いと思われるモデルを作成
#上記の演習を実施した結果として、以下を第2回リアクションペーパーにテキスト入力し提出してください。
#・最終的に一番良いと判断したモデルに使用した説明変数
#・そのモデルの自由度調整済み決定係数
#・何故そのモデルが一番良いと判断したか、自分なりの説明
cor_data <- cor(base_num , use = "complete.obs") 
cor_df_data <- as.data.frame(cor_data)  
# 相関行列をデータフレームへ変換# 翌年年俸に対する相関関係が高い順に並べる
cor_df_sorted<-cor_df_data[order(-cor_df_data$翌年年俸)]
head(cor_df_sorted)
#⚫相関行列で翌年年俸との相関係数が0.7以上だった項目で重回帰分析を実施
value <- 0.7
cor_df_name <- colnames(cor_df_sorted)[abs(cor_df_sorted$翌年年俸)>=value]
print(cor_df_name)
