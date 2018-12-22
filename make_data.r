#----------------------------
# volume prediction
# make data script
# 2017/04/11
#----------------------------

## 初期化
rm(list=ls(all=TRUE))

## ファイル読み込み
# ホーム決定
homeDir <- "Input_Dir"
scriptDir <- "Script_Dir"

# Input とOutput Directory （homeDirからの相対位置）
inputDirectory  <- "Input"
outputDirectory <- "Output"

#ファイル名
inFileName <- "day_data_20170422_5.csv"
inFileName_2 <- "setting_file.csv"

#ファイル読み込み
inDir <- paste0(homeDir,inputDirectory)
outDir <- paste0(homeDir,outputDirectory)
setwd(inDir)
inData1 <- read.delim(inFileName,header=TRUE,na.strings="NA",sep=",",stringsAsFactors=FALSE)
setting <- read.delim(inFileName_2,header=TRUE,na.strings="NA",sep=",",stringsAsFactors=FALSE)
### データの作成
library(xts)
library(car)

for (tollgate_id in 1:3){
  for (direction in 0:1){
    if (tollgate_id == 2 & direction == 1){
      next
    } else {
      assign(paste("data_",tollgate_id,"_",direction,sep="")
             ,inData1[,c(
               "start_time"
               ,paste("tollgate_volume_",tollgate_id,"_",direction,sep='')
               ,"holiday_special"
               #,"holiday_normal"
               ,"workday"
               ,"typhoon_flag"
               ,"rain_0_1"
               ,"rain_1_3"
               ,"rain_3"
               ,"before_special"
               ,"abnormal_day_9_20"
               ,"before_long_vacation_flag"
               #,"abnormal_holiday_am_1_0"
               ,"dow"
             )]
      )
    }
  }
}

for (i in 2:11){
  data_1_0[,i] <- as.numeric(data_1_0[,i])
  data_1_1[,i] <- as.numeric(data_1_1[,i])
  data_2_0[,i] <- as.numeric(data_2_0[,i])
  data_3_0[,i] <- as.numeric(data_3_0[,i])
  data_3_1[,i] <- as.numeric(data_3_1[,i])
}

# glm
# model_full <- glm(paste(colnames(data_1_0[,2])," ~.",sep = "")
#                   ,data = data_1_0[,c(3:44)]) #pまで書く
# summary(model_full) #変数を全てを採用したときの回帰分析の結果を出力
# model_back <- step(model_full)

# pcaで特徴量を抽出
# pca <- prcomp(data_1_0[,c(8:12,17:32)],scale = T)
# summary(pca)
# biplot(pca)
# volume <- pca$x[,1:2]
# 
# data_1_0_test <- cbind(data_1_0[,c(1:2)],volume)
# data_1_0 <- cbind(data_1_0_test,data_1_0[,c(33:44)])

# box-cox transform
# predictors

# for (i in 3:4){
#   data_1_0[,i] <- data_1_0[,i] + 1
#   lam = powerTransform(data_1_0[,i])
#   # lam[i-2] =  lam$roundlam
#   data_1_0[,i] = bcPower(data_1_0[,i], lam$roundlam)
#   # 0-1に正規化
#   #data_1_0[,i] = (data_1_0[,i] - min(data_1_0[,i])) /
#   #  (max(data_1_0[,i]) - min(data_1_0[,i]))
# }

# target
# data_1_0[,2] <- data_1_0[,2] + 1
# lam_1_0 = powerTransform(tollgate_volume_1_0 ~.,data = data_1_0[2:8])
# data_1_0[,2] = bcPower(data_1_0[,2], lam_1_0$roundlam)

# xts
data_1_0 <- as.xts(read.zoo(data_1_0),frequency=72)
data_1_1 <- as.xts(read.zoo(data_1_1),frequency=72)
data_2_0 <- as.xts(read.zoo(data_2_0),frequency=72)
data_3_0 <- as.xts(read.zoo(data_3_0),frequency=72)
data_3_1 <- as.xts(read.zoo(data_3_1),frequency=72)



# time_window作る
time_window <- as.xts(read.zoo(inData1[,c("start_time","time_window")]))
time_window <- time_window['2016-10-18 00:00::2016-10-24 23:40:']
#holiday <- inData2

### data save
setwd(outDir)
save(data_1_0,file="data_1_0.RData")
save(data_1_1,file="data_1_1.RData")
save(data_2_0,file="data_2_0.RData")
save(data_3_0,file="data_3_0.RData")
save(data_3_1,file="data_3_1.RData")
#save(lam_1_0,file="lam_1_0.RData")
# save(data_1_1,file="data_1_1.RData")
# save(data_2_0,file="data_2_0.RData")
# save(data_3_0,file="data_3_0.RData")
# save(data_3_1,file="data_3_1.RData")
save(time_window,file="time_window.RData")
save(setting,file="setting.RData")
#save(holiday,file="holiday.RData")

# move directory
setwd(scriptDir)
