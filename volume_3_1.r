#----------------------------
# volume prediction 3_1
# 2017/04/12
# author:hyodo
# 非効率なコードなので、後からキレイにする
#----------------------------

## 初期化
rm(list=ls(all=TRUE))

# データのロード
#setwd(DataDir)
load("option.RData")
setwd(OutPutDir)
load("data_3_1.RData")

## 必要なライブラリたち
library(xts)
library(bsts)
library(dplyr)

### Bayesian Structural Time Series Prediction
# tollgate_id,direction毎に予測
# モデルのパラメータ

# 午前中の予測
if (predict == 1){
  for (i in 18:24){
    train_1 <- data_3_1[paste('2016-09-19 00:00::2016-10-',i,' 07:40:',sep = '')]
    test_1 <- data_3_1[paste('2016-10-',i,' 08:00::2016-10-',i,' 09:40:',sep = '')]
    
    #データフレームに戻す（xtsだとfactorと数値を併用できないため）
    train_1 <- as.data.frame(train_1)
    test_base_1 <- as.data.frame(test_1)
    
    for (k in 1:1){
      train_1[,k] <- as.numeric(paste(train_1[,k]))
      train_1[,k] <- train_1[,k] + 1
      train_1[,k] <- log(train_1[,k])
      test_base_1[,k] <- as.numeric(paste(test_base_1[,k]))
      test_base_1[,k] <- test_base_1[,k] + 1
      test_base_1[,k] <- log(test_base_1[,k])
    }
    
    test_1 <- test_base_1
    test_1[,1] <- NA
    
    # model setting
    ss <- AddLocalLevel(list(), train_1[,1])
    ss <- AddSeasonal(ss, train_1[,1], nseasons = 72)
    ss <- AddAr(ss, train_1[,1], lag = 1)
    
    model <- bsts(paste(colnames(train_1[1])
                        ," ~ holiday_special + typhoon_flag +
                        before_special + 
                        abnormal_day_9_20 + workday * dow ",sep = "")
                  ,state.specification = ss
                  ,niter=niter
                  ,contrasts=list(
                    holiday_special=contrasts(train_1$holiday_special)
                    ,workday=contrasts(train_1$workday)
                    ,typhoon_flag=contrasts(train_1$typhoon_flag)
                    ,before_special=contrasts(train_1$before_special)
                    ,abnormal_day_9_20=contrasts(train_1$abnormal_day_9_20)
                    #,abnormal_holiday_am_1_0=contrasts(train_1$abnormal_holiday_am_1_0)
                    ,dow=contrasts(train_1$dow)
                  )
                  ,data = train_1[,c(1:11)])
    
    
    
    pred_1 <- predict(model, burn=burn
                      #, horizon= 100
                      ,newdata=test_1[,c(2:11)]
    )
    
    test_1[,1] <- exp(pred_1$mean)
    
    if (i == 18){
      result_1 = test_1
    } else {
      result_1 = rbind(result_1,test_1)
    }
    
  }
} else {
  for (i in 11:17){
    train_1 <- data_3_1[paste('2016-09-19 00:00::2016-10-',i,' 07:40:',sep = '')]
    
    # # 本番の欠損と同じ状況を作る
    if (i == 11){
      train_1[paste('2016-10-',i,' 00:00::2016-10-',i,' 05:40:',sep = ''),1] <- NA
    } else {
      for (j in 11:16){
        train_1[paste('2016-10-',j,' 00:00::2016-10-',j,' 05:40:',sep = ''),1] <- NA
        train_1[paste('2016-10-',j,' 08:00::2016-10-',j,' 14:40:',sep = ''),1] <- NA
        train_1[paste('2016-10-',j,' 17:00::2016-10-',j+1,' 05:40:',sep = ''),1] <- NA
      }
    }
    
    test_base_1 <- data_3_1[paste('2016-10-',i,' 08:00::2016-10-',i,' 09:40:',sep = '')]
    test_base_1_all <- data_3_1[paste('2016-10-',i,' 08:00::2016-10-',i,' 14:40:',sep = '')]
    #test_2[,1] <- NA
    
    #データフレームに戻す（xtsだとfactorと数値を併用できないため）
    train_1 <- as.data.frame(train_1)
    test_base_1 <- as.data.frame(test_base_1)
    test_base_1_all <- as.data.frame(test_base_1_all)
    
    for (k in 1:1){
      train_1[,k] <- as.numeric(paste(train_1[,k]))
      train_1[,k] <- train_1[,k] + 1
      train_1[,k] <- log(train_1[,k])
      test_base_1[,k] <- as.numeric(paste(test_base_1[,k]))
      test_base_1[,k] <- test_base_1[,k] + 1
      test_base_1[,k] <- log(test_base_1[,k])
      test_base_1_all[,k] <- as.numeric(paste(test_base_1_all[,k]))
      test_base_1_all[,k] <- test_base_1_all[,k] + 1
      test_base_1_all[,k] <- log(test_base_1_all[,k])
    }
    
    test_1 <- test_base_1
    test_1[,1] <- NA
    test_1_all <- test_base_1_all
    test_1_all[,1] <- NA
    
    ss <- list()
    ss <- AddLocalLevel(list(), train_1[,1])
    ss <- AddSeasonal(ss, train_1[,1], nseasons = 72
                      #, season.duration = 7
    )
    ss <- AddAr(ss, train_1[,1], lag = 1)
    
    model <- bsts(paste(colnames(train_1[1])," ~ holiday_special + before_long_vacation_flag + before_special + workday",sep = "")
                  ,state.specification = ss
                  ,niter=niter
                  ,contrasts=list(
                    holiday_special=contrasts(train_1$holiday_special)
                    ,workday=contrasts(train_1$workday)
                    #,typhoon_flag=contrasts(train_1$typhoon_flag)
                    #,rain_0_1=contrasts(train_1$rain_0_1)
                    ,rain_1_3=contrasts(train_1$rain_1_3)
                    ,rain_3=contrasts(train_1$rain_3)
                    ,before_special=contrasts(train_1$before_special)
                    ,before_long_vacation_flag=contrasts(train_1$before_long_vacation_flag)
                    ,abnormal_day_9_20=contrasts(train_1$abnormal_day_9_20)
                    #,abnormal_holiday_am_1_0=contrasts(train_1$abnormal_holiday_am_1_0)
                    ,dow=contrasts(train_1$dow)
                  )
                  ,data = train_1[,c(1:11)]
    )
    
    pred_1 <- predict(model, burn=burn
                      ,newdata=test_1[,c(2:11)]
    )
    
    pred_1_all <- predict(model, burn=burn
                          ,newdata=test_1_all[,c(2:11)]
    )
    
    
    test_1[,1] <- exp(pred_1$mean)
    test_1_all[,1] <- exp(pred_1_all$mean)
    test_base_1[,1] <- exp(test_base_1[,1])
    test_base_1_all[,1] <- exp(test_base_1_all[,1])
    
    if (i == 11){
      result_1 = test_1
      result_1_all = test_1_all
      result_base_1 = test_base_1
      result_base_1_all = test_base_1_all
    } else {
      result_1 = rbind(result_1,test_1)
      result_1_all = rbind(result_1_all,test_1_all)
      result_base_1 = rbind(result_base_1,test_base_1)
      result_base_1_all = rbind(result_base_1_all,test_base_1_all)
    }
  }
  # 検証時 MAPE算出
  # MAPE_v_1_1_am = mean(abs((result_base_1[,1]-result_1[,1])/result_base_1[,1]))
  # print(MAPE_v_1_1_am)
  
  time = cbind(result_base_1[,1],result_1[,1])
  time = na.omit(time)
  MAPE_am = mean(abs((time[,1]-time[,2])/time[,1]))
  print(MAPE_am)
  
  time_all = cbind(result_base_1_all[,1],result_1_all[,1])
  time_all = na.omit(time_all)
  MAPE_am_all = mean(abs((time_all[,1]-time_all[,2])/time_all[,1]))
  print(MAPE_am_all)
  
}

# データフレームに戻す
result_1_df <- data.frame(date=index(result_1), coredata(result_1))
result_1_df["tollgate_id"] = '3'
result_1_df["direction"] = '1'


# 午後の予測
if (predict == 1){
for (i in 18:24){
  train_2 <- data_3_1[paste('2016-09-19 00:00::2016-10-',i,' 16:40:',sep = '')]
  test_2 <- data_3_1[paste('2016-10-',i,' 17:00::2016-10-',i,' 18:40:',sep = '')]
  
  #データフレームに戻す（xtsだとfactorと数値を併用できないため）
  train_2 <- as.data.frame(train_2)
  test_base_2 <- as.data.frame(test_2)
  
  for (k in 1:1){
    train_2[,k] <- as.numeric(paste(train_2[,k]))
    train_2[,k] <- train_2[,k] + 1
    train_2[,k] <- log(train_2[,k])
    test_base_2[,k] <- as.numeric(paste(test_base_2[,k]))
    test_base_2[,k] <- test_base_2[,k] + 1
    test_base_2[,k] <- log(test_base_2[,k])
  }
  
  test_2 <- test_base_2
  test_2[,1] <- NA
  
  ss <- AddLocalLevel(list(), train_2[,1])
  ss <- AddSeasonal(ss, train_2[,1], nseasons = 72)
  ss <- AddAr(ss, train_2[,1], lag = 1)
  
  model <- bsts(paste(colnames(train_2[1])," ~ holiday_special + abnormal_day_9_20 + before_long_vacation_flag + workday * rain_1_3 + workday * rain_3 + before_special + workday * dow",sep = "")
                ,state.specification = ss
                ,niter=niter
                ,contrasts=list(
                  holiday_special=contrasts(train_2$holiday_special)
                  ,workday=contrasts(train_2$workday)
                  #,typhoon_flag=contrasts(train_2$typhoon_flag)
                  #,rain_0_1=contrasts(train_2$rain_0_1)
                  ,rain_1_3=contrasts(train_2$rain_1_3)
                  ,rain_3=contrasts(train_2$rain_3)
                  ,before_special=contrasts(train_2$before_special)
                  ,before_long_vacation_flag=contrasts(train_2$before_long_vacation_flag)
                  ,abnormal_day_9_20=contrasts(train_2$abnormal_day_9_20)
                  #,abnormal_holiday_am_1_0=contrasts(train_2$abnormal_holiday_am_1_0)
                  ,dow=contrasts(train_2$dow)
                )
                ,data = train_2[,c(1:11)]
  )
  
  pred_2 <- predict(model, burn=burn
                    #, horizon= 100
                    ,newdata=test_2[,c(2:11)]
  )
  
  test_2[,1] <- exp(pred_2$mean)
  
  if (i == 18){
    result_2 = test_2
  } else {
    result_2 = rbind(result_2,test_2)
  }
  
}
} else {
  for (i in 11:17){
    train_2 <- data_3_1[paste('2016-09-19 00:00::2016-10-',i,' 16:40:',sep = '')]
    
    #状態を揃える
    if (i == 11){
      train_2[paste('2016-10-',i,' 00:00::2016-10-',i,' 05:40:',sep = ''),1] <- NA
      train_2[paste('2016-10-',i,' 08:00::2016-10-',i,' 14:40:',sep = ''),1] <- NA
    } else {
      for (j in 11:16){
        train_2[paste('2016-10-',j,' 00:00::2016-10-',j,' 05:40:',sep = ''),1] <- NA
        train_2[paste('2016-10-',j,' 08:00::2016-10-',j,' 14:40:',sep = ''),1] <- NA
        train_2[paste('2016-10-',j,' 17:00::2016-10-',j+1,' 05:40:',sep = ''),1] <- NA
        train_2[paste('2016-10-',j+1,' 08:00::2016-10-',j+1,' 14:40:',sep = ''),1] <- NA
      }
    }
    
    test_base_2 <- data_3_1[paste('2016-10-',i,' 17:00::2016-10-',i,' 18:40:',sep = '')]
    test_base_2_all <- data_3_1[paste('2016-10-',i,' 17:00::2016-10-',i,' 23:40:',sep = '')]
    
    #データフレームに戻す（xtsだとfactorと数値を併用できないため）
    train_2 <- as.data.frame(train_2)
    test_base_2 <- as.data.frame(test_base_2)
    test_base_2_all <- as.data.frame(test_base_2_all)
    
    for (k in 1:1){
      train_2[,k] <- as.numeric(paste(train_2[,k]))
      train_2[,k] <- train_2[,k] + 1
      train_2[,k] <- log(train_2[,k])
      test_base_2[,k] <- as.numeric(paste(test_base_2[,k]))
      test_base_2[,k] <- test_base_2[,k] + 1
      test_base_2[,k] <- log(test_base_2[,k])
      test_base_2_all[,k] <- as.numeric(paste(test_base_2_all[,k]))
      test_base_2_all[,k] <- test_base_2_all[,k] + 1
      test_base_2_all[,k] <- log(test_base_2_all[,k])
    }
    
    test_2 <- test_base_2
    test_2[,1] <- NA
    test_2_all <- test_base_2_all
    test_2_all[,1] <- NA
    
    ss <- list()
    ss <- AddLocalLevel(list(), train_2[,1])
    ss <- AddSeasonal(ss, train_2[,1], nseasons = 72
                      #, season.duration = 7
    )
    ss <- AddAr(ss, train_2[,1], lag = 1)
    
    model <- bsts(paste(colnames(train_2[1])," ~ holiday_special + abnormal_day_9_20 + before_long_vacation_flag + workday * rain_1_3 + workday * rain_3 + before_special + workday * dow",sep = "")
                  ,state.specification = ss
                  ,niter=niter
                  ,contrasts=list(
                    holiday_special=contrasts(train_2$holiday_special)
                    ,workday=contrasts(train_2$workday)
                    #,typhoon_flag=contrasts(train_2$typhoon_flag)
                    #,rain_0_1=contrasts(train_2$rain_0_1)
                    ,rain_1_3=contrasts(train_2$rain_1_3)
                    ,rain_3=contrasts(train_2$rain_3)
                    ,before_special=contrasts(train_2$before_special)
                    ,before_long_vacation_flag=contrasts(train_2$before_long_vacation_flag)
                    ,abnormal_day_9_20=contrasts(train_2$abnormal_day_9_20)
                    #,abnormal_holiday_am_1_0=contrasts(train_2$abnormal_holiday_am_1_0)
                    #,dow=contrasts(train_2$dow)
                  )
                  ,data = train_2[,c(1:11)]
    )
    
    pred_2 <- predict(model, burn=burn
                      ,newdata=test_2[,c(2:11)]
    )
    
    pred_2_all <- predict(model, burn=burn
                          ,newdata=test_2_all[,c(2:11)]
    )
    
    test_2[,1] <- exp(pred_2$mean)
    test_2_all[,1] <- exp(pred_2_all$mean)
    test_base_2[,1] <- exp(test_base_2[,1])
    test_base_2_all[,1] <- exp(test_base_2_all[,1])
    
    if (i == 11){
      result_2 = test_2
      result_2_all = test_2_all
      result_base_2 = test_base_2
      result_base_2_all = test_base_2_all
    } else {
      result_2 = rbind(result_2,test_2)
      result_2_all = rbind(result_2_all,test_2_all)
      result_base_2 = rbind(result_base_2,test_base_2)
      result_base_2_all = rbind(result_base_2_all,test_base_2_all)
    }
  }
  # MAPE_v_1_0_pm = mean(abs((result_base_2[,1]-result_2[,1])/result_base_2[,1]))
  # print(MAPE_v_1_0_pm)
  
  time = cbind(result_base_2[,1],result_2[,1])
  time = na.omit(time)
  MAPE_pm = mean(abs((time[,1]-time[,2])/time[,1]))
  print(MAPE_pm)
  
  time_all = cbind(result_base_2_all[,1],result_2_all[,1])
  time_all = na.omit(time_all)
  MAPE_pm_all = mean(abs((time_all[,1]-time_all[,2])/time_all[,1]))
  print(MAPE_pm_all)
}
  
# 予測の場合はそのまま出力する
# 検証の場合はMAPEを出力する

if (predict == 1){
  # データフレームに戻す
  result_2_df <- data.frame(date=index(result_2), coredata(result_2))
  result_2_df["tollgate_id"] = '3'
  result_2_df["direction"] = '1'
  
  # 結合してデータを残す
  result_3_1 <- rbind(result_1_df,result_2_df)
  write.csv(result_3_1,"result_3_1.csv")
  save(result_3_1,file="result_3_1.RData")
} else {
  # 結合してMAPEを計算
  result_3_1 <- rbind(result_1,result_2)
  base_3_1 <- rbind(result_base_1,result_base_2)
  MAPE_v_3_1 = mean(abs((base_3_1[,1]-result_3_1[,1])/base_3_1[,1]))
  MAPE_v_3_1
  save(result_3_1,file="result_3_1_kensyo.RData")
  save(base_3_1,file="base_3_1_kensyo.RData")
  save(MAPE_v_3_1,file="MAPE_v_3_1.RData")
}

setwd(scriptDir)
rm(list=ls(all=TRUE))