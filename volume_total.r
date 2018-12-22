#----------------------------
# volume prediction total
# 2017/04/12
# author:hyodo
# 非効率なコードなので、後からキレイにする
#----------------------------

## 初期化
rm(list=ls(all=TRUE))

## ファイル読み込み
# データのあるパス
DataDir <- "InputDir"

## 必要なライブラリたち
library(xts)
library(bsts)
library(dplyr)

# データのロード
load("option.RData")
setwd(OutPutDir)

if (predict == 1){
  load("result_1_0.RData")
  load("result_1_1.RData")
  load("result_2_0.RData")
  load("result_3_0.RData")
  load("result_3_1.RData")
  load("time_window.RData")
  
  # 結合
  names(result_1_0)[2] <- "volume"
  names(result_1_1)[2] <- "volume"
  names(result_2_0)[2] <- "volume"
  names(result_3_0)[2] <- "volume"
  names(result_3_1)[2] <- "volume"
  
  result <- rbind_all(list(
                      result_1_0
                      ,result_1_1
                      ,result_2_0
                      ,result_3_0
                      ,result_3_1
                      )
                      )
  
  # Dataframeに戻して、time_windowをくっつける
  #pred_result <- data.frame(date=index(result), coredata(result))
  window <- data.frame(date=index(time_window), coredata(time_window))
  library(dplyr)
  all_result <- 
    dplyr::inner_join(result , window ,by="date") %>%
    dplyr::arrange(tollgate_id,direction,date)
  
  result_volume <- all_result[,c("tollgate_id"
                                 ,"coredata.time_window."
                                 ,"direction"
                                 ,"volume")]
  colnames(result_volume) <- c(
    "tollgate_id"
    ,"time_window"
    ,"direction"
    ,"volume"
  )
  
  #対数系列にしているので、結果から1を引く
  #result_volume["volume"] = result_volume["volume"] - 1
  
  write.csv(all_result,'all_result_20170419_300_80.csv')
  write.csv(result_volume,'volume_predict_20170419_300_80.csv')

} else {

  load("MAPE_v_1_0.RData")
  load("MAPE_v_1_1.RData")
  load("MAPE_v_2_0.RData")
  load("MAPE_v_3_0.RData")
  load("MAPE_v_3_1.RData")
  load("result_1_0_kensyo.RData")
  load("result_1_1_kensyo.RData")
  load("result_2_0_kensyo.RData")
  load("result_3_0_kensyo.RData")
  load("result_3_1_kensyo.RData")
  load("base_1_0_kensyo.RData")
  load("base_1_1_kensyo.RData")
  load("base_2_0_kensyo.RData")
  load("base_3_0_kensyo.RData")
  load("base_3_1_kensyo.RData")

  pred_total <- rbind(result_1_0
                      ,result_1_1
                      ,result_2_0
                      ,result_3_0
                      ,result_3_1
                      )
  
  base_total <- rbind(base_1_0
                      ,base_1_1
                      ,base_2_0
                      ,base_3_0
                      ,base_3_1
                      )
  
  MAPE_total = mean(abs((base_total[,1]-pred_total[,1])/base_total[,1]))
  
  print(paste ("MAPE_total =",MAPE_total,sep = ''))
  print(paste ("MAPE_v_1_0 =",MAPE_v_1_0,sep = ''))
  print(paste ("MAPE_v_1_1 =",MAPE_v_1_1,sep = ''))
  print(paste ("MAPE_v_2_0 =",MAPE_v_2_0,sep = ''))
  print(paste ("MAPE_v_3_0 =",MAPE_v_3_0,sep = ''))
  print(paste ("MAPE_v_3_1 =",MAPE_v_3_1,sep = ''))
  
}
