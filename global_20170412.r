#----------------------------
# volume prediction
# 2017/04/11
# author:hyodo
##
# 今後の精度改善の取り組み
# 車の比率とかETCとか入れる
# （入れ方が難しいが、とりあえずローカルレベルで良いか…？）
# enter = 0のtollgateについて、重要なリンクのボリュームと
# 分岐の比率などを入れる
# ここまで入れれば相当変わってきそう
# 曜日の情報入れる？
#----------------------------

## 初期化
rm(list=ls(all=TRUE))

## ファイル読み込み
# ホーム決定
#InPutDir <- "C:/Users/makoto.hyodo/Desktop/kddcup/kddcup2017_jiro_ramen/hyodo/data/Input"
OutPutDir <- "C:/Users/makoto.hyodo/Desktop/kddcup/kddcup2017_jiro_ramen/hyodo/data/Output"
scriptDir <- "C:/Users/makoto.hyodo/Desktop/kddcup/kddcup2017_jiro_ramen/hyodo/R/volume/each_volume/base"

setwd(scriptDir)

# オプション
predict = 1 #検証の時は0 予測の時は1
niter = 10
burn = 5

# オブジェクト保存
save(list = ls(), file= "option.Rdata")

# スクリプト実行
source("make_data.R")
source("volume_1_0_hyodo_20170511.R")
source("volume_1_1_hyodo_20170511.R")
source("volume_2_0_hyodo_20170411.R")
source("volume_3_0_hyodo_20170411.R")
source("volume_3_1_hyodo_20170411.R")
source("volume_total_hyodo_20170411.R")

# OutputDirに結果が格納されます
