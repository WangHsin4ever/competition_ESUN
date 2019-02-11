
library(dplyr)
library(readxl)
library(ggplot2)
library(caTools)
library(mice)

########### 這邊以下是王欣的喔 －－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－

getwd()
path <- "/Users/User/Desktop/MBA/比賽_玉山/data/all.xlsx"

###載入資料
conse<-read.csv('data/TBN_Y_ZERO.csv')
recent<-read.csv('data/TBN_RECENT_DT.csv')
behave<-read.csv('data/TBN_CUST_BEHAVIOR.csv')

wm<-read.csv('data/TBN_WM_TXN.csv')
ln<-read.csv('data/TBN_LN_APPLY.csv')
fx<-read.csv('data/TBN_FX_TXN.csv')
cc<-read.csv('data/TBN_CC_APPLY.csv')
cust<-read.csv('data/TBN_CIF.csv')

###檢視資料
unique(wm$CUST_NO)
table(ln$LN_USE)
colnames(cc)
colnames(recent)
colnames(ln_f90)
unique(ln$CUST_NO)
###整理信用卡
hist(cc$TXN_DT,breaks = 10) #每10天的交易次數

cc_f90<-filter(cc,TXN_DT<9538) #前90天的信用卡交易資料

cc_1 <- group_by(cc_f90, CUST_NO) %>% summarise(
  ccn=n(),
  cc_recent=min(TXN_DT))

table(cc_1$ccn)

###整理外匯
fx_f90<-filter(fx,TXN_DT<9538) #前90天的外匯交易資料

fx_1 <- group_by(fx_f90, CUST_NO) %>% summarise(
  fxn=n(),
  fx_recent=min(TXN_DT),
  fx_amt = mean(FX_TXN_AMT))

table(fx_1$fxn)

###整理信貸
ln_f90<-filter(ln,TXN_DT<9538) #前90天的外匯交易資料

ln_1 <- group_by(ln_f90, CUST_NO) %>% summarise(
  lnn=n(),
  ln_recent=min(TXN_DT),
  ln_amt = mean(LN_AMT),
  ln_use = LN_USE[which.max(TXN_DT)]
)

table(ln_1$lnn)

###整理信託
wm_f90<-filter(wm,TXN_DT<9538) #前90天的外匯交易資料
wm$CUST_RISK_CODE=as.factor(wm$CUST_RISK_CODE)
wm$INVEST_TYPE_CODE=as.factor(wm$INVEST_TYPE_CODE)

wm_1 <- group_by(wm_f90, CUST_NO) %>% summarise(
  wmn=n(),
  wm_recent=min(TXN_DT),
  wm_amt = mean(WM_TXN_AMT),
  wm_risk_code = CUST_RISK_CODE[which.max(TXN_DT)],
  wm_invest_type1 = sum(INVEST_TYPE_CODE==1),
  wm_invest_type2 = sum(INVEST_TYPE_CODE==2)
)

table(wm_1$wmn)

###交集
A <- inner_join(behave,recent,by="CUST_NO")
B <- inner_join(A,cust,by="CUST_NO")
colnames(B)
C <- B %>% group_by(CUST_NO) %>% summarise(
  recent_cc = min(CC_RECENT_DT),
  recent_fx = min(FX_RECENT_DT),
  recent_ln = min(LN_RECENT_DT),
  recent_wm = min(WM_RECENT_DT))

D <-left_join(C,cc_1,by="CUST_NO")
E <-left_join(D,fx_1,by="CUST_NO")
G <-left_join(E,ln_1,by="CUST_NO")
H <-left_join(G,wm_1,by="CUST_NO")

###補缺值
H[is.na(H)]=0

###處理新變數
H$ln_use=as.character(H$ln_use)
H$ln_use=as.numeric(H$ln_use)
H$ln_use=as.factor(H$ln_use)*6

H$wm_risk_code=as.factor(H$wm_risk_code)

write.csv(H,"model1_complete.csv")

table(ln$LN_USE)
table(wm$CUST_NO) %>% table

###全部完整版
Final <- read.csv("model_finish_20180129.csv")

















########### 這邊以下是嘉羽的喔 －－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－
# 讀取檔案
path <- "/Users/Tommy/Documents/學校資料/Esunbank/"
setwd(path)

table_TBN_FX_TXN <- read.csv(paste(path,"TBN_FX_TXN.csv",sep=""),stringsAsFactors = F)
table_TBN_CUST_BEHAVIOR <- read.csv(paste(path,"TBN_CUST_BEHAVIOR.csv",sep=""),stringsAsFactors = F)
table_TBN_WM_TXN <- read.csv(paste(path,"TBN_WM_TXN.csv",sep=""),stringsAsFactors = F)
table_TBN_Y_ZERO <- read.csv(paste(path,"TBN_Y_ZERO.csv",sep=""),stringsAsFactors = F)
table_TBN_LN_APPLY <- read.csv(paste(path,"TBN_LN_APPLY.csv",sep=""),stringsAsFactors = F)
table_TBN_CIF <- read.csv(paste(path,"TBN_CIF.csv",sep=""),stringsAsFactors = F)
table_TBN_CC_APPLY <- read.csv(paste(path,"TBN_CC_APPLY.csv",sep=""),stringsAsFactors = F)
table_TBN_RECENT_DT <- read.csv(paste(path,"TBN_RECENT_DT.csv",sep=""),stringsAsFactors = F)

# 檢驗顧客資料重疊情形
length(unique(table_TBN_Y_ZERO$CUST_NO))        # 30000 最後的人們
sum(table_TBN_Y_ZERO$CUST_NO %in% table_TBN_CIF[unique(table_TBN_CIF$CUST_NO) %in% table_TBN_CUST_BEHAVIOR$CUST_NO,]$CUST_NO)
length(unique(table_TBN_CUST_BEHAVIOR$CUST_NO)) # 195000 網頁瀏覽
sum(unique(table_TBN_CUST_BEHAVIOR$CUST_NO) %in% table_All$CUST_NO)

length(unique(table_TBN_RECENT_DT$CUST_NO))     # 195000 最近
mean(unique(table_TBN_RECENT_DT$CUST_NO) %in% table_TBN_Y_ZERO$CUST_NO)     # 0.1538
length(unique(table_TBN_CC_APPLY$CUST_NO))     # 44112 信用卡核卡日期資料
sum(unique(table_TBN_CC_APPLY$CUST_NO) %in% table_TBN_CIF[unique(table_TBN_CIF$CUST_NO) %in% table_TBN_CUST_BEHAVIOR$CUST_NO,]$CUST_NO)
sum(unique(table_TBN_CC_APPLY$CUST_NO) %in% table_TBN_CIF$CUST_NO)
length(unique(table_TBN_FX_TXN$CUST_NO)) # 74111 外匯交易
sum(unique(table_TBN_FX_TXN$CUST_NO) %in% table_All$CUST_NO)
mean(unique(table_TBN_FX_TXN$CUST_NO) %in% table_TBN_Y_ZERO$CUST_NO)        # 0.0783
length(unique(table_TBN_WM_TXN$CUST_NO)) # 14004 信託交易
sum(unique(table_TBN_WM_TXN$CUST_NO) %in% table_All$CUST_NO)
mean(unique(table_TBN_WM_TXN$CUST_NO) %in% table_TBN_Y_ZERO$CUST_NO)        # 0.0816
length(unique(table_TBN_LN_APPLY$CUST_NO)) # 6654 信貸申請    
sum(unique(table_TBN_LN_APPLY$CUST_NO) %in% table_All$CUST_NO)
mean(unique(table_TBN_LN_APPLY$CUST_NO) %in% table_TBN_Y_ZERO$CUST_NO)      # 0.0610
length(unique(table_TBN_CIF$CUST_NO)) # 187679 顧客資料庫
sum(unique(table_TBN_CIF$CUST_NO) %in% table_All$CUST_NO)
table_TBN_CIF[unique(table_TBN_CIF$CUST_NO) %in% table_TBN_CUST_BEHAVIOR$CUST_NO,]
sum(unique(table_TBN_CIF$CUST_NO) %in% table_TBN_CUST_BEHAVIOR$CUST_NO)
mean(unique(table_TBN_CIF$CUST_NO) %in% table_TBN_Y_ZERO$CUST_NO) * length(unique(table_TBN_CIF$CUST_NO)) # 29854 ??

# 9447 + 1 ~ 9447 + 120

# 製作第四個月的四項結果
# 9447 + 90 ~ 9447 + 120
# 9538 ~ 9567
table_All <- table_TBN_RECENT_DT
table_All$CC_IND <- 0
table_All$FX_IND <- 0
table_All$LN_IND <- 0
table_All$WM_IND <- 0
table_All <- table_All[,c(-2,-3,-4,-5)]

table_All_73531 <- table_All[table_All$CUST_NO %in% unique(table_TBN_CIF$CUST_NO),]
table_All_73531 <- table_All_73531[order(table_All_73531$CUST_NO,decreasing = T),]
# mean(unique(table_All_73531$CUST_NO) %in% unique(table_TBN_CIF$CUST_NO))

table_TBN_RECENT_DT_73531 <- table_TBN_RECENT_DT[table_TBN_RECENT_DT$CUST_NO %in% unique(table_TBN_CIF$CUST_NO),]
table_TBN_RECENT_DT_73531 <- table_TBN_RECENT_DT_73531[order(table_TBN_RECENT_DT_73531$CUST_NO,decreasing = T),]
table_TBN_RECENT_DT_73531_time9567 <- table_TBN_RECENT_DT_73531
# mean(unique(table_TBN_RECENT_DT_73531$CUST_NO) %in% unique(table_TBN_CIF$CUST_NO))

######## CIF 還沒調整

## 更新核卡資料 ------------------------------------

t0 <-  proc.time()
table_TBN_CC_APPLY_73531 <- table_TBN_CC_APPLY[table_TBN_CC_APPLY$CUST_NO %in% unique(table_All_73531$CUST_NO),]
# table_TBN_CC_APPLY_73531 <- table_TBN_CC_APPLY_73531[table_TBN_CC_APPLY_73531$TXN_DT <= 9537,] # for 第三個月
# table_TBN_CC_APPLY_73531 <- table_TBN_CC_APPLY_73531[table_TBN_CC_APPLY_73531$TXN_DT <= 9507,] # for 第二個月
table_TBN_CC_APPLY_73531_CP <- rbind(table_TBN_CC_APPLY_73531,data.frame(CUST_NO = "____",TXN_DT = 9800))
table_TBN_CC_APPLY_73531 <- table_TBN_CC_APPLY_73531[order(table_TBN_CC_APPLY_73531$CUST_NO,table_TBN_CC_APPLY_73531$TXN_DT,decreasing = T),]
table_TBN_CC_APPLY_73531_CP <- table_TBN_CC_APPLY_73531_CP[order(table_TBN_CC_APPLY_73531_CP$CUST_NO,table_TBN_CC_APPLY_73531_CP$TXN_DT,decreasing = T),]
table_TBN_CC_APPLY_73531_CP <- table_TBN_CC_APPLY_73531_CP[-nrow(table_TBN_CC_APPLY_73531_CP),]
table_TBN_CC_APPLY_73531$TXN_DT[table_TBN_CC_APPLY_73531_CP$CUST_NO == table_TBN_CC_APPLY_73531$CUST_NO] <- 0
proc.time() - t0  
# mean(unique(table_TBN_CC_APPLY_73531$CUST_NO) %in% unique(table_All_73531$CUST_NO))

table_TBN_CC_APPLY_73531 <- table_TBN_CC_APPLY_73531[table_TBN_CC_APPLY_73531$TXN_DT != 0,] ## 再補上次數
table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT[table_TBN_RECENT_DT_73531_time9567$CUST_NO %in% table_TBN_CC_APPLY_73531$CUST_NO] <- table_TBN_CC_APPLY_73531$TXN_DT

rm(table_TBN_CC_APPLY_73531_CP)

## 更新核卡資料 ------------------------------------

## 更新外匯資料 ------------------------------------

t0 <-  proc.time()
table_TBN_FX_TXN_73531 <- table_TBN_FX_TXN[table_TBN_FX_TXN$CUST_NO %in% unique(table_All_73531$CUST_NO),]
# table_TBN_FX_TXN_73531 <- table_TBN_FX_TXN_73531[table_TBN_FX_TXN_73531$TXN_DT <= 9537,] # for 第三個月
# table_TBN_FX_TXN_73531 <- table_TBN_FX_TXN_73531[table_TBN_FX_TXN_73531$TXN_DT <= 9507,] # for 第二個月
table_TBN_FX_TXN_73531_CP <- rbind(table_TBN_FX_TXN_73531,data.frame(CUST_NO = "____",TXN_DT = 9800,FX_TXN_AMT = 0))
table_TBN_FX_TXN_73531 <- table_TBN_FX_TXN_73531[order(table_TBN_FX_TXN_73531$CUST_NO,table_TBN_FX_TXN_73531$TXN_DT,decreasing = T),]
table_TBN_FX_TXN_73531_CP <- table_TBN_FX_TXN_73531_CP[order(table_TBN_FX_TXN_73531_CP$CUST_NO,table_TBN_FX_TXN_73531_CP$TXN_DT,decreasing = T),]
table_TBN_FX_TXN_73531_CP <- table_TBN_FX_TXN_73531_CP[-nrow(table_TBN_FX_TXN_73531_CP),]
table_TBN_FX_TXN_73531$TXN_DT[table_TBN_FX_TXN_73531_CP$CUST_NO == table_TBN_FX_TXN_73531$CUST_NO] <- 0
proc.time() - t0  
# mean(unique(table_TBN_CC_APPLY_73531$CUST_NO) %in% unique(table_All_73531$CUST_NO))

table_TBN_FX_TXN_73531 <- table_TBN_FX_TXN_73531[table_TBN_FX_TXN_73531$TXN_DT != 0,] ## 再補上次數
table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT[table_TBN_RECENT_DT_73531_time9567$CUST_NO %in% table_TBN_FX_TXN_73531$CUST_NO] <- table_TBN_FX_TXN_73531$TXN_DT

rm(table_TBN_FX_TXN_73531_CP)

## 更新外匯資料 ------------------------------------

## 更新信貸資料 ------------------------------------

t0 <-  proc.time()
table_TBN_LN_APPLY_73531 <- table_TBN_LN_APPLY[table_TBN_LN_APPLY$CUST_NO %in% unique(table_All_73531$CUST_NO),]
# table_TBN_LN_APPLY_73531 <- table_TBN_LN_APPLY_73531[table_TBN_LN_APPLY_73531$TXN_DT <= 9537,] # for 第三個月
# table_TBN_LN_APPLY_73531 <- table_TBN_LN_APPLY_73531[table_TBN_LN_APPLY_73531$TXN_DT <= 9507,] # for 第二個月
table_TBN_LN_APPLY_73531_CP <- rbind(table_TBN_LN_APPLY_73531,data.frame(CUST_NO = "____",TXN_DT = 9800,LN_AMT = 0,LN_USE = "00"))
table_TBN_LN_APPLY_73531 <- table_TBN_LN_APPLY_73531[order(table_TBN_LN_APPLY_73531$CUST_NO,table_TBN_LN_APPLY_73531$TXN_DT,decreasing = T),]
table_TBN_LN_APPLY_73531_CP <- table_TBN_LN_APPLY_73531_CP[order(table_TBN_LN_APPLY_73531_CP$CUST_NO,table_TBN_LN_APPLY_73531_CP$TXN_DT,decreasing = T),]
table_TBN_LN_APPLY_73531_CP <- table_TBN_LN_APPLY_73531_CP[-nrow(table_TBN_LN_APPLY_73531_CP),]
table_TBN_LN_APPLY_73531$TXN_DT[table_TBN_LN_APPLY_73531_CP$CUST_NO == table_TBN_LN_APPLY_73531$CUST_NO] <- 0
proc.time() - t0  
# mean(unique(table_TBN_CC_APPLY_73531$CUST_NO) %in% unique(table_All_73531$CUST_NO))

table_TBN_LN_APPLY_73531 <- table_TBN_LN_APPLY_73531[table_TBN_LN_APPLY_73531$TXN_DT != 0,] ## 再補上次數
table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT[table_TBN_RECENT_DT_73531_time9567$CUST_NO %in% table_TBN_LN_APPLY_73531$CUST_NO] <- table_TBN_LN_APPLY_73531$TXN_DT

rm(table_TBN_LN_APPLY_73531_CP)

## 更新信貸資料 ------------------------------------

## 更新信託資料 ------------------------------------

t0 <-  proc.time()
table_TBN_WM_TXN_73531 <- table_TBN_WM_TXN[table_TBN_WM_TXN$CUST_NO %in% unique(table_All_73531$CUST_NO),]
# table_TBN_WM_TXN_73531 <- table_TBN_WM_TXN_73531[table_TBN_WM_TXN_73531$TXN_DT <= 9537,] # for 第三個月
# table_TBN_WM_TXN_73531 <- table_TBN_WM_TXN_73531[table_TBN_WM_TXN_73531$TXN_DT <= 9507,] # for 第二個月
table_TBN_WM_TXN_73531_CP <- rbind(table_TBN_WM_TXN_73531,data.frame(CUST_NO = "____",TXN_DT = 9800,CUST_RISK_CODE = 0,INVEST_TYPE_CODE = 0,WM_TXN_AMT = 0))
table_TBN_WM_TXN_73531 <- table_TBN_WM_TXN_73531[order(table_TBN_WM_TXN_73531$CUST_NO,table_TBN_WM_TXN_73531$TXN_DT,decreasing = T),]
table_TBN_WM_TXN_73531_CP <- table_TBN_WM_TXN_73531_CP[order(table_TBN_WM_TXN_73531_CP$CUST_NO,table_TBN_WM_TXN_73531_CP$TXN_DT,decreasing = T),]
table_TBN_WM_TXN_73531_CP <- table_TBN_WM_TXN_73531_CP[-nrow(table_TBN_WM_TXN_73531_CP),]
table_TBN_WM_TXN_73531$TXN_DT[table_TBN_WM_TXN_73531_CP$CUST_NO == table_TBN_WM_TXN_73531$CUST_NO] <- 0
proc.time() - t0  
# mean(unique(table_TBN_CC_APPLY_73531$CUST_NO) %in% unique(table_All_73531$CUST_NO))

table_TBN_WM_TXN_73531 <- table_TBN_WM_TXN_73531[table_TBN_WM_TXN_73531$TXN_DT != 0,] ## 再補上次數
table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT[table_TBN_RECENT_DT_73531_time9567$CUST_NO %in% table_TBN_WM_TXN_73531$CUST_NO] <- table_TBN_WM_TXN_73531$TXN_DT

rm(table_TBN_WM_TXN_73531_CP)

## 更新信託資料 ------------------------------------





write.csv(table_TBN_RECENT_DT_73531_time9567,"_TBN_RECENT_DT_73531_time9567.csv")



## 製作 1 0 結果表格
# 9537 ~ 9567
table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT[is.na(table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT)] <- 0
table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT[is.na(table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT)] <- 0
table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT[is.na(table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT)] <- 0
table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT[is.na(table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT)] <- 0
table_All_73531$CC_IND[(table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT >= 9538) & (table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT <= 9567) ] <- 1
table_All_73531$FX_IND[(table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT >= 9538) & (table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT <= 9567) ] <- 1
table_All_73531$LN_IND[(table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT >= 9538) & (table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT <= 9567) ] <- 1
table_All_73531$WM_IND[(table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT >= 9538) & (table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT <= 9567) ] <- 1

# for 第三個月
# table_All_73531$CC_IND[(table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT >= 9508) & (table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT <= 9537) ] <- 1
# table_All_73531$FX_IND[(table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT >= 9508) & (table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT <= 9537) ] <- 1
# table_All_73531$LN_IND[(table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT >= 9508) & (table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT <= 9537) ] <- 1
# table_All_73531$WM_IND[(table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT >= 9508) & (table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT <= 9537) ] <- 1

# for 第二個月
# table_All_73531$CC_IND[(table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT >= 9478) & (table_TBN_RECENT_DT_73531_time9567$CC_RECENT_DT <= 9507) ] <- 1
# table_All_73531$FX_IND[(table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT >= 9478) & (table_TBN_RECENT_DT_73531_time9567$FX_RECENT_DT <= 9507) ] <- 1
# table_All_73531$LN_IND[(table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT >= 9478) & (table_TBN_RECENT_DT_73531_time9567$LN_RECENT_DT <= 9507) ] <- 1
# table_All_73531$WM_IND[(table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT >= 9478) & (table_TBN_RECENT_DT_73531_time9567$WM_RECENT_DT <= 9507) ] <- 1


# write.csv(table_All_73531,"_TBN_Y_73531_time9507.csv")




# read the output table
table_All_73531 <- read.csv("_TBN_Y_73531_time9567.csv")
table_All_73531 <- table_All_73531[,c(-1)]



# Build X variable
table_TBN_CIF_73531 <- table_TBN_CIF[table_TBN_CIF$CUST_NO %in% table_All_73531$CUST_NO,]
table_TBN_CIF_73531_mice <- mice(table_TBN_CIF_73531 , m = 1 , maxit = 5 , method = "cart", seed = 175)
table_TBN_CIF_73531_MICE <- complete(table_TBN_CIF_73531_mice, 1)

table_All_73531 <- left_join(table_All_73531,table_TBN_CIF_73531_MICE)

# 羅吉斯回歸
set.seed(2019)
spl = sample.split(table_All_73531$CC_IND, SplitRatio=0.7)

TR1 <- subset(table_All_73531,spl)
TS1<- subset(table_All_73531,!spl)

CC_model = glm(CC_IND ~ ., TR1[c(2,6:9,11,12)], family=binomial())
summary(CC_model)
pred =  predict(CC_model, TS1, type="response")
cm = table(actual = TS1$CC_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS1$CC_IND)








spl = sample.split(table_4th_Choice$FX_IND, SplitRatio=0.7)

TR2 <- subset(table_4th_Choice,spl)
TS2<- subset(table_4th_Choice,!spl)

FX_model = glm(FX_IND ~ ., TR2[c(3,6:9,11,12)], family=binomial())
summary(FX_model)
pred =  predict(FX_model, TS2, type="response")
cm = table(actual = TS2$FX_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS2$FX_IND)

spl = sample.split(table_4th_Choice$LN_IND, SplitRatio=0.7)

TR3 <- subset(table_4th_Choice,spl)
TS3<- subset(table_4th_Choice,!spl)

LN_model = glm(LN_IND ~ ., TR3[c(4,6:9,11,12)], family=binomial())
summary(LN_model)
pred =  predict(LN_model, TS3, type="response")
cm = table(actual = TS3$LN_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS3$LN_IND)

spl = sample.split(table_4th_Choice$WM_IND, SplitRatio=0.7)

TR4 <- subset(table_4th_Choice,spl)
TS4<- subset(table_4th_Choice,!spl)

WM_model = glm(WM_IND ~ ., TR4[c(4,6:9,11,12)], family=binomial())
summary(WM_model)
pred =  predict(WM_model, TS4, type="response")
cm = table(actual = TS4$WM_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS4$WM_IND)





count <- 1
tableTemp <- table_TBN_WM_TXN[c(1),]

for( a in 2:nrow(table_TBN_WM_TXN)) {
  if ( table_TBN_WM_TXN$CUST_NO[a] == table_TBN_WM_TXN$CUST_NO[a-1] ) {
    if ( !is.na(table_TBN_WM_TXN$CUST_RISK_CODE[a]) & !is.na(table_TBN_WM_TXN$CUST_RISK_CODE[a-1]) ) {
      if ( table_TBN_WM_TXN$CUST_RISK_CODE[a] != table_TBN_WM_TXN$CUST_RISK_CODE[a-1] ) {
        print("FALSE!!")
        tableTemp <- rbind(tableTemp,table_TBN_WM_TXN[(a-1):a,])
        count <- count+1
      }
    }
  }
}
tableTemp <- tableTemp[-1,]






# ---------- Sin

tableA <- read.csv("model1_incomplete.csv")









# deal with webpages browsing customer behavior 

View(table_TBN_CUST_BEHAVIOR)
table(table_TBN_CUST_BEHAVIOR$PAGE) %>% table

# 排序 CUST BEHAVIOR 資料表
table_TBN_CUST_BEHAVIOR <- arrange(table_TBN_CUST_BEHAVIOR,CUST_NO,VISITDATE)
table_All_73531 <- arrange(table_All_73531,CUST_NO)

# 篩掉 第四個月的資料
table_TBN_CUST_BEHAVIOR <- filter(table_TBN_CUST_BEHAVIOR,VISITDATE <= 9537)
table_TBN_CUST_BEHAVIOR <- table_TBN_CUST_BEHAVIOR[table_TBN_CUST_BEHAVIOR$CUST_NO %in% table_All_73531$CUST_NO,]

# 轉資料型態 並排序
tableTemp <- as.data.frame(table(table_TBN_CUST_BEHAVIOR$CUST_NO))
tableTemp <- tableTemp[order(tableTemp$Var1),]

# 增加資料表新變數欄位
table_All_73531 <- cbind(table_All_73531,Page_Amount = rep(0,73531))
table_All_73531 <- cbind(table_All_73531,Days_Between = rep(0,73531))
table_All_73531 <- cbind(table_All_73531,Daily_Avarage = rep(0,73531))
table_All_73531 <- cbind(table_All_73531,Days = rep(0,73531))

# 填入資料
table_All_73531$Page_Amount[table_All_73531$CUST_NO %in% tableTemp$Var1] <- tableTemp$Freq

# 
## 計算平均幾天觀看一次 ( 多次間隔相減取平均 )
## 計算平均一天內觀看幾次 ( 多天觀看次數取平均 )
## 計算來看過幾天
#

# 補上前一 row 時間標記
table_TBN_CUST_BEHAVIOR$Former_VISIDATE <- c(0,table_TBN_CUST_BEHAVIOR$VISITDATE[c(-nrow(table_TBN_CUST_BEHAVIOR))])
table_TBN_CUST_BEHAVIOR$VISIDATE_diff <- table_TBN_CUST_BEHAVIOR$VISITDATE - table_TBN_CUST_BEHAVIOR$Former_VISIDATE

# 把 CUST_BEHAVIOR 第一個出現的值刪掉
table_TBN_CUST_BEHAVIOR_temp <- table_TBN_CUST_BEHAVIOR
table_TBN_CUST_BEHAVIOR_temp$mark <- rep(0,1307550)
table_TBN_CUST_BEHAVIOR$Former_CUST_NO <- c(0,table_TBN_CUST_BEHAVIOR$CUST_NO[c(-nrow(table_TBN_CUST_BEHAVIOR))])
table_TBN_CUST_BEHAVIOR_temp$mark[table_TBN_CUST_BEHAVIOR$CUST_NO != table_TBN_CUST_BEHAVIOR$Former_CUST_NO] <- 1
table_TBN_CUST_BEHAVIOR_temp <- filter(table_TBN_CUST_BEHAVIOR_temp,mark == 0,VISIDATE_diff > 0)

# 計算平均值
diff <- summarise(group_by(table_TBN_CUST_BEHAVIOR_temp,CUST_NO),mean(VISIDATE_diff))
diff <- data.frame(diff)

# 填入
table_All_73531$Days_Between[table_All_73531$CUST_NO %in% diff$CUST_NO] <- diff$mean.VISIDATE_diff.

# 計算不同的日數
oneofakind <- summarise(group_by(table_TBN_CUST_BEHAVIOR,CUST_NO),n_distinct(VISITDATE))
oneofakind <- data.frame(oneofakind)

# 填入
table_All_73531$Days[table_All_73531$CUST_NO %in% oneofakind$CUST_NO] <- oneofakind$n_distinct.VISITDATE.

# 計算共出現幾次
againandagain <- summarise(group_by(table_TBN_CUST_BEHAVIOR,CUST_NO),n())
againandagain <- data.frame(againandagain)

# 填入
table_All_73531$Daily_Avarage[table_All_73531$CUST_NO %in% againandagain$CUST_NO] <- againandagain$n.. / oneofakind$n_distinct.VISITDATE.

write.csv(table_All_73531,"table_Page_Babies.csv")

















# 讀威豪的檔案
table_WeiHow <- read.csv("model_finish_20180125.csv")
table_WeiHow <- table_WeiHow[,-1]

# mice 補缺漏值
table_WeiBo <- mice(table_WeiHow[,c(2,3,5:8)], m = 1 , maxit = 5 , method = "cart", seed = 175)
table_WeiHow[,c(2,3,5:8)] <- complete(table_WeiBo, 1)

write.csv(table_WeiHow,"model_finish_20180129.csv")

# 羅吉斯回歸
set.seed(2019)
spl = sample.split(table_WeiHow$CC_IND, SplitRatio=0.7)

TR1 <- subset(table_WeiHow,spl)
TS1<- subset(table_WeiHow,!spl)

CC_model = glm(CC_IND ~ ., TR1[c(2,6:9,11,12)], family=binomial())
summary(CC_model)
pred =  predict(CC_model, TS1, type="response")
cm = table(actual = TS1$CC_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS1$CC_IND)








spl = sample.split(table_WeiHow$FX_IND, SplitRatio=0.7)

TR2 <- subset(table_WeiHow,spl)
TS2<- subset(table_WeiHow,!spl)

FX_model = glm(FX_IND ~ ., TR2[c(3,6:9,11,12)], family=binomial())
summary(FX_model)
pred =  predict(FX_model, TS2, type="response")
cm = table(actual = TS2$FX_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS2$FX_IND)




spl = sample.split(table_WeiHow$LN_IND, SplitRatio=0.7)

TR3 <- subset(table_WeiHow,spl)
TS3<- subset(table_WeiHow,!spl)

LN_model = glm(LN_IND ~ ., TR3[c(4,6:9,11,12)], family=binomial())
summary(LN_model)
pred =  predict(LN_model, TS3, type="response")
cm = table(actual = TS3$LN_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS3$LN_IND)

spl = sample.split(table_WeiHow$WM_IND, SplitRatio=0.7)

TR4 <- subset(table_WeiHow,spl)
TS4<- subset(table_WeiHow,!spl)

WM_model = glm(WM_IND ~ ., TR4[c(4,6:9,11,12)], family=binomial())
summary(WM_model)
pred =  predict(WM_model, TS4, type="response")
cm = table(actual = TS4$WM_IND, predict = pred > 0.5)
cm
acc.ts = cm %>% {sum(diag(.))/sum(.)}
acc.ts        
colAUC(pred, TS4$WM_IND)





count <- 1
tableTemp <- table_TBN_WM_TXN[c(1),]

for( a in 2:nrow(table_TBN_WM_TXN)) {
  if ( table_TBN_WM_TXN$CUST_NO[a] == table_TBN_WM_TXN$CUST_NO[a-1] ) {
    if ( !is.na(table_TBN_WM_TXN$CUST_RISK_CODE[a]) & !is.na(table_TBN_WM_TXN$CUST_RISK_CODE[a-1]) ) {
      if ( table_TBN_WM_TXN$CUST_RISK_CODE[a] != table_TBN_WM_TXN$CUST_RISK_CODE[a-1] ) {
        print("FALSE!!")
        tableTemp <- rbind(tableTemp,table_TBN_WM_TXN[(a-1):a,])
        count <- count+1
      }
    }
  }
}
tableTemp <- tableTemp[-1,]








