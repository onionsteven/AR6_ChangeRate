# 20230928 計算重現期雨量

library(lmomco) #計算L-monment

library(lattice) #調用levelplot
library(beepr)
library(magrittr)

# 檢視資料夾 csv檔名 -------------------------------------------------------------


# 指定資料夾 檢視各情境
dir_path = "D:/楊松勳/NCDR_AR6_2023/極端降雨指標"

scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾

sci_mk = sapply(scenery_namez[-1], function(x){
  sda = strsplit(x,"/")
  tail(sda[[1]],1)
});unname(sci_mk)





# 極端指標RData路徑 10情境 5指標
rx_1_to_5_path_nest = lapply(scenery_namez,function(x) list.files(x,full.names = T))
rx_1_to_5_path_nest

length(rx_1_to_5_path_nest) # 10 第一個是全覽
# 
sapply(rx_1_to_5_path_nest[-1],length)

# 取出所有rx1 RData路徑
rx1_sci9_rdata_path = sapply(rx_1_to_5_path_nest[-1],function(x){x[1]})
rx1_sci9_rdata_path
# 讀取 情境1 rx1
load(rx1_sci9_rdata_path[1])
#
length(rx1_by_modelz) #110
names(rx1_by_modelz) #模式名稱

rx_data_dim = t(unname(sapply(rx1_by_modelz,dim)))
rx_data_dim
yr_not_20_tel = which(rx_data_dim[,2]!=20)
yr_not_20_tel # 10 11 36 37 65 66 92 93

sda = rx1_by_modelz[yr_not_20_tel]
rx_data_dim[yr_not_20_tel,2]


colnames(rx1_by_modelz[[93]])


# 根據給定數字 處理情境資料夾 head----------------------------------------------------------



# 給定2~10 檢視csv檔名
sci_num = 6
rdata_namez = list.files(scenery_namez[sci_num])
length(rdata_namez)
head(rdata_namez)

rdata_pathz = list.files(scenery_namez[sci_num],full.names = T)
load(rdata_pathz[[1]])

# 取特性檢視一下
length(rx1_by_modelz)
t(unname(sapply(rx1_by_modelz, dim)))
unname(sapply(rx1_by_modelz, median,na.rm=T))



# 20230917 計算功能 -----------------------------------------------------------

# 參數估計 與 重現期強度計算 head-----

library(lmomco) #計算L-monment
#函數：以moment法及L-monment法估計參數 (若資料都是NA 返回6個NA)
#method of moment
para.est <- function(x){			#temp[1:3]、temp[4:6]分別為動差法、線性動差法推估的3參數
  x <- x[!is.na(x)];
  n <- length(x)
  if(n==0){return(rep(NA,6))}
  m=mean(x)    #mean(MOM)
  m2=sum((x-m)^2)/n
  m3=sum((x-m)^3)/n
  ms=m2^0.5    #standard deviation(MOM)
  mk=m3/m2^1.5 #skewness(MOM)
  lmr=lmom.ub(x)
  p=parpe3(lmr)  #用l-moment算PT3參數
  lm=p$para[[1]]  #mean(LM)
  ls=p$para[[2]]  #std (LM)
  lk=p$para[[3]]  #skewness(LM)
  return(c(m,ms,mk,lm,ls,lk))
}

#函數：估計abe
abe.est <- function(x){
  b <- (2/x[3])^2 #x[3]:skewness
  a <- x[2]/sqrt(b) #x[2]:std
  e <- x[1]-x[2]*sqrt(b) #x[1]:mean
  return(c(a,b,e))
}


#1.將雨量資料 藉由函數"para.est" 以mom/L-mon推估出 "mean/sd/skewness" 共6個參數
#2.由mean/sd/skewness 藉由函數"abe.est" 推估abe三個參數


# 20230929 由極端指標 計算重現期雨量 head--------------------------------------------------


ptm = proc.time()

#函數：設定資料夾路徑/ 讀入資料/選取分析年份/計算mean sd skewness參數
returned_m_sd_sk_x6 = function(sda){
  sda[is.infinite(sda)]=NA
  oc=apply(sda,1,para.est)
  return(oc)
}

#以lapply 執行mean sd skewness參數估計
length(rx1_by_modelz)
para_m_sd_sk_by_modelz = lapply(rx1_by_modelz,function(m) returned_m_sd_sk_x6(m))
# 31模式 8.45s
dim(para_m_sd_sk_by_modelz[[1]]) # 6 1471

#以lapply 執行abe參數估計
# length(fnz)
# ptm=proc.time() #約1秒?
para_abe_by_modelz = lapply(para_m_sd_sk_by_modelz,function(m) apply(m,2,function(y) abe.est(y[4:6])))
# proc.time()-ptm # 0.31

rain_return_period_6_by_modelz = lapply(para_abe_by_modelz,function(para_abe_one_model){
  # 函數:給定參數abe 建立 qgamma 函數 
  rain_cri_given_return_period=function(return_period){
    cri_rain = qgamma((1-1/(return_period)),shape=para_abe_one_model[2,],scale=para_abe_one_model[1,])+para_abe_one_model[3,]
    return(cri_rain)
  }
  # 給定重現期 產生重現期雨量強度
  rain_return_period_6 = sapply(c(5,10,25,50,100,200),function(y) rain_cri_given_return_period(y))
  rain_return_period_6
});proc.time()-ptm # 0.44
# 12.15 s

# 20230929 由極端指標 計算重現期雨量 tail--------------------------------------------------

length(rain_return_period_6_by_modelz)
t(unname(sapply(rain_return_period_6_by_modelz,dim)))
names(rain_return_period_6_by_modelz)

dim(rain_return_period_6_by_modelz[[1]])

rain_return_period_6_by_modelz[[1]] #1471 6

sda = rain_return_period_6_by_modelz[[1]] #1471 6
range(sda,na.rm = T) #99.7 1727
dim(sda)
plot(sda[700,],ty='o',ylim=c(50,2000))
plt_da = sda[(is.na(sda[,1])==F),]
dim(plt_da)
for (i in seq(1,1412)){
  lines(plt_da[i,],ty='o')
}


# 20230929 09:29 時期的判斷 ----------------------------------------------------


baseline = seq(1995,2014)
near_term = seq(2021,2040)
mid_term = seq(2041,2060)
long_term = seq(2081,2100)

# 取極端指標 rx
# 給定2~10 檢視csv檔名
sci_num = 3
rdata_namez = list.files(scenery_namez[sci_num])
length(rdata_namez)
head(rdata_namez)

rdata_pathz = list.files(scenery_namez[sci_num],full.names = T)
load(rdata_pathz[[1]])

length(rx1_by_modelz) #110

# 取一個情境 一個rx
yr_list_by_modelz = lapply(rx1_by_modelz,function(x) as.numeric(colnames(x)))
length(yr_list_by_modelz)
t(unname(sapply(yr_list_by_modelz,range)))


# 20231001 檢查 rx計算內容是否正確 --------------------------------------------------

length(rx1_by_modelz) # 110
t(unname(sapply(rx1_by_modelz,dim)))

tt = cbind(t(unname(sapply(rx1_by_modelz,dim))),sapply(csv_yrz_sci_by_modelz[[1]],length))
tt[,2]-tt[,3]

# 20231001 檢查 rx計算內容是否正確 --------------------------------------------------
# 取極端指標 rx
# 給定2~10 檢視csv檔名

dir_path = "D:/楊松勳/NCDR_AR6_2023/極端降雨指標"

scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾

# 極端指標RData路徑 10情境 5指標
rx_1_to_5_path_nest = lapply(scenery_namez,function(x) list.files(x,full.names = T))
rx_1_to_5_path_nest

length(rx_1_to_5_path_nest) # 10 第一個是全覽
# 
sapply(rx_1_to_5_path_nest[-1],length)

# 取出所有rx1 RData路徑
rx1_sci9_rdata_path = sapply(rx_1_to_5_path_nest[-1],function(x){x[1]})
rx1_sci9_rdata_path
# 讀取 情境1 rx1
load(rx1_sci9_rdata_path[1])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[2])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[3])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[5])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

# 檢視 模式數量及情境 是否正確
lapply(rx1_sci9_rdata_path,function(x){
  load(x)
  print(length(rx_x_by_modelz))
  print(names(rx_x_by_modelz)[1])
})
# 小結:看起來正確

# 檢視年分
tt = sapply(rx1_sci9_rdata_path,function(x){
  load(x)
  oc = sapply(rx_x_by_modelz,function(x)dim(x)[2])
  oc
  # print(names(rx_x_by_modelz)[1])
})
tt


# 20231004  ---------------------------------------------------------------

# 檢視年分
rx1_sci9_list = sapply(rx1_sci9_rdata_path,function(x){
  load(x)
  oc = rx_x_by_modelz
  oc
})
length(rx1_sci9_list)
unname(sapply(rx1_sci9_list,length)) # 110  99  66  31  31  28  29  27  29

# 檢視情境1 和 基期年分重疊的部分
sapply(rx1_sci9_list[[1]],function(x) intersect(baseline,as.numeric(colnames(x))))



load(rx1_sci9_rdata_path[1])
length(rx_x_by_modelz)
sapply(rx_x_by_modelz,function(x) dim(x)[2])

load(rx1_sci9_rdata_path[5])
length(rx_x_by_modelz)
sapply(rx_x_by_modelz,function(x) dim(x)[2])

# 檢視情境5 和 基期年分重疊的部分
sapply(rx1_sci9_list[[5]],function(x) intersect(baseline,as.numeric(colnames(x))))

# sci_num = 3
# rdata_namez = list.files(scenery_namez[sci_num])
# length(rdata_namez)
# head(rdata_namez)
# 
# rdata_pathz = list.files(scenery_namez[sci_num],full.names = T)
# rdata_pathz


rx1_sci9_rdata_path

for (i in 1:9){
  i= 7
  load(rx1_sci9_rdata_path[[i]])
  
  length(rx1_by_modelz) #110
  
  # 取一個情境 一個rx
  yr_list_by_modelz = lapply(rx1_by_modelz,function(x) as.numeric(colnames(x)))
  length(yr_list_by_modelz)
  t(unname(sapply(yr_list_by_modelz,range)))
  
  
  
  
  length(rx1_by_modelz) # 110
  t(unname(sapply(rx1_by_modelz,dim)))
  
  tt = cbind(t(unname(sapply(rx1_by_modelz,dim))),sapply(csv_yrz_sci_by_modelz[[i]],length))
  tt
  print(i)
  print(tt[,2]-tt[,3])
  
}

load("D:/楊松勳/NCDR_AR6_2023/極端降雨指標/ssp126/ssp126_rx1.RData")
load("D:/楊松勳/NCDR_AR6_2023/極端降雨指標/ssp126/ssp126_rx2.RData")
length(rx_tt_by_modelz)
names(rx_tt_by_modelz) #NULL
sapply(rx_tt_by_modelz,dim) #NULL


# sapply(rx_tt_by_modelz[[28]],dim)
t(sapply(rx_tt_by_modelz[[28]],range))
# 

range(rx_tt_by_modelz[[28]]) #1 365

# range(rx_tt_by_modelz[[28]][[5]])
# hist(rx_tt_by_modelz[[28]][[5]])
# 
# range(rx_tt_by_modelz[[28]][[1]])
# hist(rx_tt_by_modelz[[28]][[1]])


# 20231005 由極端指標計算重現期雨量 ---------------------------------------------------

dir_path = "D:/楊松勳/NCDR_AR6_2023/極端降雨指標"

scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾

# 極端指標RData路徑 10情境 5指標
rx_1_to_5_path_nest = lapply(scenery_namez,function(x) list.files(x,full.names = T))
rx_1_to_5_path_nest

length(rx_1_to_5_path_nest) # 10 第一個是全覽
# 
sapply(rx_1_to_5_path_nest[-1],length)

# 取出所有rx1 RData路徑
rx1_sci9_rdata_path = sapply(rx_1_to_5_path_nest[-1],function(x){x[1]})
rx1_sci9_rdata_path
# 讀取 情境1 rx1
load(rx1_sci9_rdata_path[1])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[2])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[3])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

load(rx1_sci9_rdata_path[5])
length(rx_x_by_modelz)
names(rx_x_by_modelz)
# 檢查是否年分不連續：有些不連續
sapply(rx_x_by_modelz,function(x) diff(as.numeric(colnames(x))))
sapply(rx_x_by_modelz,function(x) as.numeric(colnames(x)))
unname(sapply(rx_x_by_modelz,function(x) range(as.numeric(colnames(x)))))

load(rx1_sci9_rdata_path[6])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

dim(rx_x_by_modelz[[28]])
sda = rx_x_by_modelz[[28]]
colnames_yr = as.numeric(colnames(sda))
# colnames_yr
# baseline
intersect(near_term,colnames_yr)
# colnames_yr %in% baseline
pda = sda[,colnames_yr %in% near_term]
dim(pda)


# 重現期雨量head ---------------------------------------------------------------




#函數：前處理infinite 並以apply沿網格 計算mean sd skewness參數
returned_m_sd_sk_x6 = function(sda){
  sda[is.infinite(sda)]=NA
  oc=apply(sda,1,para.est)
  return(oc)
}

ptm = proc.time()
length(rx_x_by_modelz)
#以lapply 執行mean sd skewness參數估計
para_m_sd_sk_by_modelz = lapply(rx_x_by_modelz,function(m) returned_m_sd_sk_x6(m))
#以lapply 執行abe參數估計
para_abe_by_modelz = lapply(para_m_sd_sk_by_modelz,function(m) apply(m,2,function(y) abe.est(y[4:6])))
#以lapply 執行重現期雨量計算
rain_return_period_6_by_modelz = lapply(para_abe_by_modelz,function(para_abe_one_model){
  # 函數:給定參數abe 建立 qgamma 函數 
  rain_cri_given_return_period=function(return_period){
    cri_rain = qgamma((1-1/(return_period)),shape=para_abe_one_model[2,],scale=para_abe_one_model[1,])+para_abe_one_model[3,]
    return(cri_rain)
  }
  # 給定重現期 產生重現期雨量強度
  rain_return_period_6 = sapply(c(5,10,25,50,100,200),function(y) rain_cri_given_return_period(y))
  colnames(rain_return_period_6) = paste0(c(5,10,25,50,100,200),"yr")
  rain_return_period_6
});proc.time()-ptm 
# 10.31 

length(rain_return_period_6_by_modelz)
sapply(rain_return_period_6_by_modelz,dim)
dim(rain_return_period_6_by_modelz[[28]]) # 1741 6

names(rain_return_period_6_by_modelz)
names(rain_return_period_6_by_modelz[28])
rain_return_period_6_by_modelz[28]


# 檢查 base near mid long 資料 ------------------------------------------------

# 檢查ssp126



load(rx1_sci9_rdata_path[6])
length(rx_x_by_modelz)
names(rx_x_by_modelz)

sapply(rx_x_by_modelz,dim)
stage3_yr_checking = sapply(rx_x_by_modelz,function(x){
  colnames_yr = as.numeric(colnames(x))
  oc_1 = sum(colnames_yr %in% baseline)
  oc_2 = sum(colnames_yr %in% near_term)
  oc_3 = sum(colnames_yr %in% mid_term)
  oc_4 = sum(colnames_yr %in% long_term)
  # pda = sda[,colnames_yr %in% near_term]
  # dim(pda)
  oc = c(oc_1,oc_2,oc_3,oc_4)
})
length(stage3_yr_checking) 
112/4

stage3_yr_checking[1:6]
length(rx_x_by_modelz)
length(baseline)
length(near_term)
length(mid_term)
length(long_term)

stage3_yr_checking
length(stage3_yr_checking) #112

names(stage3_yr_checking)
# sapply(stage3_yr_checking,function(x) names(x))
unname(stage3_yr_checking)

# 小結：ssp中 只有 短中長期資料

# 以lapply 檢查 9情境 於基期 短期 中期 長期 年分情況
yr_check_of_sci9 =lapply(rx1_sci9_rdata_path,function(one_rdata_path){
  # load(rx1_sci9_rdata_path[6])
  load(one_rdata_path)
  # length(rx_x_by_modelz)
  # names(rx_x_by_modelz)
  # 
  # sapply(rx_x_by_modelz,dim)
  stage3_yr_checking = sapply(rx_x_by_modelz,function(x){
    colnames_yr = as.numeric(colnames(x))
    oc_1 = sum(colnames_yr %in% baseline)
    oc_2 = sum(colnames_yr %in% near_term)
    oc_3 = sum(colnames_yr %in% mid_term)
    oc_4 = sum(colnames_yr %in% long_term)
    # pda = sda[,colnames_yr %in% near_term]
    # dim(pda)
    oc = c(oc_1,oc_2,oc_3,oc_4)
  })
  # length(stage3_yr_checking) 
  stage3_yr_checking
})

names(yr_check_of_sci9) = sci_mk

length(yr_check_of_sci9)
sapply(yr_check_of_sci9,length)

unname(yr_check_of_sci9[[9]])
unname(yr_check_of_sci9[[8]])

# ssp126
unname(yr_check_of_sci9[[6]])
# historical
unname(yr_check_of_sci9[[5]])
# GWL 4 :數量不定 因為需一全部一起計算出 "未來 重現期雨量"
unname(yr_check_of_sci9[[4]])
unname(yr_check_of_sci9[[3]])
unname(yr_check_of_sci9[[2]])
unname(yr_check_of_sci9[[1]])

names(yr_check_of_sci9)

yr_check_of_sci_1_4 =lapply(rx1_sci9_rdata_path[1:4],function(one_rdata_path){
  # load(rx1_sci9_rdata_path[6])
  load(one_rdata_path)
  lapply(rx_x_by_modelz,function(x) as.numeric(colnames(x)))
})
length(yr_check_of_sci_1_4)
sapply(yr_check_of_sci_1_4,function(x) sapply(x,length))

# 小結: 歷史 算基期 GWL算一組重現期雨量 ssp算短中長期


# 20231006  ---------------------------------------------------------------

# 把重現期雨量寫成函數

# 函數:給定 情境list(情境_模式_年最大值矩陣) 計算所需重現期雨量(*6)
return_rain_calculate = function(one_sci_list){
  print(length(one_sci_list))
  #以lapply 執行mean sd skewness參數估計
  para_m_sd_sk_by_modelz = lapply(one_sci_list,function(m) returned_m_sd_sk_x6(m))
  #以lapply 執行abe參數估計
  para_abe_by_modelz = lapply(para_m_sd_sk_by_modelz,function(m) apply(m,2,function(y) abe.est(y[4:6])))
  #以lapply 執行重現期雨量計算
  rain_return_period_6_by_modelz = lapply(para_abe_by_modelz,function(para_abe_one_model){
    # 函數:給定參數abe 建立 qgamma 函數 
    rain_cri_given_return_period=function(return_period){
      cri_rain = qgamma((1-1/(return_period)),shape=para_abe_one_model[2,],scale=para_abe_one_model[1,])+para_abe_one_model[3,]
      return(cri_rain)
    }
    # 給定重現期 產生重現期雨量強度
    rain_return_period_6 = sapply(c(5,10,25,50,100,200),function(y) rain_cri_given_return_period(y))
    colnames(rain_return_period_6) = paste0(c(5,10,25,50,100,200),"yr")
    rain_return_period_6
  })
  rain_return_period_6_by_modelz
}

# 以lapply 計算 情境1:5 重現期雨量
ptm = proc.time()
return_rain_1_5 = lapply(rx1_sci9_rdata_path[1:5],function(x){
  load(x)
  return_rain_calculate(rx_x_by_modelz)
});proc.time()-ptm;beep(sound = 4)
# 72.49


length(return_rain_1_5)
sapply(return_rain_1_5,length)

# # 檢查NA的生成
# sapply(return_rain_1_5[[1]],function(x) dim(x))
# 
# dim(return_rain_1_5[[5]][[1]])
# ss = return_rain_1_5[[5]][[1]][2,]
# which(is.na(ss))
# length(rx1_sci9_rdata_path[[5]][[1]])
# load(rx1_sci9_rdata_path[[5]][[1]])
# dim(rx_x_by_modelz[[5]]) #1471 55
# 
# which(apply(rx_x_by_modelz[[5]],1,function(x){
#   sum(is.na(x))
# })==55)

unname(sapply(return_rain_1_5[[1]],function(x) sum(x==0,na.rm=T)))

lapply(seq(1,5), function(num){
  unname(sapply(return_rain_1_5[[num]],function(x) sum(x==1,na.rm=T)))
})

dim(return_rain_1_5[[5]][[1]])
return_rain_1_5[[5]][[1]][2,]

unname(sapply(return_rain_1_5[[1]],dim))
unname(sapply(return_rain_1_5[[1]],range,na.rm=T))
unname(sapply(return_rain_1_5[[1]],median,na.rm=T))

rx1_sci9_rdata_path[5];load(rx1_sci9_rdata_path[5]);range(rx_x_by_modelz,na.rm = T)
rx1_sci9_rdata_path[6];load(rx1_sci9_rdata_path[6]);range(rx_x_by_modelz,na.rm = T)
rx1_sci9_rdata_path[7];load(rx1_sci9_rdata_path[7]);range(rx_x_by_modelz,na.rm = T)
# 小結：數值範圍不同 應該ok 科科

return_rain_5_9[[1]][[31]]
return_rain_5_9[[5]][[29]]

# 根據模式數量 判斷情境
 sapply(rx1_sci9_rdata_path,function(x){
  load(x)
  length(rx_x_by_modelz)
})

 term3_yr_list = list(
   near_term = seq(2021,2040),
   mid_term = seq(2041,2060),
   long_term = seq(2081,2100)
 )

 term3_yr_list$near_term
 
 names(term3_yr_list)
 
# ssp情境 短中長期 區分資料
  
  
  pick_by_term3 = function(sci_path_list,term3_seq){
    pda_by_term = lapply(sci_path_list,function(x){
      # pda_by_term = lapply(rx1_sci9_rdata_path[[1]],function(x){
      load(x)
      lapply(rx_x_by_modelz,function(one_model_mat){
        colnames_yr = as.numeric(colnames(one_model_mat))
        # print(colnames_yr)
        # print(term3_yr_list$long_term)
        tel = colnames_yr %in% term3_seq
        # tel = colnames_yr %in% term3_yr_list$long_term
        one_model_mat[,tel]
      })
      
    })
    pda_by_term
  }
  
picked_data_by_term_list = pick_by_term3(rx1_sci9_rdata_path[6:9],term3_yr_list$long_term)
picked_data_by_term_list = pick_by_term3(rx1_sci9_rdata_path[6:9],term3_yr_list[[1]])
picked_data_by_term_list = pick_by_term3(rx1_sci9_rdata_path[6:9],term3_yr_list[[2]])

length(picked_data_by_term_list)
sapply(picked_data_by_term_list,length)
unname(sapply(picked_data_by_term_list[[1]],dim))

term3_yr_list[1]
term3_yr_list[2]
term3_yr_list[3]

picked_data_by_term_list

# 以lapply 計算 情境6:9 重現期雨量
ptm = proc.time()
return_rain_6_9 = lapply(picked_data_by_term_list,function(x){
  return_rain_calculate(x)
});proc.time()-ptm;beep(sound = 4)
# 21.34


length(return_rain_6_9)
sapply(return_rain_6_9,length)
dim(return_rain_6_9[[4]][[29]])
sapply(return_rain_6_9,range,na.rm=T)

# 擴展：依據短中長期 區分資料
picked_data_by_term3_list = lapply(seq(1,3),function(termi){
  pick_by_term3(rx1_sci9_rdata_path[6:9],term3_yr_list[[termi]])
})
length(picked_data_by_term3_list)
sapply(picked_data_by_term3_list,length)
# unname(sapply(picked_data_by_term3_list[[3]][[4]],dim))

# 以lapply 計算 情境6:9 重現期雨量 
# 外層是短中長期
ptm = proc.time()
return_rain_6_9_term3 = lapply(seq(1,3),function(termi){
  return_rain_6_9 = lapply(picked_data_by_term3_list[[termi]],function(x){
    return_rain_calculate(x)
  })
  return_rain_6_9
});proc.time()-ptm;beep(sound = 4)
# 51.47

