# 20230928 計算重現期雨量

library(lmomco) #計算L-monment

library(lattice) #調用levelplot
library(beepr)
# library(magrittr)


# 計算用函數 -------------------------------------------------------------------

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

#函數：前處理inf並計算mean sd skewness參數
returned_m_sd_sk_x6 = function(sda){
  sda[is.infinite(sda)]=NA
  oc=apply(sda,1,para.est)
  return(oc)
}


# 計算功能 --------------------------------------------------------------------

# 指定資料夾 檢視各情境
# dir_path = "D:/楊松勳/NCDR_AR6_2023/極端降雨指標"
dir_path = "C:/Users/user/Documents/極端指標_修改"


scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾

# 情境 名稱
sci_mk = sapply(scenery_namez[-1], function(x){
  sda = strsplit(x,"/")
  tail(sda[[1]],1)
});unname(sci_mk)


# 極端指標RData路徑 10情境 5指標
rx_1_to_5_path_nest = lapply(scenery_namez,function(x) list.files(x,full.names = T))
rx_1_to_5_path_nest

# 各時期年分
baseline = seq(1995,2014)
near_term = seq(2021,2040)
mid_term = seq(2041,2060)
long_term = seq(2081,2100)

# 短中長情境 年分 設為list
term3_yr_list = list(
  near_term = seq(2021,2040),
  mid_term = seq(2041,2060),
  long_term = seq(2081,2100)
)

names(term3_yr_list)


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


# 給定數字 選取極端指標rx1~rx5 ------------------------------------------------------

# rx RData檔 路徑 (list巢狀結構) 
rx_1_to_5_path_nest


ptm_rx5 = proc.time()
return_rain_rx1_to_rx5_sci5_sci4x3_list = lapply(seq(1,5),function(rxi){

# 給定數字1~5 取出rx1~rx5
# 取出所有rx1 RData路徑
rx1_sci9_rdata_path = sapply(rx_1_to_5_path_nest[-1],function(x){x[rxi]})
rx1_sci9_rdata_path

# 以lapply 計算 情境1:5 重現期雨量
ptm = proc.time()
return_rain_1_5 = lapply(rx1_sci9_rdata_path[1:5],function(x){
  load(x)
  return_rain_calculate(rx_x_by_modelz)
});proc.time()-ptm;beep(sound = 4)
names(return_rain_1_5) = sci_mk[1:5]
# 72.49

# spp情境需區分 短中長 三種年份 -------------------------------------------------------

# ssp情境 短中長期 區分資料
# 函數：給定 情境list 及 時期年分(短中長) 取出所需年最大值
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
  names(return_rain_6_9) = sci_mk[6:9]
  return_rain_6_9
});proc.time()-ptm;beep(sound = 4)
# 51.47
names(return_rain_6_9_term3) = names(term3_yr_list)

# 結合 1~5 6~9*3情境
return_rain_sci5_sci4x3_list = list(
  return_rain_1_5 = return_rain_1_5, 
  return_rain_6_9_term3 = return_rain_6_9_term3
)
print(paste0("rx_of_",rxi))
return_rain_sci5_sci4x3_list

});proc.time()-ptm_rx5;beep(sound = 4)
521/60 # 8.68
# save(return_rain_sci5_sci4x3_list,file = "return_rain_sci5_sci4x3_list.RData")
names(return_rain_rx1_to_rx5_sci5_sci4x3_list) = paste0("rx",seq(1,5))
save(return_rain_rx1_to_rx5_sci5_sci4x3_list,file = "return_rain_rx1_to_rx5_sci5_sci4x3_list.RData")

# 若要區分rx存放資料 檔名可以設為:
paste0("return_rain_sci5_sci5x3_list_rx",rxi,".RData")


length(return_rain_rx1_to_rx5_sci5_sci4x3_list) # 5 rx
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list,length) # ssp組 GWL組
names(return_rain_rx1_to_rx5_sci5_sci4x3_list[[5]])

names(return_rain_rx1_to_rx5_sci5_sci4x3_list[[5]]$return_rain_1_5$historical)

names(return_rain_rx1_to_rx5_sci5_sci4x3_list[[5]]$return_rain_6_9_term3$long_term$ssp126)
