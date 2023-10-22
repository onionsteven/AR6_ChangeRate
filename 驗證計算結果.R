
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

#函數：前處理inf並計算mean sd skewness參數
returned_m_sd_sk_x6 = function(sda){
  sda[is.infinite(sda)]=NA
  oc=apply(sda,1,para.est)
  return(oc)
}


# 函數:給定 情境list(情境_模式_年最大值矩陣) 計算所需重現期雨量(*6)
return_rain_calculate = function(one_sci_list){
  print(length(one_sci_list))
  #以lapply 執行mean sd skewness參數估計
  para_m_sd_sk_by_modelz = lapply(one_sci_list,function(m) returned_m_sd_sk_x6(m[,36:55]))
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

given_path = "D:/楊松勳/NCDR_AR6_2023/極端指標_修改/historical/historical_rx1.RData"
load(given_path)
names(rx_x_by_modelz)
head(names(rx_x_by_modelz))


para_m_sd_sk_by_modelz = lapply(rx_x_by_modelz,function(m) returned_m_sd_sk_x6(m))
length(para_m_sd_sk_by_modelz) #31
dim(para_m_sd_sk_by_modelz[[1]]) # 6 1471
t(para_m_sd_sk_by_modelz[[1]])[1:20,] # 6 1471

#以lapply 執行abe參數估計
para_abe_by_modelz = lapply(para_m_sd_sk_by_modelz,function(m) apply(m,2,function(y) abe.est(y[4:6])))
dim(para_abe_by_modelz[[1]]) # 1471

t(para_abe_by_modelz[[1]])

t(sapply(rx_x_by_modelz,function(x){
  range(as.numeric(colnames(x)))
}))



# 20231019 重新計算historical重現期雨量 --------------------------------------------

# 發現之前的重historical在計算重現期˙雨量時 沒有根據年份做挑選 應該要選擇36:55 columns
length(seq(36,55))

set_path = "D:/楊松勳/NCDR_AR6_2023/極端指標_修改/historical"
rx_historical_path = list.files(set_path,full.names = T)
rx_historical_path

# 以lapply 計算 情境1:5 重現期雨量
ptm = proc.time()
return_rain_1_5 = lapply(rx_historical_path,function(x){
  load(x)
  return_rain_calculate(rx_x_by_modelz)
});proc.time()-ptm;beep(sound = 4) # 24.8
names(return_rain_1_5) = paste0('rx',seq(1,5))
names(return_rain_1_5)

names(return_rain_1_5[[1]])

head(return_rain_1_5[[1]][[1]],25)

