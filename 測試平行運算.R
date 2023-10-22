# library(snow)
# library(Rmpi)
# cl <- makeCluster(4,type="MPI")



library(parallel)
library(MASS)
library(foreach)



cl <- makeCluster((numCores-1)) # Number of cores
system.time(
result <- parLapply(cl,fx,starts)
)

# 取得 CPU 核心數
cpu.cores <- detectCores()
cpu.cores # 20

# 建立 Cluster
cl <- makeCluster((cpu.cores-2))

result <- parLapply(cl, starts, function(x) c(mean(x), sd(x)))

# 關閉 Cluster
stopCluster(cl)

# 函數:rx 年最大值
rollsum_to_rx = function(sda,width){
  if(width == 1) mda = sda
  else mda = apply(sda,1,rollsum,width,na.rm=T)
  # mda = max.col(t(mda))
  mda = apply(mda,1,max)
  mda[mda==0] = NA
  return(mda)
}


ptm = proc.time()

# 建立 Cluster
cl <- makeCluster((cpu.cores-2))
clusterExport(cl, list("csv_namez_sep_by_model","csv_namez","csv_pathz","rollsum_to_rx","rollsum"))
rx_x5_by_modelz = parLapply(cl, seq(1,length(csv_namez_sep_by_model)),function(moi){
  
  take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
  take_yr = gsub(".csv","",csv_pathz[take_tel])
  # print(take_yr)
  take_yr = substr(take_yr,nchar(take_yr)-3,nchar(take_yr))
  # print(take_yr)
  ptm = proc.time()
  tt_picked_data = lapply(csv_pathz[take_tel],function(x){
    # x = csv_pathz[take_tel][1]
    sda = read.csv(x)
    sda = sda[,3:(ncol(sda)-1)]
    sda[sda<0] = NA  
    rx_1_to_5_list = lapply(seq(1,5),function(rxi){
      soc = rollsum_to_rx(sda,rxi)
      soc
    })
    rx_1_to_5_list
  });proc.time()-ptm
  # colnames(picked_data) = take_yr
  
  length(tt_picked_data) # 20
  length(tt_picked_data[[20]]) # 5
  sapply(tt_picked_data[[20]],length) # 1471*5
  tt_picked_data[[20]][[5]]
  
  rx_mat_list = lapply(seq(1,5),function(rxi){
    rx_mat = sapply(seq(1,length(tt_picked_data)),function(pi) tt_picked_data[[pi]][[rxi]])
    colnames(rx_mat) = take_yr
    rx_mat
  })
  names(rx_mat_list) = paste0('rx',seq(1,5))
  # oc = picked_data
  oc = rx_mat_list
  return(oc)
});print(proc.time()-ptm)#;beep(sound = 4)
1364/60 #22.7
# 關閉 Cluster
stopCluster(cl)

# length(rx_x5_by_modelz)
# sapply(rx_x5_by_modelz,length)
# sapply(rx_x5_by_modelz[[28]],dim)

dada = rx_x5_by_modelz
for (rxi in seq(1,5)){
  rx_x_by_modelz = lapply(seq(1,length(dada)),function(mi) dada[[mi]][[rxi]])
  save(rx_x_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx",rxi,".RData"))
}

# rx_x_by_modelz <- 變數名稱

#　檢視數值範圍
range(rx_x_by_modelz[[28]]) # 0.0096 3074.4122
sapply(rx_x_by_modelz,range,na.rm=T)
