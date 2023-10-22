# library(snow)
# library(Rmpi)
# cl <- makeCluster(4,type="MPI")

library(parallel)
library(MASS)
library(foreach)

library(zoo)

# 指定資料夾 檢視各情境
dir_path = "D:/楊松勳/AR6_統計降尺度_日資料_臺灣_降雨量"

scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾



# 取得 CPU 核心數
cpu.cores <- detectCores()
cpu.cores # 20


for (ni in seq(10,2)){
  
  
  sci_num = ni
  # sci_num = 10
  # moi = 1
  
  csv_namez = list.files(scenery_namez[sci_num])
  csv_namez
  # 檔名處理
  # 移除 .csv
  csv_namez_no_csv = gsub('.csv',"",csv_namez)
  # csv_namez_no_csv
  # 移除年分
  csv_namez_no_yr = lapply(csv_namez_no_csv,function(x){
    substr(x,1,(nchar(x)-5))
  })
  
  # 模式名稱取unique
  csv_namez_sep_by_model = unique(csv_namez_no_yr)
  
  # 待處理csv檔路徑
  csv_pathz = list.files(scenery_namez[sci_num],full.names = T)
  # 檢視待處理csv檔路徑
  length(csv_pathz )
  head(csv_pathz )
  
  # 模式數量
  print(length(csv_namez_sep_by_model))

# 函數:取rx年最大值
rollsum_to_rx = function(sda,width){
  if(width == 1) mda = sda
  else mda = t(apply(sda,1,rollsum,width,na.rm=T))
  mda = apply(mda,1,max)
  mda[mda==0] = NA
  return(mda)
}


ptm = proc.time() # 計時開始

# 建立 Cluster
cl <- makeCluster((cpu.cores-1))
clusterExport(cl, list("csv_namez_sep_by_model","csv_namez","csv_pathz","rollsum_to_rx","rollsum"))
rx_x5_by_modelz = parLapply(cl, seq(1,length(csv_namez_sep_by_model)),function(moi){
  
  # moi = 11
  # take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
  take_tel = intersect(grep(csv_namez_sep_by_model[moi],csv_namez) , which(nchar(csv_namez) == nchar(csv_namez_sep_by_model[moi])+9) )

  take_yr = gsub(".csv","",csv_pathz[take_tel])
  take_yr = substr(take_yr,nchar(take_yr)-3,nchar(take_yr))
  
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
  });proc.time()-ptm # 
  print(sapply(tt_picked_data,dim))
  # colnames(picked_data) = take_yr
  
  # length(tt_picked_data) # 20
  # length(tt_picked_data[[20]]) # 5
  # sapply(tt_picked_data[[20]],length) # 1471*5
  # tt_picked_data[[20]][[5]]
  
  rx_mat_list = lapply(seq(1,5),function(rxi){
    rx_mat = sapply(seq(1,length(tt_picked_data)),function(pi) tt_picked_data[[pi]][[rxi]])
    colnames(rx_mat) = take_yr
    rx_mat
  })
  names(rx_mat_list) = paste0('rx',seq(1,5))
  oc = rx_mat_list
  return(oc)
});print(proc.time()-ptm)#;beep(sound = 4)
1364/60 #22.7
# 關閉 Cluster
stopCluster(cl)


  dada = rx_x5_by_modelz
  for (rxi in seq(1,5)){
    rx_x_by_modelz = lapply(seq(1,length(dada)),function(mi) dada[[mi]][[rxi]])
    names(rx_x_by_modelz) = unlist(csv_namez_sep_by_model)
    save(rx_x_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx",rxi,".RData"))
  }

}



sci_num = ni

# print(length(scenery_namez)) #10
# 給定2~10 檢視csv檔名
csv_namez = list.files(scenery_namez[sci_num])
# length(csv_namez)
# head(csv_namez)

# 檔名處理
# 移除 .csv
csv_namez_no_csv = gsub('.csv',"",csv_namez)
# csv_namez_no_csv
# 移除年分
csv_namez_no_yr = lapply(csv_namez_no_csv,function(x){
  substr(x,1,(nchar(x)-5))
})

# 模式名稱取unique
csv_namez_sep_by_model = unique(csv_namez_no_yr)


# 檢視 極端指標 各模式 維度
unname(sapply(rx_x_by_modelz,dim))
colnames(rx_x_by_modelz[[28]])
colnames(rx_x_by_modelz[[10]])
colnames(rx_x_by_modelz[[11]])

# rx_x_by_modelz <- 變數名稱

#　檢視數值範圍
range(rx_x_by_modelz[[28]]) # 0.0096 3074.4122
sapply(rx_x_by_modelz,range,na.rm=T)
