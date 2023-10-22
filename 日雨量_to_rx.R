library(lmomco) #計算L-monment

library(lattice) #調用levelplot
library(beepr)
library(magrittr)
library(zoo)

# 檢視資料夾 csv檔名 -------------------------------------------------------------


# 指定資料夾 檢視各情境
dir_path = "D:/楊松勳/AR6_統計降尺度_日資料_臺灣_降雨量"
scenery_namez = list.dirs(dir_path)
scenery_namez
length(scenery_namez) # 10 第一個是自身資料夾


# 根據給定數字 處理情境資料夾 head----------------------------------------------------------


# 給定2~10 檢視csv檔名
csv_namez = list.files(scenery_namez[7])
length(csv_namez)
head(csv_namez)

# 檔名處理
csv_namez_no_csv = gsub('.csv',"",csv_namez)
# csv_namez_no_csv
csv_namez_no_yr = lapply(csv_namez_no_csv,function(x){
  substr(x,1,(nchar(x)-5))
})

# 模式名稱
csv_namez_sep_by_model = unique(csv_namez_no_yr)
# 模式數量 # 110
length(csv_namez_sep_by_model)

# 函數:取情境資料夾中 模式名稱
find_unique_model_namez_of_scenery = function(dir_num){
  # 給定2~10 檢視csv檔名
  csv_namez = list.files(scenery_namez[dir_num])
  csv_namez
  
  # 檔名處理
  csv_namez_no_csv = gsub('.csv',"",csv_namez)
  # csv_namez_no_csv
  csv_namez_no_yr = lapply(csv_namez_no_csv,function(x){
    substr(x,1,(nchar(x)-5))
  })
  
  # 模式名稱
  csv_namez_sep_by_model = unique(csv_namez_no_yr)
  csv_namez_sep_by_model
}

# 根據給定數字 處理情境資料夾 tail----------------------------------------------------------

model_names_of_sci9 = sapply(seq(2,10),find_unique_model_namez_of_scenery)
length(model_names_of_sci9)
sapply(model_names_of_sci9,length) # 110  99  66  31  31  28  29  27  29
# 檢視模式名稱
model_names_of_sci9[[9]]

# 檢視9情境中 第一個模式
sapply(seq(1,9),function(x) model_names_of_sci9[[x]][[1]])

sapply(seq(1,9),function(x) model_names_of_sci9[[x]][[27]])

sapply(seq(1,9),function(x) model_names_of_sci9[[x]][[28]])


# 檢視情境 模式 之 年分 head------------------------------------------------------------

# 情境之編號 2~10

csv_yrz_sci_by_modelz = lapply(seq(2,10),function(sci_num){
  csv_namez = list.files(scenery_namez[sci_num])
  length(csv_namez)
  head(csv_namez)
  csv_pathz = list.files(scenery_namez[sci_num],full.names = T)
  # csv_pathz
  
  
  csv_namez_sep_by_model = model_names_of_sci9[[sci_num-1]]
  head(csv_namez_sep_by_model)
  
  # 將資料夾中csv檔依據 模式 區分 整理 行稱3D array
  
  # 模式數量
  length(csv_namez_sep_by_model)
  csv_yrz_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){
    take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
    sda = csv_pathz[take_tel]  
    oc = substr(sda,nchar(sda)-7,nchar(sda)-3)
    as.numeric(oc)
  })
})
length(csv_yrz_sci_by_modelz)
sapply(csv_yrz_sci_by_modelz,length)
sapply(csv_yrz_sci_by_modelz[[9]],length)

csv_yrz_sci_by_modelz[[4]][[29]]
model_names_of_sci9[[4]][[29]]
scenery_namez[[5]]

csv_yrz_sci_by_modelz[[1]][[29]]
model_names_of_sci9[[1]][[29]]
scenery_namez[[2]]

csv_yrz_sci_by_modelz[[2]][[29]]
model_names_of_sci9[[2]][[29]]
scenery_namez[[3]]

# 檢視資料csv檔數量(有幾年) GWL 1.5 2.0 3.0 4.0  設定:scenery_namez[2:5] seq(1,4)
# 檢視資料csv檔數量(有幾年) history ssp126 ...    設定:scenery_namez[6:10] 
scenery_namez[6:10]
# 資料長度都是20年 
lapply(seq(5,9),function(x){
  sapply(csv_yrz_sci_by_modelz[[x]],length)
})



# 情境之編號 2~10
sci_num = 4
csv_namez = list.files(scenery_namez[sci_num])
length(csv_namez)
head(csv_namez)
csv_pathz = list.files(scenery_namez[sci_num],full.names = T)
# csv_pathz


csv_namez_sep_by_model = model_names_of_sci9[[sci_num-1]]
head(csv_namez_sep_by_model)

# 將資料夾中csv檔依據 模式 區分 整理 行稱3D array

# 模式數量
length(csv_namez_sep_by_model)
csv_yrz_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){
  take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
  sda = csv_pathz[take_tel]  
  oc = substr(sda,nchar(sda)-7,nchar(sda)-3)
  as.numeric(oc)
})

length(csv_yrz_by_modelz)
sapply(csv_yrz_by_modelz,length)
sapply(csv_yrz_by_modelz,range)

# csv_yrz_by_modelz

# 年分布連續檢查 不連續出現在 11 12 模式
yr_diff = lapply(csv_yrz_by_modelz,function(x){
  diff(x)
})
# 年分不連續發生次數
sapply(yr_diff,function(x) sum(x!=1))
# 年份不連續發生之模式
which(sapply(yr_diff,function(x) sum(x!=1))!=0)



# 檢視情境 模式 之 年分 tail------------------------------------------------------------

# 根據情境_模式_取CSV檔
for (ni in c(seq(9,10),seq(3,5),2)){
  
# }
sci_num = 7
# sci_num = ni

length(scenery_namez) #10
# 給定2~10 檢視csv檔名
csv_namez = list.files(scenery_namez[sci_num])
length(csv_namez)
head(csv_namez)

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
# 檢視模式名稱
head(csv_namez_sep_by_model)

# 檢視情境
scenery_namez[sci_num]
# 待處理csv檔路徑
csv_pathz = list.files(scenery_namez[sci_num],full.names = T)
# 檢視待處理csv檔路徑
head(csv_pathz )
# 將資料夾中csv檔依據 模式 區分 並取出年最大值
ptm = proc.time()
# 模式數量
length(csv_namez_sep_by_model)
rx1_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){

  take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
  
  take_yr = gsub(".csv","",csv_pathz[take_tel])
  take_yr = substr(take_yr,nchar(take_yr)-3,nchar(take_yr))
  
  picked_data = sapply(csv_pathz[take_tel],function(x){
    sda = read.csv(x)
    sda = sda[,3:(ncol(sda)-1)]
    sda[sda<0] = NA  
    # oc = apply(sda,1,max,na.rm=T)
    oc = max.col(sda)
    oc
  })
  colnames(picked_data) = take_yr
  oc = picked_data
  return(oc)
})
print(proc.time()-ptm)
names(rx1_by_modelz) = unlist(csv_namez_sep_by_model)
save(rx1_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx1.RData"))

library(zoo)
# 函數:rx 年最大值
rollsum_to_rx = function(sda,width){
  mda = apply(sda,1,rollsum,width,na.rm=T)
  # mda = apply(mda,2,max)
  mda = max.col(mda)
  mda[mda==0] = NA
  return(mda)
}

# rx_oc = rollsum_to_rx(sda,3)

# 將資料夾中csv檔依據 模式 區分 並取出年最大值(rx2~5)

for (rxi in seq(2,5)){
  ptm = proc.time()
  # 模式數量
  length(csv_namez_sep_by_model)
  rx1_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){
    
    take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
    take_yr = gsub(".csv","",csv_pathz[take_tel])
    # print(take_yr)
    take_yr = substr(take_yr,nchar(take_yr)-3,nchar(take_yr))
    # print(take_yr)
    picked_data = sapply(csv_pathz[take_tel],function(x){
      sda = read.csv(x)
      sda = sda[,3:(ncol(sda)-1)]
      sda[sda<0] = NA  
      # 取出年最大值
      # oc = apply(sda,1,max,na.rm=T)
      # oc
      rx_oc = rollsum_to_rx(sda,rxi)
      rx_oc
    })
    colnames(picked_data) = take_yr
    oc = picked_data
    return(oc)
  })
  names(rx1_by_modelz) = unlist(csv_namez_sep_by_model)
  save(rx1_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx",rxi,".RData"))
  print(proc.time()-ptm)
}
1400/60 # 23.3 min
2431.22 /60
2390.55/60
1844.12/60
}

 869.24 /60
 1828.02 /60 
 1829.01 /60
 1829.05 /60
 1053.22 
 2314.20 
 2258.66 
 2175.48 
 2297.28 
  1184.44 
  2524.06 
  2011.28 
  1937.25 
  1920.36 
  717.94 
  1514.43 
  1516.77 
  1522.39 
  1508.80 
  489.13 
  1051.16 
  1050.55 
  1054.86 
  1045.48 
  244.79 
  523.47 
  529.16 
  531.06 
  522.92 
  792.58 
  1693.83
  1724.94 
  1712.61 
  1713.03 
  

# 20231001 減少讀取csv檔次數 -----------------------------------------------------

# 讀取一個csv檔之後 就計算rx1~rx5
  
  # 根據情境_模式_取CSV檔
  for (ni in c(seq(9,10),seq(3,5),2)){
    
    # }
    # sci_num = 7
    # sci_num = ni
    
    length(scenery_namez) #10
    # 給定2~10 檢視csv檔名
    csv_namez = list.files(scenery_namez[sci_num])
    length(csv_namez)
    head(csv_namez)
    
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
    # 檢視模式名稱
    head(csv_namez_sep_by_model)
    
    # 檢視情境
    scenery_namez[sci_num]
    # 待處理csv檔路徑
    csv_pathz = list.files(scenery_namez[sci_num],full.names = T)
    # 檢視待處理csv檔路徑
    head(csv_pathz )
    # 將資料夾中csv檔依據 模式 區分 並取出年最大值
    ptm = proc.time()
    # 模式數量
    length(csv_namez_sep_by_model)
    # rx1_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){
    #   
    #   take_tel = grep(csv_namez_sep_by_model[moi],csv_namez)
    #   
    #   take_yr = gsub(".csv","",csv_pathz[take_tel])
    #   take_yr = substr(take_yr,nchar(take_yr)-3,nchar(take_yr))
    #   
    #   picked_data = sapply(csv_pathz[take_tel],function(x){
    #     sda = read.csv(x)
    #     sda = sda[,3:(ncol(sda)-1)]
    #     sda[sda<0] = NA  
    #     # oc = apply(sda,1,max,na.rm=T)
    #     oc = max.col(sda)
    #     oc
    #   })
    #   colnames(picked_data) = take_yr
    #   oc = picked_data
    #   return(oc)
    # })
    # print(proc.time()-ptm)
    # names(rx1_by_modelz) = unlist(csv_namez_sep_by_model)
    # save(rx1_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx1.RData"))
    
    
    # 函數:rx 年最大值
    rollsum_to_rx = function(sda,width){
      if(width == 1) mda = sda
      else mda = apply(sda,1,rollsum,width,na.rm=T)
      # mda = max.col(t(mda))
      mda = apply(mda,1,max)
      mda[mda==0] = NA
      return(mda)
    }
    
    # rx_oc = rollsum_to_rx(sda,3)
    
    # 將資料夾中csv檔依據 模式 區分 並取出年最大值(rx2~5)
    
    # for (rxi in seq(2,5)){
      ptm = proc.time()
      # 模式數量
      length(csv_namez_sep_by_model)
      ptm = proc.time()
      rx_x5_by_modelz = lapply(seq(1,length(csv_namez_sep_by_model)),function(moi){
        
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
      7194 /3600 #1.998
      6467/3600 #1.796
      # names(rx1_by_modelz) = unlist(csv_namez_sep_by_model)
      # save(rx1_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx",rxi,".RData"))
      
      names(rx_x5_by_modelz) = unlist(csv_namez_sep_by_model)
      for (rxi in seq(1,5)){
        rx_tt_by_modelz = lapply(seq(1,length(dada)),function(mi) dada[[mi]][[rxi]])
        # save(rx_tt_by_modelz,file = paste0("tt_ssp",rxi,".RData"))
        save(rx_tt_by_modelz,file = paste0(tail(strsplit(scenery_namez[sci_num],'/')[[1]],1),"_rx",rxi,".RData"))
      }
      
      print(proc.time()-ptm)
    # }
    # 1400/60 # 23.3 min
    # 2431.22 /60
    # 2390.55/60
    # 1844.12/60
  }
    
  
length(rx_x5_by_modelz)
sapply(rx_x5_by_modelz,dim)

dim(rx_x5_by_modelz[[28]]) # 5 86

rx_x5_by_modelz[[28]][5,86]
length(unlist(rx_x5_by_modelz[[28]][5,86])) #361
length(unlist(rx_x5_by_modelz[[28]][4,86])) #362
length(unlist(rx_x5_by_modelz[[28]][3,86])) #363
length(unlist(rx_x5_by_modelz[[28]][2,86])) #364
length(unlist(rx_x5_by_modelz[[28]][1,86])) #365

length(rx_x5_by_modelz[[28]][1,])
tt = t(sapply(rx_x5_by_modelz[[28]][1,],function(x) x))
dim(unname(tt))

dada = rx_x5_by_modelz
for (rxi in seq(1,5)){
  rx_tt_by_modelz = lapply(seq(1,length(dada)),function(mi) dada[[mi]][[rxi]])
  save(rx_tt_by_modelz,file = paste0("tt_ssp",rxi,".RData"))
}
