library(beepr)
given_path = "D:/楊松勳/NCDR_AR6_2023/重現期雨量/return_rain_rx1_to_rx5_sci5_sci4x3_list.RData"
load(given_path)


# 20231012 嘗試計算改變率 --------------------------------------------------------


# 函數:分解csv檔名 取出 模式名
take_model_name_of_one_sci = function(x){
  sapply(strsplit(x,"_"),tail,1)
}

# 20231014 以lapply 執行rx1~rx5的計算 -----------------------------------------


# 物件命名 改變率 指標rx1~rx5 情境4 4*3
ptm = proc.time()
change_rate_rx1_to_rx5_sci4_sci4x3_list = lapply(seq(1,5),function(rxi){
  

# 基期資料
da_baseline = return_rain_rx1_to_rx5_sci5_sci4x3_list[[rxi]]$return_rain_1_5[[5]]
# 基期模式名
namez_baseline = take_model_name_of_one_sci(names(da_baseline))

# 給定：待比較重現期雨量 情境1~4
da_compared_list = return_rain_rx1_to_rx5_sci5_sci4x3_list[[rxi]]$return_rain_1_5[1:4]
names(da_compared_list)
sci4_namez_list = sapply(da_compared_list,names)
sci4_namez_list

# 以lapply處理 情境1~4 改變率計算
change_rate_sci_1_4_list = lapply(seq(1,length(da_compared_list)),function(sci){
  # sci = 4
  # moi = 20
  mda = da_compared_list[[sci]]
  mda_namez = sci4_namez_list[[sci]]
  namez_for_tel = sapply(mda_namez,take_model_name_of_one_sci)
  oc_model_ist = lapply(seq(1,length(mda)),function(moi){
    # mda[[moi]]
    # print(namez_for_tel[moi])
    mda[[moi]]/da_baseline[[which(namez_baseline==namez_for_tel[moi])]]*100
  })
  names(oc_model_ist) = names(mda)
  oc_model_ist
})
names(change_rate_sci_1_4_list) = names(da_compared_list)

# 20231013 計算三時期重現期雨量改變率 --------------------------------------------------

# 給定：待比較 時期1~3 情境1~4 改變率計算
da_compared_term3_list = return_rain_rx1_to_rx5_sci5_sci4x3_list[[rxi]]$return_rain_6_9_term3

ter3_namez_list = sapply(da_compared_term3_list,names)
ter3_namez_list

change_rate_term3_sci6_9_list =lapply(seq(1,length(da_compared_term3_list)),function(termi){
  # termi = 3
  tda = da_compared_term3_list[[termi]]
  oc_term3_list = lapply(seq(1,length(tda)),function(sci){
    # sci = 4
    # moi = 20
    mda = tda[[sci]]
    mda_namez = names(mda)
    namez_for_tel = sapply(mda_namez,take_model_name_of_one_sci)
    oc_model_ist = lapply(seq(1,length(mda)),function(moi){
      # mda[[moi]]
      # print(namez_for_tel[moi])
      mda[[moi]]/da_baseline[[which(namez_baseline==namez_for_tel[moi])]]*100
    })
    names(oc_model_ist) = names(mda)
    oc_model_ist
  })
  names(oc_term3_list) = names(tda)
  oc_term3_list
})
names(change_rate_term3_sci6_9_list) = names(da_compared_term3_list)


# 20231014 合併兩種 改變率list(1~4 6~9) 並存為RData
change_rate_list =list(change_rate_sci_1_4_list=change_rate_sci_1_4_list,
                       change_rate_term3_sci6_9_list=change_rate_term3_sci6_9_list)
change_rate_list
})
# 標記上rx1~rx5
names(change_rate_rx1_to_rx5_sci4_sci4x3_list) = names(return_rain_rx1_to_rx5_sci5_sci4x3_list)
save(change_rate_rx1_to_rx5_sci4_sci4x3_list,file = "change_rate_rx1_to_rx5_sci4_sci4x3_list.RData")
proc.time()-ptm # 6.87


# 輸出為csv檔 ---------------------------------------------------------------


# 取一個原始資料的座標 增加至 產出矩陣前一二column
raw_csv_path = "D:/楊松勳/AR6_統計降尺度_日資料_臺灣_降雨量/GWL1.5/AR6_統計降尺度_日資料_臺灣_降雨量_ssp126_ACCESS-CM2_2018.csv"
raw_csv_data = read.csv(raw_csv_path)
coord = raw_csv_data[,1:2]
# 函數：取一個原始資料的座標 增加至 產出矩陣前一二column
add_coord = function(one_data_mat){
  cbind(coord,one_data_mat)
}

# 給定路徑 讀取改變率
# given_path = "D:/楊松勳/NCDR_AR6_2023/改變率/change_rate_rx1_to_rx5_sci4_sci4x3_list.RData"
# load(given_path)

length(change_rate_rx1_to_rx5_sci4_sci4x3_list)
names(change_rate_rx1_to_rx5_sci4_sci4x3_list)

change_rate_rx1_to_rx5_sci4_sci4x3_list$rx1$change_rate_sci_1_4_list$GWL4.0
length(change_rate_rx1_to_rx5_sci4_sci4x3_list$rx1$change_rate_sci_1_4_list[[4]])
sapply(change_rate_rx1_to_rx5_sci4_sci4x3_list$rx1$change_rate_sci_1_4_list,length)
names(change_rate_rx1_to_rx5_sci4_sci4x3_list$rx1$change_rate_sci_1_4_list$GWL4.0)


unname(sapply(change_rate_rx1_to_rx5_sci4_sci4x3_list$rx1$change_rate_term3_sci6_9_list$long_term$ssp126,dim))


# -------------------------------------------------------------------------

setwd("D:/楊松勳/ChangeRate_CSV_tank")
# 嘗試以lapply儲存為csv檔
name_rx = names(change_rate_rx1_to_rx5_sci4_sci4x3_list)
change_rate_nested_list = change_rate_rx1_to_rx5_sci4_sci4x3_list

ptm = proc.time()
lapply_write_csv = lapply(seq(1,length(change_rate_nested_list)),function(rxi){ 
  
  # 第一部分 head--------------------------------------------------------------------
  
  # 第一層 極端指標rx
  one_rx = change_rate_nested_list[[rxi]][[1]]
  one_rx_namez = names(one_rx)
  #
  # 輸出重現期雨量為csv檔
  moc_1_5 = lapply(seq(1,length(one_rx)),function(sci){
    one_sci = one_rx[[sci]]
    one_sci_namez =  names(one_sci)
    soc = lapply(seq(1,length(one_sci)),function(moi){
      one_model = one_sci[[moi]]
      one_model_namez = names(one_model)
      fn = paste0("改變率","_",name_rx[rxi],"_",one_rx_namez[sci],"_",one_sci_namez[moi],".csv")
      # print(fn)
      # 標上座標
      one_model_coord = add_coord(one_model)
      write.csv(one_model_coord,fn,row.names = F)
      
    })
  })
  
  # 第一部分 tail--------------------------------------------------------------------  
  
  # 第二部分 head--------------------------------------------------------------------
  
  # 第一層 極端指標rx
  one_rx = change_rate_nested_list[[rxi]][[2]]
  one_rx_namez = names(one_rx)
  
  # 輸出重現期雨量為csv檔
  moc_6_9 = lapply(seq(1,length(one_rx)),function(termi){
    one_term = one_rx[[termi]]
    one_term_namez =  names(one_term)
    soc = lapply(seq(1,length(one_term)),function(sci){
      one_sci = one_term[[sci]]
      one_sci_namez = names(one_sci)
      lapply(seq(1,length(one_sci)),function(moi){
        one_model = one_sci[[moi]]
        # one_model_namez = names(one_model)
        fn = paste0("改變率","_",name_rx[rxi],"_",one_rx_namez[termi],"_",one_sci_namez[moi],".csv")
        # print(fn)
        # 標上座標
        one_model_coord = add_coord(one_model)
        write.csv(one_model_coord,fn,row.names = F)
      })
    })
  })
  # 第二部分 tail--------------------------------------------------------------------
});proc.time()-ptm;beep(sound = 4) # 66.57
