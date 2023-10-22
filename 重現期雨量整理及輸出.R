# 20231010 由重現期雨量產生後續產品
# 1.重現期雨量：整理(加上座標及colnames)輸出為csv檔
# 2.計算改變率：整理及輸出為csv檔
# 3.計算系集分位數：整理及輸出為csv檔

given_path = "D:/楊松勳/NCDR_AR6_2023/重現期雨量/return_rain_rx1_to_rx5_sci5_sci4x3_list.RData"
load(given_path)

# 檢視物件結構
length(return_rain_rx1_to_rx5_sci5_sci4x3_list) # 5rx 
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list,length) # 2 (GWL+historical / ssp )
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list,names)

length(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_1_5) #5 情境
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_1_5,length)

sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3,length) # 3時期
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$near_term,length)
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$mid_term,length)
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$long_term,length)

unname(sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$long_term$ssp126,dim)) # 1471 6 *28


# 取一個原始資料的座標 增加至 產出矩陣前一二column
raw_csv_path = "D:/楊松勳/AR6_統計降尺度_日資料_臺灣_降雨量/GWL1.5/AR6_統計降尺度_日資料_臺灣_降雨量_ssp126_ACCESS-CM2_2018.csv"
raw_csv_data = read.csv(raw_csv_path)
coord = raw_csv_data[,1:2]
# 函數：取一個原始資料的座標 增加至 產出矩陣前一二column
add_coord = function(one_data_mat){
  cbind(coord,one_data_mat)
}

# 重現期雨量輸出csv檔 -------------------------------------------------------------


# 輸出單個看看

unname(sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$long_term$ssp126,dim)) # 1471 6 *28



# 20231010 輸出重現期雨量為csv檔 ---------------------------------------------------


# setwd("D:/楊松勳/CSV_tank")
setwd("D:/楊松勳/ReturnPeriodRain_CSV_tank")
# 嘗試以lapply儲存為csv檔
name_rx = names(return_rain_rx1_to_rx5_sci5_sci4x3_list)
rain_nested_list = return_rain_rx1_to_rx5_sci5_sci4x3_list
ptm = proc.time()
lapply_write_csv = lapply(seq(1,length(rain_nested_list)),function(rxi){ 

# 第一部分 head--------------------------------------------------------------------
  
  # 第一層 極端指標rx
  one_rx = rain_nested_list[[rxi]]$return_rain_1_5
  one_rx_namez = names(one_rx)
  #
  # 輸出重現期雨量為csv檔
  moc_1_5 = lapply(seq(1,5),function(sci){
    one_sci = one_rx[[sci]]
    one_sci_namez =  names(one_sci)
    soc = lapply(seq(1,length(one_sci)),function(moi){
      one_model = one_sci[[moi]]
      one_model_namez = names(one_model)
      fn = paste0("重現期降雨強度","_",name_rx[rxi],"_",one_rx_namez[sci],"_",one_sci_namez[moi],".csv")
      # print(fn)
      # 標上座標
      one_model_coord = add_coord(one_model)
      write.csv(one_model_coord,fn,row.names = F)

    })
  })
  
# 第一部分 tail--------------------------------------------------------------------  

# 第二部分 head--------------------------------------------------------------------

  # 第一層 極端指標rx
  one_rx = rain_nested_list[[rxi]]$return_rain_6_9_term3
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
        fn = paste0("重現期降雨強度","_",name_rx[rxi],"_",one_rx_namez[termi],"_",one_sci_namez[moi],".csv")
        # print(fn)
        # 標上座標
        one_model_coord = add_coord(one_model)
        write.csv(one_model_coord,fn,row.names = F)
      })
    })
  })
  # 第二部分 tail--------------------------------------------------------------------
});proc.time()-ptm # 64.89


# return_rain_rx1_to_rx5_sci5_sci4x3_list$rx5$return_rain_6_9_term3$long_term$
names(return_rain_rx1_to_rx5_sci5_sci4x3_list)

names(rain_nested_list[[rxi]]$return_rain_6_9_term3)
  
return_rain_rx1_to_rx5_sci5_sci4x3_list$rx5$return_rain_1_5$GWL1.5
return_rain_rx1_to_rx5_sci5_sci4x3_list$rx5$return_rain_6_9_term3$near_term$ssp126
names(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx5$return_rain_6_9_term3)
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx5$return_rain_6_9_term3,names)

