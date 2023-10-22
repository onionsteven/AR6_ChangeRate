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

# rx1 sci_1_5 csv檔名
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_1_5[1:5],names)
model_namez_nest = sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_1_5[1:5],names)
model_namez_nest

# 函數:分解csv檔名 取出 模式名
take_model_name_of_one_sci = function(x){
  sapply(strsplit(x,"_"),tail,1)
  }
"_"
# 取一個情控看看
unname(sapply(model_namez_nest[[5]],take_model_name_of_one_sci))
# lapply 取各情境模式名稱
model_namez_only_nest = lapply(model_namez_nest, function(one_sci){
  unname(sapply(one_sci,take_model_name_of_one_sci))
})

model_namez_only_nest

# 檢視重複的情況
lapply(model_namez_only_nest,table)

# rx1 sci_6_9 近期 csv檔名
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$near_term,names)

model_namez_nest_6_9 = sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$near_term,names)
length(model_namez_nest_6_9)
sapply(model_namez_nest_6_9,length)

take_model_name(model_namez_nest_6_9[[1]])
unname(sapply(model_namez_nest_6_9[[1]],take_model_name_of_one_sci))


tt = lapply(model_namez_nest_6_9,function(one_sci_list){
  # sapply(take_model_name,take_model_name)
  take_model_name_of_one_sci(one_sci_list)
})
tt

tt[[4]]

return_rain_rx1_to_rx5_sci5_sci4x3_list$rx1$return_rain_6_9_term3$near_term


# 20231012 嘗試計算改變率 --------------------------------------------------------


# 取歷史資料之重現期雨量
sapply(return_rain_rx1_to_rx5_sci5_sci4x3_list[[1]]$return_rain_1_5[[5]],names)

# 基期資料
da_baseline = return_rain_rx1_to_rx5_sci5_sci4x3_list[[1]]$return_rain_1_5[[5]]
# 基期模式名
namez_baseline = take_model_name_of_one_sci(names(da_baseline))

# 檢視基期資料 結構
length(da_baseline)
# names(da_baseline)
# 檢視資料維度
unname(sapply(da_baseline,dim))


da_compared_list = return_rain_rx1_to_rx5_sci5_sci4x3_list[[1]]$return_rain_1_5[1:4]
# length(da_compared_list)
# sapply(da_compared_list,length)
sci4_namez_list = sapply(da_compared_list,names)
sci4_namez_list

# 
change_rate_sci_1_4_list = lapply(seq(1,length(da_compared_list)),function(sci){
  # sci = 4
  # moi = 20
  mda = da_compared_list[[sci]]
  mda_namez = sci4_namez_list[[sci]]
  namez_for_tel = sapply(mda_namez,take_model_name_of_one_sci)
  lapply(seq(1,length(mda)),function(moi){
    # mda[[moi]]
    # print(namez_for_tel[moi])
    mda[[moi]]/da_baseline[[which(namez_baseline==namez_for_tel[moi])]]*100
  })
})

length(change_rate_sci_1_4_list)
sapply(change_rate_sci_1_4_list,length)
sapply(change_rate_sci_1_4_list[[1]],dim)

change_rate_sci_1_4_list[[1]][[110]]

plt_range = apply(change_rate_sci_1_4_list[[1]][[110]],2,range,na.rm=T)
plot(x = seq(1,6), y = plt_range[2,],ty="o",ylim=c(0,400))
lines(x = seq(1,6), y = plt_range[1,],ty="o")

boxplot(change_rate_sci_1_4_list[[1]][[110]])


# 20231013 計算三時期重現期雨量改變率 --------------------------------------------------

da_compared_term3_list = return_rain_rx1_to_rx5_sci5_sci4x3_list[[1]]$return_rain_6_9_term3

# 檢視結構
length(da_compared_term3_list) #
sapply(da_compared_term3_list,length) # 4 4 4
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

length(change_rate_term3_list) # 3 
sapply(change_rate_term3_list,length) # 4 4 4
sapply(change_rate_term3_list[[3]],length)

sapply(change_rate_term3_list[[3]][[4]],dim)

sapply(change_rate_term3_list[[3]][[4]],range,na.rm=T)

names(change_rate_term3_sci6_9_list)
sapply(change_rate_term3_sci6_9_list,names)
sapply(change_rate_term3_sci6_9_list[[3]],names)

