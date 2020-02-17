
## This script is used to generate sensitivity data

library(dplyr)
library(RSQLite)
library(reshape2)

### --- Connect to database

# This database below is included in the code
ep_data_file <- dplyr::src_sqlite("energypoverty_data.db", create = FALSE)
dplyr::src_tbls(ep_data_file) # List the tables in the database

### --- Generating data for sensitivity analsysis

w_cook <- 0.4 # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.
w_light <- 0.2 # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.
w_home_app <- 0.2 # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.
w_entert <- 0.2 # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.
k_cutoff <- 0.3 # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.
v_range <- 0.25 # this is the uncertianty range # these values should be the same value as those in 'energypoverty.db', so counter-check to ensure it is the same.

NumOfSim <- 1500 # number of simulations


rand_weight <- function(variaInput, factInput){
  r_weight <- runif(1, factInput*(1-variaInput),
                    factInput*(1+variaInput))
}


gen_weight <- function(lenInput, simInput, variaInput, cooInput, ligInput, homInput, entInput, cutInput){
  
  rw_coo <- rand_weight(variaInput, cooInput)
  rw_lig <- rand_weight(variaInput, ligInput)
  rw_hom <- rand_weight(variaInput, homInput)
  rw_ent <- rand_weight(variaInput, entInput)
  rw_cut <- rand_weight(variaInput, cutInput)
  fw_cut <- rep(cutInput, 1)
  
  dataVal <- dplyr::bind_cols(series = simInput, rw_coo = rw_coo, rw_lig = rw_lig,
                              rw_hom = rw_hom, rw_ent = rw_ent, rw_cut = rw_cut, fw_cut = fw_cut) %>%
    dplyr::mutate(rw_tot = rw_coo + rw_lig + rw_hom + rw_ent,
                  w_cook = rw_coo/rw_tot, w_light = rw_lig/rw_tot,
                  w_home = rw_hom/rw_tot, w_ent = rw_ent/rw_tot) %>% 
    dplyr::select(-rw_coo, -rw_lig, -rw_hom, -rw_ent)
  
  dataVal <- do.call("rbind", replicate(lenInput, dataVal, simplify = FALSE))
  
}


# ##########
# df_all <- dplyr::tbl(ep_data_file, "df_energypoverty") %>% 
#   dplyr::select(hh_key, report) %>% 
#   dplyr::tbl_df()
# 
# df_length <- length(df_all$hh_key)
# dfX <- gen_weight(df_length, 1, v_range, w_cook, w_light, 
#                   w_home_app, w_entert, k_cutoff)
# ########

ep_score_sens <- function(dbInput, dbTable1, simInput, variaInput, cooInput, ligInput, homInput, entInput, cutInput, dbTable2){
  
  start.time <- Sys.time()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")

  ifelse(dbExistsTable(writeToDB, dbTable2) == T,
         dbRemoveTable(writeToDB, dbTable2),
         'This is a new Table')
  
  dataVal1 <- dplyr::tbl(dbInput, dbTable1) %>%
    dplyr::select(hh_key, report, province_name, clean_cooking, clean_lighting, 
                  owns_appliance, entertainment, hhweight, popweight) %>% 
    dplyr::tbl_df()
  
  data_length <- length(dataVal1$hh_key)
  simulations <- 1:simInput
  
  for(simulation in simulations){
    
    start.time2 <- Sys.time()
    
    dataVal2 <- gen_weight(data_length, simulation, variaInput, cooInput, ligInput, homInput, entInput, cutInput)
    
    tableData <- dplyr::bind_cols(dataVal1, dataVal2) %>%
      dplyr::mutate(cook_score = ifelse(clean_cooking == "Yes",
                                        0, w_cook),
                    light_score = ifelse(clean_lighting == "Yes",
                                         0, w_light),
                    home_app_score = ifelse(owns_appliance == "Yes",
                                            0, w_home),
                    entert_score = ifelse(entertainment == "Yes",
                                          0, w_ent),
                    c_tot_score = cook_score + light_score +
                      home_app_score + entert_score,
                    is_energy_poor_rw = ifelse(c_tot_score <= rw_cut,
                                               'No','Yes'),
                    is_energy_poor_fw = ifelse(c_tot_score <= fw_cut,
                                               'No','Yes')) %>%
      dplyr::select(-clean_cooking, -clean_lighting, -owns_appliance, -entertainment, -rw_tot) %>% 
      dplyr::distinct()
    
    RSQLite::dbWriteTable(conn = writeToDB, name = dbTable2,
                          tableData, append = T, row.names = F)
    
    end.time2 <- Sys.time()
    time.taken2 <- end.time2 - start.time2
    print(time.taken2)
    
  }
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}


df_score_sens <- ep_score_sens(ep_data_file, "df_energypoverty", NumOfSim, v_range, w_cook,
                               w_light, w_home_app, w_entert, k_cutoff,  "df_score_sens")



## Further processing for sensitivity data

## Note that the incidence values don't change, as they are based on dataset statictics 

headcount_pro_analysis <- function(dbInput, dbTable){
  
  dataVal <- tbl(dbInput, dbTable) %>% 
    dplyr::mutate(uniq_id = paste0(hh_key, '*', report, '*', province_name),
                  c_total_score_hc = c_total_score, hhweight_hc = hhweight) %>%
    # dplyr::select(uniq_id, clean_cooking, clean_lighting, owns_appliance, 
    #               entertainment, c_total_score_hc, is_energy_poor, hhweight_hc) %>% 
    dplyr::select(uniq_id, c_total_score_hc, is_energy_poor) %>% 
    dplyr::tbl_df()
  
}


# ep_cooking_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score")
# head(ep_cooking_pro)
# rm(ep_cooking_pro)


#### intensity

intensity_pro_analysis_sens <- function(dbInput1, dbTable1){
  
  table1 <- dplyr::tbl(dbInput1, dbTable1) %>%
    dplyr::mutate(uniq_id = paste0(hh_key, '*', report, '*', province_name),
                  c_total_score_int = c_tot_score, hhweight_int = hhweight) %>%
    dplyr::select(uniq_id, hhweight_int, popweight, c_total_score_int, series,
                  rw_cut, fw_cut, w_cook, w_light, w_home, w_ent,
                  is_energy_poor_rw, is_energy_poor_fw) %>%
    dplyr::tbl_df()
  
}

# ep_int_sense2 <- intensity_pro_analysis_sens(ep_data_file, "df_score_sens")
# head(ep_int_sense2)
# rm(ep_int_sense2)


###

combine_sens <- function(dbInput){
  
  start.time <- Sys.time()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  dataVal1 <- headcount_pro_analysis(dbInput, "df_ep_score")
  
  dataVal2 <- intensity_pro_analysis_sens(dbInput, "df_score_sens")
  
  tableData <- full_join(dataVal2, dataVal1, by = c("uniq_id"))
  
  RSQLite::dbWriteTable(conn = writeToDB, name = "df_score_sens2",
                        tableData, append = T, row.names = F)
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}


df_score_sens2 <- combine_sens(ep_data_file)


## multidimensional energy poverty index sens processing

mepi_sens <- function(dbInput, dbTable){
  
  dataVal <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::select(uniq_id, series, hhweight_int, c_total_score_int,
                  is_energy_poor, is_energy_poor_rw, is_energy_poor_fw) %>% 
    dplyr::mutate(c_tot_rweighted = hhweight_int * c_total_score_int,
                  c_tot_cweighted = hhweight_int * c_total_score_hc)  
  
  df_uniq_id <- tbl_df(select(dataVal, uniq_id))
  df_report_prov <- as.data.frame(stringr::str_split_fixed(df_uniq_id$uniq_id, '\\*', 3)) %>%
    dplyr::rename(hh_key = V1, report = V2, province_name = V3) %>%
    dplyr::select(-hh_key)
  
  dataVal <- dataVal %>%
    dplyr::select(series, hhweight_int, c_tot_cweighted, c_tot_rweighted,
                  is_energy_poor, is_energy_poor_rw, is_energy_poor_fw) %>%
    dplyr::tbl_df()
  
  tableData <- dplyr::bind_cols(df_report_prov, dataVal) %>%
    dplyr::tbl_df()
  
}

# ep_index_sens <- mepi_sens(ep_data_file, "df_score_sens2")
# head(ep_index_sens, 4)



## multidimensional energy poverty index for all series
mepi_sense_all <- function(dbInput, dbTable){
  
  start.time <- Sys.time()
  
  dataVal <- mepi_sens(dbInput, dbTable) %>%
    reshape2::melt(id.vars = c('report', 'series', 'hhweight_int',
                               'c_tot_cweighted', 'c_tot_rweighted'),
                   variable.name = "ep_status",
                   value.name = "ep_response")
  
  dataVal_hc1 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor') %>%
    dplyr::group_by(report,  series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int1 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_cweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged1 <- full_join(dataVal_hc1, dataVal_int1, by = c("report", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor") %>%
    dplyr::select(-ep_response, -tot_c)
  
  dataVal_hc2 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_fw') %>%
    dplyr::group_by(report,  series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int2 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_fw',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_rweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged2 <- full_join(dataVal_hc2, dataVal_int2, by = c("report", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor_fw") %>%
    dplyr::select(-ep_response, -tot_c)
  
  
  dataVal_hc3 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_rw') %>%
    dplyr::group_by(report, series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int3 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_rw',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_rweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged3 <- full_join(dataVal_hc3, dataVal_int3, by = c("report", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor_rw") %>%
    dplyr::select(-ep_response, -tot_c)
  
  tableData <- dplyr::bind_rows(ep_merged1, ep_merged2, ep_merged3)
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  RSQLite::dbWriteTable(conn = writeToDB, name = "df_mepi_sens",
                        tableData, overwrite = T, row.names = F)
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}


ep_index_all <- mepi_sense_all(ep_data_file, "df_score_sens2")
head(ep_index_all)



## multidimensional energy poverty index at provincial level
mepi_sense_prov <- function(dbInput, dbTable){ #dbInput, dbTable, VarInput1, VarInput2, VarInput3, VarInput4){
  
  start.time <- Sys.time()
  
  dataVal <- mepi_sens(dbInput, dbTable) %>%
    reshape2::melt(id.vars = c('report', 'province_name', 'series', 'hhweight_int',
                               'c_tot_cweighted', 'c_tot_rweighted'),
                   variable.name = "ep_status", 
                   value.name = "ep_response") 
  
  dataVal_hc1 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor') %>% 
    dplyr::group_by(report, province_name, series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int1 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, province_name, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_cweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged1 <- full_join(dataVal_hc1, dataVal_int1, by = c("report", "province_name", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor") %>%
    dplyr::select(-ep_response, -tot_c)
  
  dataVal_hc2 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_fw') %>% 
    dplyr::group_by(report, province_name, series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int2 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_fw',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, province_name, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_rweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged2 <- full_join(dataVal_hc2, dataVal_int2, by = c("report", "province_name", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor_fw") %>%
    dplyr::select(-ep_response, -tot_c)
  
  
  dataVal_hc3 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_rw') %>% 
    dplyr::group_by(report, province_name, series, ep_response) %>%
    dplyr::summarise(total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(incidence = round(total_Obs/sum(total_Obs), 6)) %>% 
    filter(ep_response == 'Yes')
  
  dataVal_int3 <- dataVal %>%
    dplyr::filter(ep_status == 'is_energy_poor_rw',
                  ep_response == 'Yes') %>%
    dplyr::group_by(report, province_name, series) %>%
    dplyr::summarise(tot_c = sum(c_tot_rweighted), total_Obs = sum(hhweight_int)) %>%
    dplyr::mutate(intensity = round(tot_c/total_Obs, 6))
  
  ep_merged3 <- full_join(dataVal_hc3, dataVal_int3, by = c("report", "province_name", "total_Obs", "series")) %>%
    dplyr::mutate(mepi = incidence * intensity, status = 'energy poor', ep_status = "is_energy_poor_rw") %>%
    dplyr::select(-ep_response, -tot_c)
  
  tableData <- dplyr::bind_rows(ep_merged1, ep_merged2, ep_merged3)
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  RSQLite::dbWriteTable(conn = writeToDB, name = "df_mepi_sens_prov",
                        tableData, overwrite = T, row.names = F)
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}


ep_index_prov <- mepi_sense_prov(ep_data_file, "df_score_sens2")
ep_index_prov


