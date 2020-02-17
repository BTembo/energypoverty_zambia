
library(stringr)
library(plyr)
library(dplyr)
library(RSQLite)
library(ggplot2)
library(reshape2)
library(Rmisc)
library(ggrepel)
library(scales)

#### --------------------

ep_data_file <- dplyr::src_sqlite("energypoverty_data.db", create = FALSE) 
dplyr::src_tbls(ep_data_file) # List the tables in the database

### --- head count analysis for energy poverty

## the incidence of multidimensional energy poverty
headcount_analysis <- function(dbInput, dbTable, VarInput1, VarInput2, VarInput3){
  
  varInterest = c(VarInput1, VarInput2, VarInput3)
  
  dataVal <- tbl_df(tbl(dbInput, dbTable))
  
  dataVal <- dataVal[varInterest]
  
  names(dataVal)[2] <- 'response' ## I used '2' because that is index number of the column I want to change
  
  dataVal <- dataVal %>%
    dplyr::group_by(report, response) %>%
    dplyr::summarise(total = sum(hhweight)) %>%
    dplyr::mutate(incidence = round(total/sum(total), 6))
  
}


# ep_cooking <- headcount_analysis(ep_data_file, "df_ep_score", "report", "clean_cooking", "hhweight")
# ep_lighting <- headcount_analysis(ep_data_file, "df_ep_score", "report", "clean_lighting", "hhweight")
# ep_appliances <- headcount_analysis(ep_data_file, "df_ep_score", "report", "owns_appliance", "hhweight")
# ep_entern <- headcount_analysis(ep_data_file, "df_ep_score", "report", "entertainment", "hhweight")
ep_overall <- headcount_analysis(ep_data_file, "df_ep_score", "report", "is_energy_poor", "hhweight")


## the intensity of multidimensional energy poverty
intensity_analysis <- function(dbInput, dbTable, VarInput1, VarInput2, VarInput3, VarInput4){
  
  varInterest = c(VarInput1, VarInput2, VarInput3, VarInput4)
  
  dataVal <- tbl_df(tbl(dbInput, dbTable))
  
  dataVal <- dataVal[varInterest]
  
  dataVal <- dataVal %>%
    dplyr::filter(is_energy_poor == 'Yes') %>%
    dplyr::select(report, c_total_score, hhweight) %>%
    dplyr::group_by(report) %>%
    dplyr::summarise(tot_c = sum(c_total_score), tot_Obs = n(),
                     tot_c2 = sum(hhweight*c_total_score), tot_Obs2 = sum(hhweight)) %>%
    dplyr::mutate(intensity = round(tot_c/tot_Obs, 6),
                  intensity2 = round(tot_c2/tot_Obs2, 6))
  
}

ep_intensity <- intensity_analysis(ep_data_file, "df_ep_score", "report", "is_energy_poor","c_total_score", "hhweight")


## multidimensional energy poverty index
mepi <- function(dbInput, dbTable, VarInput1, VarInput2, VarInput3, VarInput4){
    
  ep_hc <- headcount_analysis(dbInput, dbTable, VarInput1, VarInput2, VarInput3) %>% 
    dplyr::filter(response == 'Yes')
  ep_int <- intensity_analysis(dbInput, dbTable, VarInput1, VarInput2, VarInput4, VarInput3)

  ep_merged <- full_join(ep_hc, ep_int, by = c("report")) %>% 
    dplyr::mutate(mepi = incidence * intensity,
                  mepi2 = incidence * intensity2, 
                  status = 'energy poor') %>% 
    dplyr::select(-response, -total, -tot_c, -tot_Obs)
  
}

ep_index <- mepi(ep_data_file, "df_ep_score", "report", "is_energy_poor", "hhweight", "c_total_score")
ep_index


### --- Provincial analysis
## the incidence of multidimensional energy poverty

# I am not sure if this is an efficient way of get Provincial values, but it works

headcount_pro_analysis <- function(dbInput, dbTable, VarInput1, VarInput2){
  
  dataVal <- tbl_df(tbl(dbInput, dbTable)) %>% 
    dplyr::mutate(report_prov = paste0(report, '_', province_name))
  
  ofInterest <- c('report_prov', VarInput1, VarInput2)
  
  dataVal <- dataVal[ofInterest]
  
  names(dataVal)[2] <- 'response' ## I used '2' because that is index number of the column I want to change
  
  dataVal <- dataVal %>%
    dplyr::group_by(report_prov, response) %>%
    dplyr::summarise(total = sum(hhweight)) %>%
    dplyr::mutate(incidence = round(total/sum(total), 6))
  
  df_report_prov <- as.data.frame(stringr::str_split_fixed(dataVal$report_prov, '_', 2)) %>% 
      dplyr::rename(report = V1, province_name = V2)
  
  energy_services <- c("clean_cooking", "clean_lighting", 
                       "owns_appliance", "entertainment")
  
  if (VarInput1 %in% energy_services){
    tableData <- dplyr::bind_cols(df_report_prov, dataVal) %>% 
      dplyr::select(-report_prov, -total) %>% dplyr::tbl_df() %>% 
      reshape2::dcast(report + province_name ~ response, value.var = 'incidence') %>% 
      dplyr::mutate(incidence = No) %>% 
      dplyr::select(-No, -Yes) %>% dplyr::tbl_df()
  }else if(VarInput1 == "is_energy_poor"){
    tableData <- dplyr::bind_cols(df_report_prov, dataVal) %>% 
      dplyr::select(-report_prov, -total) %>% dplyr::tbl_df() %>% 
      reshape2::dcast(report + province_name ~ response, value.var = 'incidence') %>% 
      dplyr::mutate(incidence = Yes) %>% 
      dplyr::select(-No, -Yes) %>% dplyr::tbl_df()
  }else{
    print('This is an error.')
  }
}


# ep_cooking_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score", "clean_cooking", "hhweight")
# ep_lighting_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score", "clean_lighting", "hhweight")
# ep_appliances_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score", "owns_appliance", "hhweight")
# ep_entern_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score", "entertainment", "hhweight")
ep_overall_pro <- headcount_pro_analysis(ep_data_file, "df_ep_score", "is_energy_poor", "hhweight")
head(ep_overall_pro)


## the intensity of multidimensional energy poverty
intensity_pro_analysis <- function(dbInput, dbTable, VarInput1, VarInput2, VarInput3, VarInput4, VarInput5){
  
  # dataVal <- tbl_df(tbl(dbInput, dbTable)) %>% 
  #   dplyr::mutate(report_prov = paste0(report, '_', province_name))
  
  varInterest = c(VarInput1, VarInput2, VarInput3, VarInput4, VarInput5)
  
  dataVal <- tbl_df(tbl(dbInput, dbTable))
  
  dataVal <- dataVal[varInterest]
  
  # varInterest = c("report", "is_energy_poor", "province_name", "c_total_score")
  
  dataVal <- dataVal %>%
    dplyr::filter(is_energy_poor == 'Yes') %>%
    dplyr::select(report, province_name, c_total_score, hhweight) %>%
    dplyr::group_by(report, province_name) %>%
    # dplyr::summarise(tot_c = sum(c_total_score), tot_Obs = n()) %>%
    # dplyr::mutate(intensity = round(tot_c/tot_Obs, 6)) %>% 
    dplyr::summarise(tot_c = sum(c_total_score), tot_Obs = n(),
                     tot_c2 = sum(hhweight*c_total_score), 
                     tot_Obs2 = sum(hhweight)) %>%
    dplyr::mutate(intensity = round(tot_c/tot_Obs, 6),
                  intensity2 = round(tot_c2/tot_Obs2, 6))
  
}

ep_intensity_pro <- intensity_pro_analysis(ep_data_file, "df_ep_score", "report",
                                           "is_energy_poor", "province_name",
                                           "c_total_score", "hhweight")
head(ep_intensity_pro)


## multidimensional energy poverty index
mepi_pro <- function(dbInput, dbTable, VarInput1, VarInput2, VarInput3, VarInput4, VarInput5){
  
  ep_hc <- headcount_pro_analysis(dbInput, dbTable, VarInput2, VarInput3) %>% 
    dplyr::mutate(report = as.character(report), province_name = as.character(province_name))
  
  ep_int <- intensity_pro_analysis(dbInput, dbTable, VarInput1, VarInput2,
                                   VarInput4, VarInput5, VarInput3)
  
  ep_merged <- full_join(ep_hc, ep_int, by = c("report", "province_name")) %>% 
    dplyr::mutate(mepi = incidence * intensity,
                  mepi2 = incidence * intensity2) %>% 
    dplyr::select(-tot_c, -tot_Obs)
  
}

ep_index_pro <- mepi_pro(ep_data_file, "df_ep_score", "report", "is_energy_poor",
                         "hhweight","province_name", "c_total_score")
head(ep_index_pro)


#### For code that was used to generate plots (figures) and table see:
# plots: 'plots_energypovertyZambia.R'
# tables: 'tables_energypovertyZambia.R'