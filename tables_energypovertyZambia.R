## This is linked to the main file ('analysis_energypovertyZambia.R'):

### Connect to database
ep_data_file <- dplyr::src_sqlite("energypoverty_data.db", create = FALSE) 
dplyr::src_tbls(ep_data_file) # List the tables in the database


### Table 2: MEPI at provincial level in 2010 and 2015

ep_index_pro

### 

tbl_location <- dplyr::tbl(ep_data_file, "df_energypoverty") %>% 
  dplyr::filter(!is.na(rural_urban)) %>% 
  dplyr::select(report, hh_key, province_name, poverty, rural_urban, hhweight, popweight)

tbl_access_fuels <- dplyr::tbl(ep_data_file, "section8_combined") 

access_usage_fuels_pov <- dplyr::full_join(tbl_df(tbl_location), tbl_df(tbl_access_fuels), by = c('hh_key', 'report')) %>% 

  dplyr::select(hh_key, rural_urban, poverty, sec8q13, sec8q14, sec8q16a, report, hhweight, popweight) %>%
  dplyr::mutate(location = ifelse(rural_urban == 1, 'Rural', 'Urban'),
                elec_light = ifelse(sec8q13 == 2, 'Yes', 'No'),
                elec_light = ifelse(is.na(elec_light), 'No', elec_light),
                elec_cook = ifelse(sec8q14 == 8, 'Yes', 'No'),
                elec_cook = ifelse(is.na(elec_cook), 'No', elec_cook),
                grid_access = ifelse(sec8q16a == 1, 'Yes', 'No'),
                grid_access = ifelse(is.na(grid_access), 'No', grid_access),
                poverty_name = ifelse(poverty == 1, 'Extremely Poor',
                                      ifelse(poverty == 2, 'Moderately Poor',
                                             ifelse(poverty == 3, 'Non-Poor',
                                                    'Missing Value')))) %>% 
  dplyr::filter(!is.na(rural_urban), !is.na(poverty))

access_usage_fuels_pov



### sense check and compare national poverty levels in CSO's LCMS Report (CSO, 2016)
pov_df <- access_usage_fuels_pov %>% 
  select(hh_key, poverty_name, report, hhweight, popweight) %>% 
  dplyr::group_by(report, poverty_name) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T),
                   tot_popweight = sum(popweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2),
                prop_pop = round(100*tot_popweight/sum(tot_popweight),2))

pov_df

nat_pro <- pov_df %>% 
  dplyr::filter(report == '2015 LCMS')

nat_pro


## Table 3: Share (in %) of households by poverty status in 2015

### location (rural-urban) poverty levels

prov_df <- access_usage_fuels_pov %>% 
  select(hh_key, poverty_name, report, hhweight, location) %>% 
  dplyr::group_by(report, location, poverty_name) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight),2))

prov_df


prov_loc <- prov_df %>% 
  dplyr::filter(report == '2015 LCMS')

prov_loc

### Table 4: Share (in %) of households that had access to the grid in 2015


acc_df <- access_usage_fuels_pov %>% 
  select(hh_key, poverty_name, report, hhweight, location, grid_access) %>%
  dplyr::group_by(report, location, poverty_name, grid_access) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight),2))

acc_df

grid_loc <- acc_df %>% 
  dplyr::filter(report == '2015 LCMS', grid_access == 'Yes')

grid_loc

### Table 5: Share (in %) of households that used electricity for lighting in 2015

light_df <- access_usage_fuels_pov %>% 
  select(hh_key, poverty_name, report, hhweight, location, elec_light, grid_access) %>% 
  dplyr::filter(grid_access == 'Yes') %>%
  dplyr::group_by(report, location, poverty_name, elec_light) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight),2))

light_df

grid_light_loc <- light_df %>% 
  dplyr::filter(report == '2015 LCMS', elec_light == 'Yes')

grid_light_loc


### Table 6: Share (in %) of households that used electricity for cooking in 2015

cook_df <- access_usage_fuels_pov %>% 
  select(hh_key, poverty_name, report, hhweight, location, elec_cook, grid_access) %>% 
  dplyr::filter(grid_access == 'Yes') %>%
  dplyr::group_by(report, location, poverty_name, elec_cook) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight),2))

cook_df

grid_cook_loc <- cook_df %>% 
  dplyr::filter(report == '2015 LCMS', elec_cook == 'Yes')

grid_cook_loc


### Table 7: Share (in %) of households that are not energy poor in 2015

income_energy_pov_prov_loc_analysis <- function(dbInput, dbTable1, dbTable2, VarInput){

  dataVal1 <- tbl(dbInput, dbTable1) %>%
    dplyr::select(hh_key, province_name, rural_urban, poverty, hhweight, report,
                  clean_cooking, clean_lighting, grid_access, popweight) %>%
    dplyr::tbl_df()

  dataVal2 <- tbl(dbInput, dbTable2) %>%
    dplyr::select(hh_key, report, province_name, is_energy_poor) %>%
    dplyr::tbl_df()

  df_merged <- full_join(dataVal1, dataVal2, by = c("hh_key", "report", "province_name")) %>%
    reshape2::melt(id.vars = c("hh_key","province_name", "rural_urban", "poverty","hhweight","report","popweight"),
                   variable.name = "energy_question",
                   value.name = "response") %>%
    dplyr::filter(!is.na(poverty), energy_question == VarInput, !is.na(rural_urban)) %>%
    dplyr::mutate(poverty_name = ifelse(poverty == 1, 'Extremely Poor',
                                        ifelse(poverty == 2, 'Moderately Poor',
                                               ifelse(poverty == 3, 'Non-Poor',
                                                      'Missing Value')))) %>%
    dplyr::mutate(uniq_id = paste0(report, '*', province_name, '*', poverty_name,
                                   '*', energy_question,'*', response)) %>%
    dplyr::group_by(province_name, rural_urban, uniq_id) %>%
    dplyr::summarise(total_hh = sum(hhweight),
                     total_pop = sum(popweight)) %>%
    dplyr::mutate(prop_hh = round(total_hh/sum(total_hh), 6),
                  prop_pop = round(total_pop/sum(total_pop), 6)) %>%
    dplyr::mutate(rural_urban2 = ifelse(
      rural_urban == 1, 'Rural', ifelse(
        rural_urban == 2, 'Urban', 'This is unusual')))

  df_report_prov <- as.data.frame(stringr::str_split_fixed(df_merged$uniq_id, '\\*', 5)) %>%
    dplyr::rename(report = V1, province_name2 = V2, poverty_name = V3,
                  energy_question = V4, response = V5)

  tableData <- dplyr::bind_cols(df_report_prov, df_merged) %>%
    dplyr::select(-uniq_id, -province_name2) %>% dplyr::tbl_df()

}


df_income_energy_pov_prov_loc <- income_energy_pov_prov_loc_analysis(ep_data_file, "df_energypoverty", "df_ep_score", "is_energy_poor")
head(df_income_energy_pov_prov_loc, 3)


inc_en_loc <- df_income_energy_pov_prov_loc %>% 
  dplyr::group_by(report, poverty_name, energy_question, rural_urban2, response) %>%
  dplyr::summarise(tot_nat = sum(total_hh)) %>%
  dplyr::mutate(share_nat = round(tot_nat/sum(tot_nat), 6)) %>% 
  arrange(response)

inc_en_loc

mepi_loc <- inc_en_loc %>% 
  dplyr::filter(report == '2015 LCMS', response == 'No') %>% 
  dplyr::mutate(share_nat = 100*share_nat)

mepi_loc



##

input_dimensions <- function(dbInput, dbTable){
  
  dataVal1 <- tbl(dbInput, dbTable) %>%
    dplyr::select(hh_key, province_name, hhweight, report, clean_cooking, clean_lighting,
                  owns_appliance, entertainment, grid_access, popweight) %>%
    dplyr::tbl_df()
  
}


df_dim_inputs <- input_dimensions(ep_data_file, "df_energypoverty")
head(df_dim_inputs, 3)


### Table 9: Share (in %) of households without grid access

gridaccess_df <- df_dim_inputs %>% 
  select(hh_key, province_name, report, hhweight, grid_access) %>%
  dplyr::group_by(report, province_name, grid_access) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2)) %>% 
  dplyr::filter(grid_access == 'No')

gridaccess_df


### Table 10: Share (in %) of households without clean cooking

cookingfuels_df <- df_dim_inputs %>% 
  select(hh_key, province_name, report, hhweight, clean_cooking) %>%
  dplyr::group_by(report, province_name, clean_cooking) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2)) %>% 
  dplyr::filter(clean_cooking == 'No')

cookingfuels_df


### Table 11: Share (in %) of households without clean lighting

lightingfuels_df <- df_dim_inputs %>% 
  select(hh_key, province_name, report, hhweight, clean_lighting) %>%
  dplyr::group_by(report, province_name, clean_lighting) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2)) %>% 
  dplyr::filter(clean_lighting == 'No')

lightingfuels_df


### Table 12: Share (in %) of households without electrical appliance ownership

ownsappliance_df <- df_dim_inputs %>% 
  select(hh_key, province_name, report, hhweight, owns_appliance) %>%
  dplyr::group_by(report, province_name, owns_appliance) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2)) %>% 
  dplyr::filter(owns_appliance == 'No')

ownsappliance_df


### Table 13: Share (in %) of households without entertainment

entertainment_df <- df_dim_inputs %>% 
  select(hh_key, province_name, report, hhweight, entertainment) %>%
  dplyr::group_by(report, province_name, entertainment) %>%
  dplyr::summarise(tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop_hh = round(100*tot_hhweight/sum(tot_hhweight), 2)) %>% 
  dplyr::filter(entertainment == 'No')

entertainment_df






