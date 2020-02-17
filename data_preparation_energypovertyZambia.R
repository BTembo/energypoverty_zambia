
library(stringr)
library(plyr)
library(dplyr)
library(RSQLite)
library(reshape2)


#### --------------------

# Geting data from different databases 
# You can get the datasets (which are put in these databases) are from https://www.zamstats.gov.zm/
# Because of that, I have not included them in this repository.
# The key sections in the datasets from zamstats are 'secton 7, 8 and poverty analysis'.
# The dataset are LCMS 2010 and 2015.

lcms2010_file <- dplyr::src_sqlite("raw2010_lcmsdata.db", create = FALSE) 
dplyr::src_tbls(lcms2010_file) # List the tables in the database

lcms2015_file <- dplyr::src_sqlite("lcms2015_sectionHousehold.db", create = FALSE) 
dplyr::src_tbls(lcms2015_file) # List the tables in the database

lcms2015pers_file <- dplyr::src_sqlite("lcms2015_sectionsPerson.db", create = FALSE) 
dplyr::src_tbls(lcms2015pers_file) # List the tables in the database


# Below is the database to which all process data will be read from
# create the database
ep_data_file <- dplyr::src_sqlite("energypoverty_data.db", create = TRUE)


## Start processing the data

# Household appliances

cooking <- c(21,22)
home_app <- c(23,24,25,26,27)
entern <- c(7,8,9,10,15)

# This is a function to clean up Section 7 data table to only extract three variables of interest.

from_sec7 <- c(entern, cooking, home_app)

clean_sec7 <- function(dbInput1, dbInput2, dbTable1, dbTable2, dbTable3, assetInput){
  
  start.time <- Sys.time()
  
  table1 <- dplyr::tbl(dbInput1, dbTable1)
  table1 <- table1 %>%
    dplyr::select(parentid1, assetid, sec7q2) %>%
    dplyr::rename(hh_key = parentid1, asset_id = assetid) %>%
    dplyr::filter(asset_id %in% assetInput, !is.na(sec7q2)) %>%
    dplyr::mutate(report = "2015 LCMS") %>%
    dplyr::tbl_df()

  table2 <- dplyr::tbl(dbInput1, dbTable2)
  table2 <- table2 %>%
    dplyr::select(parentid1, id, sec7q2) %>%
    dplyr::rename(hh_key = parentid1, asset_id = id) %>%
    dplyr::filter(asset_id %in% assetInput, !is.na(sec7q2)) %>%
    dplyr::mutate(report = "2015 LCMS") %>%
    dplyr::tbl_df()
  
  table3 <- dplyr::tbl(dbInput2, dbTable3)
  table3 <- table3 %>%
    dplyr::select(hh_key, asset_number, s07q02) %>%
    dplyr::rename(asset_id = asset_number, sec7q2 = s07q02) %>%
    dplyr::filter(asset_id %in% assetInput, !is.na(sec7q2)) %>%
    dplyr::mutate(report = "2010 LCMS") %>%
    dplyr::tbl_df()
  
  tableData <- dplyr::bind_rows(table1, table2, table3)
  tableData <- tableData %>%
    dplyr::distinct()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  RSQLite::dbWriteTable(conn = writeToDB, name = "section7_combined",
                        tableData, overwrite = T, row.names = F)
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

sec7_data <- clean_sec7(lcms2015_file, lcms2010_file, 
                        "part_Sec7p1_2015LCMS", "part_Sec7p2_2015LCMS", 
                        "part_Sec7_2010LCMS", from_sec7)


# This is a function to clean up Section 8 data table.
clean_sec8 <- function(dbInput1, dbInput2, dbTable1, dbTable2){
  
  start.time <- Sys.time()
  
  table1 <- dplyr::tbl(dbInput1, dbTable1)
  table1 <- table1 %>%
    dplyr::select(parentid1, sec8q13, sec8q14, sec8q15, sec8q16a) %>%
    dplyr::rename(hh_key = parentid1) %>%
    dplyr::mutate(report = "2015 LCMS") %>%
    dplyr::tbl_df()
  
  table2 <- dplyr::tbl(dbInput2, dbTable2)
  table2 <- table2 %>%
    dplyr::select(-s08q16b) %>%
    dplyr::rename(sec8q13 = s08q13, sec8q14 = s08q14,
                  sec8q15 = s08q15, sec8q16a = s08q16a) %>%
    dplyr::mutate(report = "2010 LCMS") %>%
    dplyr::tbl_df()
  
  tableData <- dplyr::bind_rows(table1, table2)
  tableData <- tableData %>%
    dplyr::distinct()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  RSQLite::dbWriteTable(conn = writeToDB, name = "section8_combined",
                        tableData, overwrite = T, row.names = F)
  
  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

sec8_data <- clean_sec8(lcms2015_file, lcms2010_file, 
                        "part_Sec8_2015LCMS", 
                        "part_Sec8_2010LCMS")


## --- Further data processing

### --- Section 7, home appliance ownership
app_ownership <- function(dbInput, dbTable, appInput){
  
  dataVal <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::filter(asset_id %in% appInput) %>% 
    dplyr::select(hh_key, report) %>% 
    dplyr::mutate(owns_home_app = 'Yes') %>% 
    dplyr::distinct() %>% 
    dplyr::tbl_df()
  
}

df_app <- app_ownership(ep_data_file, 'section7_combined', home_app)


### --- Section 7, a household has an entertainment appliance
entertainment_ownership <- function(dbInput, dbTable, entInput){
  
  dataVal <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::filter(asset_id %in% entInput) %>% 
    dplyr::select(hh_key, report) %>% 
    dplyr::mutate(has_entertainment = 'Yes') %>% 
    dplyr::distinct() %>% 
    dplyr::tbl_df()
  
}

df_ent <- entertainment_ownership(ep_data_file, 'section7_combined', entern)


### --- Section 8, a household has an clean/modern lighting
# In this analysis, only electricity and solar are considered as clean and modern forms of lighting 
# (based on the fuel listing of Sec 8)

clean_fuel_lighting <- c(2, 3)

clean_lighting <- function(dbInput, dbTable, cleanfuelInput){
  
  dataVal <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::filter(sec8q13 %in% cleanfuelInput) %>% 
    dplyr::select(hh_key, report) %>% 
    dplyr::mutate(has_clean_lighting = 'Yes') %>% 
    dplyr::distinct() %>% 
    dplyr::tbl_df()
  
}

df_lighting <- clean_lighting(ep_data_file, 'section8_combined', clean_fuel_lighting)


### --- Section 8, a household has access to the electricity grid


grid_access <- function(dbInput, dbTable){
  
  dataVal <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::filter(sec8q16a == 1) %>%  ## 1 == 'Yes' and 2 == 'No' 
    dplyr::select(hh_key, report) %>% 
    dplyr::mutate(has_grid_access = 'Yes') %>% 
    dplyr::distinct() %>% 
    dplyr::tbl_df()
  
}

df_grid_access <- grid_access(ep_data_file, 'section8_combined')


### --- Sections 7 and 8, a household has modern cooking facility and appliance

# In this analysis, only electricity and gas are considered as clean and modern forms of cooking 
# (based on the fuel listing of Sec 8 -- sec8 q14)
clean_fuel_cooking <- c(7, 8)

# Devices that could possibly be used for clean cooking are stove/cooker, vehicle tyre rim, 
# hot plate withiout stand, hot plate on welded stand and other (based on the fuel listing of Sec 8 -- sec8 q15),
app_for_clean_cooking <- c(1,6,7,8,9)

# Ownership of clean fuel household assets were taken into consideration -- sec7 assets 21 & 22.

modern_cooking <- function(dbInput, dbTable1, dbTable2, cookInput, cleanfuelInput, cleandeviceInput){
  
  dataVal1 <- dplyr::tbl(dbInput, dbTable1) %>%
    dplyr::filter(asset_id %in% cookInput) %>%
    dplyr::select(hh_key, report) %>%
    # dplyr::mutate(has_modern_cook_app = 'Yes') %>%
    dplyr::distinct() %>%  dplyr::tbl_df()
  
  dataVal2 <- dplyr::tbl(dbInput, dbTable2) %>% 
    dplyr::filter(sec8q14 %in% cleanfuelInput & 
                    sec8q15 %in% cleandeviceInput) %>% 
    dplyr::select(hh_key, report) %>% 
    # dplyr::mutate(has_modern_fuel = 'Yes') %>% 
    dplyr::distinct() %>% dplyr::tbl_df()
  
  uniq_app <- unique(dataVal1$hh_key)
  uniq_fuel <- unique(dataVal2$hh_key)
  
  clean_cook <- intersect(uniq_app, uniq_fuel)
  
  tableData <- dplyr::bind_rows(dataVal1, dataVal2)
  tableData <- tableData %>%
    dplyr::filter(hh_key %in% clean_cook) %>% 
    dplyr::mutate(has_clean_cooking = 'Yes') %>%
    dplyr::distinct() %>% 
    dplyr::tbl_df()
  
}
  
df_cooking <- modern_cooking(ep_data_file, 'section7_combined', 'section8_combined', 
                             cooking, clean_fuel_cooking, app_for_clean_cooking)


##### --- Collating all the variables considered in the energy poverty analysis

get_general_hh_data <- function(dbInput1, dbInput2, dbInput3, dbTable1, dbTable2, dbTable3, allocateInput){
  
  start.time <- Sys.time()
  
  ## Get : province, rural/urban split, age of head, gender of head, poverty, hhsize, hhadulteq, province_name
  
  to_prov <- read.csv(allocateInput, stringsAsFactors = FALSE)
  
  table1 <- dplyr::tbl(dbInput1, dbTable1)
  table1 <- table1 %>%
    dplyr::select(parentid1, prov, region, Poverty, hhsize, 
                  adulteq, dist, hhweight, popweight) %>%
    dplyr::tbl_df()
  
  dist_to_prov <- dplyr::distinct(dplyr::select(to_prov, dist_code, prov_name))
  
  p_name <- plyr::mapvalues(table1$dist, dist_to_prov$dist_code,
                            dist_to_prov$prov_name)
  
  table1 <- dplyr::bind_cols(table1, prov_name = p_name)

  table1 <- table1 %>%
    dplyr::rename(hh_key = parentid1, poverty = Poverty, province = prov,
                  rural_urban = region, hhadulteq_size = adulteq, 
                  province_name = prov_name) %>%
    dplyr::mutate(report = "2015 LCMS") %>%
    dplyr::tbl_df()

  table3 <- dplyr::tbl(dbInput3, dbTable3)
  table3 <- table3 %>%
    dplyr::select(parentid1, sec1q3b, sec1q4, sec1q5) %>%
    dplyr::mutate( sec1q3b = as.numeric(sec1q3b),
                   sec1q4 = as.numeric(sec1q4),
                   sec1q5 = as.numeric(sec1q5)) %>%
    dplyr::filter(sec1q4 == 1) %>%
    dplyr::rename(hh_key = parentid1, age = sec1q3b,
                  pid_02 = sec1q4, sex = sec1q5) %>%
    dplyr::tbl_df()

  merged_t1t3 <- full_join(table1, table3, by = c("hh_key"))
  merged_t1t3 <- merged_t1t3 %>%
    dplyr::select(hh_key, province, rural_urban, poverty, hhsize, hhadulteq_size, 
                  hhweight, popweight, sex, age, province_name, report) #, dist)

  table2 <- dplyr::tbl(dbInput2, dbTable2)
  table2 <- table2 %>%
    dplyr::select(hh_key, province, region, pline, hhsize, hhequi, 
                  sex, age, pid_02, constituency, weights) %>%
    dplyr::tbl_df()

  p_name <- plyr::mapvalues(table2$constituency, to_prov$const_code, to_prov$prov_name)
  table2 <- dplyr::bind_cols(table2, prov_name = p_name)

  table2 <- table2 %>%
    dplyr::rename(poverty = pline, rural_urban = region, hhadulteq_size = hhequi, 
                  province_name = prov_name, hhweight = weights) %>%
    dplyr::mutate(popweight = hhsize * hhweight) %>%
    dplyr::select(hh_key, province, rural_urban, poverty, hhsize, hhadulteq_size,
                  hhweight, popweight,sex, age, province_name, hhweight) %>%
    dplyr::mutate(report = "2010 LCMS") %>%
    dplyr::tbl_df()

  tableData <- dplyr::bind_rows(merged_t1t3, table2)
  tableData <- tableData %>%
    dplyr::distinct()

  # There is a hh which is duplicated i.e. this hh_key = "81113309022013" household.
  # To get it do:
  # get_dup = duplicated(tableData$hh_key)
  # get_dup_hhkey <- tableData$hh_key[get_dup]
  # get_dup_rows = filter(tableData, hh_key == get_dup_hhkey)
  # The duplicated key issue was not resolved, as least at this point.
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")

  RSQLite::dbWriteTable(conn = writeToDB, name = "general_hh_data",
                        tableData, overwrite = T, row.names = F)

  dbDisconnect(writeToDB)

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  
}

df_gen_hh <- get_general_hh_data(lcms2015_file, lcms2010_file, 
                                 lcms2015pers_file, "CSO 2015 Poverty Analysis",
                                 "poverty household", "section1", 
                                 "allocate_dist_const_to_prov.csv")


### --- Create mega data for analysis

energypoverty_data <- function(dbInput, dbTable, cookInput, ligInput, appInput, entInput, gridInput){
  
  start.time <- Sys.time()
  
  clean_cooking <- unique(cookInput$hh_key)
  clean_lighting <- unique(ligInput$hh_key)
  app_ownership <- unique(appInput$hh_key)
  with_entertainment <- unique(entInput$hh_key)
  grid_access <- unique(gridInput$hh_key)
  
  tableData <- dplyr::tbl(dbInput, dbTable) %>% 
    dplyr::mutate(clean_cooking = ifelse(hh_key %in% clean_cooking,
                                             'Yes', 'No'),
                  clean_lighting = ifelse(hh_key %in% clean_lighting,
                                              'Yes', 'No'),
                  owns_appliance = ifelse(hh_key %in% app_ownership,
                                          'Yes', 'No'),
                  entertainment = ifelse(hh_key %in% with_entertainment,
                                             'Yes', 'No'),
                  grid_access = ifelse(hh_key %in% grid_access,
                                             'Yes', 'No')) %>% 
    dplyr::tbl_df()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")

  RSQLite::dbWriteTable(conn = writeToDB, name = "df_energypoverty",
                        tableData, append = T, row.names = F)

  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}


df_energypoverty <- energypoverty_data(ep_data_file, 'general_hh_data', df_cooking, 
                                       df_lighting, df_app, df_ent, df_grid_access)


# After creating 'df_energypoverty' data, remove these dataframes from the environment -- just to free up space (memory)
rm(df_energypoverty, df_gen_hh, df_cooking, df_lighting, df_app, df_ent, df_grid_access, sec7_data, sec8_data)

#######------------------------------------------------

## --- These are initial weights and cut-off used in the analyis

w_cook <- 0.4
w_light <- 0.2
w_home_app <- 0.2
w_entert <- 0.2
k_cutoff <- 0.3


## Further processing of data

ep_score <- function(dbInput, dbTable1, dbTable2){
  
  start.time <- Sys.time()
  
  ## write data to database, for efficient reasons
  writeToDB <- RSQLite::dbConnect(SQLite(), dbname = "energypoverty_data.db")
  
  ifelse(dbExistsTable(writeToDB, dbTable2) == T,
         dbRemoveTable(writeToDB, dbTable2), 
         'This is a new Table')
  
  dataVal <- tbl(dbInput, dbTable1) %>%
    dplyr::select(hh_key, report, province_name, clean_cooking, clean_lighting, 
                  owns_appliance, entertainment, hhweight, popweight) %>% 
    dplyr::mutate(cook_score = ifelse(clean_cooking == "Yes",
                                      0, w_cook),
                  light_score = ifelse(clean_lighting == "Yes",
                                       0, w_light),
                  home_app_score = ifelse(owns_appliance == "Yes",
                                          0, w_home_app),
                  entertainment_score = ifelse(entertainment == "Yes",
                                               0, w_entert),
                  c_total_score = cook_score + light_score +
                    home_app_score + entertainment_score,
                  is_energy_poor = ifelse(c_total_score <= k_cutoff,
                                          'No','Yes')) %>% 
    dplyr::tbl_df()
  
  RSQLite::dbWriteTable(conn = writeToDB, name = dbTable2,
                        dataVal, append = T, row.names = F)

  dbDisconnect(writeToDB)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
}

df_ep_score = ep_score(ep_data_file, "df_energypoverty", "df_ep_score")



##-----------------------

## To generate sensitivity data use 'sens_data_generation_energypovertyZambia.R' script.
## This script takes about 50 minutes to complete the generate the data and then write it to the database.

############################################################################3



