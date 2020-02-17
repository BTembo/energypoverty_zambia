
library(stringr)
library(plyr)
library(dplyr)
library(RSQLite)
library(ggplot2)
library(reshape2)
library(Rmisc)
library(ggrepel)
library(scales)

### Connect to database
ep_data_file <- dplyr::src_sqlite("energypoverty_data.db", create = FALSE) 
dplyr::src_tbls(ep_data_file) # List the tables in the database

#### General colour code

## Report colours

report_colours <- c("2015 LCMS" = "#F8766D", "2010 LCMS" = "#00BFC4", 
                    "2006 LCMS" = "#B79F00", "2002-3 LCMS" = "#619CFF",
                    "1998 LCMS" = "#00BA38",  "1996 LCMS" = "#F564E3")

## Cooking fuels colours

fuel_cook <- c("Charcoal" = "green", "Electricity" = "blue", 
               "Firewood" = "forestgreen", "Gas" = "cornflowerblue", "Other" = "gold")

## Lighting fuels colours

fuel_light <- c("Electricity" = "blue", "Kerosene" = "darkmagenta",
                "None" = "black", "Other" = "gold", "Solar" = "tomato")

## Poverty categorisation colours

poverty_colour <- c("Extremely Poor" = "burlywood2",
                    "Moderately Poor" = "turquoise2",
                    "Non-Poor" = "skyblue3")


## --------------------- Plots --------------------- ##

## Energy use patterns from Section 8 of LCMS dataset

sec8_fuels <- tbl_df(read.csv('sec8_fuels.csv', stringsAsFactors = F))
cook_fuels_code <- sec8_fuels %>% filter(!is.na(fuel_cooking))
light_fuels_code <- sec8_fuels %>% filter(!is.na(fuel_lighting))

## remember to include and account for 'weights' in the analysis
tbl_location <- dplyr::tbl(ep_data_file, "df_energypoverty") %>% 
  dplyr::filter(!is.na(rural_urban)) %>% 
  dplyr::select(hh_key, province_name, rural_urban, report, hhweight, popweight)


#### General report plots

## Cooking fuels plot -- sec8 q14 only

tbl_cook_fuels <- dplyr::tbl(ep_data_file, "section8_combined") %>% 
  dplyr::select(hh_key, sec8q14, report)

cooking_fuels <- dplyr::full_join(tbl_df(tbl_location), tbl_df(tbl_cook_fuels), by = c('hh_key', 'report'))

fuel_cook_vars <- plyr::mapvalues(cooking_fuels$sec8q14, 
                                  cook_fuels_code$fuel_cooking,
                                  cook_fuels_code$fuel)

### Figure 1: Cooking fuel usage patterns
df_cooking_fuels <- dplyr::bind_cols(cooking_fuels, fuel_name = fuel_cook_vars) %>% 
  dplyr::mutate(location = ifelse(rural_urban == 1, 'Rural', 'Urban'),
                fuel_name = ifelse(!fuel_name %in% unique(cook_fuels_code$fuel),
                                   'Other', fuel_name)) %>% 
  dplyr::filter(!is.na(location), report == '2015 LCMS') %>%
  dplyr::group_by(report, location, fuel_name) %>%
  dplyr::summarise(obs = n(), tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop = 100*tot_hhweight/sum(tot_hhweight)) %>%
  ggplot(aes(location, prop, fill = fuel_name)) +
  geom_bar(stat = "identity",
           position="dodge",
           width = 0.8) +
  scale_fill_manual(values = fuel_cook) +
  # energy_poverty_style1() + ### the is my customerised style template
  labs(x = "", y = 'Percentage (%)') +
  labs(fill="") + guides(colour = FALSE)

df_cooking_fuels


## lighting fuel plot -- sec8 q13 only

tbl_light_fuels <- dplyr::tbl(ep_data_file, "section8_combined") %>% 
  dplyr::select(hh_key, sec8q13, report)

lighting_fuels <- dplyr::full_join(tbl_df(tbl_location), tbl_df(tbl_light_fuels), by = c('hh_key', 'report'))

fuel_light_vars <- plyr::mapvalues(lighting_fuels$sec8q13, 
                                   light_fuels_code$fuel_lighting,
                                   light_fuels_code$fuel)

### Figure 2: Lighting usage patterns
df_lighting_fuels <- dplyr::bind_cols(cooking_fuels, fuel_name = fuel_light_vars) %>% 
  dplyr::mutate(location = ifelse(rural_urban == 1, 'Rural', 'Urban'),
                fuel_name = ifelse(!fuel_name %in% unique(light_fuels_code$fuel),
                                   'Other', fuel_name)) %>% 
  dplyr::filter(!is.na(location), report == '2015 LCMS') %>%
  dplyr::group_by(report, location, fuel_name) %>%
  dplyr::summarise(obs = n(), tot_hhweight = sum(hhweight, na.rm = T)) %>%
  dplyr::mutate(prop = 100*tot_hhweight/sum(tot_hhweight)) %>%
  ggplot(aes(location, prop, fill = fuel_name)) +
  geom_bar(stat = "identity",
           position="dodge",
           width = 0.8) +
  scale_fill_manual(values = fuel_light) +
  # energy_poverty_style1() +  ### the is my customerised style template
  labs(x = "", y = 'Percentage (%)') +
  labs(fill="") + guides(colour = FALSE)

df_lighting_fuels


### Results Plots

### Figure 4: 2010 and 2015 Headcount ratio vs. intensity of energy poverty at provincial level

# Headcount ratio vs Intensity Provincial level 
hc_int_prov <- ep_index_pro %>% 
  ggplot(aes(intensity2, incidence, label = province_name)) + 
  geom_point(aes(colour = report)) +
  # energy_poverty_style1() +  ### the is my customerised style template
  # geom_text(check_overlap = TRUE,vjust = 0, nudge_y = 0.002) +
  # geom_label(vjust = 0, nudge_y = 0.002,aes(fill = report), colour = "white", fontface = "bold") +
  ggrepel::geom_label_repel(aes(#colour = report,
    fill = report), #shape = report,
    colour = 'white', 
    fontface = "bold",
    family = "LM Roman 10", #color = 'white',
    size = 3.5) +
  scale_fill_manual(values = report_colours) +
  scale_colour_manual(values = report_colours) +
  labs(x = "Intensity", y = "Headcount ratio") +
  # labs(fill="") +
  labs(colour="") +
  # guides(colour = FALSE) +
  guides(fill = FALSE)


hc_int_prov



### Sensitivity plots


sens_ratios <- function(dbInput, dbTable){
  
  dataVal <- tbl(dbInput, dbTable)
  
  mepi_ratios <- dataVal %>% 
    dplyr::select(report, series, mepi, ep_status) %>% 
    dplyr::tbl_df() %>% 
    reshape2::dcast(report + series ~ ep_status,
                    value.var = "mepi") %>%
    dplyr::mutate(fweight_fcut_vs_rweight_fcut = is_energy_poor_fw/is_energy_poor,
                  rweight_fcut_vs_rweight_rcut = is_energy_poor_rw/is_energy_poor_fw,
                  variable_name = "MEPI") %>%
    dplyr::select(report, variable_name, fweight_fcut_vs_rweight_fcut, rweight_fcut_vs_rweight_rcut) %>%
    reshape2::melt(id.vars = c("report", "variable_name"),
                   variable.name = "ratio_variable",
                   value.name = "ratio_value")
  
  incidence_ratios <- dataVal %>%
    dplyr::select(report, series, incidence, ep_status) %>%
    dplyr::tbl_df() %>%
    reshape2::dcast(report + series ~ ep_status,
                    value.var = "incidence") %>%
    dplyr::mutate(fweight_fcut_vs_rweight_fcut = is_energy_poor_fw/is_energy_poor,
                  rweight_fcut_vs_rweight_rcut = is_energy_poor_rw/is_energy_poor_fw,
                  variable_name = "Incidence") %>%
    dplyr::select(report, variable_name, fweight_fcut_vs_rweight_fcut, rweight_fcut_vs_rweight_rcut) %>%
    reshape2::melt(id.vars = c("report", "variable_name"),
                   variable.name = "ratio_variable",
                   value.name = "ratio_value")

  intensity_ratios <- dataVal %>%
    dplyr::select(report, series, intensity, ep_status) %>%
    dplyr::tbl_df() %>%
    reshape2::dcast(report + series ~ ep_status,
                    value.var = "intensity") %>%
    dplyr::mutate(fweight_fcut_vs_rweight_fcut = is_energy_poor_fw/is_energy_poor,
                  rweight_fcut_vs_rweight_rcut = is_energy_poor_rw/is_energy_poor_fw,
                  variable_name = "Intensity") %>%
    dplyr::select(report, variable_name, fweight_fcut_vs_rweight_fcut, rweight_fcut_vs_rweight_rcut) %>%
    reshape2::melt(id.vars = c("report", "variable_name"),
                   variable.name = "ratio_variable",
                   value.name = "ratio_value")

  dataVal <- dplyr::bind_rows(incidence_ratios,
                              intensity_ratios,
                              mepi_ratios) %>%
    dplyr::mutate(ratio_descr1 = ifelse(ratio_variable == "fweight_fcut_vs_rweight_fcut",
                                        'Varying weights',
                                        ifelse(ratio_variable == "rweight_fcut_vs_rweight_rcut",
                                               'Varying cut-off',
                                               'Not sure what is going one')),
                  ratio_descr2 = ifelse(ratio_variable == "fweight_fcut_vs_rweight_fcut",
                                        'Flat weight and cut-off vs Random weight and flat cut-off',
                                        ifelse(ratio_variable == "rweight_fcut_vs_rweight_rcut",
                                               'Random weight and flat cut-off vs Random weight and cut-off',
                                               'Not sure what is going one'))) %>%
    dplyr::tbl_df()
  
}


df_sens_ratios <- sens_ratios(ep_data_file_test, "df_mepi_sens")
head(df_sens_ratios)


### Figure 5: Relative ratios of the impacts of varying the cut-off and weights
sens_ratios <- df_sens_ratios %>%
  ggplot(aes(x = ratio_value)) + 
  # geom_density(aes(colour = ratio_variable)) +
  # geom_density(aes(colour = ratio_descr1)) +
  geom_density(aes(linetype = ratio_descr1)) +
  facet_grid( ~ variable_name) +
  labs(x = "Relative ratios", y = "Density") +
  # energy_poverty_style1() +  ### the is my customerised style template
  labs(fill="") +
  labs(colour="") +
  labs(linetype ="") +
  # guides(colour = FALSE) +
  guides(fill = FALSE)

sens_ratios






