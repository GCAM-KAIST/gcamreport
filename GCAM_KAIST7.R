
devtools::load_all()
library(tidyverse)



available_variables()


# develop




generate_report(db_path ="E:/gcam-v7.0-Windows-Release-Package_GGS621/",  # path to database under gcamreport folder
                db_name = "testdb",   # db folder name
                scenarios = c(
  #                'Reference'
                  'NZ_Electricity_Nuc_Policy'
                  #'nzM1_DACSSP2_Consv_Nuc_Extrapolation'
                  ), ## scenario names
                prj_name = "GCAM-KAIST7.dat", final_year = 2050,
                desired_regions = c('South Korea'#, 'Japan'
                                    ),
                #desired_variables = available_variables(), ## all available variable
               desired_variables = c(
                 "Capacity|Electricity*",
                 #"Capacity Additions*"
                  #'Agricultural*',
              #    'Emissions|CO2|Energy|Supply*'
                #  'Carbon Sequestration*',
                  # 'Fertilizer',
                  #'Primary Energy*'
                  'Secondary Energy*'
              #    'Final Energy*'
                 # 'GDP|MER',  # MER in $2010USD
                  #"GDP|PPP",  # PPP in $2017USD,
                  #"GDP|KRW",
                  #'Land Cover*',
                #  'Population'
                  #"Capacity|Electricity"
                  #'Price|Carbon'
                  #"Emissions|Kyoto Gases*"
                  #'Price*'
                  #'Final Energy*'
                  #  'Forestry Demand',
                  #'Land Cover*'
                  #  'Value Added'
                 ),
                save_output = TRUE, launch_ui = TRUE)




### GCAM-KAIST7 Netzero scenario


getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
  filter(!output %in% c("electricity", "elect_td_bld")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(
    vintage = as.integer(sub("year=", "", vintage)),
    output = gsub("elec_", "", output)
  ) %>%
  group_by(scenario, region, technology = output, vintage, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  bind_rows(getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
              filter(output %in% c("electricity", "elect_td_bld")) %>%
              separate(technology, into = c("technology", "vintage"), sep = ",") %>%
              mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
              group_by(scenario, region, technology, vintage, year) %>%
              summarise(value = sum(value, na.rm = T)) %>%
              ungroup()) %>%
  left_join(elec_cf, by = c("region", "technology", "vintage")) %>%
  mutate(EJ = value) %>%
  conv_EJ_GW() %>%
  mutate(gw = round(gw ,1)) %>% View()
  group_by(scenario, region, technology, year) %>%
  summarise(value = sum(gw, na.rm = T)) %>%
  ungroup() %>%
  mutate(value = round (value,1)) %>% View()
  #mutate(GW = round(value,1)) %>%
left_join(filter_variables(gcamreport::capacity_map, "elec_capacity_tot_clean") %>% select(-output), by = c("technology"), multiple = "all") %>%
  filter(!is.na(var)) %>%
  mutate(
    value = value * unit_conv,
    var = sub("Secondary Energy", "Capacity", var)
  ) %>%
  group_by(scenario, region, var, year) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  complete(nesting(scenario, region, year),
           var = unique(var),
           fill = list(value = 0)
  ) %>%
  select(all_of(gcamreport::long_columns))
)


View(elec_cf)

getwd()
library(here)

library(readxl)
library(tidyverse)



#write.csv(available_variables(), 'available_variables.csv')

KEMF2024_DB<-read_excel(here('./KMIP2024_DB_v2_js.xlsx'), sheet='Template')

KEMF2024_DB


KEMF2024_var_required<-KEMF2024_DB %>%
  distinct(tier, variable) %>%
  arrange(desc(tier)) %>%
  filter(tier =='Required') %>%
  dplyr:::select(variable) %>%
  mutate(variable = ifelse(variable =='GDP', 'GDP|KRW', variable)) %>%
  arrange(variable) %>%
  mutate(row_number = row_number()) %>%
  relocate(row_number, variable)

KEMF2024_var_required
gcamreport_var<-data.frame(variable = available_variables()) %>%
  arrange(variable)






## gcamreport has 473 variables to report
#gcamreport_var %>% View()

head(gcamreport_var)

### KMIP has requested 252 variables to be reported
KEMF2024_var_required



###  only 101 variables can be reported from gcamreport

inner_join(KEMF2024_var_required,gcamreport_var) ->KEMF_joined_var

KEMF_joined_var

### remaining  151
remaining_151<-anti_join(gcamreport_var, KEMF2024_var_required) %>%
  arrange(variable) %>%
  dplyr:::select(variable)





KMIP_gcamreport_variable_comparison<- left_join(KEMF2024_var_required, KEMF_joined_var, by ='row_number') %>%
  rename(KMIP_variable = variable.x, gcamreport_variable = variable.y) %>%
  mutate(gcamreport_variable = ifelse(is.na(gcamreport_variable), "NOT AVAILABLE", 'AVAILABLE')) %>%
  arrange(KMIP_variable)

library(qpcR)
a<-qpcR:::cbind.na(KMIP_gcamreport_variable_comparison, remaining_151) %>%
  rename(candidate_variable = variable)
write_xlsx(a, "KMIP_gcamreport_with_anti_join.xlsx")












##101개
sort(KEMF_joined_var)



### anti_join 157
anti_join(KEMF2024_var_required,gcamreport_var) %>%
  arrange(variable)




desired_variables()

library(tidyverse)

setwd("E:/gcam-v7.0-Windows-Release-Package_GGS621") ### set to your path where gcamreport results are saved
gcamreport_result<- read.csv('E:/gcam-v7.0-Windows-Release-Package_GGS621/GCAM-KAIST7_standardized.csv') %>% ### set to your path where gcamreport results are saved
  filter(Region =='South Korea') %>%
  rename_with("tolower") %>%
  select(-model, -region) %>%
  pivot_longer(-c('scenario', 'variable','unit'), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(gsub('x', '', year))) %>%
  mutate(value = ifelse(unit =='EJ/yr', value*gcamreport::convert$EJ_to_Mtoe, value),
         unit = ifelse(unit =='EJ/yr', 'Mtoe', unit)) %>%
  mutate(type ='gcamreport') %>%
  select(-scenario)


gcamreport_NZ_var<-gcamreport_result %>%
  distinct(variable)


gcamreport_NZ_var %>% arrange(variable) %>% View()



anti_join(KEMF2024_var_required,gcamreport_NZ_var)



unique(gcamreport_result$unit)

options("scipen"=100, "digits"=2)
options(digits=2)
gcamreport_result %>% View()


#### KMIP and gcamreport_result comparison



unique(gcamreport_result$variable)


getwd()

library(readxl)
library(tidyverse)
setwd("E:/gcamreport_jm/gcamreport")
KEMF_2035NDC_assumption <-read_excel(here('./KEMF_2035NDC_assumption.xlsx')) %>%
  dplyr::select(-name) %>%
  pivot_longer(-c('unit','variable'), names_to='year', values_to='value') %>%
  mutate(type ='KEMF',
         year = as.numeric(year)) %>%
  mutate(value =ifelse(variable =='Population', value/1000, value),
         unit = ifelse(variable =='Population', 'million', unit))


gcamreport_KEMF_comparison<-rbind(gcamreport_result,KEMF_2035NDC_assumption)

gcamreport_KEMF_comparison%>%
  filter(variable =='Population') %>%
  ggplot(aes(x = year, y = value, group = type,color = type))+
  geom_line()+
  labs(title = "Population comparison between NZ GCAM-KAIST result(gcamreport) and 2035NDC assumption")



gcamreport_KEMF_comparison%>%
  filter(str_detect(variable, 'Final Energy')) %>%  #############
  ggplot(aes(x = year, y = value, group = type, color = type))+
  geom_line()+
  facet_wrap(~variable)+
  labs(title = "Final energy comparison between NZ GCAM-KAIST result(gcamreport) and 2035NDC assumption")


KEMF2024_var_required

left_join(KEMF2024_var_required, gcamreport_result, by = 'variable') %>% View()


K_EMF_2023_NZ_masterfile<- read_excel('E:/K-EMF KMIP_2023_ MasterFile.xlsx', sheet='Results') %>%
  rename_with("tolower") %>%
  select(-model, -region) %>%
  filter(scenario =='NZ') %>%
  select(scenario, variable, unit, `2020`,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`)


names(K_EMF_2023_NZ_masterfile)

K_EMF_2023_NZ_masterfile

K_EMF_2023_NZ_masterfile %>%
  select(scenario, variable)








### socioeconomic

generate_report(db_path ="E:/gcam-v7.0-Windows-Release-Package_GGS621/output",  # path to database under gcamreport folder
                db_name = "socioeconomics_35NDC",   # db folder name
                scenarios = c(
                  'socioeconomics_35NDC',
                  'socioeconomics_35NDC_2'), ## scenario names
                prj_name = "socioeconomic_35NDC.dat", final_year = 2050,
                desired_regions = c('South Korea'),
                #  desired_variables = available_variables(), ## all available variable
                desired_variables = c(
                  #'Agricultural*',
                  # 'Emissions|CO2|Energy|Supply*'
                  #  'Carbon Sequestration*',
                  # 'Fertilizer',
                  #'Primary Energy*'
                  #'Secondary Energy*',
                  #'Final Energy*'
                  'GDP|MER',  # MER in $2010USD
                  "GDP|PPP",  # PPP in $2017USD,
                  "GDP|KRW",
                  "Population"
                  #'Land Cover*',
                  # 'Population',
                  #"Capacity|Electricity"
                  #'Price|Carbon'
                  #"Emissions|Kyoto Gases*"
                  #'Price*'
                  #'Final Energy*'
                  #  'Forestry Demand',
                  #'Land Cover*'
                  #  'Value Added'
                ),
                save_output = TRUE, launch_ui = TRUE)


setwd("E:/gcam-v7.0-Windows-Release-Package_GGS621/output")

socioeconomic<- read.csv('socioeconomic_35NDC_standardized.csv') %>%
  filter(Region =='South Korea') %>%
  rename_with("tolower") %>%
  select(-model, -region) %>%
#  filter(variable =='GDP|KRW') %>%
  pivot_longer(-c('scenario', 'variable','unit'), names_to = 'year', values_to = 'value') %>%
  rename(type= scenario) %>%
  mutate(year = as.numeric(gsub('x','', year)))

unique(socioeconomic$variable)


setwd("E:/gcamreport_KMIP2024/gcamreport")

KEMF_2035NDC_assumption <-read_excel('KEMF_2035NDC_assumption.xlsx') %>%
  dplyr::select(-name) %>%
  pivot_longer(-c('unit','variable'), names_to='year', values_to='value') %>%
  mutate(type ='KEMF_2035NDC_assumption',
         year = as.numeric(year)) %>%
  mutate(value =ifelse(variable =='Population', value/1000, value),
         unit = ifelse(variable =='Population', 'million', unit))


KEMF_2035NDC_assumption

socioeconomic_comparison<-rbind(socioeconomic,KEMF_2035NDC_assumption)


str(socioeconomic_comparison)

library(scales)
socioeconomic_comparison %>%
  filter(variable =='GDP|KRW') %>%
  ggplot(aes(x = year, y = value, group = type,color = type))+
  geom_line(alpha = .8, linewidth = 1.2)+
  scale_y_continuous(label = comma, limits = c(0, 3500))+
  labs(title = str_wrap("GDP differences"),
       #subtitle ='',
       y = 'Trillion KRW$2020/yr')


socioeconomic_comparison %>%
  filter(variable =='GDP|KRW') %>%
  pivot_wider(names_from = year, values_from = value)



socioeconomic_comparison %>%
  filter(variable =='Population') %>%
  ggplot(aes(x = year, y = value, group = type, color = type))+
  geom_line(alpha = .8, linewidth = 1.2)+
  facet_wrap(~type)+
  geom_text(aes(label = round(value, 1),vjust= -1))+
 scale_y_continuous(limits = c(0, 55))+
  labs(title = str_wrap("Population"),
       y = 'Million')



