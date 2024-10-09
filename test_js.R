


KOREA_NZ2050<- as_tibble(getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)"))


head(KOREA_NZ2050)
names(KOREA_NZ2050$sector)

View(co2_sector_map)
names(co2_sector_map)

### paths
rawDataFolder <- here::here()


############# CO2 EMISSION
kyotogas_js<- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "nonCO2_emissions_sector_map.csv"),
                        skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()


head(kyotogas_js)

View(kyotogas_js)


co2_js<-left_join(KOREA_NZ2050, co2_sector_map, by = 'sector') %>%
  mutate(value = value * unit_conv) %>%
  group_by(scenario, region, year, var) %>% #
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup()

# from gcamreport function


co2_js %>%
  group_by(year, var) %>%
  summarise(value = sum(value)) %>%
  filter(year == 2020) %>% View()


co2_js %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line()

co2_js %>%
  group_by(year, var) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_col()+
  scale_x_continuous(limits = c(1970, 2055), breaks = seq(1970, 2050, 5))

co2_js %>%
  ggplot(aes( x= year, y = value, group = var, fill = var))+
  geom_col()

co2_clean

KOREA_NZ2050 %>%
  ggplot(aes(x = year, y = sector, fill = sector, group = sector))+
  geom_area()



 ############################### non co2
KOR_NONCO2<-getQuery(prj, "nonCO2 emissions by sector (excluding resource production)")


KOR_NONCO2

View(LU_carbon_clean)

View(ghg_sector_clean)


unique(ghg_sector_clean$var)


a<-ghg_sector_clean %>%
  filter(var =='Emissions|Kyoto Gases') %>%
  ggplot(aes(x = year, y = value))+
  geom_col()+
  scale_x_continuous(limits = c(1970, 2055), breaks = seq(1970, 2050, 5))+
  scale_y_continuous(limits = c(-300, 1300))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


a
b<-ghg_sector_clean %>%
  filter(var !='Emissions|Kyoto Gases') %>%
  #filter(var %in% c('Emissions|Kyoto Gases|Industry',
  #                  'Emissions|Kyoto Gases|Buildings',
  #                  'Emissions|Kyoto Gases|Transportation',
  #                  'Emissions|Kyoto Gases|Other',
  #                  'Emissions|Kyoto Gases|Electricity',
  #                  'Emissions|Kyoto Gases|AFOLU')) %>%
  ggplot(aes(x = year, y = value, fill = var))+
  geom_col()+
  scale_x_continuous(limits = c(1970, 2055), breaks = seq(1970, 2050, 5))+
  scale_y_continuous(limits = c(-300, 1300))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

a+b



############# FINAL ENERGY = SECTOR + TRANSFORMATION



############# FINAL ENERGY "SECTOR"


KOR_fe_sector<-
  getQuery(prj, "final energy consumption by sector and fuel") %>%
  left_join(filter_variables(gcamreport::final_energy_map, "fe_sector"), by = c("sector", "input"), multiple = "all") %>%
  filter(!is.na(var)) %>%View() #######################
  mutate(value = value * unit_conv) %>%
  group_by(scenario, region, year, var) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  complete(nesting(scenario, region, year),
           var = unique(var),
           fill = list(value = 0)
  ) %>%
  select(all_of(gcamreport::long_columns)) %>%
  select(-scenario) %>%
  filter(year>=2020)


unique(KOR_fe_transportation$var)


KOR_fe_sector %>%
  ggplot(aes(x = year, y = value, group = var, color = var))+
  geom_line()



############# FINAL ENERGY "Transportation"


KOR_fe_transportation<-
  getQuery(prj, "transport final energy by mode and fuel") %>%
  left_join(filter_variables(gcamreport::transport_final_en_map, "fe_transportation"), by = c("sector", "input", "mode"), multiple = "all") %>%
  filter(!is.na(var)) %>% View() ##############################
  mutate(value = value * unit_conv) %>%
  group_by(scenario, region, year, var) %>%
  summarise(value = sum(value,
                        na.rm = T
  )) %>%
  ungroup() %>%
  complete(nesting(scenario, region, year),
           var = unique(var),
           fill = list(value = 0)
  ) %>%
  select(-scenario) %>%
  filter(year>=2020)


KOR_fe_transportation

unique(KOR_fe_transportation$var)

unique(KOR_fe_transportation$var)[c(2,3,4)]->fe_level1


KOR_fe_transportation %>%
  ggplot()+
  geom_line(data =. %>% filter(var == 'Final Energy'), aes( x =year, y = value), size = 2)+
  geom_area(data =. %>% filter(var %in% fe_level1), aes(x = year , y = value, group = var, fill = var))

### transportation

KOR_fe_transportation %>%
  ggplot(aes(x = year, y = value, group = var, color = var))+
  geom_line()


######### FINAL ENERGY TOTAL


KOR_fe_total <-
  bind_rows(KOR_fe_sector, KOR_fe_transportation) %>%
  group_by(region, var, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()


KOR_fe_total



############ ##################3333

setwd("E:/gcam-v7.0-Windows-Release-Package_GGS621")
library(tidyverse)
library(patchwork)

library(readxl)
gcamreport_result_KOR_CO2<- read_excel("E:/gcam-v7.0-Windows-Release-Package_GGS621/GCAM-KAIST7_standardized.xlsx") %>%
  rename_with("tolower") %>%
  filter(region == 'South Korea') %>%
  filter(grepl('Emissions\\|CO2', variable)) %>%   ## |를 문자 그대로 처리하기 위해선 앞에 \\ 붙여야 함
  filter(grepl('Emissions\\|CO2|Carbon Sequestration', variable)) %>%
  mutate(count = str_count(variable, '\\|')) %>%
  select(-model,-scenario,-region,) %>%
  pivot_longer(-c(variable, count,unit), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(year)) %>%
  filter(variable != "Emissions|CO2|Energy and Industrial Processes")


##3 kyoto gas

gcamreport_result_KOR_Kyoto<- read_excel("E:/gcam-v7.0-Windows-Release-Package_GGS621/GCAM-KAIST7_standardized.xlsx") %>%
  rename_with("tolower") %>%
  filter(region == 'South Korea') %>%
  filter(grepl('Kyoto', variable)) %>%
  select(-model,-scenario,-region,) %>%
  pivot_longer(-c(variable, unit), names_to = 'year', values_to = 'value') %>%
  mutate(year = as.numeric(year))

unique(gcamreport_result_KOR_Kyoto$variable)

gcamreport_result_KOR_Kyoto %>%
  filter(variable== 'Emissions|Kyoto Gases') %>%
  ggplot(aes(x = year, y = value, group = variable, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
               geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2-equiv/yr')+
gcamreport_result_KOR_Kyoto %>%
  filter(variable!= 'Emissions|Kyoto Gases') %>%
  filter(variable != "Emissions|Kyoto Gases|Industry") %>%
  ggplot(aes(x = year, y = value, group = variable, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2-equiv/yr')



sort(unique(gcamreport_result_KOR_Kyoto$variable))

gcamreport_result_KOR_Kyoto %>%
  filter(variable == "Emissions|Kyoto Gases|Industry") %>%
  ggplot(aes(x = year, y = value, group = variable, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
               geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2-equiv/yr')+
  gcamreport_result_KOR_Kyoto %>%
  filter(variable %in% c( "Emissions|Kyoto Gases|Industry|Cement",
                          "Emissions|Kyoto Gases|Industry|Chemicals",
                          "Emissions|Kyoto Gases|Industry|Other Industry",
                          "Emissions|Kyoto Gases|Industry|Steel")) %>%
  ggplot(aes(x = year, y = value, group = variable, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2-equiv/yr')
gcamreport_result_KOR_CO2

unique(gcamreport_result_KOR_CO2$variable)[c(24,25)]->international

unique(gcamreport_result_KOR_CO2$variable)[c(2,3,23)]->level2



gcamreport_result_KOR_CO2 %>%
  ggplot(aes(x = year, y = value, group = interaction(variable , count)))+
  geom_area()+
  facet_wrap(~count)


count1<-gcamreport_result_KOR_CO2 %>%
  filter(count == 1) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')

count2<-gcamreport_result_KOR_CO2 %>%
  filter(variable %in% level2) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')

count3<-gcamreport_result_KOR_CO2 %>%
  filter(count == 3) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')+
  geom_text(aes(label = round(value, 1)), position = position_stack(vjust = .5))

count3

count4<-gcamreport_result_KOR_CO2 %>%
  filter(count == 4) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')

count5<-gcamreport_result_KOR_CO2 %>%
  filter(count == 5) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')

(count1+count2+plot_spacer())/(count3+count4+count5)

energy_sector<-gcamreport_result_KOR_CO2 %>%
  filter(variable =="Emissions|CO2|Energy") %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')

energy_sector+count3


gcamreport_result_KOR_CO2 %>%
  filter(variable =="Emissions|CO2|Energy|Demand") %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')+
gcamreport_result_KOR_CO2 %>%
  filter(variable %in% c(
    "Emissions|CO2|Energy|Demand|AFOFI",
  "Emissions|CO2|Energy|Demand|Industry",
   "Emissions|CO2|Energy|Demand|Residential and Commercial",
   "Emissions|CO2|Energy|Demand|Transportation"
  )) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')


unique(gcamreport_result_KOR_CO2$variable)

gcamreport_result_KOR_CO2 %>%
  filter(variable =="Emissions|CO2|Energy|Supply") %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')+
gcamreport_result_KOR_CO2 %>%
  filter(variable %in% c(
 "Emissions|CO2|Energy|Supply|Electricity",
 "Emissions|CO2|Energy|Supply|Gases",
 "Emissions|CO2|Energy|Supply|Liquids",
 "Emissions|CO2|Energy|Supply|Solids",
 "Emissions|CO2|Energy|Supply|Heat" ### heat is missing??
  )) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr',
       title = "Emissions|CO2|Energy|Supply|Heat is missing?" )

gcamreport_result_KOR_CO2 %>%
  filter(variable %in% c(
    "Carbon Sequestration|CCS|Biomass|Energy|Supply"
  )) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')+
gcamreport_result_KOR_CO2 %>%
  filter(variable %in% c(
    "Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity",
    "Carbon Sequestration|CCS|Biomass|Energy|Supply|Hydrogen",
    "Carbon Sequestration|CCS|Biomass|Energy|Supply|Liquids"
  )) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x=element_text(angle =90))+  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
                                                            geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
  labs(y = 'Mt CO2/yr')




## gcamreport_result_KOR_CO2 %>%
gcamreport_result_KOR_CO2%>%
  filter(variable =="Emissions|CO2|AFOLU")


unique(gcamreport_result_KOR_CO2$variable)

gcamreport_result_KOR_CO2 %>%
  filter(count == 1) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x = element_text(angle=90))+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
               geom = "text",  vjust = -1, family ='Nanum Myeongjo')+
gcamreport_result_KOR_CO2 %>%
  filter(!variable %in% c("Emissions|CO2International|InternationalShipping"
                          ,"Emissions|CO2International|InternationalAviation")) %>%
  filter(count == 2) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x = element_text(angle=90))+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
               geom = "text",  vjust = -1, family ='Nanum Myeongjo')



gcamreport_result_KOR_CO2 %>%
  filter(variable %in%international) %>%
  ggplot(aes(x = year, y = value, fill = variable))+
  geom_col()+
  scale_x_continuous(breaks =seq(2005, 2050, 5))+
  theme(axis.text.x = element_text(angle=90))+
  stat_summary(fun = sum, aes(label = round(..y.., 1), group = year),
               geom = "text",  vjust = -1, family ='Nanum Myeongjo')




gcamreport_international<-gcamreport_result_KOR_CO2 %>%
  filter(variable %in%international) %>%
  filter(year %in% c(2005,2010,2015,2020)) %>%
  mutate(source ='(NZ2050)-CO2',
         variable = paste0(variable, source))

########## GIR


 setwd("E:/gcamreport_jm/gcamreport/inst/extdata")

GIR_international<- read_excel("★국가 온실가스 인벤토리(1990_2021).xlsx", sheet='온실가스',
                               range = 'B145:AH149') %>%
  pivot_longer(-`분야·부문/연도`, names_to = 'year', values_to = 'value') %>%
  filter(`분야·부문/연도` %in% c('a. 국제 항공', 'b. 국제 해운')) %>%
  rename(variable= `분야·부문/연도`) %>%
#  filter(year %in% c(2005,2010,2015,2020)) %>%
  mutate(value = value/1000,
         year = as.numeric(year),
         source = '(GIR)-CO2eq',
         variable = paste0(variable, source))

GIR_GHG<- read_excel("★국가 온실가스 인벤토리(1990_2021).xlsx", sheet='온실가스',
                               range = 'B4:AH142') %>%
  pivot_longer(-`분야·부문/연도`, names_to = 'year', values_to = 'value') %>%
  filter(`분야·부문/연도` %in% c('에너지', '산업공정','농업','LULUCF','폐기물')) %>%
  rename(variable= `분야·부문/연도`) %>%
  #  filter(year %in% c(2005,2010,2015,2020)) %>%
  mutate(value = value/1000,
         year = as.numeric(year),
         source = '(GIR)-CO2eq',
         variable = paste0(variable, source))




ggplot()+
  geom_line(data =GIR_international,aes(x = year, y = value, color = variable))+
  geom_point(data =gcamreport_international, aes( x = year, y = value, color = variable))+
  scale_color_manual(values = c('red','blue', 'red', 'blue'))



ggplot()+
  geom_line(data =GIR_GHG,aes(x = year, y = value, color = variable))+
  geom_point(data =gcamreport_result_KOR_Kyoto, aes( x = year, y = value, color = variable))

