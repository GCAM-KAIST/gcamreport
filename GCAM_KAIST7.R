devtools::load_all()





# available variable
available_variables()


gcamreport::template %>%
  filter(Variable =="GDP|PPP")



template %>%
  filter(Variable =="GDP|PPP")

# develop

generate_report(db_path ="E:/gcam-v7.0-Windows-Release-Package_GGS621/",  # path to database under gcamreport folder
                db_name = "testdb",   # db folder name
                scenarios = c('Reference', 'NZ_Electricity_Nuc_Policy'), ## scenario names
                prj_name = "GCAM-KAIST7.dat", final_year = 2100,
                desired_regions = c('South Korea', 'Japan'),
                desired_variables = c(
                  #'Agricultural*',
                  #'Emissions*',
                  # 'Fertilizer',
                  #'Primary Energy*',
                  #'Secondary Energy*',
                  #'Final Energy*',
                  'GDP|MER',  # MER in $2010USD
                  "GDP|PPP",  # PPP in $2017USD,
                  "GDP|KRW",
                  #'Land Cover*',
                  'Population'
                  #  'Forestry Demand',
                  #'Land Cover*'
                  #  'Value Added'
                ),
                save_output = TRUE, launch_ui = TRUE)

