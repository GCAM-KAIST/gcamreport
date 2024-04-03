devtools::load_all()

library(gcamreport)
#GDP|PPP, unit = billion INT$2017/yr

#getwd()


available_regions(print =F)

generate_report(db_path ="E:/gcam-v7.0-Windows-Release-Package_GGS621/",
                db_name = "testdb",
                scenarios = c('Reference', 'NZ_Electricity_Nuc_Policy'), ## XML file from Ahmed
                prj_name = "GCAM-KAIST2.0.dat", final_year = 2100,
                desired_regions = c('South Korea'),
                desired_variables = c(
                  #'Agricultural*',
                  #'Emissions*',
                  # 'Fertilizer',
                  #'Primary Energy*',
                  #'Secondary Energy*',
                  #'Final Energy*',
                  'GDP|MER',
                  "GDP|PPP",
                  #'Land Cover*',
                  'Population'
                  #  'Forestry Demand',
                  #'Land Cover*'
                  #  'Value Added'
                ),
                save_output = TRUE, launch_ui = TRUE)


