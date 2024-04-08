devtools::load_all()



generate_report(db_path ="examples",  # path to database under gcamreport folder
                db_name = "testdb",   # db folder name
                scenarios = c('Reference', 'NZ_Electricity_Nuc_Policy'), ## scenario names
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
