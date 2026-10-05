# Builds GDP_PPP_2021_ctry.csv: 2021 GDP at PPP by country, the target that GDP|PPP is calibrated to.
# Run from the repository root with the path to a GCAM 9.1 gcamdata checkout:
#   Rscript inst/extdata/mappings/common/prepare_GDP_PPP_2021_ctry.R <gcamdata path>
# World Bank where it has data; otherwise GCAM's own source, the SSP database (OECD ENV-Growth 2025).
library(magrittr)

gcamdata <- commandArgs(trailingOnly = TRUE)[1]
out_file <- "inst/extdata/mappings/common/GDP_PPP_2021_ctry.csv"

# US GDP deflator, 2021 relative to 2017 (BEA A191RD3A086NBEA, as in gcamdata gdp_deflator())
deflator_2017_to_2021 <- 110.213 / 100.000

# World Bank: GDP, PPP (constant 2021 international $)
wb <- jsonlite::fromJSON(paste0("https://api.worldbank.org/v2/country/all/indicator/NY.GDP.MKTP.PP.KD",
                                "?format=json&date=2021&per_page=400"))[[2]]
wb <- tibble::tibble(iso = tolower(wb$countryiso3code), value = wb$value / 1e9) %>%
  dplyr::filter(iso != "", !is.na(value)) %>%
  # gcamdata keeps the old ISO code for Romania
  dplyr::mutate(iso = dplyr::if_else(iso == "rou", "rom", iso))

# SSP database: GDP|PPP in billion USD 2017, every five years; 2021 interpolated between the nearest
# years with data (the last value where the series stops earlier)
ssp <- readr::read_csv(file.path(gcamdata, "inst/extdata/socioeconomics/SSP/SSP_database_2025.csv.gz"),
                       comment = "#", show_col_types = FALSE) %>%
  dplyr::filter(Scenario == "Historical Reference", Variable == "GDP|PPP", Unit == "billion USD_2017/yr") %>%
  tidyr::pivot_longer(dplyr::matches("^[0-9]{4}$"), names_to = "year", values_to = "gdp", values_drop_na = TRUE) %>%
  dplyr::group_by(ssp_country_name = Region) %>%
  dplyr::summarise(value = stats::approx(as.numeric(year), gdp, xout = 2021, rule = 2)$y * deflator_2017_to_2021,
                   .groups = "drop") %>%
  dplyr::inner_join(readr::read_csv(file.path(gcamdata, "inst/extdata/socioeconomics/SSP/iso_SSP_regID.csv"),
                                    comment = "#", show_col_types = FALSE) %>% dplyr::distinct(),
                    by = "ssp_country_name") %>%
  dplyr::select(iso, value)

regions <- readr::read_csv(file.path(gcamdata, "inst/extdata/common/iso_GCAM_regID.csv"), comment = "#", show_col_types = FALSE) %>%
  dplyr::left_join(readr::read_csv(file.path(gcamdata, "inst/extdata/common/GCAM_region_names.csv"), comment = "#",
                                   show_col_types = FALSE), by = "GCAM_region_ID") %>%
  dplyr::select(iso, country = country_name, GCAM_region = region)

ctry <- regions %>%
  dplyr::left_join(wb, by = "iso") %>%
  dplyr::left_join(dplyr::rename(ssp, ssp_value = value), by = "iso") %>%
  dplyr::mutate(source = dplyr::if_else(is.na(value), "SSP", "WB"),
                value = round(dplyr::coalesce(value, ssp_value), 3)) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::select(iso, country, GCAM_region, value, source) %>%
  dplyr::arrange(GCAM_region, iso)

header <- c(
  "# File: GDP_PPP_2021_ctry.csv",
  "# Title: GDP at PPP in 2021 by country, the target GDP|PPP is calibrated to",
  "# Units: billion USD 2021 (international $, PPP)",
  "# Source: WB = World Bank WDI NY.GDP.MKTP.PP.KD (constant 2021 international $), retrieved 2026-10-06;",
  "#   SSP = countries the World Bank has no 2021 value for (Venezuela, Taiwan, North Korea, Cuba, Yemen, ...):",
  "#   SSP_database_2025 (OECD ENV-Growth 2025, Historical Reference) GDP|PPP in USD 2017, 2021 interpolated",
  "#   between the nearest years with data, converted to USD 2021 with the US GDP deflator.",
  "#   GCAM_region: gcamdata GCAM 9.1 (iso_GCAM_regID, GCAM_region_names).",
  "# Built by: prepare_GDP_PPP_2021_ctry.R in this folder",
  "# ----------")
writeLines(header, out_file)
readr::write_csv(ctry, out_file, append = TRUE, col_names = TRUE)
cat(sprintf("%d countries: %d World Bank, %d SSP\n", nrow(ctry), sum(ctry$source == "WB"), sum(ctry$source == "SSP")))
