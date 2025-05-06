## Clean TB - 2007-2014 - Karamoja notifctions and Treatment outcomes 2007 - 2014 (Q!)

##### load helpers #####
library(tidyverse)
library(readxl)

filedir <- ("data-raw/tb/20072014/")


#### Notes on Data #####
# This data file is spread over multiple sheets, all named 'Karamoja-<year>'
# There are multiple sources of information in these data, located in different 
# locations in the .xls file. There are, by quarter and by region (also for total Karamoja region)

# (1) notifications by diagnosis and history and sex
# - 2007-2008 have 2 less categories than the other years
# - 2014 has more/different categories than the other years
# (2) notifications by diagnosis (summed, NOT READ IN)
# (3) notifications by year (summed) (IGNORED)
# (4) HIV+ male notifications / treatment outcomes (IGNORED FOR NOW)
# (5) treatment outcomes by TB history/diagnosis
# - 2014 has 1 extra history: LTFU, and different naming conventions 
# HOWEVER:ALL CELLS ARE MISSING DATA, so ignore.
# (6) population size by year NOT ALWAYS INTEGER. ROUNDING FOR CLEANUP

# The case notifications beyond 2014Q1 are missing, 
# as are the Treatment outcomes for 2013-2014

## The cleaned outcome data will be 
# (1) TB_20072014_Karamoja-popsize.csv
# (2) TB_20072014_Karamoja-notifications.csv
# (3) TB_20072014_Karamoja-outcomes.csv

#### Read in Data ####

df <- paste0(filedir, "Karamoja notifctions and Treatment outcomes 2007 - 2014 (Q!).xls")
sheets <- excel_sheets(path = df)

popsize_cols <- c("Z", "Z", "AQ", "AS", "AS", "AS", "AR", "AU")

## Placeholder Dataframe ##
## For case notifications by history by sex

tmp_dts <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                     c("region", "year", "quarter"))

popsize <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                    c("region", "year", "popsize"))

cn_hist_sex <- cbind(
  tmp_dts, 
  readxl::read_xls(df,
                   sheet = sheets[3], # sheet 3, because sheet 1-2 don't have 'Others'
                   range = "C8:X8")) 

cn_hist_sex_14 <- cbind(
  tmp_dts, 
  readxl::read_xls(df,
                   sheet = sheets[8],
                   range = "C8:Z8"))  

outnames <- paste0(rep(c("Cured", "Completed", "Died", "Failure", "Defaulted" ,"Transferred"),
                       each = 2),
                   rep(c("_n", "_pct"), 6))

out_hist <- cbind(
  tmp_dts,
  setNames(data.frame(matrix(ncol = 13, nrow = 0)), 
           c("tbhistory", outnames))
)

for(i in 1:length(sheets)){
  # Extract row number to determine number of districts to include  
  df_tmp <- suppressMessages(readxl::read_xls(df, sheet = sheets[i]))
  rows <- nrow(df_tmp)
  
  nregion <- round((rows/60) -1 ) # all Sheets are organized by 60 rows per district
  if(i == 8) nregion <- round((rows/65) -1 ) # 2014 (sheet 8) data has 5 extra rows per district
  
  tmp_year <- as.numeric(substr(sheets[i], 1, 4))
  
  for(r in 1:nregion){
    # extracting the cell ranges looping over the 60 rows per region
    tmp_poploc <- paste0(popsize_cols[i], 9 + 60*(r-1)) 
    tmp_loc <- paste0("B", 9 + 60*(r-1))
    tmp_range_cn <- paste0("B", 10 + (60*(r-1)), ":X", 13 + (60*(r-1)))
    tmp_range_out <- paste0("C", 36 + (60*(r-1)), ":O", 59 + (60*(r-1)))
    
    ####  Exceptions 
    if(i < 3){# For sheet 1 and 2, no "Others" category
      tmp_range_cn <- paste0("B", 10 + (60*(r-1)), ":V", 13 + (60*(r-1)))
    }
    if(i == 2 & r == 4) { ## in sheet 2, the outcomes start two lines lower than expected
      tmp_range_out <- paste0("C219:O242")
    }
    if(i == 2 & r == 5) { ## The last district in sheet 2 is 2 lines lower than expected
      tmp_loc <- "B252"
      tmp_poploc <- paste0(popsize_cols[i], "252")
      tmp_range_cn = paste0("B253:V256")
      tmp_range_out <- paste0("C279:O302")
    }
    
    
    if(i == 8){ # the last sheet has more lines per region
      tmp_loc <- paste0("B", 9 + 65*(r-1))
      tmp_poploc <- paste0(popsize_cols[i], 9 + 65*(r-1))
      tmp_range_cn <- paste0("B", 10 + (65*(r-1)), ":Z", 13 + (65*(r-1)))
    }
    
    # Extract data from sheet
    tmp_region <- as.character(suppressMessages(readxl::read_xls(df,
                                                sheet = sheets[i],
                                                range = tmp_loc,
                                                col_names = FALSE)))
    
    tmp_popsize <- as.numeric(suppressMessages(readxl::read_xls(df,
                                                 sheet = sheets[i],
                                                 range = tmp_poploc,
                                                 col_names = FALSE)))
    
    tmp_cn_hist_sex <- suppressMessages(readxl::read_xls(df, sheet = sheets[i],
                                 range = tmp_range_cn, col_names = FALSE))

    if(i < 8){
    tmp_cn_hist_sex <- cbind(tmp_region, tmp_year, tmp_cn_hist_sex)
    if(i < 3){ # Sheet 1 and 2 don't have the last two columns for 'other', so add manually
      tmp_cn_hist_sex <- cbind(tmp_cn_hist_sex, NA, NA)
    }
    
    colnames(tmp_cn_hist_sex) <- colnames(cn_hist_sex)
    cn_hist_sex <- rbind(cn_hist_sex, tmp_cn_hist_sex)
    
    } else { # Sheet 8, 2014, has different column names, so read in separately
      tmp_cn_hist_sex_14 <- cbind(tmp_region, tmp_year, tmp_cn_hist_sex)
      colnames(tmp_cn_hist_sex_14) <- colnames(cn_hist_sex_14)
      cn_hist_sex_14 <- rbind(cn_hist_sex_14, tmp_cn_hist_sex_14)
    }
    
    if(i < 7){ # no outcome data for 2013-2014
    tmp_outcomes <- suppressMessages(readxl::read_xls(df, sheet = sheets[i],
                                    range = tmp_range_out,
                                    col_names = FALSE))
    tmp_out_hist <- cbind(tmp_region, tmp_year, "quarter" = rep(1:4, each = 6),
                          tmp_outcomes)
    colnames(tmp_out_hist) <- colnames(out_hist)
    out_hist <- rbind(out_hist, tmp_out_hist)
    }
    
    popsize <- rbind(popsize, cbind("region" = tmp_region, 
                                    "year" = tmp_year, 
                                    "popsize" = tmp_popsize))
  
  }
}
#### Long to wide wrangling #####

#### Population ####
popsize %>% 
  mutate(popsize = round(as.numeric(popsize)),
         region = str_to_title(region)) %>%
  write.csv("data-clean/tb_20072014_popsize.csv")

#### Clean_data #####
cn_hist_sex %>%
  mutate(region = str_to_title(region)) %>%
  rename(
    `new_positive_male` = `New +ve M`,
    `new_positive_female` = `New +ve F`,
    `new_positive_total` = `Tot N+ve`,
    `relapse_positive_male` = `Rel +ve M`,
    `relapse_positive_female` = `Rel +ve F`,
    `relapse_positive_total` = `Tot R+ve`,
    `new_negative_male` = `New -ve M`,
    `new_negative_female` = `New -ve F`,
    `eptb_eptb_male` = `EPTB M`,
    `eptb_eptb_female` = `EPTB F`,
    `new_NSD_male` = `New NSD M`,
    `new_NSD_female` = `New NSD F`,
    `relapse_negative_male` = `Rel - M`,
    `relapse_negative_female` = `Rel - F`,
    `relapse_NSD_male` = `Rel NSD M`,
    `relapse_NSD_female` = `Rel NSD F`,
    `ltfu_positive_male` = `Default + M`,
    `ltfu_positive_female` = `Default + F`,
    `failure_positive_male` = `Failure + M`,
    `failure_positive_female`= `Failure + F`,
    `other_other_male` = `Other M`,
    `other_other_female` = `Other F`) %>%
  pivot_longer(cols =-c("region","year", "quarter"),
               names_to = c("tbhistory", 
                            "diagnosis",
                            "sex"),
               names_sep = "_",
               values_to = "n") %>%
  rbind(
cn_hist_sex_14 %>%
  mutate(region = str_to_title(region)) %>%
  rename(
  `new_positive_male` = `New +ve M`,
`new_positive_female` = `New +ve F`,
`new_positive_total` = `Tot N+ve`,
`relapse_positive_male` = `Rel +ve M`,
`relapse_positive_female` = `Rel +ve F`,
`relapse_positive_total` = `Tot R+ve`,
`ltfu_positive_male` = `LTFU + M`,
`ltfu_positive_female` = `LTFU + F`,
`failure_positive_male` = `Failure + M`,
`failure_positive_female` = `Failure + F`,
`thu_positive_male` = `THU + M`,
`thu_positive_female` = `THU + F`,
`new_negativeNSD_male` = `N -/NSD M`,
`new_negativeNSD_female` = `N -/NSD F`,
`relapse_negativeNSD_male` = `Rel -/NSD M`,
`relapse_negativeNSD_female` = `Rel -/NSD F`,
`ltfu_negativeNSD_male` = `LTFU -/NSD M`,
`ltfu_negativeNSD_female` = `LTFU -/NSD F`,
`failure_negativeNSD_male` = `Failure -/NSD M`,
`failure_negativeNSD_female` = `Failure -/NSD F`,
`thu_negativeNSD_male` = `THU -/NSD M`,
`thu_negativeNSD_female` = `THU -/NSD F`,
`unknown_eptb_male` = `EPTB M`,
`unknown_eptb_female` = `EPTB F`) %>%
  pivot_longer(cols =-c("region","year", "quarter"),
               names_to = c("tbhistory", 
                            "diagnosis",
                            "sex"),
               names_sep = "_",
               values_to = "n")) %>%
  transmute(
    slice = "notifications_diagnosis_tbhistory_sex",
    region,
    date = zoo::as.yearqtr(paste0(year, " Q", quarter)),
    diagnosis, tbhistory, 
    sex, age = "all", 
    outcome = "notified", n) -> notif

  # write.csv(notif, "data-clean/tb_20072014_notifications.csv")

##### Treatment outcomes by diagnosis ########
out_hist %>%
  mutate(region = str_to_title(region)) %>%
  pivot_longer(cols = -c("region","year","quarter","tbhistory"),
               names_pattern = "(.*)_(.*)",
               names_to = c("outcome", "format")) %>% 
  mutate(tbhistory = case_when(
    tbhistory == "New Cases" ~ "new",
    tbhistory == "Retreatment" ~ "relapse",
    tbhistory == "New P/Neg" ~ "new",
    tbhistory == "EPTB" ~ "unknown",
    tbhistory == "Defaulters" ~ "ltfu",
    tbhistory == "Failure" ~ "failure",
    TRUE ~ NA
  ),
  outcome = str_to_lower(outcome),
  outcome = case_when(outcome == "defaulted" ~ "ltfu",
                      TRUE ~ outcome),
  date = zoo::as.yearqtr(paste0(year, " Q", quarter))) %>%
  filter(format == "n") %>% 
  # group_by(year, quarter, region, tbhistory, outcome, format, popsize) %>%
    # summarize(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  transmute(slice = "outcomes_diagnosis_tbhistory",
            region, date, 
            diagnosis = "all", tbhistory, 
            sex = "all", age = "all",
            outcome, n = value) -> outcomes

write_csv(rbind(notif, outcomes),
          "data-clean/tb_20072014.csv")

