# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

library(tidyverse)

policeContactData <- read_tsv(here::here("dataset", "police-contact-data.tsv"))
View(policeContactData)

summary(policeContactData)

filtered <- policeContactData |>
  distinct()

View(filtered)

categorical_columns <- c("C4_RACE", "MALE", "MAR_STAT", "WORK_LW", "HHPOV", "FREQ_DRV", 
                         "HH_SIZE", "PUB_HOUSE", "PUB_HOUSE_SUB", "REGION", "PPCS_YEAR", 
                         "INPERSON", "CONTACT", "REASON", "ARRESTED", "CUFFED", "C_CONTCT", 
                         "VICAR_CITIZEN", "VICAR_PRO_AUTO", "VICAR_PRO_PERS", "VICAR_OTH_CONT",
                         "VICAR_IMPROPER", "D_HH_P23", "TENURE", "MSA_STATUS", "PROPER")
  
quantitative_columns <- c("AGE","EDUCATION", "EDUCATION_SUB", "NUM_MOVES", "NUM_CONT", "NUM_FU_HHINT", 
                          "NUM_FU_PERINT", "N_HH_P1", "N_PERS_P1", "NUM_CITIZEN_HH", "NUM_PRO_AUTO_HH", 
                          "NUM_PRO_PERS_HH", "NUM_OTH_CONT_HH", "NUM_IMPROPER_HH")

filtered[quantitative_columns] <- filtered[quantitative_columns] |> 
  map_df(~ as.numeric(.x)) |> 
  map_df(~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

### HIS EXAMPLE
# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project
#library(tidyverse)
#loan_data <- read_csv(here::here("dataset", "loan_refusal.csv"))
## CLEAN the data
#loan_data_clean <- loan_data |>
#  pivot_longer(2:5, names_to = "group", values_to = "refusal_rate")
#write_rds(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.rds"))
