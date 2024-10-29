# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

library(tidyverse)

policeContactData <- read_tsv("dataset/police-contact-data.tsv")
#View(policeContactData)

# View how many NA values each variable has
# This information is also available online, but has been calculated here for convenience
numNaPerVariable <- policeContactData |>
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{col}"))

filtered <- policeContactData |>
  select(!ends_with("_sub"), !starts_with("vicar_")) |>
  select(!c(SECUCODE, WEIGHT, NUM_FU_HHINT, NUM_FU_PERINT, PSTRATA, TIME2VIC_INC_P23PER)) 
  select(!c(SECUCODE, NUM_FU_HHINT, NUM_FU_PERINT, PSTRATA, ends_with("_sub"), starts_with(("vicar_")), TIME2VIC_INC_P23PER)) 
  #select(where(~sum(is.na(.x)) < 90000)) |>

View(filtered)



# Removed columns:
# PSTRATA: don't understand how to use it
# SECUCODE: not necessary
# num_fu_HHint, num_fu_perint: don't need to know number of follow-ups
# Columns ending with "_sub": unsure what " missing  carried back from later waves " means
# time2vic_inc_P23PER: don't want to investigate months from PPCS interview to victimization
# 
# QUESTIONS
# There are many columns titled whyno[number]_inc_P23[HH for household crime, PER for personal crime] with 
# description of Reason not reported: (insert reason), should I remove any of them?
# 


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
