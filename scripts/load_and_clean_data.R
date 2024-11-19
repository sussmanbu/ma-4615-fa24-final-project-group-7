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

# Here we remove columns we don't want
# Removed columns:
# PSTRATA: don't understand how to use it
# SECUCODE: not necessary
# num_fu_HHint, num_fu_perint: don't need to know number of follow-ups
# Columns that don't end with "_sub" but have a "_sub" counterpart: the "_sub" columns are objectively better -- they contain more data 
# time2vic_inc_P23PER: don't want to investigate months from PPCS interview to victimization
# 
# QUESTIONS
# There are many columns titled whyno[number]_inc_P23[HH for household crime, PER for personal crime] with 
# description of Reason not reported: (insert reason), should I remove any of them?

filtered <- policeContactData |>
  select(!ends_with("_sub"), !starts_with("vicar_")) |>
  select(!c(SECUCODE, WEIGHT, NUM_FU_HHINT, NUM_FU_PERINT, PSTRATA, TIME2VIC_INC_P23PER)) |>
  mutate(
    RACE = case_when(
      C4_RACE == 1 ~ "White, Non-Hispanic",
      C4_RACE == 2 ~ "Black, Non-Hispanic",
      C4_RACE == 3 ~ "Hispanic",
      C4_RACE == 4 ~ "Other or multiracial, Non-Hispanic"
    )
  ) |>
  relocate(RACE, .before = AGE)

View(filtered)


#Exploratory Data Analysis

# Creating a barplot (bar chart) of 8 different variables in the dataset
# Barplot function
create_barplot <- function(data, col_name) {
  ggplot(data, aes_string(x = col_name)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Barplot of", col_name),
         x = col_name,
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

columns <- c("AGE", "EDUCATION", "HH_SIZE", "NUM_MOVES", "N_HH_P1", "N_PERS_P1", "NUM_CONT", "VAL_LOSS2_INC_P23HH")
for (col in columns) {
  print(create_barplot(filtered, col))
}

#Outlier Detection
#Boxplot function
create_outlier_boxplot <- function(data, col_name) {
  ggplot(data, aes_string(y = col_name)) +
    geom_boxplot(outlier.color = "red",   
                 outlier.shape = 16,      
                 outlier.size = 2) +      
    labs(title = paste("Boxplot of", col_name, "with Outliers"),
         y = col_name) +
    theme_minimal()
}

columns <- c("AGE", "EDUCATION", "HH_SIZE", "NUM_MOVES", "N_HH_P1", "N_PERS_P1", "NUM_CONT", "VAL_LOSS2_INC_P23HH") 

for (col in columns) {
  print(create_outlier_boxplot(filtered, col))
}

write_rds(filtered, file = here::here("dataset", "police_interaction.rds"))

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


categorical_columns <- c("C4_RACE", "MALE", "MAR_STAT", "WORK_LW", "HHPOV", "FREQ_DRV", 
                         "HH_SIZE", "PUB_HOUSE", "PUB_HOUSE_SUB", "REGION", "PPCS_YEAR", 
                         "INPERSON", "CONTACT", "REASON", "ARRESTED", "CUFFED", "C_CONTCT", 
                         "VICAR_CITIZEN", "VICAR_PRO_AUTO", "VICAR_PRO_PERS", "VICAR_OTH_CONT",
                         "VICAR_IMPROPER", "D_HH_P23", "TENURE", "MSA_STATUS", "PROPER")
  
quantitative_columns <- c("AGE","EDUCATION", "EDUCATION_SUB", "NUM_MOVES", "NUM_CONT", "N_HH_P1", "N_PERS_P1", "NUM_CITIZEN_HH", "NUM_PRO_AUTO_HH", 
                          "NUM_PRO_PERS_HH", "NUM_OTH_CONT_HH", "NUM_IMPROPER_HH")

filtered[quantitative_columns] <- filtered[quantitative_columns] |> 
  map_df(~ as.numeric(.x))

View(filtered)
# write_RDS(filtered, "filtered_data.rds")
#   map_df(~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) Commenting out this line because I'm not sure how accurate it is to replace missing values with the average value (was discussed in class)

# Renaming the columns to more usable/understandable names
# If I can't easily rename them, I will include descriptions below
# 
# filtered <- filtered |>
#  rename(marital_status = MAR_STAT, employment_status = WORK_LW, years_of_education = EDUCATION, in_poverty = HHPOV, )

firearm_sh_ds <- read.csv("dataset/Firearm_suicide_homicide_dataset.csv")

firearm_sh_ds <- firearm_sh_ds |>
  filter(year == 2002 | year == 2008 | year == 2011) |>
  select(!ends_with("fss")) |>
  select(!c(division, firearm_suicides, total_suicides))

write_rds(firearm_sh_ds, file = here::here("dataset", "firearm_sh_ds.rds"))

firearm_data <- firearm_sh_ds |>
  mutate(firearm_homicide_rate = as.numeric(firearm_homicide_rate),
         state = tolower(state)) |>
  filter(!is.na(firearm_homicide_rate))

filtered <- read_rds("dataset/police_interaction.rds")

# Combining the Two Datasets into a summary of regions
police_sentiment_regions <- filtered |>
  group_by(REGION, PPCS_YEAR) |>
  summarize(
    total_count = n(),
    WHITE_NH_PROP = sum(C4_RACE == 1) / n(),
    B_NH_PROP = sum(C4_RACE == 2) / n(),
    HISPANIC_PROP = sum(C4_RACE == 3) / n(),
    OTHER_MULTI_NH_PROP = sum(C4_RACE == 4) / n(),
    CONTACT_FREQ = sum(C_CONTCT != 0, na.rm = TRUE),
    OC_FRISK_PROP = sum(OC_FRISK == 1, na.rm = TRUE) / CONTACT_FREQ,
    TC_FRISK_PROP = sum(TS_FRISK == 1, na.rm = TRUE) / CONTACT_FREQ,
    VSRCH_PROP = sum(TS_VSRCH == 1, na.rm = TRUE) / CONTACT_FREQ,
    ARREST_PROP = sum(ARRESTED == 1, na.rm = TRUE) / CONTACT_FREQ,
    CUFFED_PROP = sum(CUFFED == 1, na.rm = TRUE) / CONTACT_FREQ,
    PROPER_PROP = sum(PROPER == 1, na.rm = TRUE) / CONTACT_FREQ,
    IMPROPER_PROP = sum(PROPER == 0, na.rm = TRUE) / CONTACT_FREQ,
    AVG_CONT = sum(NUM_CONT, na.rm = TRUE) / total_count
  ) |>
  mutate(
    REGION = case_when(
      REGION == 1 ~ "northeast",
      REGION == 2 ~ "midwest",
      REGION == 3 ~ "south",
      REGION == 4 ~ "west"
    )
  ) |>
  rename(year = PPCS_YEAR,
         region = REGION)

regional_firearm <- firearm_data |>
  mutate(
    region = case_when(
      state %in% c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont") ~ "northeast",
      state %in% c("new jersey", "new york", "pennsylvania", "maryland", "delaware") ~ "northeast",
      state %in% c("illinois", "indiana", "michigan", "ohio", "wisconsin") ~ "midwest",
      state %in% c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota") ~ "midwest",
      state %in% c("florida", "georgia", "north carolina", "south carolina", "virginia", "west virginia", "district of columbia") ~ "south",
      state %in% c("alabama", "kentucky", "mississippi", "tennessee") ~ "south",
      state %in% c("arkansas", "louisiana", "oklahoma", "texas") ~ "south",
      state %in% c("arizona", "colorado", "idaho", "montana", "nevada", "new mexico", "utah", "wyoming") ~ "west",
      state %in% c("alaska", "california", "hawaii", "oregon", "washington") ~ "west",
      TRUE ~ "other"  # A fallback if the state doesn't match
    )) |>
  group_by(region, year) |>
  summarize(
    population = sum(total_population, na.rm = TRUE),
    fa_homicides = sum(firearm_homicides, na.rm = TRUE),
    nfa_homicides = sum(nonfirearm_homicides, na.rm = TRUE),
    homicides = sum(total_homicides, na.rm = TRUE),
    fa_homicide_rate = ((fa_homicides / population) * 100000),
    nfa_homicide_rate = ((nfa_homicides / population) * 100000),
    homicide_rate = ((homicides / population) * 100000)
  )

combined_regional <- police_sentiment_regions |>
  full_join(regional_firearm, by = c("region", "year"))

write_rds(combined_regional, file = here::here("dataset", "combined_regional_data.rds"))

