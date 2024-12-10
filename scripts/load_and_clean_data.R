library(tidyverse)

# Reading initial dataset
policeContactData <- read_tsv("dataset/police-contact-data.tsv")

# Here we remove columns we don't want
# Removed columns:
# PSTRATA: don't understand how to use it
# SECUCODE: not necessary
# num_fu_HHint, num_fu_perint: don't need to know number of follow-ups
# Columns that don't end with "_sub" but have a "_sub" counterpart: the "_sub" columns are objectively better -- they contain more data 
# time2vic_inc_P23PER: don't want to investigate months from PPCS interview to victimization

policeContactCleaned <- policeContactData |>
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

# Define categorial and quantitative columns
categorical_columns <- c("C4_RACE", "MALE", "MAR_STAT", "WORK_LW", "HHPOV", "FREQ_DRV", 
                         "HH_SIZE", "PUB_HOUSE", "PUB_HOUSE_SUB", "REGION", "PPCS_YEAR", 
                         "INPERSON", "CONTACT", "REASON", "ARRESTED", "CUFFED", "C_CONTCT", 
                         "VICAR_CITIZEN", "VICAR_PRO_AUTO", "VICAR_PRO_PERS", "VICAR_OTH_CONT",
                         "VICAR_IMPROPER", "D_HH_P23", "TENURE", "MSA_STATUS", "PROPER")

quantitative_columns <- c("AGE","EDUCATION", "EDUCATION_SUB", "NUM_MOVES", "NUM_CONT", "N_HH_P1", "N_PERS_P1", "NUM_CITIZEN_HH", "NUM_PRO_AUTO_HH", 
                          "NUM_PRO_PERS_HH", "NUM_OTH_CONT_HH", "NUM_IMPROPER_HH")

policeContactCleaned[quantitative_columns] <- policeContactCleaned[quantitative_columns] |> 
  map_df(~ as.numeric(.x))

# Write the cleaned dataset disk
write_rds(policeContactCleaned, file = here::here("dataset", "police_interaction.rds"))

# Load Initial Dataset
firearm_sh_ds <- read.csv("dataset/Firearm_suicide_homicide_dataset.csv")

# Keep only relevant years, and remove irrelevant columns
firearm_sh_ds <- firearm_sh_ds |>
  filter(year == 2002 | year == 2008 | year == 2011) |>
  select(!ends_with("fss")) |>
  select(!c(division, firearm_suicides, total_suicides))

# Write cleaned version to disk
write_rds(firearm_sh_ds, file = here::here("dataset", "firearm_sh_ds.rds"))

# Standardizing the states and homicide rates and remove rows without data
firearm_data <- firearm_sh_ds |>
  mutate(firearm_homicide_rate = as.numeric(firearm_homicide_rate),
         state = tolower(state)) |>
  filter(!is.na(firearm_homicide_rate))

# Combining the Two Datasets into a summary of regions
police_sentiment_regions <- policeContactCleaned |>
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

# Joining the two regional datasets
combined_regional <- police_sentiment_regions |>
  full_join(regional_firearm, by = c("region", "year"))

# Write final combined dataset to disk
write_rds(combined_regional, file = here::here("dataset", "combined_regional_data.rds"))

