# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

library(tidyverse)

policeContactData <- read_tsv("dataset/police-contact-data.tsv")
#View(policeContactData)

filtered <- policeContactData |>
  select(where(~sum(is.na(.x)) < 90000))
View(filtered)

