library(tidyverse)
library(purrrlyr)
library(readxl)
library(stringr)
library(edwr)

patient_id <- read_data("data/raw", "identifiers") %>%
    as.id()

indications = c("1" = "esrd",
                "2" = "transplant",
                "3" = "bleed",
                "4" = "jehovah",
                "5" = "other",
                "6" = "none")

data_indications <- read_excel("data/external/MUE Color Indications.xlsx",
                       range = "A2:E200",
                       col_names = c("fin", "iron_start", "iron_stop", "indication", "comments"),
                       col_types = c("text", "date", "date", "text", "text")) %>%
    dmap_at("indication", str_replace_all, pattern = indications) %>%
    left_join(patient_id, by = "fin") %>%
    select(millennium.id, indication, comments)

write_rds(data_indications, "data/tidy/data_indications.Rds", "gz")
