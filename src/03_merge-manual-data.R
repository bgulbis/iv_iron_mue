library(tidyverse)
library(purrrlyr)
library(readxl)
library(stringr)
library(edwr)
library(aws.s3)

bucket <- "iv-iron-mue"

patient_id <- s3readRDS(object = "data/raw/identifiers.Rds", bucket = bucket) %>%
    as.id()

indications = c("1" = "esrd",
                "2" = "transplant",
                "3" = "bleed",
                "4" = "jehovah",
                "5" = "other",
                "6" = "none")

data_indications <- s3read_using(FUN = read_excel,
                                 range = "A2:E200",
                                 col_names = c("fin", "iron_start", "iron_stop", "indication", "comments"),
                                 col_types = c("text", "date", "date", "text", "text"),
                                 object = "data/external/manually_collected_data.xlsx",
                                 bucket = bucket) %>%
    dmap_at("indication", str_replace_all, pattern = indications) %>%
    left_join(patient_id, by = "fin") %>%
    select(millennium.id, indication, comments)

s3saveRDS(data_indications,
          object = "data/tidy/data_indications.Rds",
          bucket = bucket,
          headers = list("x-amz-server-side-encryption" = "AES256"))
