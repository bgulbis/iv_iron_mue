library(aws.s3)
library(tidyverse)
library(edwr)

bucket <- "iv-iron-mue"

demog <- s3readRDS(object = "data/raw/demographics.Rds", bucket = bucket) %>%
    as.demographics()

s3saveRDS(demog,
          object = "data/tidy/demographics.Rds",
          bucket = bucket,
          headers = list("x-amz-server-side-encryption" = "AES256"))

library(readxl)
x <- s3read_using(FUN = read_excel,
                  object = "data/external/vizient.xlsx",
                  bucket = bucket)
