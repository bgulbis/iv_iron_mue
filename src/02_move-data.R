library(tidyverse)
library(edwr)
library(aws.s3)
library(httr)

# prevent peer checking due to MH firewall
set_config(config(ssl_verifypeer = 0L))

dir_raw <- "data/raw"
bucket <- "iv-iron-mue"

read_data(dir_raw, "demographics", FALSE) %>%
    # write_rds("data/external/demographics.Rds", "gz")
    s3saveRDS(object = "data/raw/demographics2.Rds",
              bucket = bucket,
              headers = list("x-amz-server-side-encryption" = "AES256"))

put_object(file = "data/external/vizient_service-line_peer-hospitals_2016.xlsx",
           object = "data/external/vizient.xlsx",
           bucket = bucket,
           headers = list("x-amz-server-side-encryption" = "AES256"))
