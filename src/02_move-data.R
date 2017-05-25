library(tidyverse)
library(edwr)
library(aws.s3)
library(httr)

# prevent peer checking due to MH firewall
set_config(config(ssl_verifypeer = 0L))

dir_raw <- "data/raw"
bucket <- "iv-iron-mue"

move_data <- function(x) {
    read_data(dir_raw, x, FALSE) %>%
        s3saveRDS(object = paste0("data/raw/", x, ".Rds"),
                  bucket = bucket,
                  headers = list("x-amz-server-side-encryption" = "AES256"))
}

# mbo/edw data -----------------------------------------
move_data("blood")
move_data("demographics")
move_data("diagnosis")
move_data("labs")
move_data("measures")
move_data("meds")
move_data("patients")
move_data("vent")
move_data("vitals")

read_data(dir_raw, "identifiers") %>%
    s3saveRDS(object = "data/raw/identifiers.Rds",
              bucket = bucket,
              headers = list("x-amz-server-side-encryption" = "AES256"))

# vizient data -----------------------------------------
put_object(file = "data/external/vizient_totals_peer-hospitals_2016.xlsx",
           object = "data/external/vizient_totals.xlsx",
           bucket = bucket,
           headers = list("x-amz-server-side-encryption" = "AES256"))

put_object(file = "data/external/vizient_service-line_peer-hospitals_2016.xlsx",
           object = "data/external/vizient_service-line.xlsx",
           bucket = bucket,
           headers = list("x-amz-server-side-encryption" = "AES256"))
