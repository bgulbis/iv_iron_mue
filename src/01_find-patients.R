library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

# run MBO query:
#   * Patients - by Medication (Generic)
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics
#       - Date Only - Admit (Start): 7/1/2016 12:00:00 AM
#       - Date Only - Admit (Stop): 1/1/2017 12:00:00 AM
#       - Medication (Generic): iron sucrose;iron dextran;ferumoxytol;ferric
#           carboxymaltose;sodium ferric gluconate complex

patients <- read_data(dir_raw, "patients_iv-iron", FALSE) %>%
    as.patients() %>%
    filter(discharge.datetime >= mdy("10/1/2016"),
           discharge.datetime <= mdy("12/31/2016"))

mbo_id <- concat_encounters(patients$millennium.id)

# run MBO queries:
#   * Blood Products
#   * Demographics
#   * Diagnosis - ICD-9/10-CM
#   * Labs - CBC
#   * Labs - Iron
#   * Labs - Renal
#   * Measures
#   * Medications - Inpatient - All
#   * Vent - Times
#   * Vitals - BP
