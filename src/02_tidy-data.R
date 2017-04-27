library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)

dirr::gzip_files()

dir_raw <- "data/raw"

# demographics -----------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

# iv iron dosing ---------------------------------------

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

iron_iv <- c("iron sucrose",
             "sodium ferric gluconate complex",
             "ferumoxytol",
             "iron dextran",
             "ferric carboxymaltose")

meds_iron <- meds %>%
    filter(med %in% iron_iv) %>%
    arrange(millennium.id, med.datetime)

meds_iron_start <- meds_iron %>%
    group_by(millennium.id) %>%
    summarize_at("med.datetime", funs(iron_start = first, iron_stop = last))
    # distinct(millennium.id, .keep_all = TRUE) %>%
    # select(millennium.id, iron_start = med.datetime, med, med.dose, med.dose.units)

meds_iron_doses <- meds_iron %>%
    count(millennium.id, med)

# po iron ----------------------------------------------

iron_po <- med_lookup("iron products") %>%
    filter(!(med.name %in% iron_iv)) %>%
    select(med.name) %>%
    as_vector()

meds_iron_po <- meds %>%
    filter(med %in% iron_po) %>%
    arrange(millennium.id, med.datetime) %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    mutate(prior_to_iv = med.datetime <= iron_start,
           after_iv = med.datetime > iron_stop)

iron_supp_prior <- meds_iron_po %>%
    group_by(millennium.id) %>%
    summarize_at(c("prior_to_iv", "after_iv"), funs(sum(.) >= 1)) %>%
    full_join(demog["millennium.id"], by = "millennium.id") %>%
    dmap_at(c("prior_to_iv", "after_iv"), ~ coalesce(.x, FALSE))

# weight -----------------------------------------------

measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.measures()

# use the last weight before first dose of iv iron
weight <- measures %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    filter(measure == "weight",
           measure.units == "kg",
           measure.datetime <= iron_start) %>%
    arrange(millennium.id, desc(measure.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE)

# labs -------------------------------------------------

labs <- read_data(dir_raw, "labs", FALSE) %>%
    as.labs() %>%
    tidy_data()

labs_hgb_prior <- labs %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    filter(lab == "hgb",
           lab.datetime >= iron_start - days(5),
           lab.datetime <= iron_start) %>%
    arrange(millennium.id, lab.datetime)

attr(labs_hgb_prior, "data") <- attr(labs, "data")
labs_hgb_drop <- labs_hgb_prior %>%
    lab_change("hgb", -2, max) %>%
    mutate(hgb_drop = change <= -2) %>%
    distinct(millennium.id, hgb_drop) %>%
    full_join(demog["millennium.id"], by = "millennium.id") %>%
    dmap_at("hgb_drop", ~ coalesce(.x, FALSE))

labs_reported <- c("hgb", "ferritin lvl", "iron", "tibc", "uibc", "transferrin")

labs_admit <- labs %>%
    filter(lab %in% labs_reported) %>%
    arrange(millennium.id, lab.datetime) %>%
    group_by(millennium.id, lab) %>%
    summarize_at("lab.result", first) %>%
    spread(lab, lab.result) %>%
    rename(ferritin = `ferritin lvl`) %>%
    mutate(transferrin_sat = iron / tibc * 100)

labs_prior_iron <- labs %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    filter(lab %in% labs_reported,
           lab.datetime <= iron_start) %>%
    arrange(millennium.id, lab.datetime) %>%
    group_by(millennium.id, lab) %>%
    summarize_at("lab.result", last) %>%
    spread(lab, lab.result) %>%
    rename(ferritin = `ferritin lvl`) %>%
    mutate(transferrin_sat = iron / tibc * 100)

