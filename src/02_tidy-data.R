library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)

dirr::gzip_files()

dir_raw <- "data/raw"

# demographics -----------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

patients <- read_data(dir_raw, "patients_iv-iron", FALSE) %>%
    as.patients() %>%
    semi_join(demog, by = "millennium.id")

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

data_iron_iv <- meds_iron %>%
    group_by(millennium.id, med) %>%
    summarize_at("med.dose", sum) %>%
    left_join(meds_iron_doses, by = c("millennium.id", "med")) %>%
    mutate(iv_mg_dose = med.dose / n) %>%
    rename(total_iv_dose = med.dose,
           num_doses = n)

# po iron ----------------------------------------------

iron_po <- med_lookup("iron products") %>%
    filter(!(med.name %in% iron_iv)) %>%
    select(med.name) %>%
    as_vector()

meds_iron_po <- meds %>%
    filter(med %in% iron_po) %>%
    arrange(millennium.id, med.datetime) %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    mutate(po_prior_to_iv = med.datetime <= iron_start,
           po_after_iv = med.datetime > iron_stop)

iron_supp <- meds_iron_po %>%
    group_by(millennium.id) %>%
    summarize_at(c("po_prior_to_iv", "po_after_iv"), funs(sum(.) >= 1)) %>%
    full_join(demog["millennium.id"], by = "millennium.id") %>%
    dmap_at(c("po_prior_to_iv", "po_after_iv"), ~ coalesce(.x, FALSE))

# safety meds ------------------------------------------

meds_steroid <- med_lookup("glucocorticoids") %>%
    select(med.name) %>%
    as_vector()

meds_antihist <- bind_rows(med_lookup("H2 antagonists")) %>%
    select(med.name) %>%
    as_vector()

meds_antihist <- c("diphenydramine", "hydroxyzine", meds_antihist)

safety_meds <- c("epinephrine", meds_steroid, meds_antihist)

meds_rescue <- meds %>%
    filter(med %in% safety_meds,
           is.na(event.tag)) %>%
    arrange(millennium.id, med, med.datetime) %>%
    distinct(millennium.id, med, .keep_all = TRUE) %>%
    select(millennium.id, rescue.datetime = med.datetime, med, med.dose, route, med.location)

meds_rash <- meds_rescue %>%
    left_join(meds_iron[c("millennium.id", "med.datetime")], by = "millennium.id") %>%
    filter(med %in% meds_antihist,
           rescue.datetime >= med.datetime,
           rescue.datetime <= med.datetime + hours(24)) %>%
    mutate(med_rash_time = difftime(rescue.datetime, med.datetime, units = "hours")) %>%
    select(millennium.id, med_rash = med, med_rash_dose = med.dose, med_rash_time)

meds_anaphylax <- meds_rescue %>%
    left_join(meds_iron[c("millennium.id", "med.datetime")], by = "millennium.id") %>%
    filter(rescue.datetime >= med.datetime,
           rescue.datetime <= med.datetime + hours(6)) %>%
    mutate(med_anphlx_time = difftime(rescue.datetime, med.datetime, units = "hours")) %>%
    select(millennium.id, med_anphlx = med, med_anphlx_dose = med.dose, med_anphlx_time)


# pressors ---------------------------------------------

pressors <- tibble(name = c("epinephrine",
                            "norepinpehrine",
                            "dopamine",
                            "phenylephrine",
                            "vasopressin"),
                   type = "med",
                   group = "cont")

meds_pressors <- meds %>%
    tidy_data(ref = pressors) %>%
    calc_runtime() %>%
    summarize_data()

sbp_pressors <- meds_pressors %>%
    left_join(meds_iron[c("millennium.id", "med.datetime", "event.id")], by = "millennium.id") %>%
    filter(med.datetime > start.datetime,
           med.datetime < stop.datetime)

# vitals -----------------------------------------------

vitals <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals()

sbp_after <- vitals %>%
    left_join(meds_iron[c("millennium.id", "med.datetime")], by = "millennium.id") %>%
    filter(str_detect(vital, "systolic"),
           vital.datetime >= med.datetime,
           vital.datetime <= med.datetime + hours(6),
           vital.result > 30) %>%
    group_by(millennium.id, med.datetime) %>%
    summarize_at("vital.result", funs(sbp_after = min))

sbp_before <- vitals %>%
    left_join(meds_iron[c("millennium.id", "med.datetime")], by = "millennium.id") %>%
    filter(str_detect(vital, "systolic"),
           vital.datetime >= med.datetime - hours(12),
           vital.datetime <= med.datetime,
           vital.result > 30) %>%
    group_by(millennium.id, med.datetime) %>%
    summarize_at("vital.result", funs(sbp_prior = min))

# remove any patients that are already on a pressor
sbp_drop <- sbp_before %>%
    inner_join(sbp_after, by = c("millennium.id", "med.datetime")) %>%
    anti_join(sbp_pressors, by = c("millennium.id", "med.datetime")) %>%
    rowwise() %>%
    mutate(sbp_drop = sbp_after < 90 & sbp_prior >= 100) %>%
    filter(sbp_drop) %>%
    select(-med.datetime)

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
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, weight = measure.result)

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
    distinct(millennium.id, hgb_drop, change) %>%
    rename(hgb_change = change) %>%
    full_join(demog["millennium.id"], by = "millennium.id") %>%
    dmap_at("hgb_drop", ~ coalesce(.x, FALSE)) %>%
    group_by(millennium.id, hgb_drop) %>%
    summarize_at("hgb_change", min)

labs_reported <- c("hgb", "ferritin lvl", "iron", "tibc", "uibc", "transferrin")

labs_admit <- labs %>%
    filter(lab %in% labs_reported) %>%
    arrange(millennium.id, lab.datetime) %>%
    group_by(millennium.id, lab) %>%
    summarize_at("lab.result", first) %>%
    spread(lab, lab.result, sep = "_admit_") %>%
    rename(lab_admit_ferritin = `lab_admit_ferritin lvl`) %>%
    mutate(lab_admit_transferrin_sat = lab_admit_iron / lab_admit_tibc * 100)

labs_prior_iron <- labs %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    filter(lab %in% labs_reported,
           lab.datetime <= iron_start) %>%
    arrange(millennium.id, lab.datetime) %>%
    group_by(millennium.id, lab) %>%
    summarize_at("lab.result", last) %>%
    spread(lab, lab.result, sep = "_baseline_") %>%
    rename(lab_baseline_ferritin = `lab_baseline_ferritin lvl`) %>%
    mutate(transferrin_sat = lab_baseline_iron / lab_baseline_tibc * 100)

# blood products ---------------------------------------

blood <- read_data(dir_raw, "blood", FALSE) %>%
    as.blood()

prbc <- blood %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    filter(blood.prod == "prbc",
           blood.datetime >= iron_start - days(2),
           blood.datetime <= iron_start) %>%
    count(millennium.id, blood.prod) %>%
    select(millennium.id, num_prbc = n)

# vent times -------------------------------------------

vent <- read_data(dir_raw, "vent", FALSE) %>%
    as.vent_times() %>%
    tidy_data(dc = patients)

new_intub <- vent %>%
    left_join(meds_iron[c("millennium.id", "med.datetime")], by = "millennium.id") %>%
    filter(start.datetime >= med.datetime,
           start.datetime <= med.datetime + hours(6)) %>%
    mutate(vent_time = difftime(start.datetime, med.datetime, units = "hours")) %>%
    select(millennium.id, vent_time)

# data sets ------------------------------------------

patient_id <- read_data(dir_raw, "identifiers") %>%
    as.id()

# export list for manual collection of indications
exp_manual_list <- patient_id %>%
    left_join(meds_iron_start, by = "millennium.id") %>%
    select(fin, iron_start, iron_stop)

write_excel_csv(exp_manual_list, "data/external/patient_list.csv")

# create data sets for analysis
data_patients <- demog %>%
    select(-visit.type, -facility) %>%
    left_join(weight, by = "millennium.id")

data_labs <- demog %>%
    select(millennium.id) %>%
    left_join(labs_admit, by = "millennium.id") %>%
    left_join(labs_prior_iron, by = "millennium.id") %>%
    left_join(labs_hgb_drop, by = "millennium.id")

data_iron <- demog %>%
    select(millennium.id) %>%
    left_join(prbc, by = "millennium.id") %>%
    left_join(iron_supp, by = "millennium.id")

data_safety <- demog %>%
    select(millennium.id) %>%
    left_join(meds_rash, by = "millennium.id") %>%
    left_join(meds_anaphylax, by = "millennium.id") %>%
    left_join(sbp_drop, by = "millennium.id") %>%
    left_join(new_intub, by = "millennium.id") %>%
    dmap_at("sbp_drop", ~ coalesce(.x, FALSE)) %>%
    mutate(rash_med = !is.na(med_rash),
           rescue_med = !is.na(med_anphlx),
           intubated = !is.na(vent_time),
           anaphylaxis = rescue_med | sbp_drop | intubated) %>%
    select(millennium.id, rash_med, rescue_med, sbp_drop, intubated, anaphylaxis)

dirr::save_rds("data/tidy", "data_")
