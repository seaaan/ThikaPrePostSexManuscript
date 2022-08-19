#######################################################################
# File paths and constants
#######################################################################

BELOW_LOD_SCALE_FACTOR <- 2

msd_file <- here::here("data/raw/2021-05-18_compiled_Thika_data_22Nov16_to_7Mar17.csv")
lods_file <- here::here("data/raw/LODs.xlsx")
metadata <- get_metadata(remove_inferences = TRUE)

#######################################################################
# Clean MSD data
#######################################################################

raw_msd_data <- read_csv(msd_file, guess_max = 1E4, na = c("", "NA"), 
        locale = locale(encoding = "latin1"), col_types = cols()) %>% 
    janitor::clean_names(case = "big_camel") %>% 
    rename(PatientId = Ptid) %>% 
    # remove controls and standards 
    # standard curve starts with S, pool CVLs are control samples
    filter(!str_detect(Sample, "^S00"), Sample != "pool CVL", SpId != "pool CVL") %>% 
    # select appropriate dilutions for samples run at multiple dilutions
    # Want 100X diluted for IL1-RA, 10X diluted for MIG and RANTES (based on detectability)
    filter(Assay != "IL1-RA" | Dilution == 100) %>% 
    filter(Assay != "MIG" | Dilution == 10) %>% 
    filter(Assay != "RANTES" | Dilution == 10) %>% 
    # Fix a few messed up sample identifiers
    # PatientId is missing for specimen 2016-9309
    mutate(PatientId = ifelse(SpId == "2016-9309", "630851", PatientId)) %>% 
    # 2015-28410 and 2015-28409 are duplicate entries (separate aliquots of the same sample)! 
    # 410 is used for MIG, RANTES, IL1RA; 409 is used for rest of analytes.
    mutate(PatientId = ifelse(SpId == "2015-28410", "630978", PatientId)) %>%
    mutate(SpId = ifelse(SpId == "2015-28410", "2015-28409", SpId)) 

#######################################################################
#  Handle above/below LOD samples
#######################################################################

lods <- read_excel(lods_file) %>% 
    select(Assay = Analyte, LOD = `LLOD (pg/mL)`)

complete_msd_data <- inner_join(raw_msd_data, lods, by = "Assay") %>% 
    mutate(Concentration = as.numeric(CalcConcentration)) %>% 
    mutate(Detectable = case_when(
        DetectionRange == "Above Fit Curve Range" ~ "Above",
        is.na(Concentration) | Concentration < LOD ~ "Below",
        TRUE ~ "In"
    )) %>% 
    mutate(Concentration = ifelse(Detectable == "Below", 
        LOD / BELOW_LOD_SCALE_FACTOR, Concentration)) %>% 
    group_by(SpId, PatientId, Assay, Dilution) %>% 
    summarise(Concentration = mean(Concentration), 
        Detectable = case_when(
            all(Detectable == "Below") ~ "Below",
            all(Detectable == "Above") ~ "Above",
            TRUE ~ "In"), 
        .groups = "drop") %>% 
    mutate(Concentration = Concentration * Dilution) %>% 
    fix_cytokine_names("Us")

#######################################################################
#  Combine with metadata
#######################################################################

inner_join(complete_msd_data, metadata, by = c("SpId", "PatientId")) %>% 
    write_csv(here::here("data/clean/msd_data.csv"))
