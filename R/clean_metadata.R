#######################################################################
# Clean metadata
#######################################################################

metadata_file <- here::here("data/raw/2020-08-20_DataFile 2020-aug-20_complete_BV_STI.xlsx")

raw_metadata <- readxl::read_excel(metadata_file, na = "") %>% 
    janitor::clean_names(case = "big_camel") %>% 
    rename(SpId = Spid) %>% 
    mutate(CollectionDate = as.Date(CollectionDate))

metadata <- raw_metadata %>%
    rename(SampleNumber = Time, OriginalCategory = Category) %>% 
    # two aliquots were used for one sample and these were noted separately
    mutate(OriginalCategory = ifelse(OriginalCategory == "no-sex-two records", "no-sex", OriginalCategory)) 

# First sex
metadata <- metadata %>% 
    group_by(PatientId) %>% 
    arrange(CollectionDate) %>% 
    # NA and negative days means haven't had sex yet
    mutate(Sex = case_when(
        is.na(DaysSinceFirstSex) | DaysSinceFirstSex < 0 ~ "Pre",
        TRUE ~ "Post"
    )) %>% 
    ungroup() %>% 
    mutate(SexSelfReport = case_when(
        is.na(DaysSinceFirstSex) | DaysSinceFirstSex < 0 ~ "Pre",
        OriginalCategory == "no-sex" ~ "Pre", 
        TRUE ~ "Post")) 

# Bv
metadata <- metadata %>% 
    mutate(Bv = case_when(
        BvNugentScore <= 3 ~ "Negative",
        between(BvNugentScore, 4, 6) ~ "Intermediate",
        BvNugentScore >= 7 ~ "Positive"))

# STIs
process_sti <- function(x) {
    result <- case_when(x == 1 ~ "Positive",
        x %in% c(0, 2) ~ "Negative",
        is.na(x) ~ NA_character_, 
        TRUE ~ "Invalid")
    if (any(result == "Invalid", na.rm = TRUE)) stop("Invalid input")
    
    result
}

metadata <- metadata %>% 
    mutate(Chlamydia = process_sti(Ct),
        Gonorrhea = process_sti(Gc), 
        Trichomonas = process_sti(Tv)) %>% 
    # STI inference -- some samples have missing STI tests, but we can infer
    # that they would be negative, because tests taken from visits both before 
    # and after are negative
    mutate(Chlamydia = ifelse(is.na(Chlamydia), "Inferred negative", Chlamydia)) %>% 
    mutate(Gonorrhea = ifelse(is.na(Gonorrhea), "Inferred negative", Gonorrhea)) %>% 
    mutate(Trichomonas = ifelse(is.na(Trichomonas), "Inferred negative", Trichomonas)) 

# Birth control
metadata <- metadata %>% 
    mutate(BirthControl = str_to_sentence(recode(BirthControl, "oral bcp" = "Daily pills", 
        "emergency pills   condoms" = "Emergency pills"))) %>% 
    # infer birth control is negative for pre-debut
    mutate(BirthControl = ifelse(is.na(BirthControl) & Sex == "Pre", "Inferred none", BirthControl))

# Pregnant
metadata <- metadata %>% 
    mutate(Pregnant = ifelse(Pregnant == "N", "Negative", "Positive"))

# YChrom
metadata <- metadata %>% 
    mutate(YChrom = ifelse(YChrom == "N", "Negative", "Positive"))

# HSV-2
metadata <- metadata %>% 
    mutate(Hsv2 = case_when(
        HsvPcrResult == "P2HSV" & HsvWbAtVisit == 1 ~ "Acute",
        HsvWbAtVisit == 3 ~ "Seropositive",
        HsvPcrResult != "P2HSV" & (is.na(HsvWbAtVisit) | HsvWbAtVisit < 2) ~ "Negative"
    ))

# HSV-1
metadata <- metadata %>% 
    mutate(Hsv1 = case_when(
        HsvWbAtEnrollment == 0 ~ "Negative",
        HsvPcrResult == "P1HSV" & HsvWbAtEnrollment == 1 ~ "Chronic reactivation",
        HsvWbAtEnrollment == 1 | HsvWbAtVisit == 1 ~ "Seropositive",
        # 633499 had slightly irregular testing order, but is seropositive
        PatientId == "633499" ~ "Seropositive"
    ))

# 2015-28410 and 2015-28409 are duplicate entries (separate aliquots of the same sample)! 
# 410 is used for MIG, RANTES, IL1RA; 409 is used for rest of analytes.
metadata <- metadata %>% 
    filter(SpId != "2015-28410")

metadata <- metadata %>% 
    select(SpId, CollectionDate, PatientId, VisitMonth = PfgeVscodeNum, Age = AgeAtVisit, 
        Sex, SexSelfReport, DaysSinceFirstSex,
        DaysSinceLmp, 
        Pregnant, BirthControl, 
        YChrom,
        Bv, NugentScore = BvNugentScore,
        Chlamydia:Hsv1)

#######################################################################
# Clean total protein data
#######################################################################

total_protein_file <- here::here("data/raw/2021-05-13_TotalProtein.xlsx")

total_protein_data <- read_excel(total_protein_file, range = "A01:L196")

total_protein <- total_protein_data %>% 
    mutate(TotalProtein = ifelse(is.na(`Rerun Conc. CV>20`), `Concentrationmg/mL`, `Rerun Conc. CV>20`)) %>% 
    # use rerun #2
    mutate(TotalProtein = ifelse(SpId == "2015-27895", 0.085, TotalProtein)) %>% 
    # use average of run and rerun 
    mutate(TotalProtein = ifelse(SpId == "2015-28143", 0.060, TotalProtein)) %>% 
    # use average of run and rerun
    mutate(TotalProtein = ifelse(SpId == "2016-9086", 0.072, TotalProtein)) %>% 
    mutate(TotalProtein = as.numeric(TotalProtein)) %>% 
    select(SpId, TotalProtein)

#######################################################################
# Clean PSA ELISA data
#######################################################################

psa_elisa_file <- here::here("data/raw/2021-06-29_Psa.xlsx")

psa_elisa_data <- read_excel(psa_elisa_file)

psa_elisa <- psa_elisa_data %>% 
    # I know as.numeric will generate warnings, which are handled below
    # therefore suppress the warnings
    mutate(PsaNumeric = suppressWarnings(as.numeric(`PSA Mean Result`))) %>% 
    mutate(Psa = case_when(
        `PSA Mean Result` == "Below Range" ~ min(PsaNumeric, na.rm = TRUE) / 2, 
        `PSA Mean Result` == "ABOVE range" ~ max(PsaNumeric, na.rm = TRUE) * 2,
        `PSA Mean Result` == "Gone" ~ NA_real_,
        `PSA Mean Result` == "11.307 and 8.896" ~ (11.307 + 8.896)/2,
        TRUE ~ PsaNumeric
    )) %>% 
    mutate(Psa = ifelse(Psa > 6, "Positive", "Negative")) %>% 
    select(SpId, Psa)

# Two samples are PSA-positive but Sex == Pre
# 2015-19218 (but this one is bloody, possibly messing up the assay so disregard)
# 2015-27959 Ptid 630213
    # Turn this sample to Sex == Post
    # Changes DaysSinceFirstSex to 0 
    # Change prior 630213 2015-18586 sample DaysSinceFirstSex to -169

metadata <- metadata %>% 
    mutate(Sex = ifelse(SpId == "2015-27959", "Post", Sex),
        DaysSinceFirstSex = ifelse(SpId == "2015-27959", 0, DaysSinceFirstSex),
        DaysSinceFirstSex = ifelse(SpId == "2015-18586", -169, DaysSinceFirstSex))

#######################################################################
# Clean progesterone data
#######################################################################

progesterone_file <- here::here("data/raw/2021-06-10_cleaned_progesterone_data.xlsx")

progesterone_data <- read_excel(progesterone_file)

progesterone_data <- progesterone_data %>% 
    # one mistaken sample included
    filter(!is.na(PatientId)) %>% 
    mutate(CollectionDate = as.Date(CollectionDate)) %>% 
    select(PatientId, CollectionDate, SerumProgesterone = ProgesteroneNgPerMlBelowLodImputed)

#######################################################################
# Clean demographic data
#######################################################################
# Everyone was black african
demographics_file <- here::here("data/raw/2022-07-25_Demographics.xlsx")

demographics_data <- read_excel(demographics_file)

# sdem_2: years of education
# sdem_3a: 0 = no regular income, 1 = regular income 
# sdem_4a: 1 = rural, 2 = urban
# sdem_5d: 0 = non-metal/non-tile roof, 1 = metal/tile roof
demographics_data <- demographics_data %>% 
    mutate(EducationAtEnrollment = ifelse(sdem_2 <= 12, "Still in high school", "Completed high school"),
        RegularIncomeAtEnrollment = ifelse(sdem_3a == 0, "No regular income", "Regular income"),
        RuralUrbanAtEnrollment = ifelse(sdem_4a == 1, "Rural", "Urban"), 
        MetalRoofAtEnrollment = ifelse(sdem_5d == 0, "Not tile or metal", "Tile or metal")) %>% 
    select(PatientId = ptid, contains("AtEnrollment"))

#######################################################################
# Combine metadata
#######################################################################

complete_metadata <- metadata %>% 
    inner_join(total_protein, by = "SpId") %>% 
    left_join(psa_elisa, by = "SpId") %>% 
    left_join(progesterone_data, by = c("PatientId", "CollectionDate")) %>% 
    left_join(demographics_data, by = "PatientId")

# Add phase
complete_metadata <- complete_metadata %>% 
    mutate(MenstrualPhase = case_when(
        DaysSinceLmp > 35 ~ "Other", 
        SerumProgesterone < 3 ~ "Follicular", 
        SerumProgesterone >= 3 ~ "Luteal"
    ))

#######################################################################
# Sample exclusions
#######################################################################

complete_metadata <- complete_metadata %>% 
    mutate(EligibilityPrimary = case_when(
        is.na(SerumProgesterone) ~ "Ineligible - missing serum progesterone",
        is.na(BirthControl) ~ "Ineligible - missing birth control",
        is.na(Psa) ~ "Ineligible - missing PSA", 
        is.na(YChrom) ~ "Ineligible - missing y-chromosome",
        SpId == "2015-19218" ~ "Ineligible - bloody sample", 
        BirthControl %in% c("Daily pills", "Implant") ~ "Ineligible - use of implant or daily pills",
        Gonorrhea == "Positive" ~ "Ineligible - Gonorrhea infection",
        Trichomonas == "Positive" ~ "Ineligible - Trichomonas infection",
        Hsv2 == "Acute" ~ "Ineligible - acute HSV-2 infection",
        Hsv1 == "Chronic reactivation" ~ "Ineligible - HSV-1 reactivation",
        TRUE ~ "Eligible"
    )) 

write_csv(complete_metadata, here::here("data/clean/metadata.csv"))
