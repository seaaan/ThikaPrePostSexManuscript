###################################################################################################
# Ghosh 2018
###################################################################################################

# suppress message about unnamed column that is ultimately removed
ghosh_2018 <- suppressMessages(read_excel(path = here::here("data/raw/2021-06-23_Ghosh2018.xlsx"), range = "A02:AY21")) %>% 
    filter(!is.na(ID)) %>% 
    select(-`...14`)

ghosh_2018 <- ghosh_2018 %>% 
    mutate(StudyName = "Ghosh-2018") %>% 
    rename(
        PatientId = ID,
        DaysSinceLmp = `Cycle stage`,
        MenstrualPhase = `Cycle stage final`, 
        RegularCycle = `Menstrual Cycle: Regular/Irregular`,
        Sex = `Sexually Active`,
        TotalProtein = `Total Protein  (μg/mL)`
    )  %>%
    mutate(MenstrualPhase = str_to_title(MenstrualPhase),
        Race = case_when(Race == "H" ~ "Hispanic",
            Race == "C" ~ "Caucasian",
            Race == "AA" ~ "African-American"), 
        Sex = ifelse(Sex == "Yes", "Post", "Pre"),
        TotalProtein = TotalProtein / 1E3) %>% 
    select(StudyName, PatientId, Sex, MenstrualPhase, DaysSinceLmp, RegularCycle, Age, Race,
        TotalProtein, contains("pg/mL")) %>% 
    gather(Assay, Concentration, contains ("pg/mL")) %>% 
    mutate(Assay = str_remove(Assay, " .*")) %>% 
    mutate(Log2Concentration = log2(Concentration)) %>% 
    assign_detectable_based_on_number_with_min_conc() %>% 
    # only SLPI has above LOD
    mutate(Detectable = ifelse(Assay == "SLPI" & Concentration > 1766681, "Above", Detectable)) %>% 
    mutate(Assay = Assay) %>% 
    fix_cytokine_names("Ghosh-2018") 

write_csv(ghosh_2018, here::here("data/clean/ghosh_2018.csv"))

###################################################################################################
# Jespers 2016
###################################################################################################

jespers_2016 <- read_csv(here::here("data/raw/2021-07-19_Jespers-data.csv"), col_types = cols()) %>% 
    mutate(StudyName = "Jespers-2016") %>% 
    rename(
        PatientId = Subject,
        Sex = touchsex,
        Visit = visit
    )  %>%
    mutate(Sex = case_when(
        Sex == "No sexual experience" ~ "Pre",
        Sex == "Vaginal penetration" ~ "Post",
        TRUE ~ Sex
    )) %>% 
    select(PatientId, Visit, Sex, StudyName, everything()) %>% 
    gather(Assay, Concentration, mipb:il1b) %>% 
    filter(!is.na(Concentration)) %>% 
    mutate(Log2Concentration = log2(Concentration)) %>% 
    assign_detectable_based_on_number_with_min_conc() %>% 
    fix_cytokine_names("Jespers-2016") 

write_csv(jespers_2016, here::here("data/clean/jespers_2016.csv"))
