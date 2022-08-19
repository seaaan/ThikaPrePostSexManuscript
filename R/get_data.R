
get_metadata <- function(remove_inferences = TRUE) {
    md <- read_csv(here::here("data/clean/metadata.csv"), col_types =
        cols(
            SpId = col_character(),
            PatientId = col_character(),
            VisitMonth = col_double(),
            Age = col_double(),
            Sex = col_factor(levels = c("Pre", "Post")),
            SexSelfReport = col_factor(levels = c("Pre", "Post")),
            DaysSinceFirstSex = col_double(),
            MenstrualPhase = col_factor(levels = c("Luteal", "Follicular", "Other")),
            DaysSinceLmp = col_double(),
            Pregnant = col_factor(levels = c("Negative", "Positive")),
            BirthControl = col_factor(levels = c("None", "Inferred none", "Emergency pills", "Condoms", "Daily pills", "Implant")),
            YChrom = col_factor(levels = c("Negative", "Positive")),
            Bv = col_factor(levels = c("Negative", "Intermediate", "Positive")),
            NugentScore = col_double(),
            Chlamydia = col_factor(levels = c("Negative",  "Inferred negative", "Positive")),
            Gonorrhea = col_factor(levels = c("Negative", "Inferred negative", "Positive")),
            Trichomonas = col_factor(levels = c("Negative", "Inferred negative", "Positive")),
            Hsv2 = col_factor(levels = c("Negative", "Acute", "Seropositive")),
            Hsv1 = col_factor(levels = c("Negative", "Chronic reactivation", "Seropositive")),
            TotalProtein = col_double(),
            Psa = col_factor(levels = c("Negative", "Positive")),
            SerumProgesterone = col_double(),
            EducationAtEnrollment = col_character(),
            RegularIncomeAtEnrollment = col_character(),
            RuralUrbanAtEnrollment = col_character(),
            MetalRoofAtEnrollment = col_character(),
            EligibilityPrimary = col_character()
        ))
    if (remove_inferences) {
        md %>%
            mutate(Chlamydia = fct_collapse(Chlamydia, Negative = c("Negative", "Inferred negative")),
                Gonorrhea = fct_collapse(Gonorrhea, Negative = c("Negative", "Inferred negative")),
                Trichomonas = fct_collapse(Trichomonas, Negative = c("Negative", "Inferred negative")),
                BirthControl = fct_collapse(BirthControl, None = c("None", "Inferred none")))
    } else {
        md
    }
}

get_data <- function() {
    read_csv(here::here("data/clean/msd_data.csv"), col_types =
            cols(
                SpId = col_character(),
                PatientId = col_character(),
                Assay = col_character(),
                Dilution = col_double(),
                Concentration = col_double(),
                Detectable = col_character(),
                VisitMonth = col_double(),
                Age = col_double(),
                Sex = col_factor(levels = c("Pre", "Post")),
                SexSelfReport = col_factor(levels = c("Pre", "Post")),
                DaysSinceFirstSex = col_double(),
                MenstrualPhase = col_factor(levels = c("Luteal", "Follicular", "Other")),
                DaysSinceLmp = col_double(),
                Pregnant = col_factor(levels = c("Negative", "Positive")),
                BirthControl = col_factor(levels = c("None", "Emergency pills", "Condoms", "Daily pills", "Implant")),
                YChrom = col_factor(levels = c("Negative", "Positive")),
                Bv = col_factor(levels = c("Negative", "Intermediate", "Positive")),
                NugentScore = col_double(),
                Chlamydia = col_factor(levels = c("Negative", "Positive")),
                Gonorrhea = col_factor(levels = c("Negative", "Positive")),
                Trichomonas = col_factor(levels = c("Negative", "Positive")),
                Hsv2 = col_factor(levels = c("Negative", "Acute", "Seropositive")),
                Hsv1 = col_factor(levels = c("Negative", "Chronic reactivation", "Seropositive")),
                TotalProtein = col_double(),
                Psa = col_factor(levels = c("Negative", "Positive")),
                EligibilityPrimary = col_character()
            ))
}

get_models <- function() { 
    read_csv(here::here("data/clean/models.csv"), col_types = cols(
        Assay = col_character(),
        Model = col_factor(c("Univariate", "Univariate-subset", "Univariate-fully-paired", "Multivariate")),
        Coefficient = col_character(),
        ModelOutcome = col_factor(c("pg/mL", "pg/mg protein", "Detectable")),
        Units = col_factor(c("Log-odds", "Log2 concentration")), 
        Value = col_double(),
        Std.Error = col_double(),
        Lower95Ci = col_double(),
        Upper95Ci = col_double(),
        df = col_double(),
        t = col_double(),
        z = col_double(),
        p.value = col_double(),
        Adjusted = col_double(),
        Ratio = col_double(),
        RatioLower95Ci = col_double(),
        RatioUpper95Ci = col_double()
    ))
}

get_models_days_since_first_sex <- function() { 
    read_csv(here::here("data/clean/models_days_since_first_sex.csv"), col_types = cols(
        Assay = col_character(),
        Intercept = col_double(),
        InterceptSe = col_double(),
        PreSlope = col_double(),
        PreSlopeSe = col_double(),
        PostSlope = col_double(),
        PostSlopeSe = col_double(),
        PValue = col_double()
    ))
}

get_metaanalysis <- function() {
    read_csv(here::here("data/clean/meta_analysis.csv"), col_types = cols(
        Assay = col_character(),
        Study = col_character(),
        Value = col_double(),
        Std.Error = col_double(),
        n = col_double(),
        p.value = col_double(),
        I2 = col_double(),
        Adjusted = col_double(), 
        Lower95Ci = col_double(),
        Upper95Ci = col_double(),
        Ratio = col_double(),
        RatioLower95Ci = col_double(),
        RatioUpper95Ci = col_double()
    ))
}

get_jespers_2016 <- function(convert_genital_touching_to_pre = TRUE) {
    d <- read_csv(here::here("data/clean/jespers_2016.csv"), col_types = cols(
        PatientId = col_character(),
        Visit = col_double(),
        Sex = col_factor(levels = c("Pre", "Post", "Genital touching")),
        StudyName = col_character(),
        Assay = col_character(),
        Concentration = col_double(),
        Log2Concentration = col_double(),
        Detectable = col_character()
    ))
    
    if (convert_genital_touching_to_pre) {
        d %>% 
            mutate(Sex = as.character(Sex),
                Sex = str_replace(Sex, "Genital touching", "Pre"),
                Sex = factor(Sex, levels = c("Pre", "Post")))
    } else {
        d
    }
}

get_ghosh_2018 <- function() { 
    read_csv(here::here("data/clean/ghosh_2018.csv"), col_types = cols(
        StudyName = col_character(),
        PatientId = col_character(),
        Sex = col_factor(levels = c("Pre", "Post")),
        MenstrualPhase = col_character(),
        DaysSinceLmp = col_double(),
        RegularCycle = col_character(),
        Age = col_double(),
        Race = col_character(),
        TotalProtein = col_double(),
        Assay = col_character(),
        Concentration = col_double(),
        Log2Concentration = col_double(),
        Detectable = col_character()
    ))
}
