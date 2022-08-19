#######################################################################
# Prepare the data for modeling
#######################################################################

d <- get_data()

for_linear_models <- d %>% 
    filter(EligibilityPrimary == "Eligible") %>% 
    mutate(Log2Concentration = log2(Concentration)) %>% 
    group_by(Assay) %>% 
    filter(mean(Detectable == "In") >= 0.5) %>% 
    ungroup() 

for_logistic_models <- d %>% 
    filter(EligibilityPrimary == "Eligible") %>% 
    group_by(Assay) %>% 
    filter(mean(Detectable == "In") < 0.5, mean(Detectable == "In") > 0.1) %>% 
    ungroup() %>% 
    mutate(Detectable = factor(Detectable, levels = c("Below", "In")))

#######################################################################
# Run the linear outcome models
#######################################################################

univariate_linear <- for_linear_models %>% 
    apply_model(univariate_model) %>% 
    mutate(Model = "Univariate", ModelOutcome = "pg/mL")

# one singular model boundary, okay to suppress message here
univariate_fully_paired_linear <- suppressMessages(for_linear_models %>% 
    group_by(PatientId, Assay) %>% 
    filter(n() == 2) %>% 
    filter("Pre" %in% Sex, "Post" %in% Sex) %>% 
    apply_model(univariate_model) %>% 
    mutate(Model = "Univariate-fully-paired", ModelOutcome = "pg/mL"))

multivariate_linear <- for_linear_models %>% 
    apply_model(multivariate_model) %>% 
    mutate(Model = "Multivariate", ModelOutcome = "pg/mL")

univariate_protein_linear <- for_linear_models %>% 
    mutate(Log2Concentration = log2(Concentration / TotalProtein)) %>% 
    apply_model(univariate_model) %>% 
    mutate(Model = "Univariate", ModelOutcome = "pg/mg protein")

#######################################################################
# Run the logistic outcome models
#######################################################################

univariate_logistic <- for_logistic_models %>% 
    apply_model(univariate_logistic_model, postprocessing = glmer_model_postprocessing) %>% 
    mutate(Model = "Univariate", ModelOutcome = "Detectable") 

univariate_fully_paired_logistic <- for_logistic_models %>% 
    group_by(PatientId, Assay) %>% 
    filter(n() == 2) %>% 
    filter("Pre" %in% Sex, "Post" %in% Sex) %>% 
    apply_model(univariate_logistic_model, postprocessing = glmer_model_postprocessing) %>% 
    mutate(Model = "Univariate-fully-paired", ModelOutcome = "Detectable") 

multivariate_logistic <- for_logistic_models %>% 
    apply_model(multivariate_logistic_model, postprocessing = glmer_model_postprocessing) %>% 
    mutate(Model = "Multivariate", ModelOutcome = "Detectable")

#######################################################################
# Run the all-negative subset models
#######################################################################

univariate_subset_linear <- for_linear_models %>%
    filter(EligibilityPrimary == "Eligible", 
        Pregnant == "Negative", BirthControl == "None",
        Chlamydia == "Negative", Hsv2 == "Negative", NugentScore < 4) %>% 
    apply_model(univariate_model) %>% 
    mutate(Model = "Univariate-subset", ModelOutcome = "pg/mL")

univariate_subset_logistic <- for_logistic_models %>%
    filter(EligibilityPrimary == "Eligible", 
        Pregnant == "Negative", BirthControl == "None",
        Chlamydia == "Negative", Hsv2 == "Negative", NugentScore < 4) %>% 
    apply_model(univariate_logistic_model, postprocessing = glmer_model_postprocessing) %>% 
    mutate(Model = "Univariate-subset", ModelOutcome = "Detectable") 

#######################################################################
# Combine and save the data
#######################################################################

univariate <- bind_rows(univariate_linear, univariate_logistic, 
        univariate_fully_paired_linear, univariate_fully_paired_logistic,
        univariate_protein_linear, univariate_subset_linear, univariate_subset_logistic) %>% 
    filter(Coefficient == "SexPost") %>% 
    ungroup() %>% 
    # need to adjust p-values within analysis groups
    mutate(AdjustmentGroup = case_when(
        Model == "Univariate-fully-paired" ~ "A",
        ModelOutcome == "pg/mg protein" ~ "B", 
        Model == "Univariate-subset" ~ "C",
        TRUE ~ "D"
    )) %>% 
    group_by(AdjustmentGroup) %>% 
    arrange(p.value) %>% 
    mutate(Adjusted = p.adjust(p.value, method = "holm")) %>% 
    ungroup() %>% 
    select(-AdjustmentGroup) %>% 
    arrange(Adjusted) 

multivariate <- bind_rows(multivariate_linear, multivariate_logistic) %>% 
    ungroup() %>% 
    filter(Coefficient == "SexPost") %>% 
    arrange(p.value) %>% 
    mutate(Adjusted = p.adjust(p.value, method = "holm")) %>% 
    arrange(Adjusted)

bind_rows(univariate, multivariate) %>% 
    mutate(Units = ifelse(ModelOutcome == "Detectable", "Log-odds", "Log2 concentration")) %>% 
    mutate(Lower95Ci = Value - 1.96 * Std.Error, Upper95Ci = Value + 1.96 * Std.Error) %>% 
    select(Assay, Value, Std.Error, Lower95Ci, Upper95Ci, everything()) %>% 
    select(Assay, Model, Coefficient, ModelOutcome, Units, everything()) %>% 
    # calculate ratios
    # need to use 2^x for concentration and exp(x) for odds ratios
    mutate(Ratio = map2_dbl(Value, Units, function(x, y) ifelse(y == "Log-odds", exp(x), 2^x))) %>% 
    mutate(RatioLower95Ci = map2_dbl(Lower95Ci, Units, function(x, y) ifelse(y == "Log-odds", exp(x), 2^x))) %>% 
    mutate(RatioUpper95Ci = map2_dbl(Upper95Ci, Units, function(x, y) ifelse(y == "Log-odds", exp(x), 2^x))) %>% 
    write_csv(here::here("data/clean/models.csv"))

#######################################################################
# Run models of concentrations compared to days since first sex
#######################################################################

data_for_since_first_sex <- get_data() %>% 
    filter(EligibilityPrimary == "Eligible") %>% 
    # missing DaysSinceFirstSex means no reported date for that participant
    # 0 means inferred from positive PSA, STI, etc test
    # both need to be removed
    filter(!is.na(DaysSinceFirstSex), DaysSinceFirstSex != 0) %>% 
    filter(Assay != "IFN-a2a") %>% 
    mutate(Log2Concentration = log2(Concentration)) %>% 
    # create spline variable
    # 0 for all pre-sex samples, DaysSinceFirstSex for all post
    mutate(SplineVariable = ifelse(Sex == "Pre", 0, DaysSinceFirstSex/100)) %>% 
    mutate(DaysSinceFirstSex = DaysSinceFirstSex/100)

# the interpretation is as follows: 
#   (Intercept) = Log2Concentration at DaysSinceFirstSex = 0
#   DaysSinceFirstSex = change in Log2Concentration per 100 days for the pre-first sex group
#                       ie the slope for pre-first sex
#   SplineVariable    = change in the slope between pre-first sex and post-first sex groups
# so to calculate the post-first sex slope, add DaysSinceFirstSex and SplineVariable 
days_since_first_sex_pre <- data_for_since_first_sex %>% 
    apply_model(days_since_first_sex_spline_model) 

# We can calculate the pre and post slopes from the previous model
# That doesn't give a convenient way of calculating the SE for the post-first sex group
# The easiest way to get the SE is to re-parameterize the model with post as the reference group instead of pre
# the interpretation is as follows: 
#   (Intercept) = Log2Concentration at DaysSinceFirstSex = 0
#   DaysSinceFirstSex = change in Log2Concentration per 100 days for POST
#                       ie the slope for POST sex
#   SplineVariable    = change in the slope between pre-first sex and post-first sex groups
# so to calculate the pre-first sex slope, add DaysSinceFirstSex and SplineVariable 
days_since_first_sex_post <- data_for_since_first_sex %>%
    mutate(SplineVariable = ifelse(Sex == "Post", 0, DaysSinceFirstSex)) %>% 
    mutate(Sex = factor(Sex, levels = c("Post", "Pre"))) %>% 
    apply_model(days_since_first_sex_spline_model) 

# Create one data frame:
# Intercept = (Intercept) from either model
# PreSlope = DaysSinceFirstSex from first model 
# PostSlope = DaysSinceFirstSex from second model
# PValue = p.value for the SplineVariable from either model
intercepts <- filter(days_since_first_sex_pre, Coefficient == "(Intercept)") %>% 
    select(Assay, Intercept = Value, InterceptSe = Std.Error)

pre_slopes <- filter(days_since_first_sex_pre, Coefficient == "DaysSinceFirstSex") %>% 
    select(Assay, PreSlope = Value, PreSlopeSe = Std.Error)

post_slopes <- filter(days_since_first_sex_post, Coefficient == "DaysSinceFirstSex") %>% 
    select(Assay, PostSlope = Value, PostSlopeSe = Std.Error)

p_values <- filter(days_since_first_sex_pre, Coefficient == "SplineVariable") %>% 
    select(Assay, PValue = p.value)

left_join(intercepts, pre_slopes, by = "Assay") %>% 
    left_join(post_slopes, by = "Assay") %>% 
    left_join(p_values, by = "Assay") %>% 
    write_csv(here::here("data/clean/models_days_since_first_sex.csv"))