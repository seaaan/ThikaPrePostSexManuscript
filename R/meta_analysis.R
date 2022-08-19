###################################################################################################
# Ghosh 2018
###################################################################################################

ghosh_2018_for_linear_models <- get_ghosh_2018() %>% 
    group_by(Assay) %>% 
    filter(mean(Detectable == "In") >= 0.5) %>% 
    ungroup() 

ghosh_2018_model_results <- ghosh_2018_for_linear_models %>% 
    apply_model(univariate_lm_model) %>% 
    mutate(Model = "Univariate", ModelOutcome = "pg/mL", Study = "Ghosh-2018") %>% 
    filter(Coefficient == "SexPost") 

###################################################################################################
# Jespers 2016
###################################################################################################

jespers_2016_model_results <- get_jespers_2016(TRUE) %>% 
    apply_model(univariate_model) %>% 
    mutate(Model = "Univariate", ModelOutcome = "pg/mL", Study = "Jespers-2016") %>% 
    filter(Coefficient == "SexPost") 

###################################################################################################
# Combine data sets
###################################################################################################

data_for_meta <- get_models() %>%
    filter(Model == "Univariate", ModelOutcome == "pg/mL") %>% 
    mutate(Study = "Current study") %>% 
    bind_rows(jespers_2016_model_results, ghosh_2018_model_results) %>% 
    select(Assay, Study, Value, `Std.Error`, `p.value`) 
    
###################################################################################################
# Do meta-analysis
###################################################################################################
# function to perform meta-analysis based on treatment effects in each study
do_meta <- function(data) {
    meta::metagen(TE = data$Value,
        seTE = data$Std.Error,
        studlab = data$Study, 
        fixed = FALSE,
        random = TRUE)
}

# perform meta-analysis
meta_analyzed <- data_for_meta %>%  
    group_by(Assay) %>% 
    arrange(Assay, Study) %>% 
    nest() %>% 
    mutate(Res = map(data, do_meta)) 

# function to get numeric mean and SE out of meta-analysis object
extract_meta_estimates <- function(data) {
    tibble(Value = data$TE.random, Std.Error = data$seTE.random, p.value = data$pval.random, I2 = data$I2, Study = "Meta")
}

###################################################################################################
# Save meta-analysis
###################################################################################################
# calculate number of studies per Assay
to_plot_individual_studies <- data_for_meta %>% 
    group_by(Assay) %>% 
    mutate(n = n())

to_plot_meta <- meta_analyzed %>% 
    # get numeric mean/SE out of meta-analysis object
    mutate(Extracted = map(Res, extract_meta_estimates)) %>% 
    unnest(Extracted) %>% 
    select(-data, -Res) %>% 
    # add number of studies per Assay
    left_join(unique(select(to_plot_individual_studies, Assay, n)), by = "Assay") %>% 
    # only show meta-analysis estimates for Assays with > 1 study
    filter(n > 1) %>%
    ungroup() %>% 
    arrange(p.value) %>% 
    mutate(Adjusted = p.adjust(p.value, method = "holm")) %>% 
    arrange(Adjusted)

bind_rows(to_plot_individual_studies, to_plot_meta) %>% 
    mutate(Lower95Ci = Value - 1.96 * Std.Error, 
        Upper95Ci = Value + 1.96 * Std.Error) %>% 
    mutate(Ratio = 2^Value, 
        RatioLower95Ci = 2^Lower95Ci,
        RatioUpper95Ci = 2^Upper95Ci) %>% 
    write_csv(here::here("data/clean/meta_analysis.csv"))
