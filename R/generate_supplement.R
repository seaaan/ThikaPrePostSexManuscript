all_models <- get_models() %>% 
    arrange(Model, ModelOutcome, p.value) %>% 
    mutate(Model = ifelse(ModelOutcome == "pg/mg protein", "Univariate (pg per mg protein)", as.character(Model))) %>% 
    select(-Coefficient) %>% 
    rename(Difference = Value, HolmBonferroniAdjusted = Adjusted) %>% 
    janitor::clean_names(case = "upper_camel") 
    
meta <- get_metaanalysis() %>% 
    rename(Difference = Value, 
        HolmBonferroniAdjusted = Adjusted) %>% 
    janitor::clean_names(case = "upper_camel") %>% 
    filter(N > 1) %>% 
    arrange(Assay) %>% 
    mutate(Model = "Meta-analysis", ModelOutcome = "pg/mL", Units = "Log2 concentration") %>% 
    select(Assay, Model, Study, ModelOutcome, Units, everything()) %>% 
    arrange(Assay, Study, PValue)

all_models <- bind_rows(all_models, meta) %>% 
    mutate(Ratio95CI = paste0(round(Ratio, 2), " (", round(RatioLower95Ci, 2), "-", round(RatioUpper95Ci, 2), ")")) %>% 
    select(-Ratio, -RatioLower95Ci, -RatioUpper95Ci) %>% 
    select(Assay, Difference, StdError, Ratio95CI, everything()) %>% 
    group_by(Model) %>% 
    nest()

all_models_list <- all_models$data
names(all_models_list) <- all_models$Model
writexl::write_xlsx(all_models_list, "supplement.xlsx")
