fix_cytokine_names <- function(d, study) {
    lookup <- readxl::read_excel(here::here("data/required_files/cytokine_lookup.xlsx"))
    
    lookup <- lookup[ , c("Unified name", study)] 
    colnames(lookup)[2] <- "Assay"
    
    lookup <- filter(lookup, !is.na(Assay))
    
    if(!all(d$Assay %in% lookup$Assay)) {
        stop("At least one assay is not present in cytokine_lookup.xlsx")
    }
    
    left_join(d, lookup, by = "Assay") %>% 
        mutate(Assay = `Unified name`) %>% 
        select(-`Unified name`)
}

# this function calls measurements undetectable if there are multiple measurements for 
# a particular Assay at the minimum concentration for that Assay
assign_detectable_based_on_number_with_min_conc <- function(d) {
    d %>% 
        group_by(Assay) %>% 
        mutate(Detectable = ifelse(Concentration == min(Concentration) & (sum(Concentration == min(Concentration)) > 1), 
            "Below",
            "In")) %>% 
        ungroup()
}