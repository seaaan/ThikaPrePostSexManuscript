#######################################################################
# Figure 2 - concentrations
#######################################################################

d <- get_data()

to_plot <- d %>% 
    filter(EligibilityPrimary == "Eligible")

above_lod <- to_plot %>% 
    group_by(Assay) %>% 
    summarise(AboveLOD = mean(Detectable != "Below") * 100, 
        AboveLOD = round(AboveLOD, 0), 
        AboveLOD = paste0(AboveLOD, "%"),
        Concentration = quantile(Concentration, 0.85))

to_plot %>% 
    mutate(Color = paste(Sex, ifelse(Detectable == "Below", "Below", "In"))) %>% 
    ggplot(aes(y = Concentration, x = reorder(Assay, Concentration, function(x) quantile(x, 0.85)))) +
    coord_flip() + 
    geom_boxplot(aes(group = paste(Sex, Assay)), 
        outlier.color = NA, position = position_dodge(width = 0.8)) + 
    ggbeeswarm::geom_quasirandom(aes(color = Color, group = paste(Sex, Assay)), 
        dodge.width = 0.8,
        method = "smiley", alpha = 0.75, size = 0.75) + 
    ylab("Concentration (log10, pg/mL)") +
    xlab(NULL) + 
    scale_y_log10(labels = c("1", "100", "10,000", "1,000,000"), breaks = c(1, 100, 1E4, 1E6), limits = c(0.06, 2E6)) +
    scale_color_manual(values = c("Pre Below" = "#a6cee3", "Pre In" = "#1f78b4", 
        "Post Below" = "#b2df8a", "Post In" = "#33a02c"), guide = "none") + 
    geom_rect(aes(ymin = 1E6, ymax = Inf, xmin = 0, xmax = 21), fill = "white") + 
    geom_text(data = above_lod, aes(y = Inf, label = AboveLOD), hjust = 1, color = "grey50", 
        # geom_text size is in points, which converts to normal by 5/14
        size = 8 * 5 / 14) + 
    theme_pub()

ggsave("figures/images/fig2.png", width = 5, height = 8, dpi = 600)

#######################################################################
# Figure 3 - primary analysis
#######################################################################

linear_models <- get_models() %>% 
    filter(Model == "Univariate", ModelOutcome == "pg/mL") %>% 
    mutate(String = paste0(round(Ratio, 2), " (", round(RatioLower95Ci, 2), "-", round(RatioUpper95Ci, 2), ")"))

logistic_models <- get_models() %>% 
    filter(Model == "Univariate", ModelOutcome == "Detectable") %>% 
    mutate(String = paste0(round(Ratio, 2), " (", round(RatioLower95Ci, 2), "-", round(RatioUpper95Ci, 2), ")"))

CYTOKINE_ORDER <- c(
    linear_models %>% arrange(Value) %>% pull(Assay),
    logistic_models %>% arrange(Value) %>% pull(Assay)
) 

linear <- linear_models %>% 
    mutate(Assay = factor(Assay, levels = CYTOKINE_ORDER)) %>% 
    ggplot(aes(y = Assay, 
        x = Value, 
        xmin = Lower95Ci, 
        xmax = Upper95Ci, 
        shape = p.value < 0.05, fill = p.value < 0.05)) + 
    geom_pointrange(position = position_dodge(width = 0.4)) + 
    vline(0) +
    ylab(NULL) + 
    xlab("Ratio of concentrations post to pre-first sex\n") +
    scale_x_continuous(labels = c("1", "2", "4"), breaks = c(0, 1, 2), limits = c(-0.5, 3.25)) +
    scale_shape_manual(values = c(21, 16), guide = "none") +
    scale_fill_manual(values = c("white", "black"), guide = "none") + 
    theme_pub() + 
    geom_rect(aes(xmin = 2.25, xmax = Inf, ymin = 0, ymax = 17), fill = "white") + 
    geom_text(data = linear_models, aes(x = 2.25, label = String), hjust = 0, color = "grey50", 
        # geom_text size is in points, which converts to normal by 5/14
        size = 8 * 5 / 14)

logistic <- logistic_models %>% 
    mutate(Assay = factor(Assay, levels = CYTOKINE_ORDER)) %>% 
    ggplot(aes(y = Assay, 
        x = Value, 
        xmin = Lower95Ci, 
        xmax = Upper95Ci, shape = p.value < 0.05, fill = p.value < 0.05)) + 
    geom_pointrange(position = position_dodge(width = 0.4)) + 
    vline(0) +
    ylab(NULL) + 
    scale_x_continuous(labels = c("1", "4", "16", "64"), breaks = c(0, log(4), log(16), log(64)), limits = c(-0.1, 6.2)) +
    scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 16), guide = "none") + 
    scale_fill_manual(values = c(`FALSE` ="white", `TRUE` = "black"), guide = "none") + 
    xlab("Odds ratio of detection post to pre-first sex") +
    theme_pub() + 
    geom_rect(aes(xmin = 4.55, xmax = Inf, ymin = 0, ymax = 4), fill = "white") + 
    geom_text(data = logistic_models, aes(x = 4.55, label = String), hjust = 0, color = "grey50", 
        # geom_text size is in points, which converts to normal by 5/14
        size = 8 * 5 / 14)
    
linear + logistic + plot_layout(ncol = 1, 
    heights = c(length(unique(linear_models$Assay)), length(unique(logistic_models$Assay))))

ggsave("figures/images/fig3.png", width = 3.5, height = 3.5, dpi = 600)

#######################################################################
# Figure 4 - concentrations over time
#######################################################################

data_for_since_first_sex <- get_data() %>% 
    filter(EligibilityPrimary == "Eligible") %>% 
    filter(!is.na(DaysSinceFirstSex), DaysSinceFirstSex != 0) %>% 
    filter(Assay != "IFN-a2a") 

models <- get_models_days_since_first_sex() %>% 
    # slope is calculated in units of 100 days
    # need to adjust to units of single days here
    mutate(PreSlope = PreSlope / 100,
        PostSlope = PostSlope / 100)

first_day <- min(data_for_since_first_sex$DaysSinceFirstSex)
last_day <- max(data_for_since_first_sex$DaysSinceFirstSex)

# we have models for Pre-first sex and Post-first sex
# For Pre-first sex: 
#   Starting point:
#       x = first day of observation (-1227)
#       y = (PreSlope * first_day) + model_intercept
#   Ending point
#       x = 0 
#       y = model_intercept

pre_mod_computed_start <- models %>% 
    mutate(x = first_day, y = PreSlope * first_day + Intercept)
pre_mod_computed_end <- models %>% 
    mutate(x = 0, y = Intercept)
pre_mod_finished <- bind_rows(pre_mod_computed_start, pre_mod_computed_end) %>% 
    # model is in units of log2, needs to be 2^ to work with the axis on the graph
    mutate(y = 2^y, Sex = "Pre")

# For Post-first sex: 
#   Starting point:
#       x = 0
#       y = model_intercept
#   Ending point
#       x = last day of observation (511)
#       y = (PostSlope * first_day) + model_intercept
post_mod_computed_start <- models %>% mutate(x = 0, y = Intercept)
post_mod_computed_end <- models %>% mutate(x = last_day, y = PostSlope * last_day + Intercept)
post_mod_finished <- bind_rows(post_mod_computed_start, post_mod_computed_end) %>% 
    # model is in units of log2, needs to be 2^ to work with the axis on the graph
    mutate(y = 2^y, Sex = "Post")

assay_order <- c("CCL3", "CCL4", "CCL5", "CCL20", "CXCL8", "CXCL9", "CXCL10", 
    "IL-1a", "IL-1b", "IL-1RA", "IL-2", "IL-6", "IL-7", "IL-10", "IL-12p70", "IL-17A", "IL-18", 
    "IFNg", "TNF")

time_models <- bind_rows(pre_mod_finished, post_mod_finished) %>% 
    mutate(Assay = factor(Assay, levels = assay_order))

data_for_since_first_sex %>% 
    # put immune mediators in order
    mutate(Assay = factor(Assay, levels = assay_order)) %>% 
    ggplot(aes(x = DaysSinceFirstSex, y = Concentration, color = Sex)) + 
    geom_point(size = smallPointSize()) + 
    geom_line(data = time_models, aes(x=x,y=y), size = 1, color = "black") + 
    facet_wrap(~Assay, scales = "free", ncol = 4) + 
    scale_y_log10() + 
    vline(0) + 
    ylab("Concentration (log10, pg/mL)") + 
    xlab("Days since first sex") + 
    theme_pub() + 
    scale_color_manual(values = c("Pre" = "#1f78b4", "Post" = "#33a02c"), guide = "none") +
    add_white_space(right = 2)

ggsave("figures/images/fig4.png", width = 6, height = 7, dpi = 600)

#######################################################################
# Figure 5 - sensitivity analyses
#######################################################################

linear_models <- get_models() %>% 
    filter(ModelOutcome != "Detectable")

logistic_models <- get_models() %>% 
    filter(ModelOutcome == "Detectable") 

linear <- linear_models %>% 
    mutate(Model = paste(Model, ModelOutcome)) %>% 
    mutate(Assay = factor(Assay, levels = CYTOKINE_ORDER)) %>% 
    ggplot(aes(y = Assay, 
        x = Value, 
        xmin = Lower95Ci, 
        xmax = Upper95Ci, 
        shape = p.value < 0.05, color = Model, fill = p.value < 0.05)) + 
    geom_pointrange(position = position_dodge(width = 0.4), aes(group = Model)) + 
    geom_vline(xintercept = 0) +
    ylab(NULL) + 
    xlab("Ratio of concentrations post to pre-first sex\n") +
    scale_x_continuous(labels = c("1", "2", "4"), breaks = c(0, 1, 2)) +
    scale_fill_manual(values = c("white", "black"), guide = "none") + 
    scale_shape_manual(values = c(21, 16), guide = "none") +
    scale_color_manual(guide = "none", 
        values = c("Univariate pg/mL" = "black", "Univariate-subset pg/mL" = "#1b9e77", 
            "Univariate pg/mg protein" = "#d95f02", "Univariate-fully-paired pg/mL" = "#7570b3", 
            "Multivariate pg/mL" = "#e7298a")) + 
    theme_pub()

logistic <- logistic_models %>% 
    mutate(Model = paste(Model, ModelOutcome)) %>% 
    mutate(Assay = factor(Assay, levels = CYTOKINE_ORDER)) %>% 
    ggplot(aes(y = Assay, 
        x = Value, 
        xmin = Lower95Ci, 
        xmax = Upper95Ci, 
        shape = p.value < 0.05, color = Model, fill = p.value < 0.05)) + 
    geom_pointrange(position = position_dodge(width = 0.4), aes(group = Model)) + 
    geom_vline(xintercept = 0) +
    ylab(NULL) + 
    xlab("Odds ratio of detection post to pre-first sex") +
    scale_x_continuous(labels = c("0.25", "1", "4", "16", "64", "256"), breaks = log(c(0.25, 1, 4, 16, 64, 256))) +
    coord_cartesian(xlim = c(log(0.2), log(300))) +
    scale_fill_manual(values = c("white", "black"), guide = "none") + 
    scale_shape_manual(values = c(21, 16), guide = "none") +
    scale_color_manual(guide = "none", 
        values = c("Univariate Detectable" = "black", "Univariate-subset Detectable" = "#1b9e77",
            "Multivariate Detectable" = "#e7298a",
            "Univariate-fully-paired Detectable" = "#7570b3")) + 
    theme_pub()

linear + logistic + plot_layout(ncol = 1, 
    heights = c(length(unique(linear_models$Assay)), length(unique(logistic_models$Assay))))

ggsave("figures/images/fig5ab.png", width = 3.5, height = 4, dpi = 600)

# Total protein
get_metadata() %>% 
    filter(EligibilityPrimary == "Eligible") %>% 
    ggplot(aes(x = Sex, y = TotalProtein)) + 
        geom_boxplot(outlier.color = NA) + 
        ggbeeswarm::geom_quasirandom(aes(color = Sex), dodge.width = 0.8, size = 1) + 
        ylab("Total protein (log10, mg/mL)") + 
        xlab(NULL) + 
        scale_y_log10() + 
        theme_pub() +
        scale_color_manual(values = c("Pre" = "grey50", "Post" = "black"), guide = "none")
    
ggsave("figures/images/fig5c.png", width = 2, height = 2, dpi = 600)

#######################################################################
# Figure 6 - meta analysis
#######################################################################
metaanalysis <- get_metaanalysis() %>% 
    mutate(min = Lower95Ci, 
        max = Upper95Ci) %>% 
    mutate(Significant = case_when(
        min > 0 & max > 0 ~ "Significant",
        min < 0 & max < 0 ~ "Significant",
        TRUE ~ "Non-significant"
    )) 

metaanalysis_order <- metaanalysis %>% 
    filter(Study == "Meta") %>% 
    arrange(Value) %>%
    pull(Assay)

metaanalysis %>% 
    filter(n > 1) %>% 
    mutate(Study = factor(Study, levels = c("Meta", "Ghosh-2018", "Jespers-2016", "Current study"))) %>% 
    mutate(Assay = factor(Assay, levels = metaanalysis_order)) %>% 
    ggplot(aes(x = Value, 
        xmin = Lower95Ci, 
        xmax = Upper95Ci, 
        color = Study, size = Study == "Meta", 
        y = Assay, shape = Significant, fill = Significant)) + 
        geom_pointrange(position = position_dodge(width = 0.6)) + 
        geom_vline(xintercept = 0) + 
        coord_cartesian(xlim = c(-2.5, 2.5)) + 
        theme_pub(strip.text.y = element_blank()) + 
        scale_x_continuous(labels = c("0.25", "0.5", "1", "2", "4"), breaks = c(-2:2)) +
        scale_shape_manual(values = c(21, 16), guide = "none") + 
        scale_fill_manual(values = c("white", "black"), guide = "none") + 
        scale_color_manual(guide = "none", values = c("Current study" = "#0072B2", 
            "Ghosh-2018" = "#E69F00", 
            "Jespers-2016" = "#CC79A7", "Meta" = "black")) + 
        scale_size_manual(values = c(0.5, 0.75), guide = "none") + 
        ylab(NULL) + 
        xlab("Ratio of concentrations post to pre-first sex\n")

ggsave("figures/images/fig6.png", dpi = 600, height = 3.25, width = 3.25)