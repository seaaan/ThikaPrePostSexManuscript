apply_model <- function(data, model, postprocessing = lmer_model_postprocessing) {
    model_function <- function(d) {
        model(d) %>% postprocessing()
    }
    
    data %>%
        group_by(Assay) %>%
        nest() %>%
        mutate(stats = map(data, model_function)) %>%
        select(-data) %>%
        unnest(cols = c(stats))
}

lmer_model_postprocessing <- function(m) {
    m %>% summary() %>% .$coefficients %>% 
        as.data.frame() %>% mutate(Coefficient = rownames(.)) %>% 
        rename(Value = Estimate, `Std.Error` = `Std. Error`, t = `t value`, p.value = `Pr(>|t|)`)
}

glmer_model_postprocessing <- function(m) {
    m %>% summary() %>% .$coefficients %>% 
        as.data.frame() %>% mutate(Coefficient = rownames(.)) %>% 
        rename(Value = Estimate, `Std.Error` = `Std. Error`, z = `z value`, p.value = `Pr(>|z|)`)
}

univariate_model <- function(d) {
    lmerTest::lmer(Log2Concentration ~ Sex + (1|PatientId), data = d)
}

multivariate_model <- function(d) {
    lmerTest::lmer(Log2Concentration ~ Sex + Age + EducationAtEnrollment + RegularIncomeAtEnrollment + RuralUrbanAtEnrollment + MetalRoofAtEnrollment + (1|PatientId), data = d)
}

# for Ghosh-2018
univariate_lm_model <- function(d) {
    lm(Log2Concentration ~ Sex, data = d)
}

univariate_logistic_model <- function(d) {
    lme4::glmer(Detectable ~ Sex + (1|PatientId), family = "binomial", data = d)
}

multivariate_logistic_model <- function(d) {
    lme4::glmer(Detectable ~ Sex + Age + MenstrualPhase + Pregnant + BirthControl + NugentScore + Chlamydia + Hsv2 + (1|PatientId), 
        family = "binomial", data = d, 
        control = lme4::glmerControl(check.conv.hess = "ignore", check.conv.grad = "ignore"))
}

days_since_first_sex_spline_model <- function(d) {
    lmerTest::lmer(Log2Concentration ~ DaysSinceFirstSex + SplineVariable + (1|PatientId), data = d)
}