# Facility management and antenatal care quality analysis
# Salud Mesoamerica Initiative

required_packages <- c("lme4", "knitr", "MASS")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the required R packages before running the analysis: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(lme4)
library(knitr)

data_path <- file.path("Data", "datosq4.rda")
facility_survey_path <- file.path("Data", "dataPool.rda")
table_dir <- file.path("Outputs", "Tables")
figure_dir <- file.path("Outputs", "Figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

load(data_path)
load(facility_survey_path)

if (!exists("datos")) {
  stop("Data/datosq4.rda must contain an object named 'datos'.")
}

if (!exists("dataPool")) {
  stop("Data/dataPool.rda must contain an object named 'dataPool'.")
}

required_record_columns <- c("country", "datstat_altpid", "I3050")
missing_record_columns <- setdiff(required_record_columns, names(datos))
if (length(missing_record_columns) > 0) {
  stop("Missing columns in datos: ", paste(missing_record_columns, collapse = ", "))
}

required_facility_columns <- c(
  "country", "DATSTAT_ALTPID", "FAC_TYPE",
  "MET_ROU", "KEEP_REC_ROU", "MET_MED", "KEEP_REC_MED",
  "TRA_CULT", "TRA_CULT_DEL", "HUM_RES_EVALYR", "PHAR_TYPE"
)
missing_facility_columns <- setdiff(required_facility_columns, names(dataPool))
if (length(missing_facility_columns) > 0) {
  stop("Missing columns in dataPool: ", paste(missing_facility_columns, collapse = ", "))
}

format_glmer_or <- function(model) {
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$term <- rownames(coef_table)
  names(coef_table)[names(coef_table) == "Estimate"] <- "estimate_log_odds"
  names(coef_table)[names(coef_table) == "Std. Error"] <- "std_error"
  names(coef_table)[names(coef_table) == "z value"] <- "z_value"
  names(coef_table)[names(coef_table) == "Pr(>|z|)"] <- "p_value"
  coef_table$odds_ratio <- exp(coef_table$estimate_log_odds)
  coef_table$conf_low <- exp(coef_table$estimate_log_odds - 1.96 * coef_table$std_error)
  coef_table$conf_high <- exp(coef_table$estimate_log_odds + 1.96 * coef_table$std_error)

  coef_table[, c(
    "term", "odds_ratio", "conf_low", "conf_high",
    "estimate_log_odds", "std_error", "z_value", "p_value"
  )]
}

random_effect_summary <- function(model, model_label) {
  variance_components <- as.data.frame(VarCorr(model))
  facility_variance <- variance_components$vcov[
    variance_components$grp == "facility_id"
  ][1]

  data.frame(
    model = model_label,
    n_records = nobs(model),
    n_facilities = length(unique(model@frame$facility_id)),
    n_countries = length(unique(model@frame$country)),
    facility_random_intercept_variance = facility_variance,
    facility_random_intercept_sd = sqrt(facility_variance),
    approximate_logistic_icc = facility_variance / (facility_variance + (pi^2 / 3)),
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model))
  )
}

write_model_outputs <- function(results, filename, caption) {
  write.csv(
    results,
    file.path(table_dir, paste0(filename, ".csv")),
    row.names = FALSE
  )

  writeLines(
    knitr::kable(
      results,
      format = "pipe",
      digits = 3,
      caption = caption
    ),
    file.path(table_dir, paste0(filename, ".md")),
    useBytes = TRUE
  )
}

# -------------------------------------------------------------------------
# Record-level analysis dataset
# -------------------------------------------------------------------------

analysis_data <- datos
analysis_data$facility_id <- interaction(
  analysis_data$country,
  analysis_data$datstat_altpid,
  drop = TRUE
)
analysis_data$country <- factor(analysis_data$country)

analysis_data <- analysis_data[complete.cases(
  analysis_data[, c("I3050", "country", "facility_id", "datstat_altpid")]
), ]

# -------------------------------------------------------------------------
# Facility-level management domains
# -------------------------------------------------------------------------

management_facility <- dataPool

management_facility$d_routine_documented_meeting <- as.integer(
  management_facility$MET_ROU == 1 &
    management_facility$KEEP_REC_ROU == 1
)

management_facility$d_medical_documented_meeting <- as.integer(
  management_facility$MET_MED == 1 &
    management_facility$KEEP_REC_MED == 1
)

management_facility$d_cultural_training_any <- as.integer(
  management_facility$TRA_CULT == 1 |
    management_facility$TRA_CULT_DEL == 1
)

management_facility$d_hr_evaluation <- as.integer(
  management_facility$HUM_RES_EVALYR %in% 1:6
)

domain_names <- c(
  "d_routine_documented_meeting",
  "d_medical_documented_meeting",
  "d_cultural_training_any",
  "d_hr_evaluation"
)

management_facility$management_domain_count4 <- rowSums(
  management_facility[, domain_names],
  na.rm = TRUE
)

# Pharmacy is treated separately from the four-domain management count.
management_facility$pharmacy_available <- as.integer(
  management_facility$PHAR_TYPE %in% c(1, 2)
)

management_facility$pharmacy_management_arrangement <- ifelse(
  management_facility$PHAR_TYPE == 1,
  "Ministry/Secretary",
  ifelse(
    management_facility$PHAR_TYPE == 2,
    "Facility manager",
    NA_character_
  )
)

# -------------------------------------------------------------------------
# Harmonized facility complexity
# -------------------------------------------------------------------------

management_facility$facility_complexity <- NA_character_

management_facility$facility_complexity[
  management_facility$country == "BLZ" &
    management_facility$FAC_TYPE %in% c("1")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "BLZ" &
    management_facility$FAC_TYPE %in% c("2", "3")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "GTM" &
    management_facility$FAC_TYPE %in% c("1", "2", "3", "4", "5")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "GTM" &
    management_facility$FAC_TYPE %in% c("6", "7", "8", "9")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "HND" &
    management_facility$FAC_TYPE %in% c("1", "2")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "HND" &
    management_facility$FAC_TYPE %in% c("3", "4")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "MEX" &
    management_facility$FAC_TYPE %in% c(
      "1", "5", "6", "7", "9",
      as.character(10:21)
    )
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "MEX" &
    management_facility$FAC_TYPE %in% c("2", "3", "4", "22", "23", "24")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "NIC" &
    management_facility$FAC_TYPE %in% c("1", "2", "7")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "NIC" &
    management_facility$FAC_TYPE %in% c("4", "5", "6")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "PAN" &
    management_facility$FAC_TYPE %in% c("1", "2", "4")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "PAN" &
    management_facility$FAC_TYPE %in% c("3", "7")
] <- "Higher-complexity"

management_facility$facility_complexity[
  management_facility$country == "SLV" &
    management_facility$FAC_TYPE %in% c("1", "2")
] <- "Primary/ambulatory"
management_facility$facility_complexity[
  management_facility$country == "SLV" &
    management_facility$FAC_TYPE %in% c("4")
] <- "Higher-complexity"

facility_level_variables <- unique(management_facility[, c(
  "country", "DATSTAT_ALTPID", "FAC_TYPE", "facility_complexity",
  domain_names, "management_domain_count4",
  "pharmacy_available", "pharmacy_management_arrangement", "PHAR_TYPE"
)])

analysis <- merge(
  analysis_data,
  facility_level_variables,
  by.x = c("country", "datstat_altpid"),
  by.y = c("country", "DATSTAT_ALTPID"),
  all.x = TRUE
)

analysis$facility_complexity <- factor(
  analysis$facility_complexity,
  levels = c("Primary/ambulatory", "Higher-complexity")
)

analysis$pharmacy_management_arrangement <- factor(
  analysis$pharmacy_management_arrangement,
  levels = c("Ministry/Secretary", "Facility manager")
)

fit_control <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

# -------------------------------------------------------------------------
# Mixed-effects logistic regression models
# -------------------------------------------------------------------------

model_domain_count4 <- glmer(
  I3050 ~ management_domain_count4 + country + (1 | facility_id),
  data = analysis,
  family = binomial,
  control = fit_control
)

model_domain_count4_facility_complexity <- glmer(
  I3050 ~ management_domain_count4 + country + facility_complexity +
    (1 | facility_id),
  data = analysis,
  family = binomial,
  control = fit_control
)

model_domain_count4_pharmacy_complexity <- glmer(
  I3050 ~ management_domain_count4 + country + pharmacy_available +
    facility_complexity + (1 | facility_id),
  data = analysis,
  family = binomial,
  control = fit_control
)

analysis_pharmacy_present <- analysis[
  analysis$pharmacy_available == 1 &
    !is.na(analysis$pharmacy_management_arrangement),
]

model_pharmacy_arrangement <- glmer(
  I3050 ~ pharmacy_management_arrangement + country +
    facility_complexity + (1 | facility_id),
  data = analysis_pharmacy_present,
  family = binomial,
  control = fit_control
)

results_domain_count4 <- format_glmer_or(model_domain_count4)
results_domain_count4_facility_complexity <- format_glmer_or(
  model_domain_count4_facility_complexity
)
results_domain_count4_pharmacy_complexity <- format_glmer_or(
  model_domain_count4_pharmacy_complexity
)
results_pharmacy_arrangement <- format_glmer_or(model_pharmacy_arrangement)

model_summaries <- rbind(
  random_effect_summary(
    model_domain_count4,
    "four-domain management count + country + (1 | facility_id)"
  ),
  random_effect_summary(
    model_domain_count4_facility_complexity,
    paste(
      "four-domain management count + country +",
      "facility complexity + (1 | facility_id)"
    )
  ),
  random_effect_summary(
    model_domain_count4_pharmacy_complexity,
    paste(
      "four-domain management count + country +",
      "pharmacy availability + facility complexity + (1 | facility_id)"
    )
  ),
  random_effect_summary(
    model_pharmacy_arrangement,
    paste(
      "pharmacy arrangement among facilities with pharmacy +",
      "country + facility complexity + (1 | facility_id)"
    )
  )
)

# -------------------------------------------------------------------------
# Descriptive outputs
# -------------------------------------------------------------------------

domain_dictionary <- data.frame(
  domain = c(
    "Documented routine administrative meetings",
    "Documented medical meetings",
    "Cultural training",
    "Human resources evaluation"
  ),
  operational_criterion = c(
    paste(
      "Facility reported routine internal meetings on administrative or",
      "management matters and kept records of those meetings."
    ),
    paste(
      "Facility reported routine internal meetings on medical topics and",
      "kept records of those meetings."
    ),
    paste(
      "Facility reported staff training on cultural sensitivity or",
      "culturally adapted childbirth services."
    ),
    "Facility reported staff evaluation during the previous year."
  )
)

domain_descriptives <- unique(analysis[, c(
  "country", "datstat_altpid", domain_names, "management_domain_count4"
)])

domain_count_distribution <- as.data.frame(table(
  domain_descriptives$management_domain_count4
))
names(domain_count_distribution) <- c("management_domain_count4", "facilities")

facility_complexity_descriptives <- unique(analysis[, c(
  "country", "datstat_altpid", "FAC_TYPE", "facility_complexity"
)])

facility_complexity_by_country <- as.data.frame.matrix(table(
  facility_complexity_descriptives$country,
  facility_complexity_descriptives$facility_complexity
))
facility_complexity_by_country$country <- rownames(facility_complexity_by_country)
facility_complexity_by_country <- facility_complexity_by_country[, c(
  "country", "Primary/ambulatory", "Higher-complexity"
)]
rownames(facility_complexity_by_country) <- NULL

pharmacy_facility_descriptives <- unique(analysis[, c(
  "country", "datstat_altpid", "facility_complexity", "PHAR_TYPE",
  "pharmacy_available", "pharmacy_management_arrangement",
  "management_domain_count4"
)])

model_sample_summaries <- data.frame(
  model = c(
    "four-domain main",
    "four-domain + facility complexity",
    "four-domain + pharmacy availability + facility complexity",
    "pharmacy management arrangement among facilities with pharmacy"
  ),
  n_records = c(
    nobs(model_domain_count4),
    nobs(model_domain_count4_facility_complexity),
    nobs(model_domain_count4_pharmacy_complexity),
    nobs(model_pharmacy_arrangement)
  ),
  n_facilities = c(
    length(unique(model_domain_count4@frame$facility_id)),
    length(unique(model_domain_count4_facility_complexity@frame$facility_id)),
    length(unique(model_domain_count4_pharmacy_complexity@frame$facility_id)),
    length(unique(model_pharmacy_arrangement@frame$facility_id))
  )
)

write.csv(domain_dictionary, file.path(table_dir, "table01_domain_dictionary.csv"), row.names = FALSE)
write.csv(domain_descriptives, file.path(table_dir, "domain_descriptives.csv"), row.names = FALSE)
write.csv(domain_count_distribution, file.path(table_dir, "domain_count_distribution.csv"), row.names = FALSE)
write.csv(facility_complexity_descriptives, file.path(table_dir, "facility_complexity_descriptives.csv"), row.names = FALSE)
write.csv(facility_complexity_by_country, file.path(table_dir, "facility_complexity_by_country.csv"), row.names = FALSE)
write.csv(pharmacy_facility_descriptives, file.path(table_dir, "pharmacy_facility_descriptives.csv"), row.names = FALSE)
write.csv(model_sample_summaries, file.path(table_dir, "model_sample_summaries.csv"), row.names = FALSE)
write.csv(model_summaries, file.path(table_dir, "mixed_model_summaries.csv"), row.names = FALSE)

writeLines(
  knitr::kable(
    domain_dictionary,
    format = "pipe",
    caption = "Management-related domains used in the primary exposure"
  ),
  file.path(table_dir, "table01_domain_dictionary.md"),
  useBytes = TRUE
)

writeLines(
  knitr::kable(
    domain_count_distribution,
    format = "pipe",
    caption = "Distribution of positive management domains across facilities"
  ),
  file.path(table_dir, "domain_count_distribution.md"),
  useBytes = TRUE
)

writeLines(
  knitr::kable(
    facility_complexity_by_country,
    format = "pipe",
    caption = "Facility-level distribution of harmonized facility complexity by country"
  ),
  file.path(table_dir, "facility_complexity_by_country.md"),
  useBytes = TRUE
)

writeLines(
  knitr::kable(
    model_sample_summaries,
    format = "pipe",
    caption = "Analytic sample sizes by model"
  ),
  file.path(table_dir, "model_sample_summaries.md"),
  useBytes = TRUE
)

write_model_outputs(
  results_domain_count4,
  "mixed_model_management_domain_count4",
  "Mixed-effects logistic regression: antenatal care quality by four-domain management count"
)

write_model_outputs(
  results_domain_count4_facility_complexity,
  "mixed_model_management_domain_count4_facility_complexity",
  "Four-domain model adjusted for harmonized facility complexity"
)

write_model_outputs(
  results_domain_count4_pharmacy_complexity,
  "mixed_model_management_domain_count4_pharmacy_complexity",
  "Four-domain model adjusted for pharmacy availability and facility complexity"
)

write_model_outputs(
  results_pharmacy_arrangement,
  "mixed_model_pharmacy_management_arrangement",
  "Exploratory model among facilities with a pharmacy: pharmacy administration arrangement"
)

# -------------------------------------------------------------------------
# Figures
# -------------------------------------------------------------------------

png(
  file.path(figure_dir, "figure01_domain_count_distribution.png"),
  width = 1800,
  height = 1200,
  res = 200
)
barplot(
  domain_count_distribution$facilities,
  names.arg = domain_count_distribution$management_domain_count4,
  xlab = "Number of positive management domains",
  ylab = "Facilities",
  col = "#7BAFD4",
  border = "#3B6F8F",
  main = "Distribution of positive management domains"
)
dev.off()

quality_by_country <- prop.table(table(analysis$country, analysis$I3050), 1)
png(
  file.path(figure_dir, "figure02_quality_by_country.png"),
  width = 1800,
  height = 1200,
  res = 200
)
barplot(
  t(quality_by_country[, c("0", "1")]) * 100,
  beside = FALSE,
  col = c("#D9D9D9", "#4F81BD"),
  ylab = "Records (%)",
  xlab = "Country",
  main = "Quality and non-quality antenatal care by country",
  ylim = c(0, 115)
)
legend(
  "top",
  inset = 0.01,
  legend = c("Non-quality antenatal care", "Quality antenatal care"),
  fill = c("#D9D9D9", "#4F81BD"),
  horiz = TRUE,
  bty = "n"
)
dev.off()

prediction_grid <- expand.grid(
  management_domain_count4 = 0:4,
  country = levels(analysis$country)
)

prediction_grid$predicted_probability <- predict(
  model_domain_count4,
  newdata = prediction_grid,
  re.form = NA,
  type = "response"
)

prediction_summary <- aggregate(
  predicted_probability ~ management_domain_count4,
  data = prediction_grid,
  FUN = mean
)

set.seed(20260826)
fixed_effect_draws <- MASS::mvrnorm(
  n = 1000,
  mu = fixef(model_domain_count4),
  Sigma = as.matrix(vcov(model_domain_count4))
)
model_matrix <- model.matrix(
  ~ management_domain_count4 + country,
  data = prediction_grid
)
probability_draws <- plogis(model_matrix %*% t(fixed_effect_draws))

prediction_intervals <- do.call(
  rbind,
  lapply(0:4, function(domain_count) {
    rows <- prediction_grid$management_domain_count4 == domain_count
    standardized_draws <- colMeans(probability_draws[rows, , drop = FALSE])
    data.frame(
      management_domain_count4 = domain_count,
      conf_low = unname(quantile(standardized_draws, 0.025)),
      conf_high = unname(quantile(standardized_draws, 0.975))
    )
  })
)

prediction_summary <- merge(
  prediction_summary,
  prediction_intervals,
  by = "management_domain_count4",
  sort = TRUE
)

write.csv(
  prediction_summary,
  file.path(table_dir, "predicted_quality_by_management_domain_count4.csv"),
  row.names = FALSE
)

png(
  file.path(figure_dir, "figure03_predicted_quality_by_management_domains.png"),
  width = 1800,
  height = 1200,
  res = 200
)
plot(
  prediction_summary$management_domain_count4,
  prediction_summary$predicted_probability * 100,
  type = "n",
  ylim = c(0, 100),
  xlab = "Number of positive management domains",
  ylab = "Predicted probability of quality antenatal care (%)",
  main = "Predicted quality by four-domain management count"
)
polygon(
  c(
    prediction_summary$management_domain_count4,
    rev(prediction_summary$management_domain_count4)
  ),
  c(
    prediction_summary$conf_low * 100,
    rev(prediction_summary$conf_high * 100)
  ),
  col = adjustcolor("#4F81BD", alpha.f = 0.20),
  border = NA
)
lines(
  prediction_summary$management_domain_count4,
  prediction_summary$predicted_probability * 100,
  type = "b",
  lwd = 2,
  pch = 16,
  col = "#4F81BD"
)
grid()
dev.off()

print(results_domain_count4)
print(results_domain_count4_facility_complexity)
print(results_domain_count4_pharmacy_complexity)
print(results_pharmacy_arrangement)
