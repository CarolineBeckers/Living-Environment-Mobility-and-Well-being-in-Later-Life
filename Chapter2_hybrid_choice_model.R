# ======================================================================
# COMPLETE WORKFLOW — PARTS 1 AND 2
# Data preparation, empty multilevel model and demographic multilevel model
# ======================================================================
#
# Purpose:
#   1. Start from a clean R environment.
#   2. Read MLN_subj_obj_BE.csv.
#   3. Clean missing-value codes such as #NULL!.
#   4. Recode the mode-choice outcome and neighbourhood variable.
#   5. Convert demographic, objective-environment and Likert variables.
#   6. Standardise continuous/ordinal predictors and objective variables.
#   7. Validate the data and export one prepared dataset for later scripts.
#
# This script does NOT yet:
#   - estimate multilevel models;
#   - estimate the CFA;
#   - calculate factor scores;
#   - fit the ICLV benchmark.
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES AND OUTPUT FOLDERS
# ----------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "brms",
  "rstan",
  "posterior",
  "bayesplot"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running the script:\n",
    paste(missing_packages, collapse = "\n"),
    "\n\nUse install.packages(c('dplyr', 'readr', 'purrr', 'tibble', ",
    "'brms', 'rstan', 'posterior', 'bayesplot'))."
  )
}

library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(brms)
library(rstan)
library(posterior)
library(bayesplot)

# Recommended rstan settings.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dir.create("output", showWarnings = FALSE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. USER CONFIGURATION
# ----------------------------------------------------------------------

data_path <- "MLN_subj_obj_BE.csv"

id_var <- "ID"
choice_var <- "Mode"
neighbourhood_var <- "Neighbourhood"

# Expected labels in the original Mode variable.
valid_mode_labels <- c(
  "Private car",
  "On foot",
  "Bike",
  "Public transport"
)

# Private car will be the reference category.
mode_levels <- c(
  "Private car",
  "On foot",
  "Bike",
  "Public transport"
)

continuous_vars <- c(
  "Age",
  "Nr_household",
  "EQ_5D_index"
)

ordinal_control_vars <- c(
  "Edu_level",
  "Income"
)

objective_vars_original <- c(
  "FAC_presence_walking_infr",
  "FAC_presence_difficult_surface",
  "FAC_presence_greenery",
  "FAC_presence_park_equipment",
  "FAC_presence_cycling_infrastructure",
  "FAC_rate_accidents",
  "FAC_presence_primary_secondary_roads",
  "FAC_presence_high_parking_stress",
  "FAC_presence_moderate_parking_stress",
  "FAC_presence_PT"
)

latent_blocks <- list(
  Poor_walking_infrastructure = c(
    "Walkable_with_walker_or_wheelchair",
    "Maintained_sidewalks",
    "Little_dirsuption_on_sidewalks",
    "Easy_to_cross_streets"
  ),
  
  No_walking_friendly_environment = c(
    "Walking_friendly_neighborhood",
    "Enough_benches",
    "Enough_greenery",
    "Public_spaces_are_pleasant_meeting_places"
  ),
  
  High_quality_cycling_infrastructure = c(
    "Qualitative_cycling_lanes",
    "Maintained_cycling_infrastructure",
    "Sufficient_cycling_infrastructure",
    "Maintained_streets_and_squares"
  ),
  
  Cycling_friendly_environment = c(
    "Cycling_friendly_neighborhood",
    "Enjoyable_cycling",
    "Feeling_safe_in_traffic_as_cyclist",
    "Busy_traffic_causing_unsafe_cycling_experiences"
  ),
  
  Busy_traffic = c(
    "Lot_of_traffic",
    "Lot_of_freight_traffic",
    "Nuisance_due_to_traffic",
    "Few_traffic_accidents"
  ),
  
  Traffic_safety = c(
    "Enough_public_lighting",
    "Feeling_safe_in_traffic_as_pedestrian",
    "Feeling_safe_in_traffic_as_car_user",
    "Feeling_safe_on_public_transport"
  ),
  
  Negative_parking_experiences = c(
    "Accessible_by_car",
    "Easily_finding_parking_spot_residents",
    "Easily_finding_parking_spot_visitors"
  ),
  
  High_quality_public_transport = c(
    "Satisfied_public_transport",
    "Enough_public_transport",
    "Sufficiently_frequent_public_transport",
    "Well_equipped_public_transport_stops",
    "Accessible_public_transport_stops",
    "Enough_seats_public_transport",
    "Easy_to_find_information_on_PT_for_elderly"
  )
)

indicator_vars <- unique(
  unlist(latent_blocks, use.names = FALSE)
)

likert_min <- 1L
likert_max <- 5L

# Add item names here only when an item must be reverse-coded.
# Leave empty when all indicators already have the intended direction.
reverse_items <- character(0)

# ----------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ----------------------------------------------------------------------

clean_missing_codes <- function(x) {
  x <- trimws(as.character(x))
  
  x[x %in% c(
    "",
    "NA",
    "N/A",
    "NULL",
    "#NULL!",
    ".",
    "-",
    "999",
    "9999"
  )] <- NA_character_
  
  x
}

convert_to_numeric <- function(x) {
  x <- clean_missing_codes(x)
  
  readr::parse_number(
    x,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = "."
    ),
    na = c(
      "",
      "NA",
      "N/A",
      "NULL",
      "#NULL!"
    )
  )
}

standardise_variable <- function(x, variable_name = NULL) {
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- stats::sd(x, na.rm = TRUE)
  
  if (
    is.nan(x_mean) ||
    is.na(x_sd) ||
    x_sd == 0
  ) {
    warning(
      "Variable ",
      ifelse(is.null(variable_name), "", paste0("'", variable_name, "' ")),
      "contains no usable variation and could not be standardised."
    )
    
    return(rep(NA_real_, length(x)))
  }
  
  as.numeric((x - x_mean) / x_sd)
}

make_variable_check <- function(data, variables) {
  purrr::map_dfr(
    variables,
    function(variable_name) {
      x <- data[[variable_name]]
      
      tibble::tibble(
        variable = variable_name,
        class = class(x)[1],
        n = length(x),
        n_missing = sum(is.na(x)),
        n_unique = dplyr::n_distinct(x, na.rm = TRUE),
        minimum = if (all(is.na(x))) NA_real_ else suppressWarnings(min(x, na.rm = TRUE)),
        maximum = if (all(is.na(x))) NA_real_ else suppressWarnings(max(x, na.rm = TRUE))
      )
    }
  )
}

# ----------------------------------------------------------------------
# 3. READ THE ORIGINAL DATA
# ----------------------------------------------------------------------

if (!file.exists(data_path)) {
  stop(
    "The input file was not found:\n",
    normalizePath(data_path, mustWork = FALSE)
  )
}

database_raw <- readr::read_csv2(
  data_path,
  show_col_types = FALSE,
  na = c(
    "",
    "NA",
    "N/A",
    "NULL",
    "#NULL!"
  )
)

message(
  "Original dataset: ",
  nrow(database_raw),
  " rows and ",
  ncol(database_raw),
  " columns."
)

# ----------------------------------------------------------------------
# 4. CHECK WHETHER ALL REQUIRED ORIGINAL VARIABLES EXIST
# ----------------------------------------------------------------------

required_original_vars <- unique(c(
  id_var,
  choice_var,
  neighbourhood_var,
  "Gender",
  continuous_vars,
  ordinal_control_vars,
  objective_vars_original,
  indicator_vars
))

missing_original_vars <- setdiff(
  required_original_vars,
  names(database_raw)
)

if (length(missing_original_vars) > 0) {
  stop(
    "The following required variables are absent from the original data:\n",
    paste(missing_original_vars, collapse = "\n")
  )
}

# ----------------------------------------------------------------------
# 5. CLEAN AND CONVERT NUMERIC VARIABLES
# ----------------------------------------------------------------------

numeric_source_vars <- unique(c(
  "Gender",
  continuous_vars,
  ordinal_control_vars,
  objective_vars_original,
  indicator_vars
))

database_clean <- database_raw %>%
  mutate(
    across(
      all_of(numeric_source_vars),
      convert_to_numeric
    )
  )

# ----------------------------------------------------------------------
# 6. CHECK AND, IF NECESSARY, REVERSE LIKERT INDICATORS
# ----------------------------------------------------------------------

unknown_reverse_items <- setdiff(
  reverse_items,
  indicator_vars
)

if (length(unknown_reverse_items) > 0) {
  stop(
    "The following reverse-coded items are not part of latent_blocks:\n",
    paste(unknown_reverse_items, collapse = "\n")
  )
}

if (length(reverse_items) > 0) {
  database_clean <- database_clean %>%
    mutate(
      across(
        all_of(reverse_items),
        ~ (likert_min + likert_max) - .x
      )
    )
}

indicator_check <- make_variable_check(
  database_clean,
  indicator_vars
)

write_csv(
  indicator_check,
  "output/tables/indicator_range_and_missingness.csv"
)

invalid_indicator_ranges <- indicator_check %>%
  filter(
    !is.na(minimum),
    !is.na(maximum),
    minimum < likert_min |
      maximum > likert_max
  )

if (nrow(invalid_indicator_ranges) > 0) {
  print(invalid_indicator_ranges)
  
  stop(
    "At least one Likert indicator contains values outside ",
    likert_min,
    "–",
    likert_max,
    "."
  )
}

no_variation_indicators <- indicator_check %>%
  filter(n_unique < 2)

if (nrow(no_variation_indicators) > 0) {
  print(no_variation_indicators)
  
  stop(
    "At least one Likert indicator contains no usable variation."
  )
}

# ----------------------------------------------------------------------
# 7. RECODE DEMOGRAPHIC VARIABLES
# ----------------------------------------------------------------------

unexpected_gender_values <- setdiff(
  unique(stats::na.omit(database_clean$Gender)),
  c(1, 2)
)

if (length(unexpected_gender_values) > 0) {
  stop(
    "Unexpected values found in Gender:\n",
    paste(unexpected_gender_values, collapse = "\n")
  )
}

database_clean <- database_clean %>%
  mutate(
    Gender_factor = factor(
      Gender,
      levels = c(1, 2),
      labels = c("Woman", "Man")
    ),
    
    # Numeric dummy for models that use dummy coding.
    GenderMan = case_when(
      Gender == 1 ~ 0,
      Gender == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Retain ordered factors for descriptive/sensitivity purposes.
    Edu_level_ordered = ordered(Edu_level),
    Income_ordered = ordered(Income)
  )

# ----------------------------------------------------------------------
# 8. STANDARDISE MODEL PREDICTORS
# ----------------------------------------------------------------------
#
# The main multilevel models will use:
#   Age_z, GenderMan, Edu_level_z, Nr_household_z,
#   EQ_5D_index_z and Income_z.
#
# Edu_level and Income are ordinal. Standardising them treats their categories
# as approximately equally spaced, consistent with the earlier specifications.
# The ordered versions are retained for sensitivity analyses.
# ----------------------------------------------------------------------

variables_to_standardise <- unique(c(
  continuous_vars,
  ordinal_control_vars,
  objective_vars_original
))

for (variable_name in variables_to_standardise) {
  new_name <- paste0(variable_name, "_z")
  
  database_clean[[new_name]] <- standardise_variable(
    database_clean[[variable_name]],
    variable_name = variable_name
  )
}

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- paste0(
  objective_vars_original,
  "_z"
)

# ----------------------------------------------------------------------
# 9. PREPARE OUTCOME, RESPONDENT ID AND NEIGHBOURHOOD
# ----------------------------------------------------------------------

if (anyNA(database_clean[[id_var]])) {
  stop("The respondent ID variable contains missing values.")
}

duplicate_ids <- database_clean %>%
  count(.data[[id_var]], name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_ids) > 0) {
  print(duplicate_ids)
  
  stop(
    "The dataset contains duplicate respondent IDs. ",
    "The current workflow assumes one row per respondent."
  )
}

database <- database_clean %>%
  mutate(
    respondent_id_internal =
      as.integer(factor(.data[[id_var]])),
    
    choice_original =
      clean_missing_codes(.data[[choice_var]]),
    
    # Treat the old code '0' as a missing mode choice.
    choice_original =
      na_if(choice_original, "0"),
    
    Mode_factor = factor(
      choice_original,
      levels = mode_levels
    ),
    
    # brms uses the first factor level as the reference category.
    Mode_factor = relevel(
      Mode_factor,
      ref = "Private car"
    ),
    
    Neighbourhood_factor = factor(
      trimws(as.character(.data[[neighbourhood_var]]))
    )
  )

unmapped_modes <- database %>%
  filter(
    !is.na(choice_original),
    is.na(Mode_factor)
  ) %>%
  distinct(choice_original) %>%
  pull(choice_original)

if (length(unmapped_modes) > 0) {
  stop(
    "The following mode labels are not recognised:\n",
    paste(unmapped_modes, collapse = "\n")
  )
}

if (anyNA(database$Neighbourhood_factor)) {
  stop(
    "Neighbourhood contains missing values. ",
    "Resolve these before estimating the multilevel models."
  )
}
database <- database %>%
  filter(!is.na(Neighbourhood_factor)) %>%
  droplevels()

# ----------------------------------------------------------------------
# 10. FINAL VALIDATION
# ----------------------------------------------------------------------

required_prepared_vars <- unique(c(
  "respondent_id_internal",
  "Mode_factor",
  "Neighbourhood_factor",
  control_vars,
  objective_vars,
  indicator_vars
))

missing_prepared_vars <- setdiff(
  required_prepared_vars,
  names(database)
)

if (length(missing_prepared_vars) > 0) {
  stop(
    "The following prepared variables are missing:\n",
    paste(missing_prepared_vars, collapse = "\n")
  )
}

numeric_prepared_vars <- c(
  control_vars,
  objective_vars,
  indicator_vars
)

non_numeric_prepared_vars <- numeric_prepared_vars[
  !vapply(
    database[numeric_prepared_vars],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_prepared_vars) > 0) {
  stop(
    "The following prepared variables are not numeric:\n",
    paste(non_numeric_prepared_vars, collapse = "\n")
  )
}

# ----------------------------------------------------------------------
# 11. DESCRIPTIVE CHECKS AND EXPORTS
# ----------------------------------------------------------------------

choice_distribution <- database %>%
  count(
    Mode_factor,
    name = "n",
    .drop = FALSE
  ) %>%
  mutate(
    share = n / sum(n)
  )

neighbourhood_sizes <- database %>%
  count(
    Neighbourhood_factor,
    name = "n",
    .drop = FALSE
  ) %>%
  arrange(n)

variable_check <- make_variable_check(
  database,
  unique(c(
    "Gender",
    continuous_vars,
    ordinal_control_vars,
    objective_vars_original,
    control_vars,
    objective_vars
  ))
)

missingness_overview <- tibble(
  variable = required_prepared_vars,
  n_missing = vapply(
    database[required_prepared_vars],
    function(x) sum(is.na(x)),
    integer(1)
  ),
  percentage_missing = 100 * n_missing / nrow(database)
) %>%
  arrange(desc(percentage_missing))

write_csv(
  choice_distribution,
  "output/tables/choice_distribution.csv"
)

write_csv(
  neighbourhood_sizes,
  "output/tables/neighbourhood_sizes.csv"
)

write_csv(
  variable_check,
  "output/tables/prepared_variable_check.csv"
)

write_csv(
  missingness_overview,
  "output/tables/prepared_missingness_overview.csv"
)

# Save the complete prepared dataset. Complete-case samples will be created
# separately for each model in later scripts.
saveRDS(
  database,
  "output/data/prepared_multilevel_mode_choice_data.rds"
)

write_csv(
  database,
  "output/data/prepared_multilevel_mode_choice_data.csv"
)

# ----------------------------------------------------------------------
# 12. FINAL SCREEN OUTPUT
# ----------------------------------------------------------------------

message("\nData preparation completed successfully.")
message("Prepared rows: ", nrow(database))
message("Valid mode choices: ", sum(!is.na(database$Mode_factor)))
message("Missing mode choices: ", sum(is.na(database$Mode_factor)))
message("Number of neighbourhoods: ", nlevels(database$Neighbourhood_factor))
message(
  "Neighbourhood size range: ",
  min(neighbourhood_sizes$n),
  "–",
  max(neighbourhood_sizes$n)
)

print(choice_distribution)
print(neighbourhood_sizes)

# ======================================================================
# PART 1 COMPLETED — THE PREPARED DATASET HAS BEEN SAVED
# ======================================================================

# ======================================================================
# PART 2 — EMPTY AND DEMOGRAPHIC MULTILEVEL MULTINOMIAL MODELS
# Bayesian categorical-logit models in brms using the rstan backend
# ======================================================================
#
# The dataset is deliberately reloaded from the RDS file created above.
# This guarantees that Part 2 uses Mode_factor and Neighbourhood_factor
# exactly as prepared in Part 1, rather than objects left in memory.
# ======================================================================

prepared_data_path <-
  "output/data/prepared_multilevel_mode_choice_data.rds"

if (!file.exists(prepared_data_path)) {
  stop(
    "Prepared dataset not found at:\n",
    prepared_data_path,
    "\nPart 1 did not create the expected RDS file."
  )
}

database <- readRDS(prepared_data_path)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

required_vars <- c(
  "respondent_id_internal",
  "Mode_factor",
  "Neighbourhood_factor",
  control_vars
)

missing_vars <- setdiff(
  required_vars,
  names(database)
)

if (length(missing_vars) > 0) {
  stop(
    "The following variables are absent from the prepared dataset:\n",
    paste(missing_vars, collapse = "\n"),
    "\n\nThis indicates that Part 1 and Part 2 are not using the same ",
    "prepared file."
  )
}

# Reconfirm the factor structure and reference category.
database <- database %>%
  mutate(
    Mode_factor = factor(
      Mode_factor,
      levels = c(
        "Private car",
        "On foot",
        "Bike",
        "Public transport"
      )
    ),
    Mode_factor = relevel(
      Mode_factor,
      ref = "Private car"
    ),
    Neighbourhood_factor = droplevels(
      factor(Neighbourhood_factor)
    )
  )

if (levels(database$Mode_factor)[1] != "Private car") {
  stop("Private car is not the reference category of Mode_factor.")
}

# Sampling settings
n_chains <- 4L
n_iter <- 2000L
n_warmup <- 1000L
adapt_delta_value <- 0.95
max_treedepth_value <- 12L
model_seed <- 1234L

# With rstan, one core per chain is appropriate.
n_cores <- min(
  n_chains,
  max(1L, parallel::detectCores())
)


# ----------------------------------------------------------------------
# 2. CREATE ONE COMMON ANALYSIS SAMPLE
# ----------------------------------------------------------------------
#
# ML0 and ML1 must use exactly the same respondents so changes in sigma,
# variance and ICC cannot be caused by differences in sample composition.
# ----------------------------------------------------------------------

analysis_sample <- database %>%
  filter(
    if_all(
      all_of(c(
        "Mode_factor",
        "Neighbourhood_factor",
        control_vars
      )),
      ~ !is.na(.x)
    )
  ) %>%
  droplevels() %>%
  arrange(respondent_id_internal)

if (nrow(analysis_sample) == 0) {
  stop("No complete observations remain for Part 2.")
}

if (nlevels(analysis_sample$Neighbourhood_factor) < 2) {
  stop("Fewer than two neighbourhoods remain in the analysis sample.")
}

sample_overview <- tibble(
  analysis = c(
    "Original prepared dataset",
    "Common ML0/ML1 complete-case sample"
  ),
  n_respondents = c(
    nrow(database),
    nrow(analysis_sample)
  ),
  n_neighbourhoods = c(
    nlevels(database$Neighbourhood_factor),
    nlevels(analysis_sample$Neighbourhood_factor)
  )
)

neighbourhood_sizes_part2 <- analysis_sample %>%
  count(
    Neighbourhood_factor,
    name = "n"
  ) %>%
  arrange(n)

choice_distribution_part2 <- analysis_sample %>%
  count(
    Mode_factor,
    name = "n",
    .drop = FALSE
  ) %>%
  mutate(
    share = n / sum(n)
  )

write_csv(
  sample_overview,
  "output/tables/part2_sample_overview.csv"
)

write_csv(
  neighbourhood_sizes_part2,
  "output/tables/part2_neighbourhood_sizes.csv"
)

write_csv(
  choice_distribution_part2,
  "output/tables/part2_choice_distribution.csv"
)

saveRDS(
  analysis_sample,
  "output/data/part2_common_multilevel_sample.rds"
)

message(
  "Part 2 common sample: n = ",
  nrow(analysis_sample),
  "; neighbourhoods = ",
  nlevels(analysis_sample$Neighbourhood_factor),
  "."
)

# ----------------------------------------------------------------------
# 3. MODEL FORMULAS
# ----------------------------------------------------------------------

formula_ml0 <- bf(
  Mode_factor ~ 1 + (1 | Neighbourhood_factor)
)

formula_ml1 <- bf(
  Mode_factor ~
    Age_z +
    GenderMan +
    Edu_level_z +
    Nr_household_z +
    EQ_5D_index_z +
    Income_z +
    (1 | Neighbourhood_factor)
)

categorical_family <- categorical(
  link = "logit",
  refcat = "Private car"
)

# ----------------------------------------------------------------------
# 4. PRIORS
# ----------------------------------------------------------------------
#
# Predictors were standardised in Part 1. These are weakly informative
# priors on the log-odds scale.
# ----------------------------------------------------------------------

prior_table_ml0 <- get_prior(
  formula = formula_ml0,
  data = analysis_sample,
  family = categorical_family
)

prior_table_ml1 <- get_prior(
  formula = formula_ml1,
  data = analysis_sample,
  family = categorical_family
)

print(prior_table_ml0)
print(prior_table_ml1)

write_csv(
  prior_table_ml0,
  "output/tables/ML0_available_priors.csv"
)

write_csv(
  prior_table_ml1,
  "output/tables/ML1_available_priors.csv"
)

# ----------------------------------------------------------------------
# Construct category-specific priors
# ----------------------------------------------------------------------

get_model_dpars <- function(prior_table) {
  
  dpars <- unique(
    prior_table$dpar[
      !is.na(prior_table$dpar) &
        prior_table$dpar != ""
    ]
  )
  
  if (length(dpars) == 0) {
    stop(
      "No category-specific dpar names were found in get_prior()."
    )
  }
  
  dpars
}


dpars_ml0 <- get_model_dpars(prior_table_ml0)
dpars_ml1 <- get_model_dpars(prior_table_ml1)

message(
  "ML0 category-specific parameters: ",
  paste(dpars_ml0, collapse = ", ")
)

message(
  "ML1 category-specific parameters: ",
  paste(dpars_ml1, collapse = ", ")
)

model_priors_ml0 <- do.call(
  c,
  lapply(
    dpars_ml0,
    function(current_dpar) {
      
      c(
        set_prior(
          "student_t(3, 0, 2.5)",
          class = "Intercept",
          dpar = current_dpar
        ),
        
        set_prior(
          "exponential(1)",
          class = "sd",
          group = "Neighbourhood_factor",
          dpar = current_dpar
        )
      )
    }
  )
)
model_priors_ml1 <- do.call(
  c,
  lapply(
    dpars_ml1,
    function(current_dpar) {
      
      c(
        set_prior(
          "normal(0, 1)",
          class = "b",
          dpar = current_dpar
        ),
        
        set_prior(
          "student_t(3, 0, 2.5)",
          class = "Intercept",
          dpar = current_dpar
        ),
        
        set_prior(
          "exponential(1)",
          class = "sd",
          group = "Neighbourhood_factor",
          dpar = current_dpar
        )
      )
    }
  )
)
# Save the actual prior tables recognised by brms. This is useful for
# diagnosing a prior-name mismatch before model estimation.
prior_table_ml0 <- get_prior(
  formula = formula_ml0,
  data = analysis_sample,
  family = categorical_family
)

prior_table_ml1 <- get_prior(
  formula = formula_ml1,
  data = analysis_sample,
  family = categorical_family
)

write_csv(
  prior_table_ml0,
  "output/tables/ML0_available_priors.csv"
)

write_csv(
  prior_table_ml1,
  "output/tables/ML1_available_priors.csv"
)

validate_prior(
  prior = model_priors_ml0,
  formula = formula_ml0,
  data = analysis_sample,
  family = categorical_family
)

validate_prior(
  prior = model_priors_ml1,
  formula = formula_ml1,
  data = analysis_sample,
  family = categorical_family
)

# ----------------------------------------------------------------------
# 5. ESTIMATE ML0: EMPTY MULTILEVEL CATEGORICAL MODEL
# ----------------------------------------------------------------------

fit_ml0_empty <- brm(
  formula = formula_ml0,
  data = analysis_sample,
  family = categorical_family,
  prior = model_priors_ml0,
  backend = "rstan",
  chains = n_chains,
  iter = n_iter,
  warmup = n_warmup,
  cores = n_cores,
  seed = model_seed,
  control = list(
    adapt_delta = adapt_delta_value,
    max_treedepth = max_treedepth_value
  ),
  save_pars = save_pars(all = TRUE),
  file = "output/models/ML0_empty_neighbourhood",
  file_refit = "on_change",
  refresh = 100
)

saveRDS(
  fit_ml0_empty,
  "output/models/ML0_empty_neighbourhood.rds"
)

# ----------------------------------------------------------------------
# 6. ESTIMATE ML1: DEMOGRAPHICS + RANDOM NEIGHBOURHOOD INTERCEPT
# ----------------------------------------------------------------------

fit_ml1_demographics <- brm(
  formula = formula_ml1,
  data = analysis_sample,
  family = categorical_family,
  prior = model_priors_ml1,
  backend = "rstan",
  chains = n_chains,
  iter = n_iter,
  warmup = n_warmup,
  cores = n_cores,
  seed = model_seed,
  control = list(
    adapt_delta = adapt_delta_value,
    max_treedepth = max_treedepth_value
  ),
  save_pars = save_pars(all = TRUE),
  file = "output/models/ML1_demographics_neighbourhood",
  file_refit = "on_change",
  refresh = 100
)

saveRDS(
  fit_ml1_demographics,
  "output/models/ML1_demographics_neighbourhood.rds"
)

# ----------------------------------------------------------------------
# 7. DIAGNOSTIC HELPERS
# ----------------------------------------------------------------------
extract_sampler_diagnostics <- function(
    model,
    model_name
) {
  
  if (!inherits(model, "brmsfit")) {
    stop(
      model_name,
      " is not a brmsfit object. Current class: ",
      paste(class(model), collapse = ", ")
    )
  }
  
  nuts <- brms::nuts_params(model)
  
  # Use the brms S3 method explicitly.
  model_draws <- brms::as_draws_array(model)
  
  draw_summary <- posterior::summarise_draws(
    model_draws,
    posterior::default_convergence_measures()
  )
  
  tibble::tibble(
    model = model_name,
    
    divergent_transitions = sum(
      nuts$Parameter == "divergent__" &
        nuts$Value == 1
    ),
    
    maximum_treedepth_hits = sum(
      nuts$Parameter == "treedepth__" &
        nuts$Value >= max_treedepth_value
    ),
    
    minimum_bulk_ess = min(
      draw_summary$ess_bulk,
      na.rm = TRUE
    ),
    
    minimum_tail_ess = min(
      draw_summary$ess_tail,
      na.rm = TRUE
    ),
    
    maximum_rhat = max(
      draw_summary$rhat,
      na.rm = TRUE
    )
  )
}

diagnostics_table <- bind_rows(
  extract_sampler_diagnostics(
    fit_ml0_empty,
    "ML0_empty_neighbourhood"
  ),
  extract_sampler_diagnostics(
    fit_ml1_demographics,
    "ML1_demographics_neighbourhood"
  )
)

write_csv(
  diagnostics_table,
  "output/tables/ML0_ML1_sampler_diagnostics.csv"
)

print(diagnostics_table)

# ----------------------------------------------------------------------
# 8. EXTRACT RANDOM-INTERCEPT SD, VARIANCE AND APPROXIMATE ICC
# ----------------------------------------------------------------------

clean_contrast_name <- function(parameter_name) {
  output <- parameter_name
  
  output <- sub(
    "^sd_Neighbourhood_factor__",
    "",
    output
  )
  
  output <- sub(
    "_Intercept$",
    "",
    output
  )
  
  output <- sub(
    "^mu",
    "",
    output
  )
  
  output
}

extract_neighbourhood_variance <- function(
    model,
    model_name,
    credible_probability = 0.95
) {
  draws <- posterior::as_draws_df(model)
  
  sd_parameters <- grep(
    "^sd_Neighbourhood_factor__.*_Intercept$",
    names(draws),
    value = TRUE
  )
  
  if (length(sd_parameters) == 0) {
    stop(
      "No neighbourhood random-intercept SD parameters were found in ",
      model_name,
      ". Inspect variables(model) to check the brms parameter names."
    )
  }
  
  alpha <- (1 - credible_probability) / 2
  
  purrr::map_dfr(
    sd_parameters,
    function(parameter_name) {
      sd_draws <- as.numeric(draws[[parameter_name]])
      variance_draws <- sd_draws^2
      
      # Approximate latent-logit ICC.
      icc_draws <- variance_draws / (
        variance_draws + (pi^2 / 3)
      )
      
      tibble(
        model = model_name,
        parameter = parameter_name,
        contrast_vs_private_car =
          clean_contrast_name(parameter_name),
        
        sigma_mean = mean(sd_draws),
        sigma_median = median(sd_draws),
        sigma_lower = quantile(
          sd_draws,
          probs = alpha,
          names = FALSE
        ),
        sigma_upper = quantile(
          sd_draws,
          probs = 1 - alpha,
          names = FALSE
        ),
        
        variance_mean = mean(variance_draws),
        variance_median = median(variance_draws),
        variance_lower = quantile(
          variance_draws,
          probs = alpha,
          names = FALSE
        ),
        variance_upper = quantile(
          variance_draws,
          probs = 1 - alpha,
          names = FALSE
        ),
        
        ICC_mean = mean(icc_draws),
        ICC_median = median(icc_draws),
        ICC_lower = quantile(
          icc_draws,
          probs = alpha,
          names = FALSE
        ),
        ICC_upper = quantile(
          icc_draws,
          probs = 1 - alpha,
          names = FALSE
        )
      )
    }
  )
}

random_effects_ml0 <- extract_neighbourhood_variance(
  fit_ml0_empty,
  "ML0_empty_neighbourhood"
)

random_effects_ml1 <- extract_neighbourhood_variance(
  fit_ml1_demographics,
  "ML1_demographics_neighbourhood"
)

random_effects_comparison <- bind_rows(
  random_effects_ml0,
  random_effects_ml1
)

write_csv(
  random_effects_ml0,
  "output/tables/ML0_neighbourhood_sigma_variance_ICC.csv"
)

write_csv(
  random_effects_ml1,
  "output/tables/ML1_neighbourhood_sigma_variance_ICC.csv"
)

write_csv(
  random_effects_comparison,
  "output/tables/ML0_ML1_neighbourhood_variance_comparison.csv"
)

# ----------------------------------------------------------------------
# 9. PROPORTIONAL CHANGE IN VARIANCE
# ----------------------------------------------------------------------
#
# PCV = (variance_ML0 - variance_ML1) / variance_ML0
#
# Positive values indicate a reduction in residual neighbourhood variance
# after controlling for demographics.
# ----------------------------------------------------------------------

posterior_draws_ml0 <- posterior::as_draws_df(fit_ml0_empty)
posterior_draws_ml1 <- posterior::as_draws_df(fit_ml1_demographics)

sd_names_ml0 <- grep(
  "^sd_Neighbourhood_factor__.*_Intercept$",
  names(posterior_draws_ml0),
  value = TRUE
)

sd_names_ml1 <- grep(
  "^sd_Neighbourhood_factor__.*_Intercept$",
  names(posterior_draws_ml1),
  value = TRUE
)

contrast_map_ml0 <- setNames(
  sd_names_ml0,
  vapply(
    sd_names_ml0,
    clean_contrast_name,
    character(1)
  )
)

contrast_map_ml1 <- setNames(
  sd_names_ml1,
  vapply(
    sd_names_ml1,
    clean_contrast_name,
    character(1)
  )
)

common_contrasts <- intersect(
  names(contrast_map_ml0),
  names(contrast_map_ml1)
)

if (length(common_contrasts) == 0) {
  stop(
    "No matching random-effect contrasts were found between ML0 and ML1."
  )
}

pcv_table <- purrr::map_dfr(
  common_contrasts,
  function(contrast_name) {
    variance_ml0_draws <-
      as.numeric(
        posterior_draws_ml0[[
          contrast_map_ml0[[contrast_name]]
        ]]
      )^2
    
    variance_ml1_draws <-
      as.numeric(
        posterior_draws_ml1[[
          contrast_map_ml1[[contrast_name]]
        ]]
      )^2
    
    # The two independently fitted models have the same number of posterior
    # draws under the settings above. If not, use the shared minimum length.
    n_common_draws <- min(
      length(variance_ml0_draws),
      length(variance_ml1_draws)
    )
    
    variance_ml0_draws <-
      variance_ml0_draws[seq_len(n_common_draws)]
    
    variance_ml1_draws <-
      variance_ml1_draws[seq_len(n_common_draws)]
    
    pcv_draws <- (
      variance_ml0_draws -
        variance_ml1_draws
    ) / variance_ml0_draws
    
    tibble(
      contrast_vs_private_car = contrast_name,
      variance_ML0_mean = mean(variance_ml0_draws),
      variance_ML1_mean = mean(variance_ml1_draws),
      PCV_mean = mean(pcv_draws),
      PCV_median = median(pcv_draws),
      PCV_lower = quantile(
        pcv_draws,
        0.025,
        names = FALSE
      ),
      PCV_upper = quantile(
        pcv_draws,
        0.975,
        names = FALSE
      )
    )
  }
)

write_csv(
  pcv_table,
  "output/tables/ML0_ML1_proportional_change_in_variance.csv"
)

print(random_effects_comparison)
print(pcv_table)

# ----------------------------------------------------------------------
# 10. FIXED-EFFECT TABLE FOR ML1
# ----------------------------------------------------------------------

fixed_effects_ml1 <- as.data.frame(
  brms::fixef(
    fit_ml1_demographics,
    probs = c(0.025, 0.975)
  )
) %>%
  rownames_to_column("parameter") %>%
  as_tibble() %>%
  rename(
    estimate = Estimate,
    estimate_error = Est.Error,
    lower_95 = Q2.5,
    upper_95 = Q97.5
  ) %>%
  mutate(
    odds_ratio = exp(estimate),
    odds_ratio_lower_95 = exp(lower_95),
    odds_ratio_upper_95 = exp(upper_95),
    credible_nonzero =
      lower_95 > 0 |
      upper_95 < 0
  )

write_csv(
  fixed_effects_ml1,
  "output/tables/ML1_demographic_fixed_effects.csv"
)

# ----------------------------------------------------------------------
# 11. POSTERIOR PREDICTIVE CHECKS
# ----------------------------------------------------------------------

png(
  "output/figures/ML0_posterior_predictive_check.png",
  width = 1800,
  height = 1200,
  res = 180
)

print(
  pp_check(
    fit_ml0_empty,
    type = "bars",
    ndraws = 200
  )
)

dev.off()

png(
  "output/figures/ML1_posterior_predictive_check.png",
  width = 1800,
  height = 1200,
  res = 180
)

print(
  pp_check(
    fit_ml1_demographics,
    type = "bars",
    ndraws = 200
  )
)

dev.off()

# ----------------------------------------------------------------------
# 12. OPTIONAL MODEL FIT USING LOO
# ----------------------------------------------------------------------
#
# ML0 and ML1 use the same outcome and the same sample, so their predictive
# performance can be compared using LOO. This is not needed to calculate the
# ICC but is useful for evaluating the demographic model.
# ----------------------------------------------------------------------

fit_ml0_empty <- add_criterion(
  fit_ml0_empty,
  criterion = "loo"
)

fit_ml1_demographics <- add_criterion(
  fit_ml1_demographics,
  criterion = "loo"
)

loo_comparison <- loo_compare(
  fit_ml0_empty$criteria$loo,
  fit_ml1_demographics$criteria$loo
)

loo_comparison_table <- as.data.frame(
  loo_comparison
) %>%
  tibble::rownames_to_column(
    var = "model_name"
  ) %>%
  tibble::as_tibble()

write_csv(
  loo_comparison_table,
  "output/tables/ML0_ML1_LOO_comparison.csv"
)

# Resave models including their LOO criterion.
saveRDS(
  fit_ml0_empty,
  "output/models/ML0_empty_neighbourhood.rds"
)

saveRDS(
  fit_ml1_demographics,
  "output/models/ML1_demographics_neighbourhood.rds"
)

# ----------------------------------------------------------------------
# 13. FINAL SCREEN OUTPUT
# ----------------------------------------------------------------------

message("\nPart 2 completed.")
message(
  "Inspect these files before continuing:\n",
  "  output/tables/ML0_ML1_sampler_diagnostics.csv\n",
  "  output/tables/ML0_ML1_neighbourhood_variance_comparison.csv\n",
  "  output/tables/ML0_ML1_proportional_change_in_variance.csv\n",
  "  output/tables/ML1_demographic_fixed_effects.csv\n",
  "  output/figures/ML0_posterior_predictive_check.png\n",
  "  output/figures/ML1_posterior_predictive_check.png"
)

# ======================================================================
# END OF PART 2
# ======================================================================
# ======================================================================
# PART 3 — CFA AND FACTOR SCORES FOR THE PERCEIVED ENVIRONMENT
# lavaan WLSMV measurement model + lavPredict factor scores
# ======================================================================
#
# Purpose:
#   1. Re-estimate the eight-factor CFA for the perceived environment.
#   2. Evaluate model fit, factor loadings, latent correlations, reliability,
#      convergent validity and discriminant validity.
#   3. Generate individual factor scores using lavPredict(method = "EBM").
#   4. Standardise the factor scores.
#   5. Merge them safely back into the prepared multilevel dataset.
#
# IMPORTANT:
#   - The 34 indicators are treated as ordered five-point Likert variables.
#   - WLSMV with theta parameterisation is used.
#   - Factor scores are generated only for respondents with complete data on
#     all CFA indicators. This avoids row-matching errors and ensures that
#     every respondent receives scores for all eight constructs.
#   - The original prepared dataset is not overwritten. A new RDS file with
#     factor scores is created for Part 4.
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES, PATHS AND OUTPUT FOLDERS
# ----------------------------------------------------------------------

required_packages <- c(
  "lavaan",
  "semTools",
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "tidyr",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Part 3:\n",
    paste(missing_packages, collapse = "\n"),
    "\n\nUse install.packages(c('lavaan', 'semTools', 'dplyr', ",
    "'readr', 'purrr', 'tibble', 'tidyr', 'ggplot2'))."
  )
}

library(lavaan)
library(semTools)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)

prepared_data_path <-
  "output/data/prepared_multilevel_mode_choice_data.rds"

dir.create("output", showWarnings = FALSE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(prepared_data_path)) {
  stop(
    "Prepared dataset not found at:\n",
    prepared_data_path,
    "\nRun Part 1 first."
  )
}

# ----------------------------------------------------------------------
# 1. DEFINE THE EIGHT-FACTOR MEASUREMENT MODEL
# ----------------------------------------------------------------------

latent_blocks <- list(
  Poor_walking_infrastructure = c(
    "Walkable_with_walker_or_wheelchair",
    "Maintained_sidewalks",
    "Little_dirsuption_on_sidewalks",
    "Easy_to_cross_streets"
  ),
  
  No_walking_friendly_environment = c(
    "Walking_friendly_neighborhood",
    "Enough_benches",
    "Enough_greenery",
    "Public_spaces_are_pleasant_meeting_places"
  ),
  
  High_quality_cycling_infrastructure = c(
    "Qualitative_cycling_lanes",
    "Maintained_cycling_infrastructure",
    "Sufficient_cycling_infrastructure",
    "Maintained_streets_and_squares"
  ),
  
  Cycling_friendly_environment = c(
    "Cycling_friendly_neighborhood",
    "Enjoyable_cycling",
    "Feeling_safe_in_traffic_as_cyclist",
    "Busy_traffic_causing_unsafe_cycling_experiences"
  ),
  
  Busy_traffic = c(
    "Lot_of_traffic",
    "Lot_of_freight_traffic",
    "Nuisance_due_to_traffic",
    "Few_traffic_accidents"
  ),
  
  Traffic_safety = c(
    "Enough_public_lighting",
    "Feeling_safe_in_traffic_as_pedestrian",
    "Feeling_safe_in_traffic_as_car_user",
    "Feeling_safe_on_public_transport"
  ),
  
  Negative_parking_experiences = c(
    "Accessible_by_car",
    "Easily_finding_parking_spot_residents",
    "Easily_finding_parking_spot_visitors"
  ),
  
  High_quality_public_transport = c(
    "Satisfied_public_transport",
    "Enough_public_transport",
    "Sufficiently_frequent_public_transport",
    "Well_equipped_public_transport_stops",
    "Accessible_public_transport_stops",
    "Enough_seats_public_transport",
    "Easy_to_find_information_on_PT_for_elderly"
  )
)

indicator_vars <- unique(
  unlist(latent_blocks, use.names = FALSE)
)

cfa_model <- '
  Poor_walking_infrastructure =~
    Walkable_with_walker_or_wheelchair +
    Maintained_sidewalks +
    Little_dirsuption_on_sidewalks +
    Easy_to_cross_streets

  No_walking_friendly_environment =~
    Walking_friendly_neighborhood +
    Enough_benches +
    Enough_greenery +
    Public_spaces_are_pleasant_meeting_places

  High_quality_cycling_infrastructure =~
    Qualitative_cycling_lanes +
    Maintained_cycling_infrastructure +
    Sufficiently_cycling_infrastructure +
    Maintained_streets_and_squares

  Cycling_friendly_environment =~
    Cycling_friendly_neighborhood +
    Enjoyable_cycling +
    Feeling_safe_in_traffic_as_cyclist +
    Busy_traffic_causing_unsafe_cycling_experiences

  Busy_traffic =~
    Lot_of_traffic +
    Lot_of_freight_traffic +
    Nuisance_due_to_traffic +
    Few_traffic_accidents

  Traffic_safety =~
    Enough_public_lighting +
    Feeling_safe_in_traffic_as_pedestrian +
    Feeling_safe_in_traffic_as_car_user +
    Feeling_safe_on_public_transport

  Negative_parking_experiences =~
    Accessible_by_car +
    Easily_finding_parking_spot_residents +
    Easily_finding_parking_spot_visitors

  High_quality_public_transport =~
    Satisfied_public_transport +
    Enough_public_transport +
    Sufficiently_frequent_public_transport +
    Well_equipped_public_transport_stops +
    Accessible_public_transport_stops +
    Enough_seats_public_transport +
    Easy_to_find_information_on_PT_for_elderly
'

# Correct a possible typo in the model string before fitting.
# The actual variable name in the data is Sufficient_cycling_infrastructure.
cfa_model <- gsub(
  "Sufficiently_cycling_infrastructure",
  "Sufficient_cycling_infrastructure",
  cfa_model,
  fixed = TRUE
)

# ----------------------------------------------------------------------
# 2. LOAD AND VALIDATE THE PREPARED DATA
# ----------------------------------------------------------------------

database <- readRDS(prepared_data_path)

required_vars <- c(
  "respondent_id_internal",
  indicator_vars
)

missing_vars <- setdiff(
  required_vars,
  names(database)
)

if (length(missing_vars) > 0) {
  stop(
    "The following CFA variables are absent from the prepared dataset:\n",
    paste(missing_vars, collapse = "\n")
  )
}

# Reconfirm that the indicators are numeric and coded 1 to 5.
non_numeric_indicators <- indicator_vars[
  !vapply(
    database[indicator_vars],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_indicators) > 0) {
  stop(
    "The following indicators are not numeric:\n",
    paste(non_numeric_indicators, collapse = "\n")
  )
}

indicator_validation <- purrr::map_dfr(
  indicator_vars,
  function(item) {
    x <- database[[item]]
    
    tibble(
      indicator = item,
      n = length(x),
      n_missing = sum(is.na(x)),
      n_unique = n_distinct(x, na.rm = TRUE),
      minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      maximum = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE),
      valid_range = all(
        is.na(x) |
          x %in% 1:5
      )
    )
  }
)

write_csv(
  indicator_validation,
  "output/tables/CFA_indicator_validation.csv"
)

invalid_indicators <- indicator_validation %>%
  filter(
    !valid_range |
      n_unique < 2
  )

if (nrow(invalid_indicators) > 0) {
  print(invalid_indicators)
  
  stop(
    "At least one CFA indicator has an invalid range or no variation."
  )
}

# ----------------------------------------------------------------------
# 3. CREATE THE CFA COMPLETE-CASE SAMPLE
# ----------------------------------------------------------------------
#
# A single complete-case CFA sample is used so lavPredict returns scores for
# all eight constructs for the same respondents.
# ----------------------------------------------------------------------

cfa_sample <- database %>%
  filter(
    if_all(
      all_of(indicator_vars),
      ~ !is.na(.x)
    )
  ) %>%
  arrange(respondent_id_internal)

if (nrow(cfa_sample) == 0) {
  stop("No complete observations remain for the CFA.")
}

if (anyDuplicated(cfa_sample$respondent_id_internal) > 0) {
  stop(
    "Duplicate respondent IDs were found in the CFA sample."
  )
}

cfa_sample_overview <- tibble(
  sample = c(
    "Prepared dataset",
    "CFA complete-case sample"
  ),
  n_respondents = c(
    nrow(database),
    nrow(cfa_sample)
  ),
  percentage_retained = 100 * n_respondents / nrow(database)
)

write_csv(
  cfa_sample_overview,
  "output/tables/CFA_sample_overview.csv"
)

write_csv(
  cfa_sample %>%
    select(
      respondent_id_internal,
      all_of(indicator_vars)
    ),
  "output/data/CFA_complete_case_sample.csv"
)

saveRDS(
  cfa_sample,
  "output/data/CFA_complete_case_sample.rds"
)

message(
  "CFA complete-case sample: n = ",
  nrow(cfa_sample),
  " of ",
  nrow(database),
  " respondents."
)

# ----------------------------------------------------------------------
# 4. ESTIMATE THE CFA
# ----------------------------------------------------------------------

fit_cfa <- lavaan::cfa(
  model = cfa_model,
  data = cfa_sample,
  ordered = indicator_vars,
  estimator = "WLSMV",
  parameterization = "theta",
  std.ov = TRUE,
  auto.fix.first = TRUE
)

saveRDS(
  fit_cfa,
  "output/models/CFA_perceived_environment_WLSMV.rds"
)

# ----------------------------------------------------------------------
# 5. CONVERGENCE AND GLOBAL MODEL FIT
# ----------------------------------------------------------------------

if (!lavInspect(fit_cfa, "converged")) {
  stop("The CFA did not converge.")
}

fit_measure_names <- c(
  "chisq.scaled",
  "df.scaled",
  "pvalue.scaled",
  "cfi.scaled",
  "tli.scaled",
  "rmsea.scaled",
  "rmsea.ci.lower.scaled",
  "rmsea.ci.upper.scaled",
  "srmr"
)

available_fit_measures <- intersect(
  fit_measure_names,
  names(fitMeasures(fit_cfa))
)

fit_measures <- fitMeasures(
  fit_cfa,
  fit.measures = available_fit_measures
)

fit_measures_table <- tibble(
  measure = names(fit_measures),
  value = as.numeric(fit_measures)
)

write_csv(
  fit_measures_table,
  "output/tables/CFA_global_fit_measures.csv"
)

# Save a text summary including thresholds and standardised results.
capture.output(
  summary(
    fit_cfa,
    fit.measures = TRUE,
    standardized = TRUE,
    rsquare = TRUE
  ),
  file = "output/tables/CFA_full_summary.txt"
)

# ----------------------------------------------------------------------
# 6. STANDARDISED LOADINGS, THRESHOLDS AND R-SQUARED
# ----------------------------------------------------------------------

standardised_solution <- standardizedSolution(
  fit_cfa,
  type = "std.all"
)

loading_table <- standardised_solution %>%
  as_tibble() %>%
  filter(op == "=~") %>%
  transmute(
    latent_variable = lhs,
    indicator = rhs,
    standardized_loading = est.std,
    standard_error = se,
    z_value = z,
    p_value = pvalue,
    lower_95 = ci.lower,
    upper_95 = ci.upper
  ) %>%
  arrange(
    latent_variable,
    desc(abs(standardized_loading))
  )

write_csv(
  loading_table,
  "output/tables/CFA_standardized_loadings.csv"
)

threshold_table <- parameterEstimates(
  fit_cfa,
  standardized = FALSE
) %>%
  as_tibble() %>%
  filter(op == "|") %>%
  transmute(
    indicator = lhs,
    threshold = rhs,
    estimate = est,
    standard_error = se,
    z_value = z,
    p_value = pvalue
  ) %>%
  arrange(
    indicator,
    threshold
  )

write_csv(
  threshold_table,
  "output/tables/CFA_thresholds.csv"
)

r_squared_values <- lavInspect(
  fit_cfa,
  "rsquare"
)

r_squared_table <- tibble(
  indicator = names(r_squared_values),
  r_squared = as.numeric(r_squared_values)
)

write_csv(
  r_squared_table,
  "output/tables/CFA_indicator_R_squared.csv"
)

# ----------------------------------------------------------------------
# 7. LATENT CORRELATIONS
# ----------------------------------------------------------------------

latent_correlation_matrix <- lavInspect(
  fit_cfa,
  "cor.lv"
)

write_csv(
  as.data.frame(latent_correlation_matrix) %>%
    rownames_to_column("latent_variable"),
  "output/tables/CFA_latent_correlation_matrix.csv"
)

latent_correlations_long <- as.data.frame(
  as.table(latent_correlation_matrix)
) %>%
  as_tibble() %>%
  rename(
    latent_variable_1 = Var1,
    latent_variable_2 = Var2,
    correlation = Freq
  ) %>%
  filter(
    as.character(latent_variable_1) <
      as.character(latent_variable_2)
  ) %>%
  arrange(desc(abs(correlation)))

write_csv(
  latent_correlations_long,
  "output/tables/CFA_latent_correlations_long.csv"
)

# ----------------------------------------------------------------------
# 8. COMPOSITE RELIABILITY AND AVE
# ----------------------------------------------------------------------
#
# Using standardised loadings and residual variances:
#
#   CR  = (sum lambda)^2 /
#         [(sum lambda)^2 + sum theta]
#
#   AVE = sum(lambda^2) /
#         [sum(lambda^2) + sum theta]
# ----------------------------------------------------------------------

residual_variance_table <- standardised_solution %>%
  as_tibble() %>%
  filter(
    op == "~~",
    lhs == rhs,
    lhs %in% indicator_vars
  ) %>%
  transmute(
    indicator = lhs,
    residual_variance = est.std
  )

reliability_table <- loading_table %>%
  select(
    latent_variable,
    indicator,
    standardized_loading
  ) %>%
  left_join(
    residual_variance_table,
    by = "indicator"
  ) %>%
  group_by(latent_variable) %>%
  summarise(
    n_indicators = n(),
    composite_reliability =
      sum(standardized_loading)^2 /
      (
        sum(standardized_loading)^2 +
          sum(residual_variance)
      ),
    average_variance_extracted =
      sum(standardized_loading^2) /
      (
        sum(standardized_loading^2) +
          sum(residual_variance)
      ),
    minimum_loading = min(standardized_loading),
    maximum_loading = max(standardized_loading),
    .groups = "drop"
  )

write_csv(
  reliability_table,
  "output/tables/CFA_reliability_CR_AVE.csv"
)

# semTools reliability output as a supplementary check.
semtools_reliability <- tryCatch(
  {
    semTools::compRelSEM(
      fit_cfa,
      ord.scale = TRUE,
      return.total = FALSE
    )
  },
  error = function(e) {
    warning(
      "semTools::compRelSEM could not be calculated: ",
      conditionMessage(e)
    )
    
    NULL
  }
)

if (!is.null(semtools_reliability)) {
  semtools_reliability_table <- tibble(
    latent_variable = names(semtools_reliability),
    semTools_composite_reliability =
      as.numeric(semtools_reliability)
  )
  
  write_csv(
    semtools_reliability_table,
    "output/tables/CFA_semTools_composite_reliability.csv"
  )
}

# ----------------------------------------------------------------------
# 9. DISCRIMINANT VALIDITY
# ----------------------------------------------------------------------

# 9.1 Fornell-Larcker matrix:
# diagonal = square root of AVE
# off-diagonal = latent correlations
sqrt_ave <- reliability_table %>%
  select(
    latent_variable,
    average_variance_extracted
  ) %>%
  mutate(
    sqrt_AVE = sqrt(average_variance_extracted)
  )

fornell_larcker_matrix <- latent_correlation_matrix

for (construct_name in sqrt_ave$latent_variable) {
  if (construct_name %in% rownames(fornell_larcker_matrix)) {
    fornell_larcker_matrix[
      construct_name,
      construct_name
    ] <- sqrt_ave$sqrt_AVE[
      sqrt_ave$latent_variable == construct_name
    ]
  }
}

write_csv(
  as.data.frame(fornell_larcker_matrix) %>%
    rownames_to_column("latent_variable"),
  "output/tables/CFA_Fornell_Larcker_matrix.csv"
)

# Pairwise Fornell-Larcker decision table.
fornell_larcker_pairs <- latent_correlations_long %>%
  left_join(
    sqrt_ave %>%
      select(
        latent_variable_1 = latent_variable,
        sqrt_AVE_1 = sqrt_AVE
      ),
    by = "latent_variable_1"
  ) %>%
  left_join(
    sqrt_ave %>%
      select(
        latent_variable_2 = latent_variable,
        sqrt_AVE_2 = sqrt_AVE
      ),
    by = "latent_variable_2"
  ) %>%
  mutate(
    absolute_correlation = abs(correlation),
    discriminant_validity_supported =
      absolute_correlation < sqrt_AVE_1 &
      absolute_correlation < sqrt_AVE_2
  )

write_csv(
  fornell_larcker_pairs,
  "output/tables/CFA_Fornell_Larcker_pairwise.csv"
)

# 9.2 HTMT as a supplementary diagnostic.
htmt_matrix <- tryCatch(
  {
    semTools::htmt(
      model = cfa_model,
      data = cfa_sample,
      ordered = indicator_vars
    )
  },
  error = function(e) {
    warning(
      "HTMT could not be calculated: ",
      conditionMessage(e)
    )
    
    NULL
  }
)

if (!is.null(htmt_matrix)) {
  write_csv(
    as.data.frame(htmt_matrix) %>%
      rownames_to_column("latent_variable"),
    "output/tables/CFA_HTMT_matrix.csv"
  )
}

# ----------------------------------------------------------------------
# 10. MODIFICATION INDICES — DIAGNOSTIC ONLY
# ----------------------------------------------------------------------
#
# Modification indices are exported for transparency but should not be used
# automatically to change the measurement model. Any change must be
# theoretically defensible.
# ----------------------------------------------------------------------

modification_indices_table <- modificationIndices(
  fit_cfa,
  sort. = TRUE,
  minimum.value = 10
) %>%
  as_tibble()

write_csv(
  modification_indices_table,
  "output/tables/CFA_modification_indices_above_10.csv"
)

# ----------------------------------------------------------------------
# 11. GENERATE FACTOR SCORES
# ----------------------------------------------------------------------

factor_scores_matrix <- lavPredict(
  fit_cfa,
  type = "lv",
  method = "EBM"
)

factor_scores <- as.data.frame(
  factor_scores_matrix
) %>%
  as_tibble()

expected_factor_names <- names(latent_blocks)

if (!all(expected_factor_names %in% names(factor_scores))) {
  stop(
    "lavPredict did not return all expected factor-score columns.\n",
    "Returned columns:\n",
    paste(names(factor_scores), collapse = "\n")
  )
}

if (nrow(factor_scores) != nrow(cfa_sample)) {
  stop(
    "The number of factor-score rows does not match the CFA sample."
  )
}

factor_scores <- bind_cols(
  cfa_sample %>%
    select(respondent_id_internal),
  factor_scores
)

# Add clear FS_ prefixes.
factor_score_names_original <- expected_factor_names
factor_score_names_prefixed <- paste0(
  "FS_",
  factor_score_names_original
)

factor_scores <- factor_scores %>%
  rename(
    !!!setNames(
      factor_score_names_original,
      factor_score_names_prefixed
    )
  )

# Standardise factor scores for later model comparison.
standardise_score <- function(x) {
  as.numeric(
    (x - mean(x, na.rm = TRUE)) /
      sd(x, na.rm = TRUE)
  )
}

factor_scores <- factor_scores %>%
  mutate(
    across(
      all_of(factor_score_names_prefixed),
      standardise_score,
      .names = "{.col}_z"
    )
  )

factor_score_vars <- paste0(
  factor_score_names_prefixed,
  "_z"
)

# Check the score distributions.
factor_score_summary <- purrr::map_dfr(
  factor_score_names_prefixed,
  function(score_name) {
    x <- factor_scores[[score_name]]
    
    tibble(
      factor_score = score_name,
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      standard_deviation = sd(x, na.rm = TRUE),
      minimum = min(x, na.rm = TRUE),
      first_quartile = quantile(
        x,
        0.25,
        names = FALSE,
        na.rm = TRUE
      ),
      median = median(x, na.rm = TRUE),
      third_quartile = quantile(
        x,
        0.75,
        names = FALSE,
        na.rm = TRUE
      ),
      maximum = max(x, na.rm = TRUE)
    )
  }
)

write_csv(
  factor_score_summary,
  "output/tables/CFA_factor_score_summary.csv"
)

write_csv(
  factor_scores,
  "output/data/perceived_environment_factor_scores.csv"
)

saveRDS(
  factor_scores,
  "output/data/perceived_environment_factor_scores.rds"
)

# ----------------------------------------------------------------------
# 12. MERGE FACTOR SCORES BACK INTO THE PREPARED DATA
# ----------------------------------------------------------------------

duplicate_factor_score_ids <- factor_scores %>%
  count(
    respondent_id_internal,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(duplicate_factor_score_ids) > 0) {
  stop("Duplicate respondent IDs were found in the factor-score table.")
}

database_with_factor_scores <- database %>%
  left_join(
    factor_scores,
    by = "respondent_id_internal"
  )

if (nrow(database_with_factor_scores) != nrow(database)) {
  stop(
    "The row count changed after merging factor scores into the dataset."
  )
}

n_with_all_factor_scores <- database_with_factor_scores %>%
  summarise(
    n = sum(
      if_all(
        all_of(factor_score_vars),
        ~ !is.na(.x)
      )
    )
  ) %>%
  pull(n)

factor_score_merge_overview <- tibble(
  statistic = c(
    "Rows in prepared dataset",
    "Rows in CFA sample",
    "Rows with all eight standardised factor scores",
    "Rows without complete factor scores"
  ),
  value = c(
    nrow(database),
    nrow(cfa_sample),
    n_with_all_factor_scores,
    nrow(database) - n_with_all_factor_scores
  )
)

write_csv(
  factor_score_merge_overview,
  "output/tables/CFA_factor_score_merge_overview.csv"
)

saveRDS(
  database_with_factor_scores,
  "output/data/prepared_multilevel_data_with_factor_scores.rds"
)

write_csv(
  database_with_factor_scores,
  "output/data/prepared_multilevel_data_with_factor_scores.csv"
)

# ----------------------------------------------------------------------
# 13. FACTOR-SCORE CORRELATION MATRIX
# ----------------------------------------------------------------------

factor_score_correlation_matrix <- cor(
  database_with_factor_scores[
    factor_score_vars
  ],
  use = "pairwise.complete.obs"
)

write_csv(
  as.data.frame(factor_score_correlation_matrix) %>%
    rownames_to_column("factor_score"),
  "output/tables/CFA_factor_score_correlation_matrix.csv"
)

# ----------------------------------------------------------------------
# 14. FACTOR-SCORE DISTRIBUTION FIGURES
# ----------------------------------------------------------------------

factor_scores_long <- factor_scores %>%
  select(
    respondent_id_internal,
    all_of(factor_score_vars)
  ) %>%
  pivot_longer(
    cols = all_of(factor_score_vars),
    names_to = "factor_score",
    values_to = "value"
  )

factor_score_plot <- ggplot(
  factor_scores_long,
  aes(x = value)
) +
  geom_histogram(
    bins = 30
  ) +
  facet_wrap(
    ~ factor_score,
    scales = "free"
  ) +
  labs(
    x = "Standardised factor score",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename =
    "output/figures/CFA_standardised_factor_score_distributions.png",
  plot = factor_score_plot,
  width = 14,
  height = 9,
  dpi = 180
)

# ----------------------------------------------------------------------
# 15. FINAL SCREEN OUTPUT
# ----------------------------------------------------------------------

message("\nPart 3 completed successfully.")
message("CFA sample size: ", nrow(cfa_sample))
message(
  "Prepared multilevel dataset with factor scores saved to:\n",
  "output/data/prepared_multilevel_data_with_factor_scores.rds"
)
message(
  "Number of respondents with all eight factor scores: ",
  n_with_all_factor_scores
)

print(fit_measures_table)
print(reliability_table)
print(factor_score_merge_overview)

# ======================================================================
# END OF PART 3
# ======================================================================
# ======================================================================
# PART 3B — RENAME PERCEIVED-ENVIRONMENT FACTORS
# ======================================================================
#
# Purpose:
#   Rename three latent constructs for clearer and thesis-consistent wording,
#   without changing any estimates, factor scores, or respondent values.
#
# Renaming:
#   Poor_walking_infrastructure
#       -> High_quality_walking_infrastructure
#
#   No_walking_friendly_environment
#       -> Walking_friendly_environment
#
#   Negative_parking_experiences
#       -> Positive_parking_experiences
#
# All other construct names remain unchanged.
# ======================================================================

rm(list = ls())
gc()

library(dplyr)
library(readr)
library(tibble)
library(purrr)

dir.create("output", showWarnings = FALSE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. DEFINE THE RENAMING MAP
# ----------------------------------------------------------------------

factor_name_map <- c(
  "Poor_walking_infrastructure" =
    "High_quality_walking_infrastructure",
  
  "No_walking_friendly_environment" =
    "Walking_friendly_environment",
  
  "Negative_parking_experiences" =
    "Positive_parking_experiences"
)

# ----------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ----------------------------------------------------------------------

replace_factor_names_in_text <- function(x) {
  
  x <- as.character(x)
  
  for (old_name in names(factor_name_map)) {
    
    new_name <- factor_name_map[[old_name]]
    
    x <- gsub(
      old_name,
      new_name,
      x,
      fixed = TRUE
    )
  }
  
  x
}


rename_factor_columns <- function(data) {
  
  old_names <- names(data)
  new_names <- replace_factor_names_in_text(old_names)
  
  names(data) <- new_names
  
  data
}


replace_factor_names_in_character_columns <- function(data) {
  
  data %>%
    mutate(
      across(
        where(is.character),
        replace_factor_names_in_text
      )
    )
}

# ----------------------------------------------------------------------
# 3. UPDATE THE FACTOR-SCORE FILE
# ----------------------------------------------------------------------

factor_score_rds <-
  "output/data/perceived_environment_factor_scores.rds"

if (!file.exists(factor_score_rds)) {
  stop(
    "Factor-score file not found:\n",
    factor_score_rds,
    "\nRun Part 3 first."
  )
}

factor_scores <- readRDS(factor_score_rds)

factor_scores <- factor_scores %>%
  rename_factor_columns()

saveRDS(
  factor_scores,
  "output/data/perceived_environment_factor_scores_renamed.rds"
)

write_csv(
  factor_scores,
  "output/data/perceived_environment_factor_scores_renamed.csv"
)

# ----------------------------------------------------------------------
# 4. UPDATE THE PREPARED MULTILEVEL DATASET WITH FACTOR SCORES
# ----------------------------------------------------------------------

prepared_factor_data_rds <-
  "output/data/prepared_multilevel_data_with_factor_scores.rds"

if (!file.exists(prepared_factor_data_rds)) {
  stop(
    "Prepared multilevel dataset with factor scores not found:\n",
    prepared_factor_data_rds,
    "\nRun Part 3 first."
  )
}

database_with_factor_scores <-
  readRDS(prepared_factor_data_rds)

database_with_factor_scores <-
  database_with_factor_scores %>%
  rename_factor_columns()

saveRDS(
  database_with_factor_scores,
  "output/data/prepared_multilevel_data_with_factor_scores_renamed.rds"
)

write_csv(
  database_with_factor_scores,
  "output/data/prepared_multilevel_data_with_factor_scores_renamed.csv"
)

# ----------------------------------------------------------------------
# 5. UPDATE RELEVANT CFA OUTPUT TABLES
# ----------------------------------------------------------------------
#
# These files contain construct names either as values, column names,
# or both. The numerical results are not changed.
# ----------------------------------------------------------------------

tables_to_update <- c(
  "CFA_standardized_loadings.csv",
  "CFA_reliability_CR_AVE.csv",
  "CFA_latent_correlations_long.csv",
  "CFA_latent_correlation_matrix.csv",
  "CFA_Fornell_Larcker_matrix.csv",
  "CFA_Fornell_Larcker_pairwise.csv",
  "CFA_HTMT_matrix.csv",
  "CFA_factor_score_summary.csv",
  "CFA_factor_score_correlation_matrix.csv"
)

for (file_name in tables_to_update) {
  
  input_path <- file.path(
    "output/tables",
    file_name
  )
  
  if (!file.exists(input_path)) {
    message(
      "Skipping missing file: ",
      input_path
    )
    next
  }
  
  current_table <- read_csv(
    input_path,
    show_col_types = FALSE
  )
  
  current_table <- current_table %>%
    rename_factor_columns() %>%
    replace_factor_names_in_character_columns()
  
  output_name <- sub(
    "\\.csv$",
    "_renamed.csv",
    file_name
  )
  
  write_csv(
    current_table,
    file.path(
      "output/tables",
      output_name
    )
  )
}

# ----------------------------------------------------------------------
# 6. DEFINE THE FACTOR-SCORE VARIABLES FOR PART 4
# ----------------------------------------------------------------------

factor_score_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

missing_factor_score_vars <- setdiff(
  factor_score_vars,
  names(database_with_factor_scores)
)

if (length(missing_factor_score_vars) > 0) {
  stop(
    "The following renamed factor-score variables are missing:\n",
    paste(
      missing_factor_score_vars,
      collapse = "\n"
    )
  )
}

# Save the canonical factor names for later scripts.
factor_name_reference <- tibble(
  old_name = c(
    "Poor_walking_infrastructure",
    "No_walking_friendly_environment",
    "High_quality_cycling_infrastructure",
    "Cycling_friendly_environment",
    "Busy_traffic",
    "Traffic_safety",
    "Negative_parking_experiences",
    "High_quality_public_transport"
  ),
  
  final_name = c(
    "High_quality_walking_infrastructure",
    "Walking_friendly_environment",
    "High_quality_cycling_infrastructure",
    "Cycling_friendly_environment",
    "Busy_traffic",
    "Traffic_safety",
    "Positive_parking_experiences",
    "High_quality_public_transport"
  )
)

write_csv(
  factor_name_reference,
  "output/tables/final_factor_name_reference.csv"
)

# ----------------------------------------------------------------------
# 7. FINAL CHECK
# ----------------------------------------------------------------------

message("\nFactor renaming completed successfully.")

message(
  "\nUse this dataset in Part 4:\n",
  "output/data/prepared_multilevel_data_with_factor_scores_renamed.rds"
)

message("\nFinal standardised factor-score variables:")

print(
  tibble(
    factor_score_variable = factor_score_vars
  )
)

# ======================================================================
# END OF PART 3B
# ======================================================================

# ======================================================================
# PART 4 — COMMON-SAMPLE MULTILEVEL MODE-CHOICE MODELS
# brms + rstan
# ======================================================================
#
# All models in Part 4 are estimated on EXACTLY THE SAME respondents.
#
# Model sequence:
#
#   ML0b: Mode ~ 1 + (1 | Neighbourhood)
#
#   ML1b: Mode ~ demographics + (1 | Neighbourhood)
#
#   ML2:  Mode ~ demographics + objective environment
#          + (1 | Neighbourhood)
#
#   ML3:  Mode ~ demographics + perceived-environment factor scores
#          + (1 | Neighbourhood)
#
#   ML4:  Mode ~ demographics + objective environment
#          + perceived-environment factor scores
#          + (1 | Neighbourhood)
#
# Main purposes:
#   1. Compare objective and perceived environment on one common sample.
#   2. Examine whether each set adds predictive information beyond demographics.
#   3. Examine remaining between-neighbourhood variation across models.
#   4. Create a full combined model before any later parsimonious reduction.
#
# IMPORTANT:
#   - Private car is the reference outcome category.
#   - Higher perceived factor scores have the following interpretation:
#       * higher-quality walking infrastructure
#       * more walking-friendly environment
#       * higher-quality cycling infrastructure
#       * more cycling-friendly environment
#       * busier traffic
#       * greater traffic safety
#       * more positive parking experiences
#       * higher-quality public transport
#
#   - The objective variables are neighbourhood-level measures. With only
#     25 neighbourhoods, their estimates should be interpreted cautiously.
#   - Part 4 does NOT yet perform data-driven variable selection.
#     A parsimonious sensitivity model can be estimated in Part 5.
# ======================================================================

# ----------------------------------------------------------------------
# Stable temporary directory for rstan on Windows
# ----------------------------------------------------------------------

dir.create(
  "C:/Rtemp",
  recursive = TRUE,
  showWarnings = FALSE
)

Sys.setenv(
  TMPDIR = "C:/Rtemp",
  TEMP = "C:/Rtemp",
  TMP = "C:/Rtemp"
)

library(rstan)
library(brms)

rstan_options(auto_write = TRUE)

tempdir()

stan_temp_dir <- "C:/Rtemp"

if (!dir.exists(stan_temp_dir)) {
  dir.create(
    stan_temp_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

Sys.setenv(
  TMPDIR = stan_temp_dir,
  TEMP = stan_temp_dir,
  TMP = stan_temp_dir
)

options(
  tmpdir = stan_temp_dir
)
stan_cache_dir <- "C:/Rstan_cache"

if (!dir.exists(stan_cache_dir)) {
  dir.create(
    stan_cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}
rstan_options(auto_write = TRUE)

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES, PATHS AND SETTINGS
# ----------------------------------------------------------------------

required_packages <- c(
  "brms",
  "rstan",
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "posterior",
  "bayesplot",
  "loo"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Part 4:\n",
    paste(missing_packages, collapse = "\n"),
    "\n\nUse install.packages(c('brms', 'rstan', 'dplyr', 'readr', ",
    "'purrr', 'tibble', 'posterior', 'bayesplot', 'loo'))."
  )
}

library(brms)
library(rstan)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(posterior)
library(bayesplot)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data_path <-
  "output/data/prepared_multilevel_data_with_factor_scores_renamed.rds"

dir.create("output", showWarnings = FALSE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(data_path)) {
  stop(
    "Part 4 input file not found:\n",
    data_path,
    "\nRun Part 3 and Part 3B first."
  )
}

# MCMC settings
n_chains <- 4L
n_iter <- 2000L
n_warmup <- 1000L
adapt_delta_value <- 0.95
max_treedepth_value <- 12L
model_seed <- 1234L

n_cores <- min(
  n_chains,
  max(1L, parallel::detectCores())
)

# ----------------------------------------------------------------------
# 1. LOAD DATA AND DEFINE MODEL VARIABLES
# ----------------------------------------------------------------------

database <- readRDS(data_path)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- c(
  "FAC_presence_walking_infr_z",
  "FAC_presence_difficult_surface_z",
  "FAC_presence_greenery_z",
  "FAC_presence_park_equipment_z",
  "FAC_presence_cycling_infrastructure_z",
  "FAC_rate_accidents_z",
  "FAC_presence_primary_secondary_roads_z",
  "FAC_presence_high_parking_stress_z",
  "FAC_presence_moderate_parking_stress_z",
  "FAC_presence_PT_z"
)

factor_score_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

required_vars <- unique(c(
  "respondent_id_internal",
  "Mode_factor",
  "Neighbourhood_factor",
  control_vars,
  objective_vars,
  factor_score_vars
))

missing_vars <- setdiff(
  required_vars,
  names(database)
)

if (length(missing_vars) > 0) {
  stop(
    "The following variables required for Part 4 are missing:\n",
    paste(missing_vars, collapse = "\n")
  )
}

# Reconfirm factor structure and reference category.
database <- database %>%
  mutate(
    Mode_factor = factor(
      Mode_factor,
      levels = c(
        "Private car",
        "On foot",
        "Bike",
        "Public transport"
      )
    ),
    Mode_factor = relevel(
      Mode_factor,
      ref = "Private car"
    ),
    Neighbourhood_factor =
      droplevels(factor(Neighbourhood_factor))
  )

if (levels(database$Mode_factor)[1] != "Private car") {
  stop("Private car is not the reference category of Mode_factor.")
}

# ----------------------------------------------------------------------
# 2. CREATE ONE COMMON SAMPLE FOR ALL FIVE MODELS
# ----------------------------------------------------------------------
#
# The common sample requires:
#   - valid mode choice;
#   - valid neighbourhood;
#   - all demographic controls;
#   - all 10 objective variables;
#   - all 8 perceived factor scores.
#
# This guarantees valid direct model comparisons.
# ----------------------------------------------------------------------

analysis_sample_part4 <- database %>%
  filter(
    if_all(
      all_of(c(
        "Mode_factor",
        "Neighbourhood_factor",
        control_vars,
        objective_vars,
        factor_score_vars
      )),
      ~ !is.na(.x)
    )
  ) %>%
  droplevels() %>%
  arrange(respondent_id_internal)

if (nrow(analysis_sample_part4) == 0) {
  stop("No complete observations remain for Part 4.")
}

if (nlevels(analysis_sample_part4$Neighbourhood_factor) < 2) {
  stop("Fewer than two neighbourhoods remain in the Part 4 sample.")
}

if (anyDuplicated(
  analysis_sample_part4$respondent_id_internal
) > 0) {
  stop(
    "Duplicate respondent IDs are present in the Part 4 sample."
  )
}

sample_overview <- tibble(
  sample = c(
    "Input dataset",
    "Common ML0b-ML4 sample"
  ),
  n_respondents = c(
    nrow(database),
    nrow(analysis_sample_part4)
  ),
  n_neighbourhoods = c(
    nlevels(database$Neighbourhood_factor),
    nlevels(analysis_sample_part4$Neighbourhood_factor)
  )
)

write_csv(
  sample_overview,
  "output/tables/part4_sample_overview.csv"
)

neighbourhood_sizes_part4 <- analysis_sample_part4 %>%
  count(
    Neighbourhood_factor,
    name = "n"
  ) %>%
  arrange(n)

write_csv(
  neighbourhood_sizes_part4,
  "output/tables/part4_neighbourhood_sizes.csv"
)

choice_distribution_part4 <- analysis_sample_part4 %>%
  count(
    Mode_factor,
    name = "n",
    .drop = FALSE
  ) %>%
  mutate(
    share = n / sum(n)
  )

write_csv(
  choice_distribution_part4,
  "output/tables/part4_choice_distribution.csv"
)

saveRDS(
  analysis_sample_part4,
  "output/data/part4_common_analysis_sample.rds"
)

message(
  "Part 4 common sample: n = ",
  nrow(analysis_sample_part4),
  "; neighbourhoods = ",
  nlevels(analysis_sample_part4$Neighbourhood_factor),
  "."
)

# ----------------------------------------------------------------------
# 3. CORRELATION CHECKS BEFORE MODEL ESTIMATION
# ----------------------------------------------------------------------
#
# These are descriptive diagnostics only. They do not automatically remove
# variables. Particular attention should be paid to correlations among the
# neighbourhood-level objective variables.
# ----------------------------------------------------------------------

objective_correlation_matrix <- cor(
  analysis_sample_part4[objective_vars],
  use = "pairwise.complete.obs"
)

write_csv(
  as.data.frame(objective_correlation_matrix) %>%
    rownames_to_column("objective_variable"),
  "output/tables/part4_objective_correlation_matrix.csv"
)

perceived_correlation_matrix <- cor(
  analysis_sample_part4[factor_score_vars],
  use = "pairwise.complete.obs"
)

write_csv(
  as.data.frame(perceived_correlation_matrix) %>%
    rownames_to_column("factor_score"),
  "output/tables/part4_perceived_factor_score_correlation_matrix.csv"
)

# Long-form correlations, useful for quickly identifying the strongest pairs.
matrix_to_pairwise <- function(correlation_matrix, type_label) {
  
  as.data.frame(
    as.table(correlation_matrix)
  ) %>%
    as_tibble() %>%
    rename(
      variable_1 = Var1,
      variable_2 = Var2,
      correlation = Freq
    ) %>%
    mutate(
      variable_1 = as.character(variable_1),
      variable_2 = as.character(variable_2),
      variable_type = type_label
    ) %>%
    filter(variable_1 < variable_2) %>%
    arrange(desc(abs(correlation)))
}

correlation_pairs <- bind_rows(
  matrix_to_pairwise(
    objective_correlation_matrix,
    "Objective"
  ),
  matrix_to_pairwise(
    perceived_correlation_matrix,
    "Perceived factor score"
  )
)

write_csv(
  correlation_pairs,
  "output/tables/part4_pairwise_correlations.csv"
)

# ----------------------------------------------------------------------
# 4. BUILD MODEL FORMULAS
# ----------------------------------------------------------------------

make_multilevel_formula <- function(predictors = character(0)) {
  
  if (length(predictors) == 0) {
    
    formula_text <-
      "Mode_factor ~ 1 + (1 | Neighbourhood_factor)"
    
  } else {
    
    formula_text <- paste0(
      "Mode_factor ~ ",
      paste(predictors, collapse = " + "),
      " + (1 | Neighbourhood_factor)"
    )
  }
  
  brms::bf(
    stats::as.formula(formula_text)
  )
}

formula_ml0b <- make_multilevel_formula()

formula_ml1b <- make_multilevel_formula(
  control_vars
)

formula_ml2_objective <- make_multilevel_formula(
  c(
    control_vars,
    objective_vars
  )
)

formula_ml3_perceived <- make_multilevel_formula(
  c(
    control_vars,
    factor_score_vars
  )
)

formula_ml4_combined <- make_multilevel_formula(
  c(
    control_vars,
    objective_vars,
    factor_score_vars
  )
)

categorical_family <- categorical(
  link = "logit",
  refcat = "Private car"
)

# ----------------------------------------------------------------------
# 5. CATEGORY-SPECIFIC PRIORS
# ----------------------------------------------------------------------

make_categorical_priors <- function(
    formula,
    data,
    include_fixed_slopes = TRUE
) {
  
  prior_table <- get_prior(
    formula = formula,
    data = data,
    family = categorical_family
  )
  
  dpars <- unique(
    prior_table$dpar[
      !is.na(prior_table$dpar) &
        prior_table$dpar != ""
    ]
  )
  
  if (length(dpars) == 0) {
    stop(
      "No category-specific dpar names were found for a model."
    )
  }
  
  prior_list <- lapply(
    dpars,
    function(current_dpar) {
      
      current_priors <- c(
        set_prior(
          "student_t(3, 0, 2.5)",
          class = "Intercept",
          dpar = current_dpar
        ),
        set_prior(
          "exponential(1)",
          class = "sd",
          group = "Neighbourhood_factor",
          dpar = current_dpar
        )
      )
      
      if (include_fixed_slopes) {
        current_priors <- c(
          set_prior(
            "normal(0, 1)",
            class = "b",
            dpar = current_dpar
          ),
          current_priors
        )
      }
      
      current_priors
    }
  )
  
  priors <- do.call(
    c,
    prior_list
  )
  
  list(
    priors = priors,
    prior_table = prior_table,
    dpars = dpars
  )
}

prior_info_ml0b <- make_categorical_priors(
  formula_ml0b,
  analysis_sample_part4,
  include_fixed_slopes = FALSE
)

prior_info_ml1b <- make_categorical_priors(
  formula_ml1b,
  analysis_sample_part4,
  include_fixed_slopes = TRUE
)

prior_info_ml2 <- make_categorical_priors(
  formula_ml2_objective,
  analysis_sample_part4,
  include_fixed_slopes = TRUE
)

prior_info_ml3 <- make_categorical_priors(
  formula_ml3_perceived,
  analysis_sample_part4,
  include_fixed_slopes = TRUE
)

prior_info_ml4 <- make_categorical_priors(
  formula_ml4_combined,
  analysis_sample_part4,
  include_fixed_slopes = TRUE
)

write_csv(
  prior_info_ml0b$prior_table,
  "output/tables/ML0b_available_priors.csv"
)

write_csv(
  prior_info_ml1b$prior_table,
  "output/tables/ML1b_available_priors.csv"
)

write_csv(
  prior_info_ml2$prior_table,
  "output/tables/ML2_objective_available_priors.csv"
)

write_csv(
  prior_info_ml3$prior_table,
  "output/tables/ML3_perceived_available_priors.csv"
)

write_csv(
  prior_info_ml4$prior_table,
  "output/tables/ML4_combined_available_priors.csv"
)

# Validate priors before any long model fit begins.
validate_prior(
  prior = prior_info_ml0b$priors,
  formula = formula_ml0b,
  data = analysis_sample_part4,
  family = categorical_family
)

validate_prior(
  prior = prior_info_ml1b$priors,
  formula = formula_ml1b,
  data = analysis_sample_part4,
  family = categorical_family
)

validate_prior(
  prior = prior_info_ml2$priors,
  formula = formula_ml2_objective,
  data = analysis_sample_part4,
  family = categorical_family
)

validate_prior(
  prior = prior_info_ml3$priors,
  formula = formula_ml3_perceived,
  data = analysis_sample_part4,
  family = categorical_family
)

validate_prior(
  prior = prior_info_ml4$priors,
  formula = formula_ml4_combined,
  data = analysis_sample_part4,
  family = categorical_family
)

message("All Part 4 priors validated successfully.")

# ----------------------------------------------------------------------
# 6. MODEL-FITTING HELPER
# ----------------------------------------------------------------------

fit_multilevel_categorical_model <- function(
    formula,
    priors,
    model_name,
    model_description
) {
  
  message(
    "\n",
    paste(rep("=", 72), collapse = ""),
    "\nEstimating ",
    model_name,
    "\n",
    model_description,
    "\n",
    paste(rep("=", 72), collapse = ""),
    "\n"
  )
  
  model <- brm(
    formula = formula,
    data = analysis_sample_part4,
    family = categorical_family,
    prior = priors,
    backend = "rstan",
    chains = n_chains,
    iter = n_iter,
    warmup = n_warmup,
    cores = n_cores,
    seed = model_seed,
    control = list(
      adapt_delta = adapt_delta_value,
      max_treedepth = max_treedepth_value
    ),
    save_pars = save_pars(all = TRUE),
    file = file.path(
      "output/models",
      model_name
    ),
    file_refit = "on_change",
    refresh = 100
  )
  
  saveRDS(
    model,
    file.path(
      "output/models",
      paste0(model_name, ".rds")
    )
  )
  
  model
}

# ----------------------------------------------------------------------
# 7. ESTIMATE THE FIVE COMMON-SAMPLE MODELS
# ----------------------------------------------------------------------

fit_ml0b <- fit_multilevel_categorical_model(
  formula = formula_ml0b,
  priors = prior_info_ml0b$priors,
  model_name = "ML0b_empty_common_sample",
  model_description =
    "Empty multilevel categorical model on the Part 4 common sample"
)

fit_ml1b <- fit_multilevel_categorical_model(
  formula = formula_ml1b,
  priors = prior_info_ml1b$priors,
  model_name = "ML1b_demographics_common_sample",
  model_description =
    "Demographic multilevel categorical model on the Part 4 common sample"
)

fit_ml2_objective <- fit_multilevel_categorical_model(
  formula = formula_ml2_objective,
  priors = prior_info_ml2$priors,
  model_name = "ML2_objective_environment",
  model_description =
    "Demographics plus objective environment and neighbourhood random intercept"
)

fit_ml3_perceived <- fit_multilevel_categorical_model(
  formula = formula_ml3_perceived,
  priors = prior_info_ml3$priors,
  model_name = "ML3_perceived_environment",
  model_description =
    "Demographics plus perceived factor scores and neighbourhood random intercept"
)

fit_ml4_combined <- fit_multilevel_categorical_model(
  formula = formula_ml4_combined,
  priors = prior_info_ml4$priors,
  model_name = "ML4_objective_plus_perceived",
  model_description =
    "Demographics plus objective and perceived environment with neighbourhood random intercept"
)

model_list <- list(
  ML0b_empty_common_sample = fit_ml0b,
  ML1b_demographics_common_sample = fit_ml1b,
  ML2_objective_environment = fit_ml2_objective,
  ML3_perceived_environment = fit_ml3_perceived,
  ML4_objective_plus_perceived = fit_ml4_combined
)

# ----------------------------------------------------------------------
# 8. SAMPLER DIAGNOSTICS
# ----------------------------------------------------------------------

extract_sampler_diagnostics <- function(
    model,
    model_name
) {
  
  if (!inherits(model, "brmsfit")) {
    stop(
      model_name,
      " is not a brmsfit object."
    )
  }
  
  nuts <- brms::nuts_params(model)
  
  model_draws <- brms::as_draws_array(model)
  
  draw_summary <- posterior::summarise_draws(
    model_draws,
    posterior::default_convergence_measures()
  )
  
  tibble(
    model = model_name,
    
    divergent_transitions = sum(
      nuts$Parameter == "divergent__" &
        nuts$Value == 1
    ),
    
    maximum_treedepth_hits = sum(
      nuts$Parameter == "treedepth__" &
        nuts$Value >= max_treedepth_value
    ),
    
    minimum_bulk_ess = min(
      draw_summary$ess_bulk,
      na.rm = TRUE
    ),
    
    minimum_tail_ess = min(
      draw_summary$ess_tail,
      na.rm = TRUE
    ),
    
    maximum_rhat = max(
      draw_summary$rhat,
      na.rm = TRUE
    )
  )
}

diagnostics_table <- imap_dfr(
  model_list,
  ~ extract_sampler_diagnostics(
    model = .x,
    model_name = .y
  )
)

write_csv(
  diagnostics_table,
  "output/tables/part4_sampler_diagnostics_all_models.csv"
)

print(diagnostics_table)

# ----------------------------------------------------------------------
# 9. RANDOM-INTERCEPT SD, VARIANCE AND APPROXIMATE ICC
# ----------------------------------------------------------------------

clean_contrast_name <- function(parameter_name) {
  
  output <- parameter_name
  
  output <- sub(
    "^sd_Neighbourhood_factor__",
    "",
    output
  )
  
  output <- sub(
    "_Intercept$",
    "",
    output
  )
  
  output <- sub(
    "^mu",
    "",
    output
  )
  
  output
}

extract_neighbourhood_variance <- function(
    model,
    model_name,
    credible_probability = 0.95
) {
  
  draws <- brms::as_draws_df(model)
  
  sd_parameters <- grep(
    "^sd_Neighbourhood_factor__.*_Intercept$",
    names(draws),
    value = TRUE
  )
  
  if (length(sd_parameters) == 0) {
    stop(
      "No neighbourhood random-intercept SD parameters were found in ",
      model_name,
      ". Inspect variables(model) if parameter naming changed."
    )
  }
  
  alpha <- (1 - credible_probability) / 2
  
  map_dfr(
    sd_parameters,
    function(parameter_name) {
      
      sd_draws <-
        as.numeric(draws[[parameter_name]])
      
      variance_draws <-
        sd_draws^2
      
      icc_draws <-
        variance_draws /
        (
          variance_draws +
            (pi^2 / 3)
        )
      
      tibble(
        model = model_name,
        parameter = parameter_name,
        contrast_vs_private_car =
          clean_contrast_name(parameter_name),
        
        sigma_mean = mean(sd_draws),
        sigma_median = median(sd_draws),
        sigma_lower = quantile(
          sd_draws,
          0.025,
          names = FALSE
        ),
        sigma_upper = quantile(
          sd_draws,
          0.975,
          names = FALSE
        ),
        
        variance_mean = mean(variance_draws),
        variance_median = median(variance_draws),
        variance_lower = quantile(
          variance_draws,
          0.025,
          names = FALSE
        ),
        variance_upper = quantile(
          variance_draws,
          0.975,
          names = FALSE
        ),
        
        ICC_mean = mean(icc_draws),
        ICC_median = median(icc_draws),
        ICC_lower = quantile(
          icc_draws,
          0.025,
          names = FALSE
        ),
        ICC_upper = quantile(
          icc_draws,
          0.975,
          names = FALSE
        )
      )
    }
  )
}

random_effects_table <- imap_dfr(
  model_list,
  ~ extract_neighbourhood_variance(
    model = .x,
    model_name = .y
  )
)

write_csv(
  random_effects_table,
  "output/tables/part4_neighbourhood_sigma_variance_ICC_all_models.csv"
)

# ----------------------------------------------------------------------
# 10. DESCRIPTIVE CHANGE IN NEIGHBOURHOOD VARIANCE
# ----------------------------------------------------------------------
#
# Because the models are fitted separately, the following PCV is presented
# as a descriptive comparison of posterior mean variances:
#
#   PCV = (variance_reference - variance_model) / variance_reference
#
# It is calculated relative to:
#   - ML0b: total reduction from the empty model
#   - ML1b: additional reduction beyond demographics
# ----------------------------------------------------------------------

variance_means_wide <- random_effects_table %>%
  select(
    model,
    contrast_vs_private_car,
    variance_mean
  ) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = variance_mean
  )

variance_change_table <- variance_means_wide %>%
  mutate(
    PCV_vs_ML0b_ML1b =
      (
        ML0b_empty_common_sample -
          ML1b_demographics_common_sample
      ) /
      ML0b_empty_common_sample,
    
    PCV_vs_ML0b_ML2 =
      (
        ML0b_empty_common_sample -
          ML2_objective_environment
      ) /
      ML0b_empty_common_sample,
    
    PCV_vs_ML0b_ML3 =
      (
        ML0b_empty_common_sample -
          ML3_perceived_environment
      ) /
      ML0b_empty_common_sample,
    
    PCV_vs_ML0b_ML4 =
      (
        ML0b_empty_common_sample -
          ML4_objective_plus_perceived
      ) /
      ML0b_empty_common_sample,
    
    additional_PCV_vs_ML1b_ML2 =
      (
        ML1b_demographics_common_sample -
          ML2_objective_environment
      ) /
      ML1b_demographics_common_sample,
    
    additional_PCV_vs_ML1b_ML3 =
      (
        ML1b_demographics_common_sample -
          ML3_perceived_environment
      ) /
      ML1b_demographics_common_sample,
    
    additional_PCV_vs_ML1b_ML4 =
      (
        ML1b_demographics_common_sample -
          ML4_objective_plus_perceived
      ) /
      ML1b_demographics_common_sample
  )

write_csv(
  variance_change_table,
  "output/tables/part4_neighbourhood_variance_change.csv"
)

# ----------------------------------------------------------------------
# 11. FIXED EFFECTS AND ODDS RATIOS
# ----------------------------------------------------------------------

extract_fixed_effects <- function(
    model,
    model_name
) {
  
  as.data.frame(
    brms::fixef(
      model,
      probs = c(0.025, 0.975)
    )
  ) %>%
    rownames_to_column("parameter") %>%
    as_tibble() %>%
    transmute(
      model = model_name,
      parameter = parameter,
      estimate = Estimate,
      estimate_error = Est.Error,
      lower_95 = Q2.5,
      upper_95 = Q97.5,
      
      odds_ratio = exp(estimate),
      odds_ratio_lower_95 = exp(lower_95),
      odds_ratio_upper_95 = exp(upper_95),
      
      credible_nonzero =
        lower_95 > 0 |
        upper_95 < 0
    )
}

fixed_effects_all_models <- imap_dfr(
  model_list[names(model_list) != "ML0b_empty_common_sample"],
  ~ extract_fixed_effects(
    model = .x,
    model_name = .y
  )
)

write_csv(
  fixed_effects_all_models,
  "output/tables/part4_fixed_effects_all_models.csv"
)

# Separate files for the models most central to the research question.
write_csv(
  fixed_effects_all_models %>%
    filter(model == "ML2_objective_environment"),
  "output/tables/ML2_objective_fixed_effects.csv"
)

write_csv(
  fixed_effects_all_models %>%
    filter(model == "ML3_perceived_environment"),
  "output/tables/ML3_perceived_fixed_effects.csv"
)

write_csv(
  fixed_effects_all_models %>%
    filter(model == "ML4_objective_plus_perceived"),
  "output/tables/ML4_combined_fixed_effects.csv"
)

# ----------------------------------------------------------------------
# 12. LOO FOR DIRECT MODEL COMPARISON
# ----------------------------------------------------------------------
#
# All five models use exactly the same observations and outcome, so LOO
# comparisons are meaningful.
# ----------------------------------------------------------------------

fit_ml0b <- add_criterion(
  fit_ml0b,
  criterion = "loo"
)

fit_ml1b <- add_criterion(
  fit_ml1b,
  criterion = "loo"
)

fit_ml2_objective <- add_criterion(
  fit_ml2_objective,
  criterion = "loo"
)

fit_ml3_perceived <- add_criterion(
  fit_ml3_perceived,
  criterion = "loo"
)

fit_ml4_combined <- add_criterion(
  fit_ml4_combined,
  criterion = "loo"
)

# Update list after adding LOO.
model_list <- list(
  ML0b_empty_common_sample = fit_ml0b,
  ML1b_demographics_common_sample = fit_ml1b,
  ML2_objective_environment = fit_ml2_objective,
  ML3_perceived_environment = fit_ml3_perceived,
  ML4_objective_plus_perceived = fit_ml4_combined
)

loo_list <- lapply(
  model_list,
  function(model) {
    model$criteria$loo
  }
)

loo_comparison <- do.call(
  loo::loo_compare,
  unname(loo_list)
)

loo_comparison_table <- as.data.frame(
  loo_comparison
)

loo_comparison_table <- loo_comparison_table %>%
  tibble::rownames_to_column(
    var = "model_name"
  ) %>%
  as_tibble()

write_csv(
  loo_comparison_table,
  "output/tables/part4_LOO_model_comparison.csv"
)

# Useful targeted pairwise comparisons for the main research question.
loo_targeted_comparisons <- bind_rows(
  
  as.data.frame(
    loo::loo_compare(
      fit_ml1b$criteria$loo,
      fit_ml2_objective$criteria$loo
    )
  ) %>%
    tibble::rownames_to_column("model_name") %>%
    tibble::as_tibble() %>%
    mutate(
      comparison = "ML1b vs ML2 objective"
    ),
  
  as.data.frame(
    loo::loo_compare(
      fit_ml1b$criteria$loo,
      fit_ml3_perceived$criteria$loo
    )
  ) %>%
    tibble::rownames_to_column("model_name") %>%
    tibble::as_tibble() %>%
    mutate(
      comparison = "ML1b vs ML3 perceived"
    ),
  
  as.data.frame(
    loo::loo_compare(
      fit_ml2_objective$criteria$loo,
      fit_ml3_perceived$criteria$loo
    )
  ) %>%
    tibble::rownames_to_column("model_name") %>%
    tibble::as_tibble() %>%
    mutate(
      comparison = "ML2 objective vs ML3 perceived"
    ),
  
  as.data.frame(
    loo::loo_compare(
      fit_ml2_objective$criteria$loo,
      fit_ml4_combined$criteria$loo
    )
  ) %>%
    tibble::rownames_to_column("model_name") %>%
    tibble::as_tibble() %>%
    mutate(
      comparison = "ML2 objective vs ML4 combined"
    ),
  
  as.data.frame(
    loo::loo_compare(
      fit_ml3_perceived$criteria$loo,
      fit_ml4_combined$criteria$loo
    )
  ) %>%
    tibble::rownames_to_column("model_name") %>%
    tibble::as_tibble() %>%
    mutate(
      comparison = "ML3 perceived vs ML4 combined"
    )
)

write_csv(
  loo_targeted_comparisons,
  "output/tables/part4_LOO_targeted_comparisons.csv"
)

# ----------------------------------------------------------------------
# 13. POSTERIOR PREDICTIVE CHECKS
# ----------------------------------------------------------------------

save_pp_check <- function(
    model,
    model_name
) {
  
  png(
    file.path(
      "output/figures",
      paste0(
        model_name,
        "_posterior_predictive_check.png"
      )
    ),
    width = 1800,
    height = 1200,
    res = 180
  )
  
  print(
    pp_check(
      model,
      type = "bars",
      ndraws = 200
    )
  )
  
  dev.off()
}

iwalk(
  model_list,
  ~ save_pp_check(
    model = .x,
    model_name = .y
  )
)

# ----------------------------------------------------------------------
# 14. SAVE MODELS INCLUDING LOO RESULTS
# ----------------------------------------------------------------------

iwalk(
  model_list,
  function(model, model_name) {
    
    saveRDS(
      model,
      file.path(
        "output/models",
        paste0(model_name, "_with_LOO.rds")
      )
    )
  }
)

# ----------------------------------------------------------------------
# 15. FINAL SCREEN OUTPUT
# ----------------------------------------------------------------------

message("\nPart 4 completed successfully.")

message(
  "\nCommon sample used for ALL five models: n = ",
  nrow(analysis_sample_part4)
)

message(
  "\nMain files to inspect:\n",
  "  output/tables/part4_sampler_diagnostics_all_models.csv\n",
  "  output/tables/part4_neighbourhood_sigma_variance_ICC_all_models.csv\n",
  "  output/tables/part4_neighbourhood_variance_change.csv\n",
  "  output/tables/part4_LOO_model_comparison.csv\n",
  "  output/tables/part4_LOO_targeted_comparisons.csv\n",
  "  output/tables/ML2_objective_fixed_effects.csv\n",
  "  output/tables/ML3_perceived_fixed_effects.csv\n",
  "  output/tables/ML4_combined_fixed_effects.csv"
)

print(sample_overview)
print(diagnostics_table)
print(random_effects_table)
print(variance_change_table)
print(loo_comparison_table)

# ======================================================================
# END OF PART 4
# ======================================================================
# ======================================================================
# PART 5 — PARSIMONIOUS MULTILEVEL MODEL
# Conservative reduction based on the domain-specific full models
# brms + rstan
# ======================================================================
#
# Aim:
#   Estimate a parsimonious sensitivity model after the theory-driven
#   full models from Part 4.
#
# Selection strategy:
#   - ALL demographic control variables are retained, irrespective of their
#     posterior interval.
#   - Objective-environment variables are retained if they had a 95% credible
#     interval excluding zero for at least one mode contrast in ML2.
#   - Perceived-environment factor scores are retained if they had a 95%
#     credible interval excluding zero for at least one mode contrast in ML3.
#
# Why this strategy?
#   It avoids selecting predictors from the already-combined ML4 alone,
#   where collinearity/shared variance can suppress effects. Instead, each
#   environmental domain is screened in its own full model and the surviving
#   predictors are then combined in one parsimonious multilevel model.
#
# IMPORTANT:
#   - This is a sensitivity / parsimonious model, NOT a replacement for the
#     theory-driven ML4.
#   - If no objective predictor survives the ML2 screening, the script reports
#     this transparently and the parsimonious model will contain no objective
#     environmental predictor.
#   - All comparisons use the exact same Part 4 common sample.
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES, PATHS AND SETTINGS
# ----------------------------------------------------------------------

required_packages <- c(
  "brms",
  "rstan",
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "posterior",
  "bayesplot",
  "loo",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Part 5:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(brms)
library(rstan)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(posterior)
library(bayesplot)
library(loo)
library(stringr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

common_sample_path <-
  "output/data/part4_common_analysis_sample.rds"

ml2_results_path <-
  "output/tables/ML2_objective_fixed_effects.csv"

ml3_results_path <-
  "output/tables/ML3_perceived_fixed_effects.csv"

ml4_full_model_path <-
  "output/models/ML4_objective_plus_perceived_with_LOO.rds"

ml3_full_model_path <-
  "output/models/ML3_perceived_environment_with_LOO.rds"

ml2_full_model_path <-
  "output/models/ML2_objective_environment_with_LOO.rds"

dir.create("output", showWarnings = FALSE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

required_files <- c(
  common_sample_path,
  ml2_results_path,
  ml3_results_path,
  ml4_full_model_path,
  ml3_full_model_path,
  ml2_full_model_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "The following Part 4 files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

# Sampling settings: same as Part 4.
n_chains <- 4L
n_iter <- 2000L
n_warmup <- 1000L
adapt_delta_value <- 0.95
max_treedepth_value <- 12L
model_seed <- 1234L

n_cores <- min(
  n_chains,
  max(1L, parallel::detectCores())
)

# ----------------------------------------------------------------------
# 1. LOAD THE COMMON SAMPLE AND PART 4 RESULTS
# ----------------------------------------------------------------------

analysis_sample <- readRDS(
  common_sample_path
)

ml2_results <- read_csv(
  ml2_results_path,
  show_col_types = FALSE
)

ml3_results <- read_csv(
  ml3_results_path,
  show_col_types = FALSE
)

fit_ml4_full <- readRDS(
  ml4_full_model_path
)

fit_ml3_full <- readRDS(
  ml3_full_model_path
)

fit_ml2_full <- readRDS(
  ml2_full_model_path
)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- c(
  "FAC_presence_walking_infr_z",
  "FAC_presence_difficult_surface_z",
  "FAC_presence_greenery_z",
  "FAC_presence_park_equipment_z",
  "FAC_presence_cycling_infrastructure_z",
  "FAC_rate_accidents_z",
  "FAC_presence_primary_secondary_roads_z",
  "FAC_presence_high_parking_stress_z",
  "FAC_presence_moderate_parking_stress_z",
  "FAC_presence_PT_z"
)

factor_score_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

# ----------------------------------------------------------------------
# 2. IDENTIFY ENVIRONMENTAL PREDICTORS WITH CREDIBLE EFFECTS
# ----------------------------------------------------------------------
#
# brms parameter names contain a mode-specific prefix, for example:
#
#   muOnfoot_FAC_presence_walking_infr_z
#   muBike_FS_Cycling_friendly_environment_z
#
# We therefore identify a predictor by checking whether its exact variable
# name occurs at the END of a parameter name.
# ----------------------------------------------------------------------

extract_selected_predictors <- function(
    result_table,
    candidate_vars
) {
  
  if (!"credible_nonzero" %in% names(result_table)) {
    stop(
      "The fixed-effect table does not contain 'credible_nonzero'."
    )
  }
  
  result_table <- result_table %>%
    mutate(
      credible_nonzero = as.logical(credible_nonzero)
    ) %>%
    filter(credible_nonzero)
  
  selected <- candidate_vars[
    vapply(
      candidate_vars,
      function(variable_name) {
        any(
          stringr::str_ends(
            result_table$parameter,
            stringr::fixed(variable_name)
          )
        )
      },
      logical(1)
    )
  ]
  
  selected
}

selected_objective_vars <- extract_selected_predictors(
  result_table = ml2_results,
  candidate_vars = objective_vars
)

selected_perceived_vars <- extract_selected_predictors(
  result_table = ml3_results,
  candidate_vars = factor_score_vars
)

message(
  "\nObjective variables retained from ML2:\n",
  if (
    length(selected_objective_vars) == 0
  ) {
    "  NONE"
  } else {
    paste(
      paste0("  ", selected_objective_vars),
      collapse = "\n"
    )
  }
)

message(
  "\nPerceived variables retained from ML3:\n",
  if (
    length(selected_perceived_vars) == 0
  ) {
    "  NONE"
  } else {
    paste(
      paste0("  ", selected_perceived_vars),
      collapse = "\n"
    )
  }
)

# Save the selection decision transparently.
selection_table <- bind_rows(
  tibble(
    domain = "Objective",
    predictor = objective_vars,
    retained = objective_vars %in%
      selected_objective_vars
  ),
  tibble(
    domain = "Perceived",
    predictor = factor_score_vars,
    retained = factor_score_vars %in%
      selected_perceived_vars
  )
)

write_csv(
  selection_table,
  "output/tables/ML4p_variable_selection.csv"
)

# ----------------------------------------------------------------------
# 3. CREATE THE PARSIMONIOUS PREDICTOR SET
# ----------------------------------------------------------------------

parsimonious_environment_vars <- unique(
  c(
    selected_objective_vars,
    selected_perceived_vars
  )
)

if (length(parsimonious_environment_vars) == 0) {
  stop(
    "No environmental predictors were retained. ",
    "A parsimonious environmental model cannot be estimated."
  )
}

parsimonious_predictors <- c(
  control_vars,
  parsimonious_environment_vars
)

required_predictors <- setdiff(
  parsimonious_predictors,
  names(analysis_sample)
)

if (length(required_predictors) > 0) {
  stop(
    "The following selected predictors are absent from the common sample:\n",
    paste(required_predictors, collapse = "\n")
  )
}

# ----------------------------------------------------------------------
# 4. MODEL FORMULA
# ----------------------------------------------------------------------

formula_ml4p_text <- paste0(
  "Mode_factor ~ ",
  paste(
    parsimonious_predictors,
    collapse = " + "
  ),
  " + (1 | Neighbourhood_factor)"
)

formula_ml4p <- bf(
  stats::as.formula(
    formula_ml4p_text
  )
)

categorical_family <- categorical(
  link = "logit",
  refcat = "Private car"
)

message(
  "\nParsimonious model formula:\n",
  formula_ml4p_text,
  "\n"
)

# ----------------------------------------------------------------------
# 5. PRIORS
# ----------------------------------------------------------------------

prior_table_ml4p <- get_prior(
  formula = formula_ml4p,
  data = analysis_sample,
  family = categorical_family
)

dpars_ml4p <- unique(
  prior_table_ml4p$dpar[
    !is.na(prior_table_ml4p$dpar) &
      prior_table_ml4p$dpar != ""
  ]
)

if (length(dpars_ml4p) == 0) {
  stop(
    "No category-specific dpar names were found."
  )
}

model_priors_ml4p <- do.call(
  c,
  lapply(
    dpars_ml4p,
    function(current_dpar) {
      
      c(
        set_prior(
          "normal(0, 1)",
          class = "b",
          dpar = current_dpar
        ),
        
        set_prior(
          "student_t(3, 0, 2.5)",
          class = "Intercept",
          dpar = current_dpar
        ),
        
        set_prior(
          "exponential(1)",
          class = "sd",
          group = "Neighbourhood_factor",
          dpar = current_dpar
        )
      )
    }
  )
)

write_csv(
  prior_table_ml4p,
  "output/tables/ML4p_available_priors.csv"
)

validate_prior(
  prior = model_priors_ml4p,
  formula = formula_ml4p,
  data = analysis_sample,
  family = categorical_family
)

message("ML4p priors validated successfully.")

# ----------------------------------------------------------------------
# 6. ESTIMATE THE PARSIMONIOUS MODEL
# ----------------------------------------------------------------------

fit_ml4p <- brm(
  formula = formula_ml4p,
  data = analysis_sample,
  family = categorical_family,
  prior = model_priors_ml4p,
  backend = "rstan",
  chains = n_chains,
  iter = n_iter,
  warmup = n_warmup,
  cores = n_cores,
  seed = model_seed,
  control = list(
    adapt_delta = adapt_delta_value,
    max_treedepth = max_treedepth_value
  ),
  save_pars = save_pars(all = TRUE),
  file = "output/models/ML4p_parsimonious_environment",
  file_refit = "on_change",
  refresh = 100
)

saveRDS(
  fit_ml4p,
  "output/models/ML4p_parsimonious_environment.rds"
)

# ----------------------------------------------------------------------
# 7. SAMPLER DIAGNOSTICS
# ----------------------------------------------------------------------

nuts <- brms::nuts_params(
  fit_ml4p
)

model_draws <- brms::as_draws_array(
  fit_ml4p
)

draw_summary <- posterior::summarise_draws(
  model_draws,
  posterior::default_convergence_measures()
)

diagnostics_ml4p <- tibble(
  model = "ML4p_parsimonious_environment",
  
  divergent_transitions = sum(
    nuts$Parameter == "divergent__" &
      nuts$Value == 1
  ),
  
  maximum_treedepth_hits = sum(
    nuts$Parameter == "treedepth__" &
      nuts$Value >= max_treedepth_value
  ),
  
  minimum_bulk_ess = min(
    draw_summary$ess_bulk,
    na.rm = TRUE
  ),
  
  minimum_tail_ess = min(
    draw_summary$ess_tail,
    na.rm = TRUE
  ),
  
  maximum_rhat = max(
    draw_summary$rhat,
    na.rm = TRUE
  )
)

write_csv(
  diagnostics_ml4p,
  "output/tables/ML4p_sampler_diagnostics.csv"
)

print(diagnostics_ml4p)

# ----------------------------------------------------------------------
# 8. FIXED EFFECTS AND ODDS RATIOS
# ----------------------------------------------------------------------

fixed_effects_ml4p <- as.data.frame(
  brms::fixef(
    fit_ml4p,
    probs = c(
      0.025,
      0.975
    )
  )
) %>%
  rownames_to_column(
    "parameter"
  ) %>%
  as_tibble() %>%
  transmute(
    model =
      "ML4p_parsimonious_environment",
    parameter = parameter,
    estimate = Estimate,
    estimate_error = Est.Error,
    lower_95 = Q2.5,
    upper_95 = Q97.5,
    
    odds_ratio =
      exp(estimate),
    
    odds_ratio_lower_95 =
      exp(lower_95),
    
    odds_ratio_upper_95 =
      exp(upper_95),
    
    credible_nonzero =
      lower_95 > 0 |
      upper_95 < 0
  )

write_csv(
  fixed_effects_ml4p,
  "output/tables/ML4p_fixed_effects.csv"
)

# ----------------------------------------------------------------------
# 9. RANDOM-INTERCEPT SD, VARIANCE AND APPROXIMATE ICC
# ----------------------------------------------------------------------

draws_df <- brms::as_draws_df(
  fit_ml4p
)

sd_parameters <- grep(
  "^sd_Neighbourhood_factor__.*_Intercept$",
  names(draws_df),
  value = TRUE
)

if (length(sd_parameters) == 0) {
  stop(
    "No neighbourhood random-intercept SD parameters were found."
  )
}

clean_contrast_name <- function(
    parameter_name
) {
  
  parameter_name %>%
    sub(
      "^sd_Neighbourhood_factor__",
      "",
      .
    ) %>%
    sub(
      "_Intercept$",
      "",
      .
    ) %>%
    sub(
      "^mu",
      "",
      .
    )
}

random_effects_ml4p <- map_dfr(
  sd_parameters,
  function(parameter_name) {
    
    sd_draws <-
      as.numeric(
        draws_df[[parameter_name]]
      )
    
    variance_draws <-
      sd_draws^2
    
    icc_draws <-
      variance_draws /
      (
        variance_draws +
          (pi^2 / 3)
      )
    
    tibble(
      model =
        "ML4p_parsimonious_environment",
      
      parameter =
        parameter_name,
      
      contrast_vs_private_car =
        clean_contrast_name(
          parameter_name
        ),
      
      sigma_mean =
        mean(sd_draws),
      
      sigma_median =
        median(sd_draws),
      
      sigma_lower =
        quantile(
          sd_draws,
          0.025,
          names = FALSE
        ),
      
      sigma_upper =
        quantile(
          sd_draws,
          0.975,
          names = FALSE
        ),
      
      variance_mean =
        mean(variance_draws),
      
      variance_median =
        median(variance_draws),
      
      variance_lower =
        quantile(
          variance_draws,
          0.025,
          names = FALSE
        ),
      
      variance_upper =
        quantile(
          variance_draws,
          0.975,
          names = FALSE
        ),
      
      ICC_mean =
        mean(icc_draws),
      
      ICC_median =
        median(icc_draws),
      
      ICC_lower =
        quantile(
          icc_draws,
          0.025,
          names = FALSE
        ),
      
      ICC_upper =
        quantile(
          icc_draws,
          0.975,
          names = FALSE
        )
    )
  }
)

write_csv(
  random_effects_ml4p,
  "output/tables/ML4p_neighbourhood_sigma_variance_ICC.csv"
)

# ----------------------------------------------------------------------
# 10. ADD LOO AND COMPARE WITH PART 4 MODELS
# ----------------------------------------------------------------------

fit_ml4p <- add_criterion(
  fit_ml4p,
  criterion = "loo"
)

# Ensure the previously saved models contain LOO.
if (
  is.null(fit_ml4_full$criteria$loo) |
  is.null(fit_ml3_full$criteria$loo) |
  is.null(fit_ml2_full$criteria$loo)
) {
  stop(
    "At least one Part 4 comparison model does not contain a LOO criterion."
  )
}

loo_compare_models <- list(
  ML2_objective = fit_ml2_full$criteria$loo,
  ML3_perceived = fit_ml3_full$criteria$loo,
  ML4_full = fit_ml4_full$criteria$loo,
  ML4p_parsimonious = fit_ml4p$criteria$loo
)

loo_comparison_ml4p <- do.call(
  loo::loo_compare,
  unname(
    loo_compare_models
  )
)

loo_comparison_ml4p_table <- as.data.frame(
  loo_comparison_ml4p
) %>%
  tibble::rownames_to_column(
    var = "model_name"
  ) %>%
  tibble::as_tibble()

write_csv(
  loo_comparison_ml4p_table,
  "output/tables/ML4p_LOO_comparison.csv"
)

# Pairwise full ML4 versus parsimonious ML4p.
loo_ml4_vs_ml4p <- as.data.frame(
  loo::loo_compare(
    fit_ml4_full$criteria$loo,
    fit_ml4p$criteria$loo
  )
) %>%
  tibble::rownames_to_column(
    var = "model_name"
  ) %>%
  as_tibble()

write_csv(
  loo_ml4_vs_ml4p,
  "output/tables/ML4_full_vs_ML4p_LOO.csv"
)

# ----------------------------------------------------------------------
# 11. POSTERIOR PREDICTIVE CHECK
# ----------------------------------------------------------------------

png(
  "output/figures/ML4p_posterior_predictive_check.png",
  width = 1800,
  height = 1200,
  res = 180
)

print(
  pp_check(
    fit_ml4p,
    type = "bars",
    ndraws = 200
  )
)

dev.off()

# ----------------------------------------------------------------------
# 12. SAVE MODEL WITH LOO
# ----------------------------------------------------------------------

saveRDS(
  fit_ml4p,
  "output/models/ML4p_parsimonious_environment_with_LOO.rds"
)

# ----------------------------------------------------------------------
# 13. FINAL OUTPUT
# ----------------------------------------------------------------------

message(
  "\nPart 5 completed successfully."
)

message(
  "\nNumber of demographic controls retained: ",
  length(control_vars)
)

message(
  "\nNumber of objective environmental variables retained: ",
  length(selected_objective_vars)
)

message(
  "\nNumber of perceived environmental variables retained: ",
  length(selected_perceived_vars)
)

message(
  "\nTotal environmental variables in ML4p: ",
  length(parsimonious_environment_vars)
)

message(
  "\nImportant files:\n",
  "  output/tables/ML4p_variable_selection.csv\n",
  "  output/tables/ML4p_sampler_diagnostics.csv\n",
  "  output/tables/ML4p_fixed_effects.csv\n",
  "  output/tables/ML4p_neighbourhood_sigma_variance_ICC.csv\n",
  "  output/tables/ML4p_LOO_comparison.csv\n",
  "  output/tables/ML4_full_vs_ML4p_LOO.csv\n",
  "  output/figures/ML4p_posterior_predictive_check.png"
)

print(selection_table)
print(fixed_effects_ml4p)
print(random_effects_ml4p)
print(loo_comparison_ml4p_table)

# ======================================================================
# END OF PART 5
# ======================================================================
# ======================================================================
# PART 5B — MULTICOLLINEARITY CHECK FOR ML4p
# ======================================================================
#
# Purpose:
#   Check whether predictors retained in the parsimonious ML4p model are
#   strongly collinear.
#
# The checks include:
#   1. Pairwise correlations among retained environmental predictors.
#   2. VIF / tolerance based on an auxiliary linear model matrix.
#   3. Condition indices from the standardised predictor matrix.
#
# Notes:
#   - VIF is a property of the predictor matrix, not of the multinomial
#     outcome itself, so it can be assessed using the design matrix.
#   - Demographic controls are included in the VIF calculation because
#     multicollinearity can arise between any predictors in ML4p.
# ======================================================================

rm(list = ls())
gc()

required_packages <- c(
  "dplyr",
  "readr",
  "tibble",
  "purrr",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(stringr)

common_sample_path <-
  "output/data/part4_common_analysis_sample.rds"

selection_path <-
  "output/tables/ML4p_variable_selection.csv"

if (!file.exists(common_sample_path)) {
  stop("Part 4 common sample not found.")
}

if (!file.exists(selection_path)) {
  stop("ML4p variable-selection table not found. Run Part 5 first.")
}

analysis_sample <- readRDS(common_sample_path)

selection_table <- read_csv(
  selection_path,
  show_col_types = FALSE
)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

selected_environment_vars <- selection_table %>%
  mutate(retained = as.logical(retained)) %>%
  filter(retained) %>%
  pull(predictor)

ml4p_predictors <- unique(
  c(
    control_vars,
    selected_environment_vars
  )
)

missing_predictors <- setdiff(
  ml4p_predictors,
  names(analysis_sample)
)

if (length(missing_predictors) > 0) {
  stop(
    "The following ML4p predictors are missing:\n",
    paste(missing_predictors, collapse = "\n")
  )
}

multicollinearity_data <- analysis_sample %>%
  select(
    all_of(ml4p_predictors)
  ) %>%
  filter(
    if_all(
      everything(),
      ~ !is.na(.x)
    )
  )

# ----------------------------------------------------------------------
# 1. PAIRWISE CORRELATIONS
# ----------------------------------------------------------------------

correlation_matrix <- cor(
  multicollinearity_data,
  use = "complete.obs"
)

write_csv(
  as.data.frame(correlation_matrix) %>%
    rownames_to_column("predictor"),
  "output/tables/ML4p_predictor_correlation_matrix.csv"
)

pairwise_correlations <- as.data.frame(
  as.table(correlation_matrix)
) %>%
  as_tibble() %>%
  rename(
    predictor_1 = Var1,
    predictor_2 = Var2,
    correlation = Freq
  ) %>%
  mutate(
    predictor_1 = as.character(predictor_1),
    predictor_2 = as.character(predictor_2)
  ) %>%
  filter(
    predictor_1 < predictor_2
  ) %>%
  arrange(
    desc(abs(correlation))
  )

write_csv(
  pairwise_correlations,
  "output/tables/ML4p_pairwise_correlations.csv"
)

# ----------------------------------------------------------------------
# 2. VIF AND TOLERANCE
# ----------------------------------------------------------------------
#
# For each predictor x_j, regress x_j on all remaining predictors.
#
# VIF_j = 1 / (1 - R²_j)
# tolerance_j = 1 / VIF_j
# ----------------------------------------------------------------------

calculate_vif <- function(
    data,
    target_variable
) {
  
  other_variables <- setdiff(
    names(data),
    target_variable
  )
  
  auxiliary_formula <- as.formula(
    paste0(
      target_variable,
      " ~ ",
      paste(
        other_variables,
        collapse = " + "
      )
    )
  )
  
  auxiliary_model <- lm(
    auxiliary_formula,
    data = data
  )
  
  r_squared <- summary(
    auxiliary_model
  )$r.squared
  
  vif <- 1 / (1 - r_squared)
  
  tibble(
    predictor = target_variable,
    auxiliary_R_squared = r_squared,
    tolerance = 1 / vif,
    VIF = vif
  )
}

vif_table <- map_dfr(
  names(multicollinearity_data),
  ~ calculate_vif(
    multicollinearity_data,
    .x
  )
) %>%
  arrange(
    desc(VIF)
  ) %>%
  mutate(
    flag_VIF_above_5 = VIF > 5,
    flag_VIF_above_10 = VIF > 10
  )

write_csv(
  vif_table,
  "output/tables/ML4p_VIF_tolerance.csv"
)

# ----------------------------------------------------------------------
# 3. CONDITION INDICES
# ----------------------------------------------------------------------

X <- model.matrix(
  ~ .,
  data = multicollinearity_data
)

# Remove intercept.
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

# Standardise columns.
X_scaled <- scale(X)

eigenvalues <- eigen(
  crossprod(X_scaled),
  symmetric = TRUE,
  only.values = TRUE
)$values

condition_indices <- sqrt(
  max(eigenvalues) /
    eigenvalues
)

condition_index_table <- tibble(
  dimension = seq_along(eigenvalues),
  eigenvalue = eigenvalues,
  condition_index = condition_indices
)

write_csv(
  condition_index_table,
  "output/tables/ML4p_condition_indices.csv"
)

# ----------------------------------------------------------------------
# 4. SUMMARY
# ----------------------------------------------------------------------

summary_table <- tibble(
  statistic = c(
    "Maximum absolute pairwise correlation",
    "Maximum VIF",
    "Minimum tolerance",
    "Maximum condition index"
  ),
  value = c(
    max(
      abs(pairwise_correlations$correlation),
      na.rm = TRUE
    ),
    max(
      vif_table$VIF,
      na.rm = TRUE
    ),
    min(
      vif_table$tolerance,
      na.rm = TRUE
    ),
    max(
      condition_index_table$condition_index,
      na.rm = TRUE
    )
  )
)

write_csv(
  summary_table,
  "output/tables/ML4p_multicollinearity_summary.csv"
)

message("\nML4p multicollinearity check completed.")

print(vif_table)
print(head(pairwise_correlations, 10))
print(summary_table)

# ======================================================================
# END OF PART 5B
# ======================================================================
# ======================================================================
# PART 6 — WITHIN-BETWEEN MULTILEVEL MODE-CHOICE MODEL
# Perceived environment decomposed into:
#   - within-neighbourhood deviations
#   - between-neighbourhood mean perceptions
#
# Objective GIS variables remain neighbourhood-level predictors.
# brms + rstan
# ======================================================================
#
# Interpretation:
#
# For each perceived factor score X_ij:
#
#   Between component:
#       Xbar_j = mean perception in neighbourhood j
#
#   Within component:
#       X_ij - Xbar_j
#
# Therefore:
#   - within coefficient:
#       association with being more/less positive than other respondents
#       living in the same neighbourhood;
#
#   - between coefficient:
#       association with living in a neighbourhood whose respondents have,
#       on average, a more positive perception.
#
# IMPORTANT:
#   - Objective variables already vary only between neighbourhoods, so they
#     cannot be decomposed into a meaningful within component.
#   - With only 25 neighbourhoods, between-neighbourhood estimates should be
#     interpreted cautiously.
#   - This script uses the Part 4 common sample, ensuring direct comparability.
# ======================================================================

rm(list = ls())
gc()

required_packages <- c(
  "brms",
  "rstan",
  "dplyr",
  "readr",
  "purrr",
  "tibble",
  "posterior",
  "bayesplot",
  "loo",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Part 6:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(brms)
library(rstan)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(posterior)
library(bayesplot)
library(loo)
library(stringr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

common_sample_path <-
  "output/data/part4_common_analysis_sample.rds"

selection_path <-
  "output/tables/ML4p_variable_selection.csv"

ml4p_model_path <-
  "output/models/ML4p_parsimonious_environment_with_LOO.rds"

if (!file.exists(common_sample_path)) {
  stop("Part 4 common sample not found.")
}

if (!file.exists(selection_path)) {
  stop("ML4p selection file not found.")
}

if (!file.exists(ml4p_model_path)) {
  stop("ML4p model with LOO not found.")
}

analysis_sample <- readRDS(
  common_sample_path
)

selection_table <- read_csv(
  selection_path,
  show_col_types = FALSE
)

fit_ml4p <- readRDS(
  ml4p_model_path
)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- c(
  "FAC_presence_walking_infr_z",
  "FAC_presence_difficult_surface_z",
  "FAC_presence_greenery_z",
  "FAC_presence_park_equipment_z",
  "FAC_presence_cycling_infrastructure_z",
  "FAC_rate_accidents_z",
  "FAC_presence_primary_secondary_roads_z",
  "FAC_presence_high_parking_stress_z",
  "FAC_presence_moderate_parking_stress_z",
  "FAC_presence_PT_z"
)

all_factor_score_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

# Use the same perceived predictors retained in ML4p.
selected_perceived_vars <- selection_table %>%
  mutate(retained = as.logical(retained)) %>%
  filter(
    domain == "Perceived",
    retained
  ) %>%
  pull(predictor)

# Use the same objective predictors retained in ML4p.
selected_objective_vars <- selection_table %>%
  mutate(retained = as.logical(retained)) %>%
  filter(
    domain == "Objective",
    retained
  ) %>%
  pull(predictor)

if (length(selected_perceived_vars) == 0) {
  stop(
    "No perceived predictors were retained in ML4p. ",
    "A within-between model cannot be estimated."
  )
}

# ----------------------------------------------------------------------
# 1. CREATE WITHIN AND BETWEEN COMPONENTS
# ----------------------------------------------------------------------

wb_data <- analysis_sample

for (variable_name in selected_perceived_vars) {
  
  between_name <- paste0(
    variable_name,
    "_between"
  )
  
  within_name <- paste0(
    variable_name,
    "_within"
  )
  
  neighbourhood_means <- wb_data %>%
    group_by(
      Neighbourhood_factor
    ) %>%
    summarise(
      neighbourhood_mean =
        mean(
          .data[[variable_name]],
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  wb_data <- wb_data %>%
    left_join(
      neighbourhood_means,
      by = "Neighbourhood_factor"
    ) %>%
    mutate(
      !!between_name :=
        neighbourhood_mean,
      
      !!within_name :=
        .data[[variable_name]] -
        neighbourhood_mean
    ) %>%
    select(
      -neighbourhood_mean
    )
}

between_vars <- paste0(
  selected_perceived_vars,
  "_between"
)

within_vars <- paste0(
  selected_perceived_vars,
  "_within"
)

# ----------------------------------------------------------------------
# 2. VERIFY THE DECOMPOSITION
# ----------------------------------------------------------------------

within_mean_check <- map_dfr(
  within_vars,
  function(variable_name) {
    
    wb_data %>%
      group_by(
        Neighbourhood_factor
      ) %>%
      summarise(
        neighbourhood_within_mean =
          mean(
            .data[[variable_name]],
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      summarise(
        variable = variable_name,
        maximum_absolute_neighbourhood_mean =
          max(
            abs(
              neighbourhood_within_mean
            ),
            na.rm = TRUE
          )
      )
  }
)

write_csv(
  within_mean_check,
  "output/tables/within_between_decomposition_check.csv"
)

# ----------------------------------------------------------------------
# 3. STANDARDISE BETWEEN COMPONENTS ACROSS NEIGHBOURHOODS
# ----------------------------------------------------------------------
#
# The original factor scores are already z-standardised at the individual
# level. The within deviations inherit that scale.
#
# For easier comparison of between effects, neighbourhood means are
# re-standardised across the 25 neighbourhoods.
# ----------------------------------------------------------------------

for (variable_name in between_vars) {
  
  # Extract one value per neighbourhood.
  neighbourhood_level_values <- wb_data %>%
    distinct(
      Neighbourhood_factor,
      .data[[variable_name]]
    )
  
  neighbourhood_mean <-
    mean(
      neighbourhood_level_values[[variable_name]],
      na.rm = TRUE
    )
  
  neighbourhood_sd <-
    sd(
      neighbourhood_level_values[[variable_name]],
      na.rm = TRUE
    )
  
  if (
    is.na(neighbourhood_sd) ||
    neighbourhood_sd == 0
  ) {
    stop(
      "No between-neighbourhood variation for ",
      variable_name
    )
  }
  
  wb_data[[variable_name]] <-
    (
      wb_data[[variable_name]] -
        neighbourhood_mean
    ) /
    neighbourhood_sd
}

# Standardise within deviations globally for easier coefficient comparison.
for (variable_name in within_vars) {
  
  variable_sd <-
    sd(
      wb_data[[variable_name]],
      na.rm = TRUE
    )
  
  if (
    is.na(variable_sd) ||
    variable_sd == 0
  ) {
    stop(
      "No within-neighbourhood variation for ",
      variable_name
    )
  }
  
  wb_data[[variable_name]] <-
    wb_data[[variable_name]] /
    variable_sd
}

saveRDS(
  wb_data,
  "output/data/part6_within_between_analysis_data.rds"
)

# ----------------------------------------------------------------------
# 4. CHECK CORRELATIONS
# ----------------------------------------------------------------------

wb_predictor_vars <- c(
  control_vars,
  selected_objective_vars,
  within_vars,
  between_vars
)

wb_correlation_matrix <- cor(
  wb_data[
    wb_predictor_vars
  ],
  use = "pairwise.complete.obs"
)

write_csv(
  as.data.frame(
    wb_correlation_matrix
  ) %>%
    rownames_to_column("predictor"),
  "output/tables/part6_within_between_correlation_matrix.csv"
)

# ----------------------------------------------------------------------
# 5. BUILD WITHIN-BETWEEN MODEL
# ----------------------------------------------------------------------

wb_predictors <- c(
  control_vars,
  selected_objective_vars,
  within_vars,
  between_vars
)

formula_wb_text <- paste0(
  "Mode_factor ~ ",
  paste(
    wb_predictors,
    collapse = " + "
  ),
  " + (1 | Neighbourhood_factor)"
)

formula_wb <- bf(
  as.formula(
    formula_wb_text
  )
)

categorical_family <- categorical(
  link = "logit",
  refcat = "Private car"
)

message(
  "\nWithin-between model formula:\n",
  formula_wb_text,
  "\n"
)

# ----------------------------------------------------------------------
# 6. PRIORS
# ----------------------------------------------------------------------

prior_table_wb <- get_prior(
  formula = formula_wb,
  data = wb_data,
  family = categorical_family
)

dpars_wb <- unique(
  prior_table_wb$dpar[
    !is.na(prior_table_wb$dpar) &
      prior_table_wb$dpar != ""
  ]
)

if (length(dpars_wb) == 0) {
  stop(
    "No category-specific dpar names were found."
  )
}

model_priors_wb <- do.call(
  c,
  lapply(
    dpars_wb,
    function(current_dpar) {
      
      c(
        set_prior(
          "normal(0, 1)",
          class = "b",
          dpar = current_dpar
        ),
        
        set_prior(
          "student_t(3, 0, 2.5)",
          class = "Intercept",
          dpar = current_dpar
        ),
        
        set_prior(
          "exponential(1)",
          class = "sd",
          group = "Neighbourhood_factor",
          dpar = current_dpar
        )
      )
    }
  )
)

validate_prior(
  prior = model_priors_wb,
  formula = formula_wb,
  data = wb_data,
  family = categorical_family
)

write_csv(
  prior_table_wb,
  "output/tables/part6_available_priors.csv"
)

# ----------------------------------------------------------------------
# 7. ESTIMATE MODEL
# ----------------------------------------------------------------------

fit_within_between <- brm(
  formula = formula_wb,
  data = wb_data,
  family = categorical_family,
  prior = model_priors_wb,
  backend = "rstan",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = min(
    4L,
    max(
      1L,
      parallel::detectCores()
    )
  ),
  seed = 1234,
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 12
  ),
  save_pars = save_pars(
    all = TRUE
  ),
  file =
    "output/models/ML5_within_between_perceived_environment",
  file_refit = "on_change",
  refresh = 100
)

saveRDS(
  fit_within_between,
  "output/models/ML5_within_between_perceived_environment.rds"
)

# ----------------------------------------------------------------------
# 8. DIAGNOSTICS
# ----------------------------------------------------------------------

nuts <- brms::nuts_params(
  fit_within_between
)

model_draws <- brms::as_draws_array(
  fit_within_between
)

draw_summary <- posterior::summarise_draws(
  model_draws,
  posterior::default_convergence_measures()
)

diagnostics_wb <- tibble(
  model =
    "ML5_within_between_perceived_environment",
  
  divergent_transitions =
    sum(
      nuts$Parameter == "divergent__" &
        nuts$Value == 1
    ),
  
  maximum_treedepth_hits =
    sum(
      nuts$Parameter == "treedepth__" &
        nuts$Value >= 12
    ),
  
  minimum_bulk_ess =
    min(
      draw_summary$ess_bulk,
      na.rm = TRUE
    ),
  
  minimum_tail_ess =
    min(
      draw_summary$ess_tail,
      na.rm = TRUE
    ),
  
  maximum_rhat =
    max(
      draw_summary$rhat,
      na.rm = TRUE
    )
)

write_csv(
  diagnostics_wb,
  "output/tables/part6_sampler_diagnostics.csv"
)

# ----------------------------------------------------------------------
# 9. FIXED EFFECTS
# ----------------------------------------------------------------------

fixed_effects_wb <- as.data.frame(
  brms::fixef(
    fit_within_between,
    probs = c(
      0.025,
      0.975
    )
  )
) %>%
  rownames_to_column(
    "parameter"
  ) %>%
  as_tibble() %>%
  mutate(
    odds_ratio =
      exp(Estimate),
    
    odds_ratio_lower_95 =
      exp(Q2.5),
    
    odds_ratio_upper_95 =
      exp(Q97.5),
    
    credible_nonzero =
      Q2.5 > 0 |
      Q97.5 < 0,
    
    effect_type =
      case_when(
        str_detect(
          parameter,
          "_within$"
        ) ~ "Within-neighbourhood perception",
        
        str_detect(
          parameter,
          "_between$"
        ) ~ "Between-neighbourhood perception",
        
        str_detect(
          parameter,
          "FAC_"
        ) ~ "Objective neighbourhood environment",
        
        TRUE ~ "Demographic/control"
      )
  )

write_csv(
  fixed_effects_wb,
  "output/tables/part6_within_between_fixed_effects.csv"
)

# ----------------------------------------------------------------------
# 10. RANDOM EFFECTS / ICC
# ----------------------------------------------------------------------

draws_df <- brms::as_draws_df(
  fit_within_between
)

sd_parameters <- grep(
  "^sd_Neighbourhood_factor__.*_Intercept$",
  names(draws_df),
  value = TRUE
)

if (length(sd_parameters) == 0) {
  stop(
    "No neighbourhood random-intercept SD parameters found."
  )
}

clean_contrast_name <- function(parameter_name) {
  
  parameter_name %>%
    sub(
      "^sd_Neighbourhood_factor__",
      "",
      .
    ) %>%
    sub(
      "_Intercept$",
      "",
      .
    ) %>%
    sub(
      "^mu",
      "",
      .
    )
}

random_effects_wb <- map_dfr(
  sd_parameters,
  function(parameter_name) {
    
    sd_draws <-
      as.numeric(
        draws_df[[parameter_name]]
      )
    
    variance_draws <-
      sd_draws^2
    
    icc_draws <-
      variance_draws /
      (
        variance_draws +
          (pi^2 / 3)
      )
    
    tibble(
      parameter =
        parameter_name,
      
      contrast_vs_private_car =
        clean_contrast_name(
          parameter_name
        ),
      
      sigma_mean =
        mean(sd_draws),
      
      variance_mean =
        mean(variance_draws),
      
      ICC_mean =
        mean(icc_draws),
      
      ICC_lower =
        quantile(
          icc_draws,
          0.025,
          names = FALSE
        ),
      
      ICC_upper =
        quantile(
          icc_draws,
          0.975,
          names = FALSE
        )
    )
  }
)

write_csv(
  random_effects_wb,
  "output/tables/part6_neighbourhood_sigma_variance_ICC.csv"
)

# ----------------------------------------------------------------------
# 11. LOO COMPARISON WITH ML4p
# ----------------------------------------------------------------------

fit_within_between <- add_criterion(
  fit_within_between,
  criterion = "loo"
)

if (
  is.null(
    fit_ml4p$criteria$loo
  )
) {
  stop(
    "ML4p does not contain a LOO criterion."
  )
}

loo_wb_vs_ml4p <- as.data.frame(
  loo::loo_compare(
    fit_ml4p$criteria$loo,
    fit_within_between$criteria$loo
  )
) %>%
  rownames_to_column(
    "model_name"
  ) %>%
  as_tibble()

write_csv(
  loo_wb_vs_ml4p,
  "output/tables/part6_LOO_ML4p_vs_within_between.csv"
)

# ----------------------------------------------------------------------
# 12. POSTERIOR PREDICTIVE CHECK
# ----------------------------------------------------------------------

png(
  "output/figures/ML5_within_between_posterior_predictive_check.png",
  width = 1800,
  height = 1200,
  res = 180
)

print(
  pp_check(
    fit_within_between,
    type = "bars",
    ndraws = 200
  )
)

dev.off()

saveRDS(
  fit_within_between,
  "output/models/ML5_within_between_perceived_environment_with_LOO.rds"
)

message(
  "\nPart 6 completed successfully."
)

message(
  "\nWithin-neighbourhood perceived predictors:\n",
  paste(
    within_vars,
    collapse = "\n"
  )
)

message(
  "\nBetween-neighbourhood perceived predictors:\n",
  paste(
    between_vars,
    collapse = "\n"
  )
)

print(diagnostics_wb)
print(fixed_effects_wb)
print(random_effects_wb)
print(loo_wb_vs_ml4p)

# ======================================================================
# END OF PART 6
# ======================================================================
# ======================================================================
# SCRIPT 7 — TARGETED ICLV / HYBRID CHOICE BENCHMARK
# Apollo: three perceived-environment latent variables + multinomial choice
# ======================================================================
#
# PURPOSE
# -------
# This is a targeted benchmark for the factor-score multilevel analysis.
# It explicitly models measurement error in the three perceived constructs
# retained in the parsimonious ML4p model:
#
#   1. High_quality_cycling_infrastructure
#   2. Cycling_friendly_environment
#   3. Positive_parking_experiences
#
# The original Likert indicators are used as ordered-logit measurement
# models, while the three latent variables enter the multinomial mode-choice
# model together with the same demographic controls used previously.
#
# IMPORTANT INTERPRETATION
# ------------------------
# - This ICLV is a BENCHMARK / SENSITIVITY ANALYSIS, not a replacement for
#   the multilevel model.
# - Neighbourhood random intercepts are NOT included in Apollo.
# - The model is intended to assess whether the substantive perceived-
#   environment findings remain when the constructs are treated as latent
#   rather than as plug-in factor scores.
# - Do NOT compare the total ICLV AIC/log-likelihood directly with ML4p:
#   the ICLV likelihood also contains the Likert measurement models.
# - Compare primarily the direction and uncertainty of the latent-variable
#   effects on mode choice with the factor-score results.
#
# LATENT VARIABLE STRUCTURAL EQUATIONS
# ------------------------------------
# For this benchmark, each latent variable is represented by a standard
# normal latent disturbance only:
#
#   LV_HQCI = eta_HQCI
#   LV_CFE  = eta_CFE
#   LV_PPE  = eta_PPE
#
# This keeps the benchmark focused on measurement error. Demographics enter
# the choice model directly rather than additionally explaining the LVs.
#
# DATA
# ----
# Uses the exact Part 4 common sample so the respondent set is the same as
# the multilevel factor-score models.
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES AND FOLDERS
# ----------------------------------------------------------------------

required_packages <- c(
  "apollo",
  "dplyr",
  "readr",
  "tibble",
  "purrr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Script 7:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(apollo)
library(dplyr)
library(readr)
library(tibble)
library(purrr)

dir.create("output", showWarnings = FALSE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)

apollo_initialise()

# ----------------------------------------------------------------------
# 1. USER SETTINGS
# ----------------------------------------------------------------------

data_path <-
  "output/data/part4_common_analysis_sample.rds"

model_name <-
  "ICLV_benchmark_three_perceived_constructs"

# Start with 500 Halton draws. After a stable run, the benchmark can be
# repeated with 1000+ draws as a simulation-stability check.
n_draws <- 500L

if (!file.exists(data_path)) {
  stop(
    "Part 4 common analysis sample was not found:\n",
    data_path
  )
}

apollo_control <- list(
  modelName = model_name,
  modelDescr =
    "Targeted ICLV benchmark with three perceived-environment latent variables",
  indivID = "respondent_id_internal",
  mixing = TRUE,
  nCores = max(
    1L,
    min(
      4L,
      parallel::detectCores() - 1L
    )
  ),
  outputDirectory = "output/models"
)

# ----------------------------------------------------------------------
# 2. LOAD AND PREPARE THE COMMON SAMPLE
# ----------------------------------------------------------------------

database_raw <- readRDS(
  data_path
)

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

# Measurement indicators retained for the three latent constructs.
hqci_indicators <- c(
  "Qualitative_cycling_lanes",
  "Maintained_cycling_infrastructure",
  "Sufficient_cycling_infrastructure",
  "Maintained_streets_and_squares"
)

cfe_indicators <- c(
  "Cycling_friendly_neighborhood",
  "Enjoyable_cycling",
  "Feeling_safe_in_traffic_as_cyclist",
  "Busy_traffic_causing_unsafe_cycling_experiences"
)

ppe_indicators <- c(
  "Accessible_by_car",
  "Easily_finding_parking_spot_residents",
  "Easily_finding_parking_spot_visitors"
)

indicator_vars <- c(
  hqci_indicators,
  cfe_indicators,
  ppe_indicators
)

required_vars <- unique(
  c(
    "respondent_id_internal",
    "Mode_factor",
    control_vars,
    indicator_vars
  )
)

missing_vars <- setdiff(
  required_vars,
  names(database_raw)
)

if (length(missing_vars) > 0) {
  stop(
    "The following variables required by Script 7 are missing:\n",
    paste(missing_vars, collapse = "\n")
  )
}

# One row per respondent.
if (anyDuplicated(
  database_raw$respondent_id_internal
) > 0) {
  stop(
    "Duplicate respondent IDs detected. ",
    "This benchmark assumes one choice row per respondent."
  )
}

# Recode mode choice numerically for Apollo.
database <- database_raw %>%
  mutate(
    choice = case_when(
      as.character(Mode_factor) == "Private car" ~ 1L,
      as.character(Mode_factor) == "On foot" ~ 2L,
      as.character(Mode_factor) == "Bike" ~ 3L,
      as.character(Mode_factor) == "Public transport" ~ 4L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(
    respondent_id_internal,
    choice,
    all_of(control_vars),
    all_of(indicator_vars)
  ) %>%
  filter(
    if_all(
      everything(),
      ~ !is.na(.x)
    )
  )

if (nrow(database) == 0) {
  stop("No complete observations remain for the ICLV benchmark.")
}

if (!all(database$choice %in% 1:4)) {
  stop("Unexpected mode-choice codes after recoding.")
}

# Validate five-point Likert indicators.
indicator_validation <- map_dfr(
  indicator_vars,
  function(item) {
    x <- database[[item]]
    
    tibble(
      indicator = item,
      n = length(x),
      minimum = min(x),
      maximum = max(x),
      n_unique = n_distinct(x),
      valid_1_to_5 = all(x %in% 1:5)
    )
  }
)

write_csv(
  indicator_validation,
  "output/tables/ICLV_indicator_validation.csv"
)

invalid_indicators <- indicator_validation %>%
  filter(
    !valid_1_to_5 |
      n_unique < 2
  )

if (nrow(invalid_indicators) > 0) {
  print(invalid_indicators)
  
  stop(
    "At least one ICLV indicator is not valid for a 1–5 ordered model."
  )
}

# Save exact benchmark sample.
saveRDS(
  database,
  "output/data/ICLV_benchmark_analysis_sample.rds"
)

write_csv(
  database %>%
    count(choice, name = "n") %>%
    mutate(share = n / sum(n)),
  "output/tables/ICLV_choice_distribution.csv"
)

message(
  "ICLV benchmark sample: n = ",
  nrow(database)
)

# ----------------------------------------------------------------------
# 3. MODEL PARAMETERS
# ----------------------------------------------------------------------
#
# Choice model:
#   - three ASCs (car is reference)
#   - all six demographic controls, alternative-specific
#   - three LV effects, alternative-specific
#
# Measurement models:
#   - one zeta/loading per indicator
#   - four ordered-logit thresholds per five-point indicator
# ----------------------------------------------------------------------

apollo_beta <- c(
  
  # ------------------------------------------------------------
  # Choice-model alternative-specific constants
  # ------------------------------------------------------------
  
  asc_walk = 0,
  asc_bike = 0,
  asc_pt = 0,
  
  # ------------------------------------------------------------
  # Demographic controls: WALK vs car
  # ------------------------------------------------------------
  
  b_walk_Age_z = 0,
  b_walk_GenderMan = 0,
  b_walk_Edu_level_z = 0,
  b_walk_Nr_household_z = 0,
  b_walk_EQ_5D_index_z = 0,
  b_walk_Income_z = 0,
  
  # ------------------------------------------------------------
  # Demographic controls: BIKE vs car
  # ------------------------------------------------------------
  
  b_bike_Age_z = 0,
  b_bike_GenderMan = 0,
  b_bike_Edu_level_z = 0,
  b_bike_Nr_household_z = 0,
  b_bike_EQ_5D_index_z = 0,
  b_bike_Income_z = 0,
  
  # ------------------------------------------------------------
  # Demographic controls: PT vs car
  # ------------------------------------------------------------
  
  b_pt_Age_z = 0,
  b_pt_GenderMan = 0,
  b_pt_Edu_level_z = 0,
  b_pt_Nr_household_z = 0,
  b_pt_EQ_5D_index_z = 0,
  b_pt_Income_z = 0,
  
  # ------------------------------------------------------------
  # Latent-variable effects on mode choice
  # lambda_<alternative>_<latent variable>
  # ------------------------------------------------------------
  
  lambda_walk_HQCI = 0,
  lambda_bike_HQCI = 0,
  lambda_pt_HQCI = 0,
  
  lambda_walk_CFE = 0,
  lambda_bike_CFE = 0,
  lambda_pt_CFE = 0,
  
  lambda_walk_PPE = 0,
  lambda_bike_PPE = 0,
  lambda_pt_PPE = 0,
  
  # ------------------------------------------------------------
  # Ordered measurement loadings: HQ cycling infrastructure
  # Positive starting values match the CFA orientation.
  # ------------------------------------------------------------
  
  zeta_hqci_qualitative_lanes = 1,
  zeta_hqci_maintained = 1,
  zeta_hqci_sufficient = 1,
  zeta_hqci_streets_squares = 1,
  
  # ------------------------------------------------------------
  # Ordered measurement loadings: cycling-friendly environment
  # The final item is negatively oriented relative to CFE.
  # ------------------------------------------------------------
  
  zeta_cfe_neighbourhood = 1,
  zeta_cfe_enjoyable = 1,
  zeta_cfe_safe_cyclist = 1,
  zeta_cfe_busy_unsafe = -1,
  
  # ------------------------------------------------------------
  # Ordered measurement loadings: positive parking experiences
  # ------------------------------------------------------------
  
  zeta_ppe_accessible_car = 1,
  zeta_ppe_parking_residents = 1,
  zeta_ppe_parking_visitors = 1,
  
  # ------------------------------------------------------------
  # Thresholds: HQCI indicator 1
  # ------------------------------------------------------------
  
  tau_hqci_1_1 = -1.5,
  tau_hqci_1_2 = -0.5,
  tau_hqci_1_3 =  0.5,
  tau_hqci_1_4 =  1.5,
  
  # HQCI indicator 2
  tau_hqci_2_1 = -1.5,
  tau_hqci_2_2 = -0.5,
  tau_hqci_2_3 =  0.5,
  tau_hqci_2_4 =  1.5,
  
  # HQCI indicator 3
  tau_hqci_3_1 = -1.5,
  tau_hqci_3_2 = -0.5,
  tau_hqci_3_3 =  0.5,
  tau_hqci_3_4 =  1.5,
  
  # HQCI indicator 4
  tau_hqci_4_1 = -1.5,
  tau_hqci_4_2 = -0.5,
  tau_hqci_4_3 =  0.5,
  tau_hqci_4_4 =  1.5,
  
  # ------------------------------------------------------------
  # Thresholds: CFE indicator 1
  # ------------------------------------------------------------
  
  tau_cfe_1_1 = -1.5,
  tau_cfe_1_2 = -0.5,
  tau_cfe_1_3 =  0.5,
  tau_cfe_1_4 =  1.5,
  
  # CFE indicator 2
  tau_cfe_2_1 = -1.5,
  tau_cfe_2_2 = -0.5,
  tau_cfe_2_3 =  0.5,
  tau_cfe_2_4 =  1.5,
  
  # CFE indicator 3
  tau_cfe_3_1 = -1.5,
  tau_cfe_3_2 = -0.5,
  tau_cfe_3_3 =  0.5,
  tau_cfe_3_4 =  1.5,
  
  # CFE indicator 4
  tau_cfe_4_1 = -1.5,
  tau_cfe_4_2 = -0.5,
  tau_cfe_4_3 =  0.5,
  tau_cfe_4_4 =  1.5,
  
  # ------------------------------------------------------------
  # Thresholds: PPE indicator 1
  # ------------------------------------------------------------
  
  tau_ppe_1_1 = -1.5,
  tau_ppe_1_2 = -0.5,
  tau_ppe_1_3 =  0.5,
  tau_ppe_1_4 =  1.5,
  
  # PPE indicator 2
  tau_ppe_2_1 = -1.5,
  tau_ppe_2_2 = -0.5,
  tau_ppe_2_3 =  0.5,
  tau_ppe_2_4 =  1.5,
  
  # PPE indicator 3
  tau_ppe_3_1 = -1.5,
  tau_ppe_3_2 = -0.5,
  tau_ppe_3_3 =  0.5,
  tau_ppe_3_4 =  1.5
)

# No parameters are fixed in this benchmark.
apollo_fixed <- character(0)

# ----------------------------------------------------------------------
# 4. RANDOM DRAWS AND LATENT VARIABLES
# ----------------------------------------------------------------------
#
# The three latent disturbances are standard normal and independent in this
# targeted benchmark. This deliberately keeps the model compact.
#
# Because each respondent contributes one observation, panelProd() is NOT
# used in apollo_probabilities().
# ----------------------------------------------------------------------

apollo_draws <- list(
  interDrawsType = "halton",
  interNDraws = n_draws,
  interUnifDraws = c(),
  interNormDraws = c(
    "eta_HQCI",
    "eta_CFE",
    "eta_PPE"
  ),
  
  intraDrawsType = "",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(
    apollo_beta,
    apollo_inputs
) {
  
  randcoeff <- list()
  
  randcoeff[["LV_HQCI"]] <- eta_HQCI
  randcoeff[["LV_CFE"]]  <- eta_CFE
  randcoeff[["LV_PPE"]]  <- eta_PPE
  
  return(randcoeff)
}

# ----------------------------------------------------------------------
# 5. VALIDATE APOLLO INPUTS
# ----------------------------------------------------------------------

apollo_inputs <- apollo_validateInputs()

# ----------------------------------------------------------------------
# 6. MODEL LIKELIHOOD
# ----------------------------------------------------------------------

apollo_probabilities <- function(
    apollo_beta,
    apollo_inputs,
    functionality = "estimate"
) {
  
  apollo_attach(
    apollo_beta,
    apollo_inputs
  )
  
  on.exit(
    apollo_detach(
      apollo_beta,
      apollo_inputs
    )
  )
  
  P <- list()
  
  # ================================================================
  # 6A. MEASUREMENT MODEL:
  #     HIGH-QUALITY CYCLING INFRASTRUCTURE
  # ================================================================
  
  ol_hqci_1 <- list(
    outcomeOrdered =
      Qualitative_cycling_lanes,
    utility =
      zeta_hqci_qualitative_lanes * LV_HQCI,
    tau = list(
      tau_hqci_1_1,
      tau_hqci_1_2,
      tau_hqci_1_3,
      tau_hqci_1_4
    ),
    componentName = "HQCI_qualitative_lanes"
  )
  
  ol_hqci_2 <- list(
    outcomeOrdered =
      Maintained_cycling_infrastructure,
    utility =
      zeta_hqci_maintained * LV_HQCI,
    tau = list(
      tau_hqci_2_1,
      tau_hqci_2_2,
      tau_hqci_2_3,
      tau_hqci_2_4
    ),
    componentName = "HQCI_maintained"
  )
  
  ol_hqci_3 <- list(
    outcomeOrdered =
      Sufficient_cycling_infrastructure,
    utility =
      zeta_hqci_sufficient * LV_HQCI,
    tau = list(
      tau_hqci_3_1,
      tau_hqci_3_2,
      tau_hqci_3_3,
      tau_hqci_3_4
    ),
    componentName = "HQCI_sufficient"
  )
  
  ol_hqci_4 <- list(
    outcomeOrdered =
      Maintained_streets_and_squares,
    utility =
      zeta_hqci_streets_squares * LV_HQCI,
    tau = list(
      tau_hqci_4_1,
      tau_hqci_4_2,
      tau_hqci_4_3,
      tau_hqci_4_4
    ),
    componentName = "HQCI_streets_squares"
  )
  
  P[["HQCI_qualitative_lanes"]] <-
    apollo_ol(
      ol_hqci_1,
      functionality
    )
  
  P[["HQCI_maintained"]] <-
    apollo_ol(
      ol_hqci_2,
      functionality
    )
  
  P[["HQCI_sufficient"]] <-
    apollo_ol(
      ol_hqci_3,
      functionality
    )
  
  P[["HQCI_streets_squares"]] <-
    apollo_ol(
      ol_hqci_4,
      functionality
    )
  
  # ================================================================
  # 6B. MEASUREMENT MODEL:
  #     CYCLING-FRIENDLY ENVIRONMENT
  # ================================================================
  
  ol_cfe_1 <- list(
    outcomeOrdered =
      Cycling_friendly_neighborhood,
    utility =
      zeta_cfe_neighbourhood * LV_CFE,
    tau = list(
      tau_cfe_1_1,
      tau_cfe_1_2,
      tau_cfe_1_3,
      tau_cfe_1_4
    ),
    componentName = "CFE_neighbourhood"
  )
  
  ol_cfe_2 <- list(
    outcomeOrdered =
      Enjoyable_cycling,
    utility =
      zeta_cfe_enjoyable * LV_CFE,
    tau = list(
      tau_cfe_2_1,
      tau_cfe_2_2,
      tau_cfe_2_3,
      tau_cfe_2_4
    ),
    componentName = "CFE_enjoyable"
  )
  
  ol_cfe_3 <- list(
    outcomeOrdered =
      Feeling_safe_in_traffic_as_cyclist,
    utility =
      zeta_cfe_safe_cyclist * LV_CFE,
    tau = list(
      tau_cfe_3_1,
      tau_cfe_3_2,
      tau_cfe_3_3,
      tau_cfe_3_4
    ),
    componentName = "CFE_safe_cyclist"
  )
  
  ol_cfe_4 <- list(
    outcomeOrdered =
      Busy_traffic_causing_unsafe_cycling_experiences,
    utility =
      zeta_cfe_busy_unsafe * LV_CFE,
    tau = list(
      tau_cfe_4_1,
      tau_cfe_4_2,
      tau_cfe_4_3,
      tau_cfe_4_4
    ),
    componentName = "CFE_busy_unsafe"
  )
  
  P[["CFE_neighbourhood"]] <-
    apollo_ol(
      ol_cfe_1,
      functionality
    )
  
  P[["CFE_enjoyable"]] <-
    apollo_ol(
      ol_cfe_2,
      functionality
    )
  
  P[["CFE_safe_cyclist"]] <-
    apollo_ol(
      ol_cfe_3,
      functionality
    )
  
  P[["CFE_busy_unsafe"]] <-
    apollo_ol(
      ol_cfe_4,
      functionality
    )
  
  # ================================================================
  # 6C. MEASUREMENT MODEL:
  #     POSITIVE PARKING EXPERIENCES
  # ================================================================
  
  ol_ppe_1 <- list(
    outcomeOrdered =
      Accessible_by_car,
    utility =
      zeta_ppe_accessible_car * LV_PPE,
    tau = list(
      tau_ppe_1_1,
      tau_ppe_1_2,
      tau_ppe_1_3,
      tau_ppe_1_4
    ),
    componentName = "PPE_accessible_car"
  )
  
  ol_ppe_2 <- list(
    outcomeOrdered =
      Easily_finding_parking_spot_residents,
    utility =
      zeta_ppe_parking_residents * LV_PPE,
    tau = list(
      tau_ppe_2_1,
      tau_ppe_2_2,
      tau_ppe_2_3,
      tau_ppe_2_4
    ),
    componentName = "PPE_parking_residents"
  )
  
  ol_ppe_3 <- list(
    outcomeOrdered =
      Easily_finding_parking_spot_visitors,
    utility =
      zeta_ppe_parking_visitors * LV_PPE,
    tau = list(
      tau_ppe_3_1,
      tau_ppe_3_2,
      tau_ppe_3_3,
      tau_ppe_3_4
    ),
    componentName = "PPE_parking_visitors"
  )
  
  P[["PPE_accessible_car"]] <-
    apollo_ol(
      ol_ppe_1,
      functionality
    )
  
  P[["PPE_parking_residents"]] <-
    apollo_ol(
      ol_ppe_2,
      functionality
    )
  
  P[["PPE_parking_visitors"]] <-
    apollo_ol(
      ol_ppe_3,
      functionality
    )
  
  # ================================================================
  # 6D. CHOICE MODEL
  #     Private car is the reference utility (= 0)
  # ================================================================
  
  V <- list()
  
  V[["car"]] <- 0
  
  V[["walk"]] <-
    asc_walk +
    b_walk_Age_z * Age_z +
    b_walk_GenderMan * GenderMan +
    b_walk_Edu_level_z * Edu_level_z +
    b_walk_Nr_household_z * Nr_household_z +
    b_walk_EQ_5D_index_z * EQ_5D_index_z +
    b_walk_Income_z * Income_z +
    lambda_walk_HQCI * LV_HQCI +
    lambda_walk_CFE * LV_CFE +
    lambda_walk_PPE * LV_PPE
  
  V[["bike"]] <-
    asc_bike +
    b_bike_Age_z * Age_z +
    b_bike_GenderMan * GenderMan +
    b_bike_Edu_level_z * Edu_level_z +
    b_bike_Nr_household_z * Nr_household_z +
    b_bike_EQ_5D_index_z * EQ_5D_index_z +
    b_bike_Income_z * Income_z +
    lambda_bike_HQCI * LV_HQCI +
    lambda_bike_CFE * LV_CFE +
    lambda_bike_PPE * LV_PPE
  
  V[["pt"]] <-
    asc_pt +
    b_pt_Age_z * Age_z +
    b_pt_GenderMan * GenderMan +
    b_pt_Edu_level_z * Edu_level_z +
    b_pt_Nr_household_z * Nr_household_z +
    b_pt_EQ_5D_index_z * EQ_5D_index_z +
    b_pt_Income_z * Income_z +
    lambda_pt_HQCI * LV_HQCI +
    lambda_pt_CFE * LV_CFE +
    lambda_pt_PPE * LV_PPE
  
  mnl_settings <- list(
    alternatives = c(
      car = 1,
      walk = 2,
      bike = 3,
      pt = 4
    ),
    avail = list(
      car = 1,
      walk = 1,
      bike = 1,
      pt = 1
    ),
    choiceVar = choice,
    utilities = V,
    componentName = "mode_choice"
  )
  
  P[["mode_choice"]] <-
    apollo_mnl(
      mnl_settings,
      functionality
    )
  
  # ================================================================
  # 6E. COMBINE MEASUREMENT AND CHOICE LIKELIHOODS
  # ================================================================
  
  P <- apollo_combineModels(
    P,
    apollo_inputs,
    functionality
  )
  
  # One row per respondent: no apollo_panelProd() is required.
  
  P <- apollo_avgInterDraws(
    P,
    apollo_inputs,
    functionality
  )
  
  P <- apollo_prepareProb(
    P,
    apollo_inputs,
    functionality
  )
  
  return(P)
}

# ----------------------------------------------------------------------
# 7. OPTIONAL PRE-ESTIMATION LIKELIHOOD CHECK
# ----------------------------------------------------------------------

message(
  "\nChecking the ICLV likelihood at starting values..."
)

starting_ll <- apollo_llCalc(
  apollo_beta,
  apollo_probabilities,
  apollo_inputs
)

print(starting_ll)

# ----------------------------------------------------------------------
# 8. ESTIMATE THE ICLV
# ----------------------------------------------------------------------

message(
  "\nEstimating targeted three-LV ICLV benchmark with ",
  n_draws,
  " Halton draws...\n"
)

model_iclv <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    maxIterations = 1000
  )
)

saveRDS(
  model_iclv,
  file.path(
    "output/models",
    paste0(
      model_name,
      "_",
      n_draws,
      "draws.rds"
    )
  )
)

# Standard Apollo output.
apollo_modelOutput(
  model_iclv
)

apollo_saveOutput(
  model_iclv
)

# ----------------------------------------------------------------------
# 9. EXPORT PARAMETER TABLE
# ----------------------------------------------------------------------

parameter_names <- names(
  model_iclv$estimate
)

parameter_estimates <- as.numeric(
  model_iclv$estimate
)

# Prefer robust SE when available.
if (
  !is.null(model_iclv$robse) &&
  length(model_iclv$robse) ==
  length(parameter_estimates)
) {
  
  parameter_se <- as.numeric(
    model_iclv$robse
  )
  
  se_type <- "Robust"
  
} else if (
  !is.null(model_iclv$se) &&
  length(model_iclv$se) ==
  length(parameter_estimates)
) {
  
  parameter_se <- as.numeric(
    model_iclv$se
  )
  
  se_type <- "Classical"
  
} else {
  
  parameter_se <- rep(
    NA_real_,
    length(parameter_estimates)
  )
  
  se_type <- "Unavailable"
}

parameter_table <- tibble(
  parameter = parameter_names,
  estimate = parameter_estimates,
  standard_error = parameter_se,
  se_type = se_type
) %>%
  mutate(
    z_value =
      estimate / standard_error,
    
    lower_95 =
      estimate -
      1.96 * standard_error,
    
    upper_95 =
      estimate +
      1.96 * standard_error,
    
    p_value =
      2 * pnorm(
        -abs(z_value)
      ),
    
    interval_excludes_zero =
      lower_95 > 0 |
      upper_95 < 0
  )

write_csv(
  parameter_table,
  "output/tables/ICLV_all_parameters.csv"
)

# ----------------------------------------------------------------------
# 10. LATENT-VARIABLE EFFECTS ON MODE CHOICE
# ----------------------------------------------------------------------

latent_choice_effects <- parameter_table %>%
  filter(
    grepl(
      "^lambda_",
      parameter
    )
  ) %>%
  mutate(
    alternative = case_when(
      grepl(
        "^lambda_walk_",
        parameter
      ) ~ "On foot vs Private car",
      
      grepl(
        "^lambda_bike_",
        parameter
      ) ~ "Bike vs Private car",
      
      grepl(
        "^lambda_pt_",
        parameter
      ) ~ "Public transport vs Private car",
      
      TRUE ~ NA_character_
    ),
    
    latent_construct = case_when(
      grepl(
        "_HQCI$",
        parameter
      ) ~ "High_quality_cycling_infrastructure",
      
      grepl(
        "_CFE$",
        parameter
      ) ~ "Cycling_friendly_environment",
      
      grepl(
        "_PPE$",
        parameter
      ) ~ "Positive_parking_experiences",
      
      TRUE ~ NA_character_
    ),
    
    odds_ratio = exp(
      estimate
    ),
    
    odds_ratio_lower_95 = exp(
      lower_95
    ),
    
    odds_ratio_upper_95 = exp(
      upper_95
    )
  ) %>%
  select(
    latent_construct,
    alternative,
    parameter,
    estimate,
    standard_error,
    z_value,
    p_value,
    lower_95,
    upper_95,
    interval_excludes_zero,
    odds_ratio,
    odds_ratio_lower_95,
    odds_ratio_upper_95
  ) %>%
  arrange(
    latent_construct,
    alternative
  )

write_csv(
  latent_choice_effects,
  "output/tables/ICLV_latent_choice_effects.csv"
)

# ----------------------------------------------------------------------
# 11. MEASUREMENT-MODEL LOADINGS
# ----------------------------------------------------------------------

measurement_loadings <- parameter_table %>%
  filter(
    grepl(
      "^zeta_",
      parameter
    )
  ) %>%
  mutate(
    latent_construct = case_when(
      grepl(
        "^zeta_hqci_",
        parameter
      ) ~ "High_quality_cycling_infrastructure",
      
      grepl(
        "^zeta_cfe_",
        parameter
      ) ~ "Cycling_friendly_environment",
      
      grepl(
        "^zeta_ppe_",
        parameter
      ) ~ "Positive_parking_experiences",
      
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(
    latent_construct,
    parameter
  )

write_csv(
  measurement_loadings,
  "output/tables/ICLV_measurement_loadings.csv"
)

# ----------------------------------------------------------------------
# 12. DEMOGRAPHIC CHOICE EFFECTS
# ----------------------------------------------------------------------

demographic_choice_effects <- parameter_table %>%
  filter(
    grepl(
      "^b_(walk|bike|pt)_",
      parameter
    )
  ) %>%
  mutate(
    odds_ratio = exp(
      estimate
    ),
    
    odds_ratio_lower_95 = exp(
      lower_95
    ),
    
    odds_ratio_upper_95 = exp(
      upper_95
    )
  )

write_csv(
  demographic_choice_effects,
  "output/tables/ICLV_demographic_choice_effects.csv"
)

# ----------------------------------------------------------------------
# 13. BASIC MODEL-FIT SUMMARY
# ----------------------------------------------------------------------

get_scalar <- function(
    model,
    candidates
) {
  
  for (candidate in candidates) {
    
    current_value <-
      model[[candidate]]
    
    if (
      !is.null(current_value) &&
      length(current_value) > 0
    ) {
      
      current_value <-
        suppressWarnings(
          as.numeric(
            current_value
          )
        )
      
      current_value <-
        current_value[
          is.finite(
            current_value
          )
        ]
      
      if (
        length(current_value) > 0
      ) {
        return(
          current_value[1]
        )
      }
    }
  }
  
  NA_real_
}

fit_summary <- tibble(
  model = model_name,
  n_respondents = nrow(database),
  n_draws = n_draws,
  n_parameters = length(
    model_iclv$estimate
  ),
  log_likelihood = get_scalar(
    model_iclv,
    c(
      "LLout",
      "LLfinal",
      "maximum"
    )
  ),
  AIC = get_scalar(
    model_iclv,
    "AIC"
  ),
  BIC = get_scalar(
    model_iclv,
    "BIC"
  ),
  successful_estimation =
    if (
      !is.null(
        model_iclv$successfulEstimation
      )
    ) {
      as.logical(
        model_iclv$successfulEstimation
      )
    } else {
      NA
    },
  estimation_message =
    if (
      !is.null(
        model_iclv$message
      )
    ) {
      as.character(
        model_iclv$message
      )
    } else {
      NA_character_
    }
)

write_csv(
  fit_summary,
  "output/tables/ICLV_fit_summary.csv"
)

# ----------------------------------------------------------------------
# 14. CHOICE PREDICTIONS
# ----------------------------------------------------------------------
#
# Prediction is useful for checking whether the ICLV reproduces observed
# mode shares. If Apollo's prediction return structure differs in the local
# package version, the model estimation and parameter exports remain valid.
# ----------------------------------------------------------------------

prediction_result <- tryCatch(
  {
    
    apollo_prediction(
      model_iclv,
      apollo_probabilities,
      apollo_inputs,
      prediction_settings = list(
        modelComponent =
          "mode_choice"
      )
    )
    
  },
  error = function(e) {
    
    warning(
      "apollo_prediction could not be exported: ",
      conditionMessage(e)
    )
    
    NULL
  }
)

if (
  !is.null(
    prediction_result
  )
) {
  
  saveRDS(
    prediction_result,
    "output/data/ICLV_mode_choice_predictions.rds"
  )
}

# ----------------------------------------------------------------------
# 15. DRAW-STABILITY REMINDER
# ----------------------------------------------------------------------
#
# If this model converges and the substantive latent-variable results are
# stable, re-run Script 7 with:
#
#   n_draws <- 1000L
#
# and check whether the lambda estimates and their uncertainty change
# materially. This is a simulation-stability check, not a different model.
# ----------------------------------------------------------------------

message(
  "\nScript 7 completed."
)

message(
  "\nMain outputs to inspect:\n",
  "  output/tables/ICLV_fit_summary.csv\n",
  "  output/tables/ICLV_latent_choice_effects.csv\n",
  "  output/tables/ICLV_measurement_loadings.csv\n",
  "  output/tables/ICLV_all_parameters.csv\n",
  "  output/tables/ICLV_demographic_choice_effects.csv"
)

print(
  fit_summary
)

print(
  latent_choice_effects
)

# ======================================================================
# END OF SCRIPT 7
# ======================================================================

# ======================================================================
# SCRIPT 8 — PUBLICATION TABLES 2A, 2B AND 2C
# Comparison of ML1b, ML2, ML3, ML4 and ML4p
# ======================================================================
#
# OUTPUT
# ------
# Table 2A: On foot vs Private car
# Table 2B: Bike vs Private car
# Table 2C: Public transport vs Private car
#
# Each table contains:
#   - first column: ALL demographic, objective-BE and perceived-BE variables;
#   - one column for each model: ML1b, ML2, ML3, ML4 and ML4p;
#   - odds ratio with 95% Bayesian credible interval;
#   - em dash (—) when a variable was not included in that model.
#
# IMPORTANT
# ---------
# These are Bayesian models, so the intervals are 95% CREDIBLE INTERVALS
# (CrI), not classical confidence intervals.
# ======================================================================

rm(list = ls())
gc()

required_packages <- c(
  "brms", "dplyr", "readr", "tibble", "purrr", "stringr", "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Script 8:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(brms)
library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(stringr)
library(tidyr)

dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. MODEL FILES
# ----------------------------------------------------------------------

model_paths <- c(
  ML1b = "output/models/ML1b_demographics_common_sample_with_LOO.rds",
  ML2  = "output/models/ML2_objective_environment_with_LOO.rds",
  ML3  = "output/models/ML3_perceived_environment_with_LOO.rds",
  ML4  = "output/models/ML4_objective_plus_perceived_with_LOO.rds",
  ML4p = "output/models/ML4p_parsimonious_environment_with_LOO.rds"
)

missing_model_files <- model_paths[!file.exists(model_paths)]

if (length(missing_model_files) > 0) {
  stop(
    "The following model files could not be found:\n",
    paste(missing_model_files, collapse = "\n"),
    "\n\nCheck filenames in output/models and adapt model_paths if needed."
  )
}

models <- lapply(model_paths, readRDS)

if (!all(vapply(models, inherits, logical(1), what = "brmsfit"))) {
  stop("At least one loaded object is not a brmsfit model.")
}

# ----------------------------------------------------------------------
# 2. ALL VARIABLES, IN THE DESIRED TABLE ORDER
# ----------------------------------------------------------------------

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- c(
  "FAC_presence_walking_infr_z",
  "FAC_presence_difficult_surface_z",
  "FAC_presence_greenery_z",
  "FAC_presence_park_equipment_z",
  "FAC_presence_cycling_infrastructure_z",
  "FAC_rate_accidents_z",
  "FAC_presence_primary_secondary_roads_z",
  "FAC_presence_high_parking_stress_z",
  "FAC_presence_moderate_parking_stress_z",
  "FAC_presence_PT_z"
)

perceived_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

all_predictors <- c(control_vars, objective_vars, perceived_vars)

variable_labels <- c(
  Age_z = "Age",
  GenderMan = "Gender: male",
  Edu_level_z = "Educational level",
  Nr_household_z = "Household size",
  EQ_5D_index_z = "EQ-5D index",
  Income_z = "Household income",
  
  FAC_presence_walking_infr_z =
    "Objective: walking infrastructure",
  FAC_presence_difficult_surface_z =
    "Objective: difficult walking surfaces",
  FAC_presence_greenery_z =
    "Objective: greenery",
  FAC_presence_park_equipment_z =
    "Objective: park equipment",
  FAC_presence_cycling_infrastructure_z =
    "Objective: cycling infrastructure",
  FAC_rate_accidents_z =
    "Objective: traffic accidents",
  FAC_presence_primary_secondary_roads_z =
    "Objective: primary/secondary roads",
  FAC_presence_high_parking_stress_z =
    "Objective: high parking stress",
  FAC_presence_moderate_parking_stress_z =
    "Objective: moderate parking stress",
  FAC_presence_PT_z =
    "Objective: public transport",
  
  FS_High_quality_walking_infrastructure_z =
    "Perceived: high-quality walking infrastructure",
  FS_Walking_friendly_environment_z =
    "Perceived: walking-friendly environment",
  FS_High_quality_cycling_infrastructure_z =
    "Perceived: high-quality cycling infrastructure",
  FS_Cycling_friendly_environment_z =
    "Perceived: cycling-friendly environment",
  FS_Busy_traffic_z =
    "Perceived: busy traffic",
  FS_Traffic_safety_z =
    "Perceived: traffic safety",
  FS_Positive_parking_experiences_z =
    "Perceived: positive parking experiences",
  FS_High_quality_public_transport_z =
    "Perceived: high-quality public transport"
)

variable_reference <- tibble(
  predictor = all_predictors,
  Variable = unname(variable_labels[all_predictors]),
  Domain = c(
    rep("Demographic characteristics", length(control_vars)),
    rep("Objective built environment", length(objective_vars)),
    rep("Perceived built environment", length(perceived_vars))
  ),
  variable_order = seq_along(all_predictors)
)

# ----------------------------------------------------------------------
# 3. EXTRACT ALL FIXED EFFECTS DIRECTLY FROM THE BRMS MODELS
# ----------------------------------------------------------------------

extract_model_fixed_effects <- function(model, model_label) {
  
  as.data.frame(
    brms::fixef(
      model,
      probs = c(0.025, 0.975)
    )
  ) %>%
    rownames_to_column("parameter") %>%
    as_tibble() %>%
    transmute(
      model = model_label,
      parameter = parameter,
      estimate = Estimate,
      lower_95 = Q2.5,
      upper_95 = Q97.5,
      OR = exp(Estimate),
      OR_lower = exp(Q2.5),
      OR_upper = exp(Q97.5)
    )
}

all_fixed_effects <- imap_dfr(
  models,
  ~ extract_model_fixed_effects(.x, .y)
) %>%
  filter(!str_detect(parameter, "Intercept$"))

# ----------------------------------------------------------------------
# 4. MAP BRMS PARAMETERS TO PREDICTORS AND MODE CONTRASTS
# ----------------------------------------------------------------------

identify_predictor <- function(parameter_name) {
  
  matches <- all_predictors[
    vapply(
      all_predictors,
      function(current_predictor) {
        str_ends(parameter_name, fixed(current_predictor))
      },
      logical(1)
    )
  ]
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  matches[which.max(nchar(matches))]
}

identify_contrast <- function(parameter_name, predictor_name) {
  
  prefix <- substr(
    parameter_name,
    1,
    nchar(parameter_name) - nchar(predictor_name)
  )
  
  prefix_clean <- prefix %>%
    str_replace("^mu", "") %>%
    str_replace("_$", "") %>%
    str_to_lower()
  
  case_when(
    str_detect(prefix_clean, "onfoot|walk") ~
      "Walking vs Private car",
    
    str_detect(prefix_clean, "bike|cycl") ~
      "Cycling vs Private car",
    
    str_detect(prefix_clean, "publictransport|public_transport|pt") ~
      "Public transport vs Private car",
    
    TRUE ~ NA_character_
  )
}

all_fixed_effects <- all_fixed_effects %>%
  mutate(
    predictor = map_chr(parameter, identify_predictor),
    contrast = map2_chr(parameter, predictor, identify_contrast)
  ) %>%
  filter(!is.na(predictor))

unmapped_parameters <- all_fixed_effects %>%
  filter(is.na(contrast))

if (nrow(unmapped_parameters) > 0) {
  print(
    unmapped_parameters %>%
      select(model, parameter, predictor)
  )
  
  stop(
    "At least one coefficient could not be assigned to a mode contrast. ",
    "Inspect the printed parameter names and adapt identify_contrast()."
  )
}

# ----------------------------------------------------------------------
# 5. FORMAT OR (95% CrI)
# ----------------------------------------------------------------------

format_or_credible_interval <- function(OR, lower, upper, digits = 2) {
  
  paste0(
    formatC(OR, format = "f", digits = digits),
    " (",
    formatC(lower, format = "f", digits = digits),
    "–",
    formatC(upper, format = "f", digits = digits),
    ")"
  )
}

all_fixed_effects <- all_fixed_effects %>%
  mutate(
    formatted_result = format_or_credible_interval(
      OR, OR_lower, OR_upper
    )
  )

# ----------------------------------------------------------------------
# 6. CREATE ONE TABLE FOR A GIVEN MODE CONTRAST
# ----------------------------------------------------------------------

create_model_comparison_table <- function(
    requested_contrast,
    output_filename
) {
  
  result_for_contrast <- all_fixed_effects %>%
    filter(contrast == requested_contrast) %>%
    select(
      predictor,
      model,
      formatted_result
    )
  
  complete_grid <- expand_grid(
    predictor = all_predictors,
    model = names(models)
  )
  
  final_table <- complete_grid %>%
    left_join(
      result_for_contrast,
      by = c("predictor", "model")
    ) %>%
    mutate(
      formatted_result = replace_na(
        formatted_result,
        "—"
      )
    ) %>%
    pivot_wider(
      names_from = model,
      values_from = formatted_result
    ) %>%
    left_join(
      variable_reference,
      by = "predictor"
    ) %>%
    arrange(variable_order) %>%
    select(
      Variable,
      Domain,
      ML1b,
      ML2,
      ML3,
      ML4,
      ML4p
    )
  
  write_csv(
    final_table,
    file.path(
      "output/tables",
      output_filename
    )
  )
  
  final_table
}

# ----------------------------------------------------------------------
# 7. TABLES 2A, 2B AND 2C
# ----------------------------------------------------------------------

table_2A <- create_model_comparison_table(
  "Walking vs Private car",
  "Table_2A_Walking_vs_Private_car.csv"
)

table_2B <- create_model_comparison_table(
  "Cycling vs Private car",
  "Table_2B_Cycling_vs_Private_car.csv"
)

table_2C <- create_model_comparison_table(
  "Public transport vs Private car",
  "Table_2C_Public_transport_vs_Private_car.csv"
)

# Compact versions: first column = variable, followed immediately by models.
table_2A_compact <- table_2A %>% select(-Domain)
table_2B_compact <- table_2B %>% select(-Domain)
table_2C_compact <- table_2C %>% select(-Domain)

write_csv(
  table_2A_compact,
  "output/tables/Table_2A_Walking_vs_Private_car_compact.csv"
)

write_csv(
  table_2B_compact,
  "output/tables/Table_2B_Cycling_vs_Private_car_compact.csv"
)

write_csv(
  table_2C_compact,
  "output/tables/Table_2C_Public_transport_vs_Private_car_compact.csv"
)

# ----------------------------------------------------------------------
# 8. QUALITY CHECKS
# ----------------------------------------------------------------------

expected_rows <- length(all_predictors)

if (
  nrow(table_2A) != expected_rows ||
  nrow(table_2B) != expected_rows ||
  nrow(table_2C) != expected_rows
) {
  stop("Unexpected number of rows in one or more publication tables.")
}

message("\nTables 2A–2C created successfully.")
message(
  "\nEach cell reports OR (95% CrI). ",
  "An em dash indicates that the predictor was not included in that model."
)

message(
  "\nMain compact files:\n",
  "  output/tables/Table_2A_Walking_vs_Private_car_compact.csv\n",
  "  output/tables/Table_2B_Cycling_vs_Private_car_compact.csv\n",
  "  output/tables/Table_2C_Public_transport_vs_Private_car_compact.csv"
)

print(table_2A_compact)
print(table_2B_compact)
print(table_2C_compact)

# ======================================================================
# END OF SCRIPT 8
# ======================================================================
# ======================================================================
# SCRIPT 8B — PUBLICATION-READY TABLES 2A, 2B AND 2C
# Correct bolding from UNROUNDED posterior intervals
# Word (landscape, Aptos 9) + HTML + Excel
# ======================================================================
#
# IMPORTANT IMPROVEMENTS
# ----------------------
# 1. Statistical highlighting is determined DIRECTLY from the original
#    unrounded posterior estimates in the fitted brms models.
#    It is NOT inferred from the rounded text shown in the table.
#
# 2. Word output is landscape so all model columns remain readable.
#
# 3. Word table font is Aptos, 9 pt (equivalent to the current Word
#    "Aptos (Body)" appearance).
#
# 4. Values are shown as:
#       OR (95% CrI)
#
# 5. Bold values indicate that the original 95% Bayesian credible interval
#    for the log-odds coefficient excludes zero (equivalently, the OR
#    interval excludes 1).
#
# 6. ALL predictors are displayed, including predictors that were not
#    statistically supported in any model.
#
# 7. An em dash (—) means the predictor was not included in that model.
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES
# ----------------------------------------------------------------------

required_packages <- c(
  "brms",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "purrr",
  "tidyr",
  "flextable",
  "officer",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Script 8B:\n",
    paste(missing_packages, collapse = "\n"),
    "\n\nFor example:\n",
    "install.packages(c('flextable', 'officer', 'openxlsx'))"
  )
}

library(brms)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)
library(tidyr)
library(flextable)
library(officer)
library(openxlsx)

dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. LOAD THE FIVE FITTED MODELS DIRECTLY
# ----------------------------------------------------------------------
#
# Change only these paths if your local filenames differ.
# ----------------------------------------------------------------------

model_paths <- c(
  ML1b = "output/models/ML1b_demographics_common_sample_with_LOO.rds",
  ML2  = "output/models/ML2_objective_environment_with_LOO.rds",
  ML3  = "output/models/ML3_perceived_environment_with_LOO.rds",
  ML4  = "output/models/ML4_objective_plus_perceived_with_LOO.rds",
  ML4p = "output/models/ML4p_parsimonious_environment_with_LOO.rds"
)

missing_model_files <- model_paths[
  !file.exists(model_paths)
]

if (length(missing_model_files) > 0) {
  stop(
    "The following model files could not be found:\n",
    paste(missing_model_files, collapse = "\n"),
    "\n\nCheck output/models and adapt model_paths if needed."
  )
}

models <- lapply(
  model_paths,
  readRDS
)

if (!all(
  vapply(
    models,
    inherits,
    logical(1),
    what = "brmsfit"
  )
)) {
  stop("At least one loaded object is not a brmsfit model.")
}

model_columns <- names(models)

# ----------------------------------------------------------------------
# 2. DEFINE ALL PREDICTORS AND PUBLICATION LABELS
# ----------------------------------------------------------------------

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

objective_vars <- c(
  "FAC_presence_walking_infr_z",
  "FAC_presence_difficult_surface_z",
  "FAC_presence_greenery_z",
  "FAC_presence_park_equipment_z",
  "FAC_presence_cycling_infrastructure_z",
  "FAC_rate_accidents_z",
  "FAC_presence_primary_secondary_roads_z",
  "FAC_presence_high_parking_stress_z",
  "FAC_presence_moderate_parking_stress_z",
  "FAC_presence_PT_z"
)

perceived_vars <- c(
  "FS_High_quality_walking_infrastructure_z",
  "FS_Walking_friendly_environment_z",
  "FS_High_quality_cycling_infrastructure_z",
  "FS_Cycling_friendly_environment_z",
  "FS_Busy_traffic_z",
  "FS_Traffic_safety_z",
  "FS_Positive_parking_experiences_z",
  "FS_High_quality_public_transport_z"
)

all_predictors <- c(
  control_vars,
  objective_vars,
  perceived_vars
)

variable_labels <- c(
  # Demographics
  Age_z = "Age",
  GenderMan = "Gender: male",
  Edu_level_z = "Educational level",
  Nr_household_z = "Household size",
  EQ_5D_index_z = "EQ-5D index",
  Income_z = "Household income",
  
  # Objective built environment
  FAC_presence_walking_infr_z =
    "Walking infrastructure",
  FAC_presence_difficult_surface_z =
    "Difficult walking surfaces",
  FAC_presence_greenery_z =
    "Greenery",
  FAC_presence_park_equipment_z =
    "Park equipment",
  FAC_presence_cycling_infrastructure_z =
    "Cycling infrastructure",
  FAC_rate_accidents_z =
    "Traffic accidents",
  FAC_presence_primary_secondary_roads_z =
    "Primary/secondary roads",
  FAC_presence_high_parking_stress_z =
    "High parking stress",
  FAC_presence_moderate_parking_stress_z =
    "Moderate parking stress",
  FAC_presence_PT_z =
    "Public transport",
  
  # Perceived built environment
  FS_High_quality_walking_infrastructure_z =
    "High-quality walking infrastructure",
  FS_Walking_friendly_environment_z =
    "Walking-friendly environment",
  FS_High_quality_cycling_infrastructure_z =
    "High-quality cycling infrastructure",
  FS_Cycling_friendly_environment_z =
    "Cycling-friendly environment",
  FS_Busy_traffic_z =
    "Busy traffic",
  FS_Traffic_safety_z =
    "Traffic safety",
  FS_Positive_parking_experiences_z =
    "Positive parking experiences",
  FS_High_quality_public_transport_z =
    "High-quality public transport"
)

variable_reference <- tibble(
  predictor = all_predictors,
  Variable = unname(
    variable_labels[
      all_predictors
    ]
  ),
  Domain = c(
    rep(
      "Demographic characteristics",
      length(control_vars)
    ),
    rep(
      "Objective built environment",
      length(objective_vars)
    ),
    rep(
      "Perceived built environment",
      length(perceived_vars)
    )
  ),
  variable_order = seq_along(
    all_predictors
  )
)

# ----------------------------------------------------------------------
# 3. EXTRACT UNROUNDED FIXED EFFECTS FROM ALL MODELS
# ----------------------------------------------------------------------

extract_model_fixed_effects <- function(
    model,
    model_label
) {
  
  as.data.frame(
    brms::fixef(
      model,
      probs = c(
        0.025,
        0.975
      )
    )
  ) %>%
    rownames_to_column(
      "parameter"
    ) %>%
    as_tibble() %>%
    transmute(
      model = model_label,
      parameter = parameter,
      
      # Original posterior values on log-odds scale
      estimate = Estimate,
      lower_95 = Q2.5,
      upper_95 = Q97.5,
      
      # OR scale
      OR = exp(Estimate),
      OR_lower = exp(Q2.5),
      OR_upper = exp(Q97.5),
      
      # IMPORTANT:
      # This flag is based on the ORIGINAL, UNROUNDED interval.
      credible_nonzero =
        Q2.5 > 0 |
        Q97.5 < 0
    )
}

all_fixed_effects <- imap_dfr(
  models,
  ~ extract_model_fixed_effects(
    .x,
    .y
  )
) %>%
  filter(
    !str_detect(
      parameter,
      "Intercept$"
    )
  )

# ----------------------------------------------------------------------
# 4. MAP BRMS PARAMETER NAMES TO PREDICTORS AND CONTRASTS
# ----------------------------------------------------------------------

identify_predictor <- function(
    parameter_name
) {
  
  matches <- all_predictors[
    vapply(
      all_predictors,
      function(current_predictor) {
        
        str_ends(
          parameter_name,
          fixed(
            current_predictor
          )
        )
      },
      logical(1)
    )
  ]
  
  if (length(matches) == 0) {
    return(
      NA_character_
    )
  }
  
  # Safeguard for overlapping variable names.
  matches[
    which.max(
      nchar(matches)
    )
  ]
}

identify_contrast <- function(
    parameter_name,
    predictor_name
) {
  
  prefix <- substr(
    parameter_name,
    1,
    nchar(parameter_name) -
      nchar(predictor_name)
  )
  
  prefix_clean <- prefix %>%
    str_replace(
      "^mu",
      ""
    ) %>%
    str_replace(
      "_$",
      ""
    ) %>%
    str_to_lower()
  
  case_when(
    str_detect(
      prefix_clean,
      "onfoot|walk"
    ) ~ "Walking vs Private car",
    
    str_detect(
      prefix_clean,
      "bike|cycl"
    ) ~ "Cycling vs Private car",
    
    str_detect(
      prefix_clean,
      "publictransport|public_transport|pt"
    ) ~ "Public transport vs Private car",
    
    TRUE ~ NA_character_
  )
}

all_fixed_effects <- all_fixed_effects %>%
  mutate(
    predictor = map_chr(
      parameter,
      identify_predictor
    ),
    contrast = map2_chr(
      parameter,
      predictor,
      identify_contrast
    )
  ) %>%
  filter(
    !is.na(predictor)
  )

unmapped <- all_fixed_effects %>%
  filter(
    is.na(contrast)
  )

if (nrow(unmapped) > 0) {
  
  print(
    unmapped %>%
      select(
        model,
        parameter,
        predictor
      )
  )
  
  stop(
    "At least one coefficient could not be assigned to a mode contrast."
  )
}

# ----------------------------------------------------------------------
# 5. FORMAT DISPLAY VALUE
# ----------------------------------------------------------------------

format_or_cri <- function(
    OR,
    lower,
    upper,
    digits = 2
) {
  
  paste0(
    formatC(
      OR,
      format = "f",
      digits = digits
    ),
    " (",
    formatC(
      lower,
      format = "f",
      digits = digits
    ),
    "–",
    formatC(
      upper,
      format = "f",
      digits = digits
    ),
    ")"
  )
}

all_fixed_effects <- all_fixed_effects %>%
  mutate(
    formatted_result =
      format_or_cri(
        OR,
        OR_lower,
        OR_upper
      )
  )

# ----------------------------------------------------------------------
# 6. CREATE A TABLE + AN INDEPENDENT SIGNIFICANCE MATRIX
# ----------------------------------------------------------------------

create_table_data <- function(
    requested_contrast
) {
  
  contrast_results <- all_fixed_effects %>%
    filter(
      contrast ==
        requested_contrast
    ) %>%
    select(
      predictor,
      model,
      formatted_result,
      credible_nonzero
    )
  
  # Complete predictor x model grid:
  # ensures that ALL variables appear.
  complete_grid <- tidyr::expand_grid(
    predictor = all_predictors,
    model = model_columns
  )
  
  long_complete <- complete_grid %>%
    left_join(
      contrast_results,
      by = c(
        "predictor",
        "model"
      )
    ) %>%
    mutate(
      formatted_result =
        tidyr::replace_na(
          formatted_result,
          "—"
        ),
      
      # FALSE when variable was not included.
      credible_nonzero =
        tidyr::replace_na(
          credible_nonzero,
          FALSE
        )
    )
  
  # Display values
  display_values <- long_complete %>%
    select(
      predictor,
      model,
      formatted_result
    ) %>%
    pivot_wider(
      names_from = model,
      values_from = formatted_result
    ) %>%
    left_join(
      variable_reference,
      by = "predictor"
    ) %>%
    arrange(
      variable_order
    ) %>%
    select(
      Variable,
      Domain,
      all_of(
        model_columns
      )
    )
  
  # Exact significance flags
  significance_values <- long_complete %>%
    select(
      predictor,
      model,
      credible_nonzero
    ) %>%
    pivot_wider(
      names_from = model,
      values_from = credible_nonzero
    ) %>%
    left_join(
      variable_reference %>%
        select(
          predictor,
          Variable,
          Domain,
          variable_order
        ),
      by = "predictor"
    ) %>%
    arrange(
      variable_order
    ) %>%
    select(
      Variable,
      Domain,
      all_of(
        model_columns
      )
    )
  
  list(
    display = display_values,
    significance = significance_values
  )
}

tables_base <- list(
  `2A` = create_table_data(
    "Walking vs Private car"
  ),
  `2B` = create_table_data(
    "Cycling vs Private car"
  ),
  `2C` = create_table_data(
    "Public transport vs Private car"
  )
)

# ----------------------------------------------------------------------
# 7. INSERT DOMAIN HEADER ROWS
# ----------------------------------------------------------------------

make_display_with_domain_rows <- function(
    table_object
) {
  
  display_data <-
    table_object$display
  
  significance_data <-
    table_object$significance
  
  domains <- c(
    "Demographic characteristics",
    "Objective built environment",
    "Perceived built environment"
  )
  
  display_output <- list()
  significance_output <- list()
  
  for (current_domain in domains) {
    
    current_display <-
      display_data %>%
      filter(
        Domain ==
          current_domain
      )
    
    current_significance <-
      significance_data %>%
      filter(
        Domain ==
          current_domain
      )
    
    # Header row for display
    header_display <- tibble(
      Variable = current_domain,
      Domain = "HEADER"
    )
    
    for (m in model_columns) {
      header_display[[m]] <- ""
    }
    
    # Header row for significance
    header_significance <- tibble(
      Variable = current_domain,
      Domain = "HEADER"
    )
    
    for (m in model_columns) {
      header_significance[[m]] <- FALSE
    }
    
    display_output[[current_domain]] <- bind_rows(
      header_display,
      current_display
    )
    
    significance_output[[current_domain]] <-
      bind_rows(
        header_significance,
        current_significance
      )
    
  }
  
  list(
    display = bind_rows(
      display_output
    ),
    significance = bind_rows(
      significance_output
    )
  )
}

tables_display <- lapply(
  tables_base,
  make_display_with_domain_rows
)

# ----------------------------------------------------------------------
# 8. TITLES
# ----------------------------------------------------------------------

table_titles <- c(
  `2A` =
    "Table 2A. Multilevel multinomial model results: walking versus private car",
  
  `2B` =
    "Table 2B. Multilevel multinomial model results: cycling versus private car",
  
  `2C` =
    "Table 2C. Multilevel multinomial model results: public transport versus private car"
)

# ----------------------------------------------------------------------
# 9. CREATE FLEXTABLE
# ----------------------------------------------------------------------

create_publication_flextable <- function(
    table_object,
    title
) {
  
  display_data <-
    table_object$display
  
  significance_data <-
    table_object$significance
  
  ft_data <- display_data %>%
    select(
      -Domain
    )
  
  header_rows <- which(
    display_data$Domain ==
      "HEADER"
  )
  
  ft <- flextable(
    ft_data
  )
  
  # Header
  ft <- set_header_labels(
    ft,
    Variable = "Variable",
    ML1b = "ML1b",
    ML2 = "ML2",
    ML3 = "ML3",
    ML4 = "ML4",
    ML4p = "ML4p"
  )
  
  ft <- theme_booktabs(
    ft
  )
  
  # ------------------------------------------------------------
  # FONT: APTOS (BODY), 9 PT
  # ------------------------------------------------------------
  #
  # In Word the Body font is shown as "Aptos (Body)" when the theme
  # body font is Aptos. flextable uses the actual font family name
  # "Aptos".
  # ------------------------------------------------------------
  
  ft <- font(
    ft,
    fontname = "Aptos",
    part = "all"
  )
  
  ft <- fontsize(
    ft,
    size = 9,
    part = "all"
  )
  
  ft <- bold(
    ft,
    part = "header"
  )
  
  ft <- align(
    ft,
    j = "Variable",
    align = "left",
    part = "all"
  )
  
  ft <- align(
    ft,
    j = model_columns,
    align = "center",
    part = "all"
  )
  
  ft <- valign(
    ft,
    valign = "center",
    part = "all"
  )
  
  # ------------------------------------------------------------
  # DOMAIN HEADINGS
  # ------------------------------------------------------------
  
  if (length(header_rows) > 0) {
    
    ft <- bold(
      ft,
      i = header_rows,
      part = "body"
    )
    
    ft <- bg(
      ft,
      i = header_rows,
      bg = "#E6E6E6",
      part = "body"
    )
    
    # Keep group title in first column, blank rest.
    ft <- align(
      ft,
      i = header_rows,
      j = "Variable",
      align = "left",
      part = "body"
    )
  }
  
  # ------------------------------------------------------------
  # BOLD SIGNIFICANT/CREDIBLE RESULTS
  # Uses ORIGINAL UNROUNDED posterior intervals
  # ------------------------------------------------------------
  
  for (current_model in model_columns) {
    
    credible_rows <- which(
      significance_data[[current_model]] %in% TRUE
    )
    
    if (length(credible_rows) > 0) {
      
      ft <- bold(
        ft,
        i = credible_rows,
        j = current_model,
        bold = TRUE,
        part = "body"
      )
    }
    
    dash_rows <- which(
      display_data[[current_model]] == "—"
    )
    
    if (length(dash_rows) > 0) {
      
      ft <- color(
        ft,
        i = dash_rows,
        j = current_model,
        color = "#808080",
        part = "body"
      )
    }
  }
  
  # ------------------------------------------------------------
  # WIDTHS FOR LANDSCAPE PAGE
  # ------------------------------------------------------------
  
  ft <- width(
    ft,
    j = "Variable",
    width = 3.15
  )
  
  ft <- width(
    ft,
    j = model_columns,
    width = 1.42
  )
  
  ft <- padding(
    ft,
    padding.top = 2,
    padding.bottom = 2,
    padding.left = 3,
    padding.right = 3,
    part = "all"
  )
  
  # Keep rows compact.
  ft <- height_all(
    ft,
    height = 0.25
  )
  
  # Caption
  ft <- set_caption(
    ft,
    caption = title
  )
  
  # Footnote
  ft <- add_footer_lines(
    ft,
    values = paste0(
      "Note. Values are odds ratios (OR) with 95% Bayesian credible intervals (CrI). ",
      "Bold values indicate that the original, unrounded 95% CrI excludes OR = 1. ",
      "An em dash indicates that the predictor was not included in the model. ",
      "Private car is the reference outcome category."
    )
  )
  
  ft <- font(
    ft,
    fontname = "Aptos",
    part = "footer"
  )
  
  ft <- fontsize(
    ft,
    size = 8,
    part = "footer"
  )
  
  ft
}

flextables <- Map(
  create_publication_flextable,
  tables_display,
  table_titles
)

# ----------------------------------------------------------------------
# 10. EXPORT WORD — LANDSCAPE
# ----------------------------------------------------------------------

doc <- read_docx()

# Set the document's default section to landscape.
landscape_section <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 11.69,
    height = 8.27
  ),
  page_margins = page_mar(
    top = 0.55,
    bottom = 0.55,
    left = 0.55,
    right = 0.55
  ),
  type = "continuous"
)

doc <- body_set_default_section(
  doc,
  landscape_section
)

table_ids <- names(
  flextables
)

for (i in seq_along(table_ids)) {
  
  table_id <- table_ids[i]
  
  doc <- body_add_flextable(
    doc,
    flextables[[table_id]]
  )
  
  if (i < length(table_ids)) {
    
    doc <- body_add_break(
      doc
    )
  }
}

word_output <-
  "output/tables/Tables_2A_2B_2C_multilevel_models_landscape.docx"

print(
  doc,
  target = word_output
)

# ----------------------------------------------------------------------
# 11. EXPORT HTML
# ----------------------------------------------------------------------

for (table_id in names(flextables)) {
  
  html_output <- file.path(
    "output/tables",
    paste0(
      "Table_",
      table_id,
      "_publication_ready.html"
    )
  )
  
  save_as_html(
    flextables[
      [table_id]
    ],
    path = html_output
  )
}

# ----------------------------------------------------------------------
# 12. EXPORT EXCEL
# ----------------------------------------------------------------------

wb <- createWorkbook()

header_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

domain_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  fgFill = "#E6E6E6",
  halign = "left"
)

credible_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center"
)

normal_model_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  halign = "center"
)

normal_variable_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  halign = "left"
)

dash_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  fontColour = "#808080",
  halign = "center"
)

footnote_style <- createStyle(
  fontName = "Aptos",
  fontSize = 8,
  textDecoration = "italic",
  wrapText = TRUE
)

for (table_id in names(tables_display)) {
  
  display_data <- tables_display[[table_id]]$display
  
  significance_data <- tables_display[[table_id]]$significance
  
  current_table <- display_data %>%
    select(
      -Domain
    )
  
  addWorksheet(
    wb,
    table_id
  )
  
  writeData(
    wb,
    sheet = table_id,
    x = current_table,
    startRow = 1,
    startCol = 1,
    headerStyle = header_style
  )
  
  # Variable column
  addStyle(
    wb,
    sheet = table_id,
    style = normal_variable_style,
    rows = 2:(nrow(current_table) + 1),
    cols = 1,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  setColWidths(
    wb,
    sheet = table_id,
    cols = 1,
    widths = 42
  )
  
  setColWidths(
    wb,
    sheet = table_id,
    cols = 2:6,
    widths = 21
  )
  
  freezePane(
    wb,
    sheet = table_id,
    firstRow = TRUE
  )
  
  # Domain rows
  domain_rows <- which(
    display_data$Domain ==
      "HEADER"
  ) + 1
  
  if (length(domain_rows) > 0) {
    
    for (excel_row in domain_rows) {
      
      mergeCells(
        wb,
        sheet = table_id,
        cols = 1:6,
        rows = excel_row
      )
      
      addStyle(
        wb,
        sheet = table_id,
        style = domain_style,
        rows = excel_row,
        cols = 1:6,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }
  
  # Model cells:
  # exact significance flag from original posterior intervals.
  for (
    column_index in seq_along(
      model_columns
    )
  ) {
    
    current_model <-
      model_columns[
        column_index
      ]
    
    excel_column <-
      column_index + 1
    
    for (
      row_index in seq_len(
        nrow(
          display_data
        )
      )
    ) {
      
      if (
        display_data$Domain[
          row_index
        ] == "HEADER"
      ) {
        next
      }
      
      cell_value <- display_data[[current_model]][row_index]
      
      is_credible <- significance_data[[current_model]][row_index]
      
      excel_row <-
        row_index + 1
      
      if (
        identical(
          cell_value,
          "—"
        )
      ) {
        
        addStyle(
          wb,
          sheet = table_id,
          style = dash_style,
          rows = excel_row,
          cols = excel_column,
          stack = TRUE
        )
        
      } else if (
        isTRUE(
          is_credible
        )
      ) {
        
        addStyle(
          wb,
          sheet = table_id,
          style = credible_style,
          rows = excel_row,
          cols = excel_column,
          stack = TRUE
        )
        
      } else {
        
        addStyle(
          wb,
          sheet = table_id,
          style = normal_model_style,
          rows = excel_row,
          cols = excel_column,
          stack = TRUE
        )
      }
    }
  }
  
  # Footnote
  footnote_row <-
    nrow(
      current_table
    ) + 3
  
  footnote_text <- paste0(
    "Note. Values are odds ratios (OR) with 95% Bayesian credible intervals (CrI). ",
    "Bold values indicate that the original, unrounded 95% CrI excludes OR = 1. ",
    "An em dash indicates that the predictor was not included in the model. ",
    "Private car is the reference outcome category."
  )
  
  writeData(
    wb,
    sheet = table_id,
    x = footnote_text,
    startRow = footnote_row,
    startCol = 1,
    colNames = FALSE
  )
  
  mergeCells(
    wb,
    sheet = table_id,
    cols = 1:6,
    rows = footnote_row
  )
  
  addStyle(
    wb,
    sheet = table_id,
    style = footnote_style,
    rows = footnote_row,
    cols = 1:6,
    gridExpand = TRUE,
    stack = TRUE
  )
}

excel_output <-
  "output/tables/Tables_2A_2B_2C_multilevel_models.xlsx"

saveWorkbook(
  wb,
  excel_output,
  overwrite = TRUE
)

# ----------------------------------------------------------------------
# 13. SAVE PLAIN CSV COPIES TOO
# ----------------------------------------------------------------------

for (table_id in names(tables_base)) {
  
  csv_table <- tables_base[[table_id]]$display %>%
    select(
      Variable,
      all_of(
        model_columns
      )
    )
  
  write_csv(
    csv_table,
    file.path(
      "output/tables",
      paste0(
        "Table_",
        table_id,
        "_final.csv"
      )
    )
  )
}
# ----------------------------------------------------------------------
# 14. FINAL MESSAGE
# ----------------------------------------------------------------------

message(
  "\nPublication-ready Tables 2A–2C created successfully."
)

message(
  "\nStatistical highlighting is based on UNROUNDED posterior intervals."
)

message(
  "\nWord (landscape, Aptos 9):\n  ",
  word_output
)

message(
  "\nExcel:\n  ",
  excel_output
)

message(
  "\nHTML:\n",
  "  output/tables/Table_2A_publication_ready.html\n",
  "  output/tables/Table_2B_publication_ready.html\n",
  "  output/tables/Table_2C_publication_ready.html"
)

# ======================================================================
# END OF SCRIPT 8B
# ======================================================================
# ======================================================================
# SCRIPT 8C — TABLE 4: WITHIN-BETWEEN MULTILEVEL MODEL
# Publication-ready Word + Excel + CSV
# ======================================================================
#
# Table 4 reports:
#   - Individual-level demographic/control variables
#   - Within-neighbourhood perceived-environment effects
#   - Between-neighbourhood perceived-environment effects
#
# Columns:
#   - Walking vs Private car
#   - Cycling vs Private car
#   - Public transport vs Private car
#
# Each cell contains:
#   OR (95% Bayesian CrI)
#
# Bold values indicate that the ORIGINAL, UNROUNDED 95% CrI excludes OR = 1.
#
# Word output:
#   - landscape
#   - Aptos, 9 pt
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES
# ----------------------------------------------------------------------

required_packages <- c(
  "brms",
  "dplyr",
  "stringr",
  "tibble",
  "purrr",
  "tidyr",
  "readr",
  "flextable",
  "officer",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running Script 8C:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(brms)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(tidyr)
library(readr)
library(flextable)
library(officer)
library(openxlsx)

dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. LOAD FINAL WITHIN-BETWEEN MODEL
# ----------------------------------------------------------------------

model_path <-
  "output/models/ML5_within_between_perceived_environment_with_LOO.rds"

if (!file.exists(model_path)) {
  stop(
    "Model file not found:\n",
    model_path,
    "\nCheck the filename in output/models."
  )
}

fit_ml5 <- readRDS(model_path)

if (!inherits(fit_ml5, "brmsfit")) {
  stop("The loaded object is not a brmsfit model.")
}

# ----------------------------------------------------------------------
# 2. DEFINE VARIABLES USED IN TABLE 4
# ----------------------------------------------------------------------

control_vars <- c(
  "Age_z",
  "GenderMan",
  "Edu_level_z",
  "Nr_household_z",
  "EQ_5D_index_z",
  "Income_z"
)

within_vars <- c(
  "FS_High_quality_cycling_infrastructure_z_within",
  "FS_Cycling_friendly_environment_z_within",
  "FS_Positive_parking_experiences_z_within"
)

between_vars <- c(
  "FS_High_quality_cycling_infrastructure_z_between",
  "FS_Cycling_friendly_environment_z_between",
  "FS_Positive_parking_experiences_z_between"
)

all_predictors <- c(
  control_vars,
  within_vars,
  between_vars
)

variable_labels <- c(
  Age_z = "Age",
  GenderMan = "Gender: male",
  Edu_level_z = "Educational level",
  Nr_household_z = "Household size",
  EQ_5D_index_z = "EQ-5D index",
  Income_z = "Household income",
  
  FS_High_quality_cycling_infrastructure_z_within =
    "High-quality cycling infrastructure",
  FS_Cycling_friendly_environment_z_within =
    "Cycling-friendly environment",
  FS_Positive_parking_experiences_z_within =
    "Positive parking experiences",
  
  FS_High_quality_cycling_infrastructure_z_between =
    "High-quality cycling infrastructure",
  FS_Cycling_friendly_environment_z_between =
    "Cycling-friendly environment",
  FS_Positive_parking_experiences_z_between =
    "Positive parking experiences"
)

variable_reference <- tibble(
  predictor = all_predictors,
  Variable = unname(variable_labels[all_predictors]),
  Domain = c(
    rep("Individual-level covariates", length(control_vars)),
    rep("Within-neighbourhood perceived environment", length(within_vars)),
    rep("Between-neighbourhood perceived environment", length(between_vars))
  ),
  variable_order = seq_along(all_predictors)
)

# ----------------------------------------------------------------------
# 3. EXTRACT UNROUNDED FIXED EFFECTS
# ----------------------------------------------------------------------

fixed_effects <- as.data.frame(
  brms::fixef(
    fit_ml5,
    probs = c(0.025, 0.975)
  )
) %>%
  rownames_to_column("parameter") %>%
  as_tibble() %>%
  filter(
    !str_detect(parameter, "Intercept$")
  ) %>%
  transmute(
    parameter = parameter,
    estimate = Estimate,
    lower_95 = Q2.5,
    upper_95 = Q97.5,
    OR = exp(Estimate),
    OR_lower = exp(Q2.5),
    OR_upper = exp(Q97.5),
    credible_nonzero = Q2.5 > 0 | Q97.5 < 0
  )

# ----------------------------------------------------------------------
# 4. MAP PARAMETER NAMES TO PREDICTORS
# ----------------------------------------------------------------------

identify_predictor <- function(parameter_name) {
  
  matches <- all_predictors[
    vapply(
      all_predictors,
      function(current_predictor) {
        stringr::str_ends(
          parameter_name,
          stringr::fixed(current_predictor)
        )
      },
      logical(1)
    )
  ]
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  matches[which.max(nchar(matches))]
}

identify_contrast <- function(parameter_name, predictor_name) {
  
  prefix <- substr(
    parameter_name,
    1,
    nchar(parameter_name) - nchar(predictor_name)
  )
  
  prefix_clean <- prefix %>%
    str_replace("^mu", "") %>%
    str_replace("_$", "") %>%
    str_to_lower()
  
  case_when(
    str_detect(prefix_clean, "onfoot|walk") ~ "Walking vs Private car",
    str_detect(prefix_clean, "bike|cycl") ~ "Cycling vs Private car",
    str_detect(prefix_clean, "publictransport|public_transport|pt") ~
      "Public transport vs Private car",
    TRUE ~ NA_character_
  )
}

fixed_effects <- fixed_effects %>%
  mutate(
    predictor = map_chr(parameter, identify_predictor),
    contrast = map2_chr(parameter, predictor, identify_contrast)
  ) %>%
  filter(!is.na(predictor))

unmapped <- fixed_effects %>%
  filter(is.na(contrast))

if (nrow(unmapped) > 0) {
  print(unmapped %>% select(parameter, predictor))
  stop(
    "At least one coefficient could not be mapped to a mode contrast. ",
    "Inspect the printed parameter names."
  )
}

# ----------------------------------------------------------------------
# 5. FORMAT OR (95% CrI)
# ----------------------------------------------------------------------

format_or_cri <- function(OR, lower, upper, digits = 2) {
  
  paste0(
    formatC(OR, format = "f", digits = digits),
    " (",
    formatC(lower, format = "f", digits = digits),
    "–",
    formatC(upper, format = "f", digits = digits),
    ")"
  )
}

fixed_effects <- fixed_effects %>%
  mutate(
    formatted_result = format_or_cri(
      OR,
      OR_lower,
      OR_upper
    )
  )

# ----------------------------------------------------------------------
# 6. BUILD WIDE DISPLAY TABLE
# ----------------------------------------------------------------------

contrast_order <- c(
  "Walking vs Private car",
  "Cycling vs Private car",
  "Public transport vs Private car"
)

display_table <- fixed_effects %>%
  select(
    predictor,
    contrast,
    formatted_result
  ) %>%
  tidyr::pivot_wider(
    names_from = contrast,
    values_from = formatted_result
  ) %>%
  right_join(
    variable_reference,
    by = "predictor"
  ) %>%
  arrange(variable_order) %>%
  select(
    predictor,
    Variable,
    Domain,
    all_of(contrast_order)
  )

significance_table <- fixed_effects %>%
  select(
    predictor,
    contrast,
    credible_nonzero
  ) %>%
  tidyr::pivot_wider(
    names_from = contrast,
    values_from = credible_nonzero
  ) %>%
  right_join(
    variable_reference,
    by = "predictor"
  ) %>%
  arrange(variable_order) %>%
  select(
    predictor,
    Variable,
    Domain,
    all_of(contrast_order)
  )

# Replace any missing display cells with em dash.
for (current_contrast in contrast_order) {
  
  display_table[[current_contrast]] <-
    tidyr::replace_na(
      display_table[[current_contrast]],
      "—"
    )
  
  significance_table[[current_contrast]] <-
    tidyr::replace_na(
      significance_table[[current_contrast]],
      FALSE
    )
}

# ----------------------------------------------------------------------
# 7. INSERT DOMAIN HEADER ROWS
# ----------------------------------------------------------------------

domain_order <- c(
  "Individual-level covariates",
  "Within-neighbourhood perceived environment",
  "Between-neighbourhood perceived environment"
)

display_output <- list()
significance_output <- list()

for (current_domain in domain_order) {
  
  current_display <- display_table %>%
    filter(Domain == current_domain)
  
  current_significance <- significance_table %>%
    filter(Domain == current_domain)
  
  header_display <- tibble(
    predictor = NA_character_,
    Variable = current_domain,
    Domain = "HEADER",
    `Walking vs Private car` = "",
    `Cycling vs Private car` = "",
    `Public transport vs Private car` = ""
  )
  
  header_significance <- tibble(
    predictor = NA_character_,
    Variable = current_domain,
    Domain = "HEADER",
    `Walking vs Private car` = FALSE,
    `Cycling vs Private car` = FALSE,
    `Public transport vs Private car` = FALSE
  )
  
  display_output[[current_domain]] <- bind_rows(
    header_display,
    current_display
  )
  
  significance_output[[current_domain]] <- bind_rows(
    header_significance,
    current_significance
  )
}

display_final <- bind_rows(display_output)
significance_final <- bind_rows(significance_output)

# ----------------------------------------------------------------------
# 8. SAVE PLAIN CSV
# ----------------------------------------------------------------------

csv_table <- display_table %>%
  select(
    Variable,
    Domain,
    all_of(contrast_order)
  )

write_csv(
  csv_table,
  "output/tables/Table_4_within_between_model.csv"
)

# ----------------------------------------------------------------------
# 9. CREATE PUBLICATION-READY FLEXTABLE
# ----------------------------------------------------------------------

ft_data <- display_final %>%
  select(
    Variable,
    all_of(contrast_order)
  )

header_rows <- which(
  display_final$Domain == "HEADER"
)

ft <- flextable(ft_data)

ft <- set_header_labels(
  ft,
  Variable = "Variable",
  `Walking vs Private car` = "Walking vs car",
  `Cycling vs Private car` = "Cycling vs car",
  `Public transport vs Private car` = "Public transport vs car"
)

ft <- theme_booktabs(ft)

# Aptos 9 pt
ft <- font(
  ft,
  fontname = "Aptos",
  part = "all"
)

ft <- fontsize(
  ft,
  size = 9,
  part = "all"
)

ft <- bold(
  ft,
  part = "header"
)

ft <- align(
  ft,
  j = "Variable",
  align = "left",
  part = "all"
)

ft <- align(
  ft,
  j = contrast_order,
  align = "center",
  part = "all"
)

ft <- valign(
  ft,
  valign = "center",
  part = "all"
)

# Domain headings
if (length(header_rows) > 0) {
  
  ft <- bold(
    ft,
    i = header_rows,
    part = "body"
  )
  
  ft <- bg(
    ft,
    i = header_rows,
    bg = "#E6E6E6",
    part = "body"
  )
}

# Bold credible effects using original, unrounded posterior intervals.
for (current_contrast in contrast_order) {
  
  credible_rows <- which(
    significance_final[[current_contrast]] %in% TRUE
  )
  
  if (length(credible_rows) > 0) {
    
    ft <- bold(
      ft,
      i = credible_rows,
      j = current_contrast,
      bold = TRUE,
      part = "body"
    )
  }
}

# Widths
ft <- width(
  ft,
  j = "Variable",
  width = 3.4
)

ft <- width(
  ft,
  j = contrast_order,
  width = 1.75
)

ft <- padding(
  ft,
  padding.top = 2,
  padding.bottom = 2,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)

ft <- set_caption(
  ft,
  caption =
    "Table 4. Within- and between-neighbourhood associations with transport mode choice"
)

ft <- add_footer_lines(
  ft,
  values = paste0(
    "Note. Values are odds ratios (OR) with 95% Bayesian credible intervals (CrI). ",
    "Bold values indicate that the original, unrounded 95% CrI excludes OR = 1. ",
    "Within-neighbourhood effects represent individual deviations from the neighbourhood mean; ",
    "between-neighbourhood effects represent neighbourhood mean perceptions. ",
    "Private car is the reference outcome category."
  )
)

ft <- font(
  ft,
  fontname = "Aptos",
  part = "footer"
)

ft <- fontsize(
  ft,
  size = 8,
  part = "footer"
)

# ----------------------------------------------------------------------
# 10. EXPORT WORD — LANDSCAPE
# ----------------------------------------------------------------------

doc <- read_docx()

landscape_section <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 11.69,
    height = 8.27
  ),
  page_margins = page_mar(
    top = 0.55,
    bottom = 0.55,
    left = 0.55,
    right = 0.55
  ),
  type = "continuous"
)

doc <- body_set_default_section(
  doc,
  landscape_section
)

doc <- body_add_flextable(
  doc,
  ft
)

word_output <-
  "output/tables/Table_4_within_between_model_landscape.docx"

print(
  doc,
  target = word_output
)

# ----------------------------------------------------------------------
# 11. EXPORT EXCEL
# ----------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(
  wb,
  "Table 4"
)

excel_table <- display_final %>%
  select(
    Variable,
    all_of(contrast_order)
  )

writeData(
  wb,
  sheet = "Table 4",
  x = excel_table,
  startRow = 1,
  startCol = 1
)

header_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

domain_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  fgFill = "#E6E6E6"
)

credible_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center"
)

normal_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9
)

center_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  halign = "center"
)

addStyle(
  wb,
  sheet = "Table 4",
  style = header_style,
  rows = 1,
  cols = 1:4,
  gridExpand = TRUE,
  stack = TRUE
)

addStyle(
  wb,
  sheet = "Table 4",
  style = normal_style,
  rows = 2:(nrow(excel_table) + 1),
  cols = 1,
  gridExpand = TRUE,
  stack = TRUE
)

addStyle(
  wb,
  sheet = "Table 4",
  style = center_style,
  rows = 2:(nrow(excel_table) + 1),
  cols = 2:4,
  gridExpand = TRUE,
  stack = TRUE
)

# Domain rows
excel_domain_rows <- which(
  display_final$Domain == "HEADER"
) + 1

for (current_row in excel_domain_rows) {
  
  addStyle(
    wb,
    sheet = "Table 4",
    style = domain_style,
    rows = current_row,
    cols = 1:4,
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Bold credible effects
for (contrast_index in seq_along(contrast_order)) {
  
  current_contrast <- contrast_order[contrast_index]
  excel_col <- contrast_index + 1
  
  credible_rows <- which(
    significance_final[[current_contrast]] %in% TRUE
  ) + 1
  
  if (length(credible_rows) > 0) {
    
    addStyle(
      wb,
      sheet = "Table 4",
      style = credible_style,
      rows = credible_rows,
      cols = excel_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

setColWidths(
  wb,
  sheet = "Table 4",
  cols = 1,
  widths = 42
)

setColWidths(
  wb,
  sheet = "Table 4",
  cols = 2:4,
  widths = 23
)

freezePane(
  wb,
  sheet = "Table 4",
  firstRow = TRUE
)

excel_output <-
  "output/tables/Table_4_within_between_model.xlsx"

saveWorkbook(
  wb,
  excel_output,
  overwrite = TRUE
)

# ----------------------------------------------------------------------
# 12. FINAL MESSAGE
# ----------------------------------------------------------------------

message(
  "\nTable 4 created successfully."
)

message(
  "\nWord:\n  ",
  word_output
)

message(
  "\nExcel:\n  ",
  excel_output
)

message(
  "\nCSV:\n  output/tables/Table_4_within_between_model.csv"
)

# ======================================================================
# END OF SCRIPT 8C
# ======================================================================
# ======================================================================
# SCRIPT 8D
# SUPPLEMENTARY TABLE — ICLV BENCHMARK RESULTS
# ======================================================================

rm(list = ls())
gc()

# ----------------------------------------------------------------------
# 0. PACKAGES
# ----------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "tibble",
  "flextable",
  "officer",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages first:\n",
    paste(missing_packages, collapse = "\n")
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(flextable)
library(officer)
library(openxlsx)

dir.create(
  "output/tables",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. INPUT FILES
# ----------------------------------------------------------------------

latent_choice_path <-
  "output/tables/ICLV_latent_choice_effects.csv"

measurement_path <-
  "output/tables/ICLV_measurement_loadings.csv"

fit_path <-
  "output/tables/ICLV_fit_summary.csv"


# If your files contain "(1)" in their filenames, use instead:
#
# latent_choice_path <-
#   "output/tables/ICLV_latent_choice_effects(1).csv"
#
# measurement_path <-
#   "output/tables/ICLV_measurement_loadings(1).csv"
#
# fit_path <-
#   "output/tables/ICLV_fit_summary(1).csv"


required_files <- c(
  latent_choice_path,
  measurement_path,
  fit_path
)

if (any(!file.exists(required_files))) {
  
  stop(
    "One or more ICLV output files could not be found.\n",
    "Check the paths specified in Section 1."
  )
}


# ----------------------------------------------------------------------
# 2. READ DATA
# ----------------------------------------------------------------------

latent_choice <- read_csv(
  latent_choice_path,
  show_col_types = FALSE
)

measurement <- read_csv(
  measurement_path,
  show_col_types = FALSE
)

fit_summary <- read_csv(
  fit_path,
  show_col_types = FALSE
)

n_iclv <- fit_summary$n_respondents[1]


# ----------------------------------------------------------------------
# 3. HELPER FUNCTION
# ----------------------------------------------------------------------

format_ci <- function(
    estimate,
    lower,
    upper,
    digits = 2
) {
  
  paste0(
    formatC(
      estimate,
      format = "f",
      digits = digits
    ),
    " (",
    formatC(
      lower,
      format = "f",
      digits = digits
    ),
    "–",
    formatC(
      upper,
      format = "f",
      digits = digits
    ),
    ")"
  )
}


# ======================================================================
# PANEL A — LATENT CONSTRUCTS AND MODE CHOICE
# ======================================================================

# ----------------------------------------------------------------------
# 4. CLEAN CONSTRUCT NAMES
# ----------------------------------------------------------------------

construct_labels <- c(
  "High_quality_cycling_infrastructure" =
    "High-quality cycling infrastructure",
  
  "Cycling_friendly_environment" =
    "Cycling-friendly environment",
  
  "Positive_parking_experiences" =
    "Positive parking experiences"
)


alternative_labels <- c(
  "On foot vs Private car" =
    "Walking vs car",
  
  "Bike vs Private car" =
    "Cycling vs car",
  
  "Public transport vs Private car" =
    "Public transport vs car"
)


# ----------------------------------------------------------------------
# 5. FORMAT OR AND 95% CI
# ----------------------------------------------------------------------

latent_choice_clean <- latent_choice %>%
  mutate(
    
    Construct =
      recode(
        latent_construct,
        !!!construct_labels
      ),
    
    Alternative =
      recode(
        alternative,
        !!!alternative_labels
      ),
    
    result =
      format_ci(
        odds_ratio,
        odds_ratio_lower_95,
        odds_ratio_upper_95
      ),
    
    significant =
      interval_excludes_zero
  )


# ----------------------------------------------------------------------
# 6. WIDE TABLE
# ----------------------------------------------------------------------

panel_A <- latent_choice_clean %>%
  select(
    Construct,
    Alternative,
    result
  ) %>%
  pivot_wider(
    names_from = Alternative,
    values_from = result
  ) %>%
  select(
    Construct,
    `Walking vs car`,
    `Cycling vs car`,
    `Public transport vs car`
  )


# Separate significance matrix
panel_A_sig <- latent_choice_clean %>%
  select(
    Construct,
    Alternative,
    significant
  ) %>%
  pivot_wider(
    names_from = Alternative,
    values_from = significant
  ) %>%
  select(
    Construct,
    `Walking vs car`,
    `Cycling vs car`,
    `Public transport vs car`
  )


# ======================================================================
# PANEL B — MEASUREMENT MODEL
# ======================================================================

# ----------------------------------------------------------------------
# 7. INDICATOR LABELS
# ----------------------------------------------------------------------

indicator_labels <- c(
  
  "zeta_hqci_qualitative_lanes" =
    "Quality of cycling lanes",
  
  "zeta_hqci_maintained" =
    "Maintenance of cycling infrastructure",
  
  "zeta_hqci_sufficient" =
    "Sufficient cycling infrastructure",
  
  "zeta_hqci_streets_squares" =
    "Maintenance of streets and squares",
  
  "zeta_cfe_neighbourhood" =
    "Cycling-friendly neighbourhood",
  
  "zeta_cfe_enjoyable" =
    "Enjoyable cycling",
  
  "zeta_cfe_safe_cyclist" =
    "Feeling safe in traffic as cyclist",
  
  "zeta_cfe_busy_unsafe" =
    "Busy traffic causing unsafe cycling experiences",
  
  "zeta_ppe_accessible_car" =
    "Accessible by car",
  
  "zeta_ppe_parking_residents" =
    "Easy parking for residents",
  
  "zeta_ppe_parking_visitors" =
    "Easy parking for visitors"
)


construct_labels_measurement <- c(
  
  "High_quality_cycling_infrastructure" =
    "High-quality cycling infrastructure",
  
  "Cycling_friendly_environment" =
    "Cycling-friendly environment",
  
  "Positive_parking_experiences" =
    "Positive parking experiences"
)


# ----------------------------------------------------------------------
# 8. CLEAN MEASUREMENT RESULTS
# ----------------------------------------------------------------------

panel_B <- measurement %>%
  mutate(
    
    Construct =
      recode(
        latent_construct,
        !!!construct_labels_measurement
      ),
    
    Indicator =
      recode(
        parameter,
        !!!indicator_labels
      ),
    
    Loading =
      formatC(
        estimate,
        format = "f",
        digits = 2
      ),
    
    `Robust SE` =
      formatC(
        standard_error,
        format = "f",
        digits = 2
      ),
    
    `95% CI` =
      paste0(
        formatC(
          lower_95,
          format = "f",
          digits = 2
        ),
        "–",
        formatC(
          upper_95,
          format = "f",
          digits = 2
        )
      )
  ) %>%
  select(
    Construct,
    Indicator,
    Loading,
    `Robust SE`,
    `95% CI`
  ) %>%
  arrange(
    factor(
      Construct,
      levels = c(
        "High-quality cycling infrastructure",
        "Cycling-friendly environment",
        "Positive parking experiences"
      )
    )
  )


# ======================================================================
# 9. WORD TABLE — PANEL A
# ======================================================================

ft_A <- flextable(
  panel_A
)

ft_A <- theme_booktabs(
  ft_A
)

ft_A <- font(
  ft_A,
  fontname = "Aptos",
  part = "all"
)

ft_A <- fontsize(
  ft_A,
  size = 9,
  part = "all"
)

ft_A <- bold(
  ft_A,
  part = "header"
)

ft_A <- align(
  ft_A,
  j = "Construct",
  align = "left",
  part = "all"
)

ft_A <- align(
  ft_A,
  j = c(
    "Walking vs car",
    "Cycling vs car",
    "Public transport vs car"
  ),
  align = "center",
  part = "all"
)


# ----------------------------------------------------------------------
# 10. BOLD SUPPORTED CHOICE EFFECTS
# ----------------------------------------------------------------------

choice_columns <- c(
  "Walking vs car",
  "Cycling vs car",
  "Public transport vs car"
)

for (current_column in choice_columns) {
  
  significant_rows <- which(
    panel_A_sig[[current_column]] %in% TRUE
  )
  
  if (length(significant_rows) > 0) {
    
    ft_A <- bold(
      ft_A,
      i = significant_rows,
      j = current_column,
      bold = TRUE,
      part = "body"
    )
  }
}


ft_A <- width(
  ft_A,
  j = "Construct",
  width = 2.8
)

ft_A <- width(
  ft_A,
  j = choice_columns,
  width = 1.8
)

ft_A <- padding(
  ft_A,
  padding.top = 3,
  padding.bottom = 3,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)


# ======================================================================
# 11. WORD TABLE — PANEL B
# ======================================================================

ft_B <- flextable(
  panel_B
)

ft_B <- theme_booktabs(
  ft_B
)

ft_B <- font(
  ft_B,
  fontname = "Aptos",
  part = "all"
)

ft_B <- fontsize(
  ft_B,
  size = 9,
  part = "all"
)

ft_B <- bold(
  ft_B,
  part = "header"
)

ft_B <- align(
  ft_B,
  j = c(
    "Construct",
    "Indicator"
  ),
  align = "left",
  part = "all"
)

ft_B <- align(
  ft_B,
  j = c(
    "Loading",
    "Robust SE",
    "95% CI"
  ),
  align = "center",
  part = "all"
)

ft_B <- width(
  ft_B,
  j = "Construct",
  width = 2.4
)

ft_B <- width(
  ft_B,
  j = "Indicator",
  width = 3.3
)

ft_B <- width(
  ft_B,
  j = c(
    "Loading",
    "Robust SE"
  ),
  width = 1.1
)

ft_B <- width(
  ft_B,
  j = "95% CI",
  width = 1.6
)

ft_B <- padding(
  ft_B,
  padding.top = 2,
  padding.bottom = 2,
  padding.left = 3,
  padding.right = 3,
  part = "all"
)


# ======================================================================
# 12. CREATE WORD DOCUMENT
# ======================================================================

doc <- read_docx()


# Landscape
landscape_section <- prop_section(
  
  page_size = page_size(
    orient = "landscape",
    width = 11.69,
    height = 8.27
  ),
  
  page_margins = page_mar(
    top = 0.6,
    bottom = 0.6,
    left = 0.6,
    right = 0.6
  ),
  
  type = "continuous"
)


doc <- body_set_default_section(
  doc,
  landscape_section
)


# ----------------------------------------------------------------------
# TITLE
# ----------------------------------------------------------------------

doc <- body_add_par(
  doc,
  paste0(
    "Supplementary Table Sx. ",
    "Integrated choice and latent variable (ICLV) benchmark model results"
  ),
  style = "heading 1"
)


# ----------------------------------------------------------------------
# PANEL A
# ----------------------------------------------------------------------

doc <- body_add_par(
  doc,
  "Panel A. Associations between latent constructs and transport mode choice",
  style = "heading 2"
)

doc <- body_add_flextable(
  doc,
  ft_A
)


doc <- body_add_par(
  doc,
  paste0(
    "Note. Values are odds ratios (OR) with 95% confidence intervals (CI). ",
    "Bold values indicate that the original, unrounded 95% CI excludes OR = 1. ",
    "Private car is the reference outcome category. ",
    "N = ",
    n_iclv,
    "."
  )
)


# ----------------------------------------------------------------------
# PANEL B
# ----------------------------------------------------------------------

doc <- body_add_par(
  doc,
  "Panel B. Measurement model loadings",
  style = "heading 2"
)

doc <- body_add_flextable(
  doc,
  ft_B
)


doc <- body_add_par(
  doc,
  paste0(
    "Note. SE = standard error; CI = confidence interval. ",
    "Robust standard errors are reported. ",
    "The measurement model used ordinal indicators with five response categories. ",
    "N = ",
    n_iclv,
    "."
  )
)


# ----------------------------------------------------------------------
# SAVE WORD
# ----------------------------------------------------------------------

word_output <-
  "output/tables/Supplementary_Table_ICLV_benchmark.docx"

print(
  doc,
  target = word_output
)


# ======================================================================
# 13. EXCEL OUTPUT
# ======================================================================

wb <- createWorkbook()


# ----------------------------------------------------------------------
# PANEL A SHEET
# ----------------------------------------------------------------------

addWorksheet(
  wb,
  "Choice effects"
)

writeData(
  wb,
  sheet = "Choice effects",
  x = panel_A,
  startRow = 1,
  startCol = 1
)


header_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center",
  border = "Bottom"
)

normal_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9
)

center_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  halign = "center"
)

significant_style <- createStyle(
  fontName = "Aptos",
  fontSize = 9,
  textDecoration = "bold",
  halign = "center"
)


addStyle(
  wb,
  "Choice effects",
  header_style,
  rows = 1,
  cols = 1:4,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Choice effects",
  normal_style,
  rows = 2:(nrow(panel_A) + 1),
  cols = 1,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Choice effects",
  center_style,
  rows = 2:(nrow(panel_A) + 1),
  cols = 2:4,
  gridExpand = TRUE
)


for (column_index in seq_along(choice_columns)) {
  
  current_column <- choice_columns[column_index]
  
  significant_rows <- which(
    panel_A_sig[[current_column]] %in% TRUE
  ) + 1
  
  if (length(significant_rows) > 0) {
    
    addStyle(
      wb,
      "Choice effects",
      significant_style,
      rows = significant_rows,
      cols = column_index + 1,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}


setColWidths(
  wb,
  "Choice effects",
  cols = 1,
  widths = 36
)

setColWidths(
  wb,
  "Choice effects",
  cols = 2:4,
  widths = 24
)


# ----------------------------------------------------------------------
# PANEL B SHEET
# ----------------------------------------------------------------------

addWorksheet(
  wb,
  "Measurement model"
)

writeData(
  wb,
  sheet = "Measurement model",
  x = panel_B,
  startRow = 1,
  startCol = 1
)

addStyle(
  wb,
  "Measurement model",
  header_style,
  rows = 1,
  cols = 1:5,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Measurement model",
  normal_style,
  rows = 2:(nrow(panel_B) + 1),
  cols = 1:2,
  gridExpand = TRUE
)

addStyle(
  wb,
  "Measurement model",
  center_style,
  rows = 2:(nrow(panel_B) + 1),
  cols = 3:5,
  gridExpand = TRUE
)

setColWidths(
  wb,
  "Measurement model",
  cols = 1,
  widths = 35
)

setColWidths(
  wb,
  "Measurement model",
  cols = 2,
  widths = 45
)

setColWidths(
  wb,
  "Measurement model",
  cols = 3:5,
  widths = 18
)


# ----------------------------------------------------------------------
# SAVE EXCEL
# ----------------------------------------------------------------------

excel_output <-
  "output/tables/Supplementary_Table_ICLV_benchmark.xlsx"

saveWorkbook(
  wb,
  excel_output,
  overwrite = TRUE
)


# ======================================================================
# 14. CSV OUTPUTS
# ======================================================================

write_csv(
  panel_A,
  "output/tables/Supplementary_ICLV_choice_effects.csv"
)

write_csv(
  panel_B,
  "output/tables/Supplementary_ICLV_measurement_model.csv"
)


# ======================================================================
# 15. FINAL MESSAGE
# ======================================================================

message(
  "\nICLV supplementary table created successfully."
)

message(
  "\nWord:\n  ",
  word_output
)

message(
  "\nExcel:\n  ",
  excel_output
)

message(
  "\nCSV files:\n",
  "  output/tables/Supplementary_ICLV_choice_effects.csv\n",
  "  output/tables/Supplementary_ICLV_measurement_model.csv"
)

# ======================================================================
# END SCRIPT 8D
# ======================================================================
