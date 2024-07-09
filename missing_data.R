########################################################################################################
# Missing data investigation
########################################################################################################

# Clear Variables
rm(list=ls(all=TRUE))

library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(haven)
library(mice)
library(naniar)

# Load data
data <- read_dta("xxxx/USA_causal.dta") ## put in your working directory

## See Stata code for variable name transformation ##

############### Missing data information
# Load necessary library
library(dplyr)


# Define the variable list
variables <- c("ST161Q01HA", "ST161Q02HA", "ST161Q03HA", "highpf", "female", "ESCS", "pv2read", 
               "JOYREAD", "COMPETE", "wellbeing", "ST034Q01TA", "ST034Q02TA", "ST034Q04TA", "ST034Q06TA", 
               "parecoura", "stutearela")

# Function to calculate missing values and their percentages
missing_info <- function(data, variables) {
  data %>%
    summarise(across(all_of(variables), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "MissingCount") %>%
    mutate(TotalCount = nrow(data),
           MissingPercentage = (MissingCount / TotalCount) * 100)
}

# Calculate missing values and percentages
missing_summary <- missing_info(data, variables)

# Display the results
print(missing_summary)



############### Little’s MCAR Test

vars <- c("ST161Q01HA", "ST161Q02HA", "ST161Q03HA", "highpf", "female", "ESCS", "pv2read", 
          "JOYREAD", "COMPETE", "wellbeing", "ST034Q01TA", "ST034Q02TA", "ST034Q04TA", "ST034Q06TA", 
          "parecoura", "stutearela")
df <-data[vars]
result <- naniar::mcar_test(df)
print(result)
# Interpretation: if the p-value is high (typically > 0.05), you fail to reject the null hypothesis, suggesting that the data is MCAR.


############### Logistic regression to predict missingness
## generate composite var for self-efficacy
data$efficacy <- ifelse(!is.na(data$ST161Q01HA) & !is.na(data$ST161Q02HA) & !is.na(data$ST161Q03HA), 
                        (data$ST161Q01HA + data$ST161Q02HA + data$ST161Q03HA) / 3, NA)


## generate composite var for peer relationships
# Reverse code ST034Q02TA
data$ST034Q02TA_r <- ifelse(!is.na(data$ST034Q02TA), 5 - data$ST034Q02TA, NA)

# Generate peerlation variable
data$peerlation <- ifelse(!is.na(data$ST034Q01TA) & !is.na(data$ST034Q02TA_r) & 
                          !is.na(data$ST034Q04TA) & !is.na(data$ST034Q06TA), 
                        (data$ST034Q01TA + data$ST034Q02TA_r + data$ST034Q04TA + data$ST034Q06TA) / 4, NA)

data$missing_efficacy <- ifelse(is.na(data$efficacy), 1, 0)

missingness_model <- glm(missing_efficacy ~ potafeed + female + ESCS + pv2read + 
                           JOYREAD + COMPETE + wellbeing + peerlation + parecoura + stutearela, data=data, family="binomial")
summary(missingness_model)

# Interpretation:
# Significant results (p<.05) suggest the missingness of gktmathss can be predicted by other observed variables, which could suggest MAR (but need to make the decisio with theoretical considerations).


############### Multiple imputation
main_weight <- "w_fstuwt"
replicate_weights <- paste0("w_fsturwt", 1:80)
all_weights <- c(main_weight, replicate_weights)

other_vars <- c("efficacy", "highpf", "female", "ESCS", "pv2read",  
          "JOYREAD", "COMPETE", "wellbeing", "peerlation", "parecoura", 
          "stutearela", "CNTSCHID", "CNTSTUID", 
          "Region", "ADMINMODE", "LANGTEST_QQQ", 
          "LANGTEST_COG", "LANGTEST_PAQ", "AGE", "IMMIG", "MISCED", "FISCED", 
          "HISEI", "HOMEPOS", "CULTPOSS", "HEDRES", "WEALTH", "ATTLNACT", 
          "BEINGBULLIED", "EMOSUPS", "SCREADCOMP", "SCREADDIFF", "PISADIFF", 
          "WORKMAST", "GFOFAIL", "RESILIENCE", "LMINS", "MMINS", "SMINS", "TMINS", 
          "DISCLIMA", "TEACHSUP", "DIRINS", "PERFEED", "STIMREAD")

vars2 <- c(other_vars, all_weights)

data2 <- data[vars2]

vars_to_impute <- c("efficacy", "highpf", "female", "ESCS", "pv2read", 
                    "JOYREAD", "COMPETE", "wellbeing", "peerlation", 
                    "parecoura", "stutearela")

data2_impute <- data2 %>%
  select(all_of(vars_to_impute), all_of(all_weights)) %>%
  mutate(across(everything(), as.numeric))

pred_matrix <- mice(data2_impute, maxit = 0)$predictorMatrix

# Don't use weights as predictors and don't impute weights
pred_matrix[, all_weights] <- 0
pred_matrix[all_weights, ] <- 0

# Set a seed for reproducibility
set.seed(123)
imp <- mice(data2_impute, m = 5, maxit = 50, method = 'pmm', 
            predictorMatrix = pred_matrix, seed = 500)

install.packages("survey")
library(survey)
install.packages("mitools")
library(mitools)
install.packages("purrr")
library(purrr)

# Create a list of imputed datasets
imputed_datasets <- lapply(1:5, function(i) complete(imp, i))

# Define the survey design for each imputed dataset
survey_designs <- lapply(imputed_datasets, function(data) {
  svrepdesign(
    data = data,
    weights = ~w_fstuwt,
    repweights = data[, replicate_weights],
    type = "Fay",
    rho = 0.5,  # Fay's adjustment factor for PISA
    scale = 1,
    rscales = rep(1, 80),
    mse = TRUE
  )
})

# Function to run regression
run_regression <- function(design) {
  svyglm(efficacy ~ highpf + ESCS + pv2read + female + JOYREAD + 
           COMPETE + wellbeing + peerlation + parecoura + stutearela, design = design)
}

# Run regression on each imputed dataset
model_list <- lapply(survey_designs, run_regression)

# Combine results using Rubin's rules
combined_results <- MIcombine(model_list)

# Extract combined coefficients and standard errors
final_results <- summary(combined_results)

# Add R-squared (average across imputations)
r_squared <- mean(sapply(model_list, function(model) {
  1 - (model$deviance / model$null.deviance)
}))

# Print results
print(final_results)
print(paste("Average R-squared:", r_squared))


