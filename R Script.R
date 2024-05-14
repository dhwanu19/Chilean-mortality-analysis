# ACTL3141 Assignment R Code

# 0 - Set Up #######################################################################

# Set Directory
setwd("C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment")

# List of required packages
required_packages <- c("readr", "dplyr", "survival", "readxl", "ggplot2", 
                       "ggfortify", "splines", "knitr", "gridExtra", "survival",
                       "KMsurv", "coin", "BeSS", "MASS", "flexsurv", "survminer",
                       "actxps", "splines", "vcd")

# Identify which packages are not installed yet
packages_to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

# Load the packages
library(readr)
library(dplyr)
library(survival)
library(readxl)
library(ggplot2)
library(ggfortify)
library(splines)
library(knitr)
library(gridExtra)
library(survival) 
library(KMsurv)
library(coin)
library(BeSS)
library(MASS)
library(flexsurv)
library(survminer)
library(actxps)
library(splines)
library(vcd)

mortality <- read_csv("ChileanMortality.csv")
# LT <- read_excel("ChileanLifeTables.xlsx", sheet = 1)

# 0 - Data Cleaning ################################################################

# Convert dates to "Date" class if not already
mortality$BIRTHDATE <- as.Date(mortality$BIRTHDATE, format="%d/%m/%Y")
mortality$DATE_START <- as.Date(mortality$DATE_START, format="%d/%m/%Y")
mortality$DATE_END <- as.Date(mortality$DATE_END, format="%d/%m/%Y")

mortality$DEATH <- as.numeric(mortality$DEATH)


# Calculate entry age and exit age (or age at death/censoring)
mortality$age_at_start <- as.numeric(
  difftime(mortality$DATE_START, mortality$BIRTHDATE, units = "days")) / 365.25
mortality$age_at_end <- as.numeric(
  difftime(mortality$DATE_END, mortality$BIRTHDATE, units = "days")) / 365.25

mortality <- mortality %>% 
  filter(age_at_end != age_at_start)  

mortality_saved <- mortality
#print(mortality)
# Checking for missing values
sum(is.na(mortality$SEX))
sum(is.na(mortality$HEALTH))
sum(is.na(mortality$PERSON_TYPE))
sum(is.na(mortality$age_at_start))
sum(is.na(mortality$age_at_end))

problematic_indices <- which(mortality$age_at_end <= mortality$age_at_start)
if(length(problematic_indices) > 0) {
  print(paste("Problematic rows: ", length(problematic_indices)))
  head(mortality[problematic_indices, ]) # Display a few of these problematic rows for review
} else {
  print("No problematic rows found.")
}

# 1 - Data Summary #############################################################
summary(mortality)
#mortality
#print(levels(factor(mortality$HEALTH)))
#print(levels(factor(mortality$PERSON_TYPE)))

# 1 - Age Distribution #########################################################
# Age at start of exposure !!!!!!!!!!!!!!!!
mortality$age_group <- cut(mortality$age_at_end,
                           breaks = c(60, 65, 70, 75, 80, 85, 90, 95, Inf),
                           labels = c("60-65", "65-70", "70-75", "75-80", "80-85", 
                                      "85-90", "90-95", "95+"),
                           right = FALSE,
                           include.lowest = TRUE)

# Plot histogram of age groups
plot <- ggplot(mortality, aes(x = age_group)) + 
  geom_bar(fill = "gray", color = "black") + 
  theme_minimal() +
  labs(title = "Distribution of Age Groups",
       x = "Age Group",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

ggsave("age_group_distribution.jpg", plot = plot, width = 8, height = 6, dpi = 300)

# 1 - Sex Distribution #########################################################
sex_plot <- ggplot(mortality, aes(x = SEX, fill = SEX)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = c("M" = "gray", "F" = "lightblue")) +
  theme_minimal() +
  labs(title = "Distribution of Sex",
       x = "Sex",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

ggsave("sex_distribution_colored.jpg", plot = sex_plot, width = 8, height = 6, dpi = 300)

# 1 - Health Distribution ######################################################
health_plot <- ggplot(mortality, aes(x = HEALTH, fill = HEALTH)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = c("Disabled" = "gray", "Healthy" = "lightblue")) +
  theme_minimal() +
  labs(title = "Distribution of Health Status",
       x = "Health Status",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

ggsave("health_distribution_colored.jpg", plot = health_plot, width = 8, height = 6, dpi = 300)


# 1 - Person_Type Distribution #################################################
person_type_plot <- ggplot(mortality, aes(x = PERSON_TYPE, fill = PERSON_TYPE)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = c("Beneficiary" = "gray", "Main Annuitant" = "lightblue")) +
  theme_minimal() +
  labs(title = "Distribution of Person Type",
       x = "Person Type",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for better readability

ggsave("person_type_distribution_colored.jpg", plot = person_type_plot, width = 8, height = 6, dpi = 300)

# 1 - Proportions ##############################################################
# Total number of observations
#summary(mortality)
total_observations <- nrow(mortality)
cat("Total observations:", total_observations, "\n\n")

# Proportions of SEX
sex_counts <- table(mortality$SEX)
sex_proportions <- prop.table(sex_counts)
cat("Proportion of SEX = M:", format(sex_proportions["M"], nsmall = 4), "\n")
cat("Proportion of SEX = F:", format(sex_proportions["F"], nsmall = 4), "\n\n")

# Proportions of HEALTH
health_counts <- table(mortality$HEALTH)
health_proportions <- prop.table(health_counts)
cat("Proportion of HEALTH = Disabled:", format(health_proportions["Disabled"], nsmall = 4), "\n")
cat("Proportion of HEALTH = Healthy:", format(health_proportions["Healthy"], nsmall = 4), "\n\n")

# Proportions of PERSON_TYPE
person_type_counts <- table(mortality$PERSON_TYPE)
person_type_proportions <- prop.table(person_type_counts)
cat("Proportion of Main Annuitants:", format(person_type_proportions["Main Annuitant"], nsmall = 4), "\n")
cat("Proportion of Beneficiaries:", format(person_type_proportions["Beneficiary"], nsmall = 4), "\n")

# 1 - 4x4 Grid of Plots ########################################################
age_group_plot <- plot

grid.arrange(age_group_plot, sex_plot, health_plot, person_type_plot, 
             ncol = 2, nrow = 2)

ggsave("combined_task1_plots.jpg", 
       grid.arrange(age_group_plot, sex_plot, health_plot, person_type_plot, ncol = 2, nrow = 2),
       width = 8, height = 6, dpi = 1600)
# 1 - Death Distribution #######################################################
# Function to calculate death proportions by a given covariate
calculate_death_proportions <- function(data, covariate) {
  data %>%
    group_by(!!sym(covariate)) %>%
    summarise(
      Total = n(),
      Deaths = sum(DEATH),
      Death_Proportion = Deaths / Total
    ) %>%
    mutate(Covariate = !!sym(covariate))
}

death_proportions_sex <- calculate_death_proportions(mortality, 'SEX')
print(death_proportions_sex)

death_proportions_health <- calculate_death_proportions(mortality, 'HEALTH')
print(death_proportions_health)

death_proportions_type <- calculate_death_proportions(mortality, 'PERSON_TYPE')
print(death_proportions_type)

# Adjusted calculation for death proportions by age group
death_proportions_age_group <- mortality %>%
  group_by(age_group) %>%
  summarise(
    Total = n(), 
    Deaths = sum(DEATH, na.rm = TRUE), 
    Death_Proportion = Deaths / Total 
  ) %>%
  ungroup() 

print(death_proportions_age_group)




# 1 - Correlation ##############################################################


mortality_copy <- mortality

mortality_copy$SEX_2 <- as.integer(as.factor(mortality_copy$SEX))
mortality_copy$PERSON_TYPE_2 <- as.integer(as.factor(mortality_copy$PERSON_TYPE))
mortality_copy$HEALTH_2 <- as.integer(as.factor(mortality_copy$HEALTH))

mortality_dummies <- cbind(mortality_copy, model.matrix(~SEX_2 + PERSON_TYPE_2 + HEALTH_2 - 1, data = mortality_copy))

correlation_matrix <- cor(mortality_dummies[, grepl("SEX_2|PERSON_TYPE_2|HEALTH_2", colnames(mortality_dummies))])
print(correlation_matrix)
print(mortality_copy)

# 2 - Survival Analysis (KM & NA Estimators) ###################################
# 2 - Survival Object ###########################################################
surv_obj <- Surv(time = mortality$age_at_start, time2 = mortality$age_at_end, 
                 event = mortality$DEATH)

# 2 - KM Estimator for S(x) #####################################################
km_fit <- survfit(surv_obj ~ 1, conf.int = 0.95)
plot(km_fit, main="Kaplan-Meier Survival Curve", xlab="Age", ylab="S(x)", lwd=1.5, xlim=c(60, 120))
# ggsave("KM_S(x).jpg", plot = km_plot, width = 8, height = 6, dpi = 300)

# 2 - NA Estimator for S(x) ###################################################
na_fit <- survfit(surv_obj ~ 1, type = "fh", conf.int = 0.95)
# plot(na_fit, main="Nelson-Aalen Survival Curve", xlab="Age", ylab="S(x)", lwd=1.5, col = "blue")

plot(km_fit, main = "KM v.s. NA estimates", xlab= "time", ylab = "S(x)", col = "red", lwd=1.5, xlim=c(60, 120),) 
lines(na_fit, col = rgb(0, 0, 1, 0.5), lwd=1.5) 
legend("bottomleft", c("KM", "NA"), col = c("red", "blue"), lty = 1, lwd=1.5)

# 2 - KM survival curves by Gender #############################################
par(mfrow=c(2, 2))
plot(km_fit, main="Kaplan-Meier Survival Curve", xlab="Age", ylab="S(x)", lwd=1.5, xlim=c(60, 120))
# print(levels(factor(mortality$SEX)))
km_gender <- survfit(surv_obj ~ mortality$SEX, data = mortality)
plot(km_gender, col=c("red", "black"), lwd=1.5, xlim=c(60, 120),
     main="KM Survival Curves by Gender", xlab="Time", ylab="S(x)")
legend("bottomleft", legend=c("Female", "Male"), col=c("red", "black"), lty=1, lwd=1.5)

# 2 - KM survival curves by Person Type ########################################
km_person_type <- survfit(surv_obj ~ mortality$PERSON_TYPE)
plot(km_person_type, col=c("red", "black"), lwd=1.5, xlim=c(60, 120),
     main="KM Survival Curves by Person Type", xlab="Time", ylab="S(x)")
legend("bottomleft", legend=c("Beneficiary", "Main A"), col=c("red", "black"), lty=1, lwd=1.5)

# 2 - KM survival curves by Health #############################################
km_health <- survfit(surv_obj ~ mortality$HEALTH)
plot(km_health, col=c("red", "black"), lwd=1.5, xlim=c(60, 120),
     main="KM Survival Curves by Health", xlab="Time", ylab="S(x)")
legend("bottomleft", legend=c("Disabled", "Healthy"), col=c("red", "black"), lty=1, lwd=1.5)
par(mfrow=c(1, 1))

# 2 - IGNORE ###################################################################
# For Males by Person Type
par(mfrow=c(1, 2))
male_data <- mortality %>% 
  filter(SEX == 'M')
surv_obj <- Surv(time = male_data$age_at_start, time2 = male_data$age_at_end, 
                 event = male_data$DEATH)
# male_data

km_person_type_male <- survfit(surv_obj ~ male_data$PERSON_TYPE)
plot(km_person_type_male, col=c("red", "black"), lwd=1.5, xlim=c(60, 100), 
     main="KM Survival Curves by Person Type (MALE)", xlab="Time", ylab="S(x)")
legend("bottomleft", legend=c("Beneficiary", "Main A"), col=c("red", "black"), lty=1, lwd=1.5)

f_data <- mortality %>% 
  filter(SEX == 'F')
surv_obj <- Surv(time = f_data$age_at_start, time2 = f_data$age_at_end, 
                 event = f_data$DEATH)
head(f_data)
km_person_type <- survfit(surv_obj ~ f_data$PERSON_TYPE)
plot(km_person_type, col=c("red", "black"), lwd=1.5, xlim=c(60, 100), 
     main="KM Survival Curves by Person Type (FEMALE)", xlab="Time", ylab="S(x)")
legend("bottomleft", legend=c("Beneficiary", "Main A"), col=c("red", "black"), lty=1, lwd=1.5)
par(mfrow=c(1, 1))
# 2 - Survival Analysis (Cox Regression) ###########################################
morality <- mortality_saved
surv_obj <- Surv(time = mortality$age_at_start, time2 = mortality$age_at_end, 
                 event = mortality$DEATH)

cox_model_full <- coxph(surv_obj ~ SEX + HEALTH + PERSON_TYPE, data = mortality, method="breslow")
step_model <- stepAIC(cox_model_full, direction = "backward") # Could use "both" direction
# summary(step_model)

# Best model (using AIC)
best_model <- coxph(Surv(time = mortality$age_at_start, time2 = mortality$age_at_end, 
                         event = mortality$DEATH) ~ SEX + HEALTH + PERSON_TYPE, data = mortality, method="breslow")

summary(best_model)
surv_fit <- survfit(best_model)

# Data frame of survival estimates for plotting
surv_data <- data.frame(time = surv_fit$time, surv = surv_fit$surv)

# Find the last time point in the survival data (assuming it's the closest to survival hitting 0)
# last_point <- tail(surv_data, 1)

# Plot survival function with annotation
ggplot(surv_data, aes(x = time, y = surv)) +
  geom_line(size = 1.5) + 
  labs(title = "Survival Function S(x) with Last Point Labelled", x = "Time", y = "Survival Probability") +
  theme_minimal()

# WHY AIC?
# AIC Considers Entropy (trade-off of GOF and Complexity)
# Smaller AIC = Better
# There isn't a large difference in the number of parameters in the model (min 0
# and max 3), therefore AIC is suitable. There is also a large sample size. 

# WHY BACKWARD SELECTION?

# Test the PH assumption using cox.zph for the step model 
par(mfrow=c(2, 2))
ph_test <- cox.zph(best_model)
ph_test
summary(ph_test)
plot(ph_test)
par(mfrow=c(1, 1))

# (https://dr.lib.iastate.edu/server/api/core/bitstreams/40ec8d92-910e-4d15-8032-2e523285f7ef/content#:~:text=Schoenfeld%20residuals%20are%20intended%20to,form%20for%20the%20observed%20responses.)
best_model_zph <- cox.zph(best_model)
# Dashed lines are basically a CI 
ggcoxzph(best_model_zph, var = c("SEX"), font.main = 12, point.size = 1, ylim = c(-10, 10))
ggcoxzph(best_model_zph, var = c("PERSON_TYPE"), font.main = 12, point.size = 1, ylim = c(-10, 10))
ggcoxzph(best_model_zph, var = c("HEALTH"), font.main = 12, point.size = 1, ylim = c(-10, 10))
# METHOD 2
# (https://rpubs.com/kaz_yos/resid_cox)
mortality$mart_res <- residuals(best_model, type="martingale")
mortality$cox_snell_res <- -(mortality$mart_res - mortality$DEATH)

fit_coxsnell <- coxph(formula = Surv(cox_snell_res, DEATH) ~ 1,
                      data    = mortality,
                      ties    = c("efron","breslow","exact")[1])

df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)

summary(df_base_haz)

# Plotting with a 45-degree reference line
ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +  # Or geom_line() depending on your preference
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Adds a 45-degree reference line
  scale_x_continuous(limit = c(0, 5.5)) +
  scale_y_continuous(limit = c(0, 5.5)) +
  labs(x = "Cox-Snell residuals as pseudo observed times",
       y = "Estimated cumulative hazard at pseudo observed times") +
  theme_bw() +
  theme(legend.key = element_blank())

# 2 - Plotting all S(x)
plot(km_fit, main="Survival Curves: KM Estimate vs Cox Model", xlab="Time", ylab="S(x)", col="blue", lwd=1.5)
lines(surv_fit, col="black", lwd=1.5)

legend("bottomleft", legend=c("KM Estimate", "Cox Model"), col=c("blue", "black"), lty=1, lwd=1.5)


# 2 - IGNORE 2 #################################################################
best_model_f <- coxph(Surv(time = f_data$age_at_start, time2 = f_data$age_at_end, 
                         event = f_data$DEATH) ~ PERSON_TYPE, data = f_data, method="breslow")

summary(best_model_f)
# 2 - Interaction (PERSON_TYPE) ################################################

mortality <- mortality_saved
  
best_model_with_interaction <- coxph(Surv(time = mortality$age_at_start, time2 = mortality$age_at_end, 
                                          event = mortality$DEATH) ~ HEALTH + SEX + PERSON_TYPE:SEX + PERSON_TYPE:HEALTH + PERSON_TYPE:HEALTH:SEX, data = mortality, method="breslow")

summary(best_model_with_interaction)

# 2 - NUMBER OF DISABLED FEMALES #############################################
disabled_female_annuitants <- mortality %>%
  filter(SEX == 'F' & HEALTH == 'Disabled' & PERSON_TYPE %in% c('Beneficiary', 'Main Annuitant')) %>%
  group_by(PERSON_TYPE) %>%
  summarise(Count = n())

# Print the results
print(disabled_female_annuitants)

# 3 - Crude Estimate (qx) ######################################################
healthy_main_annuitants <- mortality %>% 
  filter(HEALTH == 'Healthy')

nrow(healthy_main_annuitants)
sum(healthy_main_annuitants$DEATH == 1)

# Function to calculate initial exposures, number of deaths, and mortality rate (qx) for a given age interval
calculate_qx <- function(data, birthdate_col, entry_col, exit_col, death_col) {
  results <- data.frame(Age = integer(), InitialExposure = numeric(), Deaths = integer(), qx = numeric())
  
  # Loop over each age from 60 to 100
  for(age in 60:100) {
    at_risk <- data[data[[entry_col]] < age + 1 & data[[exit_col]] > age,]
    
    a_i <- pmax(at_risk[[entry_col]], age)
    
    b_i <- ifelse(at_risk[[exit_col]] >= age & at_risk[[exit_col]] < age + 1 & at_risk[[death_col]] == 1, 
                  age + 1, 
                  pmin(at_risk[[exit_col]], age + 1))
    
    initial_exposure_age <- sum((b_i - a_i)[b_i >= a_i])
    
    # Calculate deaths for the given age interval
    deaths_age <- sum(at_risk[[exit_col]] >= age & at_risk[[exit_col]] < age + 1 & at_risk[[death_col]] == 1)
    
    # Calculate qx
    qx_age <- ifelse(initial_exposure_age > 0, deaths_age / initial_exposure_age, 0)
    
    # Add the calculated exposure, deaths and qx to the results data frame
    results <- rbind(results, data.frame(x = age, 
                                         Ex_lastB = initial_exposure_age, 
                                         dx_lastB = deaths_age, 
                                         qx = qx_age))
  }
  
  return(results)
}

# Call the function using your mortality data
qx_results <- calculate_qx(healthy_main_annuitants, 'BIRTHDATE', 'age_at_start', 'age_at_end', 'DEATH')

# Print the calculated initial exposures, deaths, and mortality rate (qx)
print(qx_results)
print(sum(qx_results$dx_lastB))
ggplot(data = qx_results, aes(x = x, y = qx)) +
  geom_line() + # This creates a line plot
  geom_point() + # This adds points to the line plot
  labs(title = "Mortality Prob (qx) vs Age (x)",
       x = "Age (x)",
       y = "Mortality Prob (qx)") +
  theme_minimal() # This sets a minimal theme for the plot
#write.csv(qx_results, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/qx_results.csv", row.names = TRUE)

# 3 - Crude Estimate (mx) ##########################################################
calculate_mx <- function(data, birthdate_col, entry_col, exit_col, death_col) {
  results <- data.frame(Age = integer(), InitialExposure = numeric(), Deaths = integer(), qx = numeric())
  
  # Loop over each age from 60 to 100
  age <- 60
  while (age <= 100) {
    at_risk <- data[data[[entry_col]] < age + 1 & data[[exit_col]] > age,]
    
    a_i <- pmax(at_risk[[entry_col]], age)
    
    b_i <- pmin(at_risk[[exit_col]], age + 1)
    
    initial_exposure_age <- sum((b_i - a_i)[b_i >= a_i])
    
    # Calculate deaths for the given age interval
    deaths_age <- sum(at_risk[[exit_col]] > age & at_risk[[exit_col]] <= age + 1 & at_risk[[death_col]] == 1)
    
    # Calculate qx
    mx_age <- ifelse(initial_exposure_age > 0, deaths_age / initial_exposure_age, 0)
    
    # Add the calculated exposure, deaths and qx to the results data frame
    results <- rbind(results, data.frame(x = age + 0.5, 
                                         ExC_nearestB = initial_exposure_age, 
                                         dx_nearestB = deaths_age, 
                                         mx = mx_age))
    age <- age + 1
  }

  
  return(results)
}

# Call the function using your mortality data
mx_results <- calculate_mx(healthy_main_annuitants, 'BIRTHDATE', 'age_at_start', 'age_at_end', 'DEATH')

# Print the calculated initial exposures, deaths, and mortality rate (qx)
print(mx_results)

ggplot(data = mx_results, aes(x = x, y = mx)) +
  geom_line() + # This creates a line plot
  geom_point() + # This adds points to the line plot
  labs(title = "Force of Mortality (mx) vs Age (x)",
       x = "Age (x)",
       y = "Force of Mortality (mx)") +
  theme_minimal() # This sets a minimal theme for the plot

#write.csv(mx_results, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/mx_results.csv", row.names = TRUE)
# 3 - Gompertz #################################################################
#mx_results
# TALK ABOUT WEIGHTS IN REPORT
# Fit the Gompertz model to the data
gompertz_fit <- nls(mx ~ exp(b0 + b1 * x), data = mx_results, 
                    start = list(b0 = 0, b1 = 0), weights = mx_results$ExC_nearestB / mx_results$mx)

summary(gompertz_fit)
# Obtain the coefficients from the model
coefs <- coef(gompertz_fit)

# fitted_values$cumulative_hazard <- (exp(coefs["b0"]) / coefs["b1"]) * (exp(coefs["b1"] * fitted_values$x) - 1)

# Create a new data frame for fitted values
fitted_values <- data.frame(x = mx_results$x, 
                            fitted_mx = exp(coefs["b0"] + coefs["b1"] * mx_results$x))


# Plot the observed and fitted mortality rates
ggplot(data = mx_results, aes(x = x)) +
  geom_point(aes(y = mx), colour = "red") +
  geom_line(data = fitted_values, aes(x = x, y = fitted_mx), colour = "blue") +
  labs(title = "Observed vs Fitted Mortality Rates",
       x = "Age",
       y = "Mortality Rate (mx)",
       subtitle = "Red points: Observed, Blue line: Fitted Gompertz Model") +
  theme_minimal()

# Plot with log scale for the mortality rates 
ggplot(data = mx_results, aes(x = x)) +
  geom_point(aes(y = log(mx)), colour = "red") + # Observed data in log scale
  geom_line(data = fitted_values, aes(x = x, y = log(fitted_mx)), colour = "blue") + # Fitted data in log scale
  labs(title = "Observed vs Fitted Mortality Rates on Log Scale",
       x = "Age",
       y = "Log of Mortality Rate (log(mx))",
       subtitle = "Red points: Observed, Blue line: Fitted Gompertz Model") +
  theme_minimal()

# 3 - Makeham ##################################################################

# Fit the Makeham model to the data using the Gompertz model estimates as starting values
makeham_fit <- nls(mx ~ A + exp(b0 + b1 * x), data = mx_results, 
                   start = list(A = 0, b0 = coef(gompertz_fit)[1], b1 = coef(gompertz_fit)[2]), 
                   weights = mx_results$ExC_nearestB / mx_results$mx)

# Print the summary of the fit
summary(makeham_fit)

# Obtain the fitted mortality rates
mx_makeham <- fitted(makeham_fit)

# Create a new data frame for fitted values
fitted_values_makeham <- data.frame(x = mx_results$x, 
                                    fitted_mx = mx_makeham)

# Plot the observed and fitted mortality rates
ggplot(data = mx_results, aes(x = x)) +
  geom_point(aes(y = mx), colour = "red") + # Observed data
  geom_line(data = fitted_values_makeham, aes(x = x, y = fitted_mx), colour = "blue") + # Fitted data
  labs(title = "Observed vs Fitted Mortality Rates: Makeham Law",
       x = "Age",
       y = "Mortality Rate (mx)",
       subtitle = "Red points: Observed, Blue line: Fitted Makeham Model") +
  theme_minimal()

# Plot with log scale for the mortality rates
ggplot(data = mx_results, aes(x = x)) +
  geom_point(aes(y = log(mx)), colour = "red") + # Observed data in log scale
  geom_line(data = fitted_values_makeham, aes(x = x, y = log(fitted_mx)), colour = "blue") + # Fitted data in log scale
  labs(title = "Observed vs Fitted Mortality Rates on Log Scale: Makeham Law",
       x = "Age",
       y = "Log of Mortality Rate (log(mx))",
       subtitle = "Red points: Observed, Blue line: Fitted Makeham Model") +
  theme_minimal()

# 3 - mx to qx Conversions ##################################################################
# Your existing code for loading data and calculating mx (mortality rates) goes here

# Function to integrate mx over the interval [x, x+1] for Gompertz Model
integrate_mx_gompertz <- function(age, b0, b1) {
  f <- function(s) { exp(b0 + b1 * s) } # Define the Gompertz function
  integrate(f, lower = age - 0.5, upper = age + 0.5)$value # Perform numerical integration
}

# Convert mx to qx using the provided formula and the Gompertz model
convert_mx_to_qx_gompertz <- function(mx_data, coefs) {
  mx_data$qx_floor <- sapply(mx_data$x, function(x) {
    integral <- integrate_mx_gompertz(x, coefs["b0"], coefs["b1"])
    1 - exp(-integral)
  })
  mx_data
}

# Obtain coefficients from Gompertz model fit
coefs <- coef(gompertz_fit)

# Convert mx to qx for the entire dataset using Gompertz Model
qx_from_mx_gompertz_results <- convert_mx_to_qx_gompertz(mx_results, coefs)

# Define the Makeham coefficients (Assuming you have estimated these from your data)
makeham_coefs <- coef(makeham_fit)
  
# Function to integrate mx over the interval [x, x+1] for Makeham Model
integrate_mx_makeham <- function(age, A, b0, b1) {
  f <- function(s) { A + exp(b0 + b1 * s) }
  integrate(f, lower = age - 0.5, upper = age + 0.5)$value
}

# Convert mx to qx using the provided formula and the Makeham model
convert_mx_to_qx_makeham <- function(mx_data, coefs) {
  mx_data$qx_makeham_floor <- sapply(mx_data$x, function(x) {
    integral <- integrate_mx_makeham(x, coefs['A'], coefs['b0'], coefs['b1'])
    1 - exp(-integral)
  })
  mx_data
}

qx_from_mx_makeham_results <- convert_mx_to_qx_makeham(mx_results, makeham_coefs)

print(qx_from_mx_gompertz_results)
print(qx_from_mx_makeham_results)

#write.csv(qx_from_mx_gompertz_results, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/qx_gompertz.csv", row.names = FALSE)
#write.csv(qx_from_mx_makeham_results, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/qx_makeham.csv", row.names = FALSE)
# 3 - Vanilla Regression Splines ###############################################

# And the knots are as follows:
knots <- c(72, 88)

# Create the cubic spline basis using the ns function from the splines package
cubic_basis <- ns(qx_results$x, knots = knots)

# Fit a linear model to the mortality data with the cubic spline basis as the predictor
cubSpline_2k <- lm(qx ~ cubic_basis, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)

# Extract the fitted values from the spline model
fitted_qx_2k <- fitted(cubSpline_2k)

# Create a ggplot of the observed vs. fitted mortality rates using the spline model
ggplot(qx_results, aes(x = x)) +
  geom_point(aes(y = qx), colour = "red") +
  geom_line(aes(y = fitted_qx_2k), colour = "blue") +
  labs(title = "Observed vs Fitted Mortality Rates: Regression Spline",
       x = "Age",
       y = "Mortality Prob (px)",
       subtitle = "Red points: Observed, Blue line: Fitted Regression Spline") +
  theme_minimal()

# 3 - Optimal Regression Splines #############################################
# Initialization of the optimal model structure for each number of knots
optimal_models <- list()

#sum(is.na(qx_results$x) | is.nan(qx_results$x) | is.infinite(qx_results$x))
#sum(is.na(qx_results$qx) | is.nan(qx_results$qx) | is.infinite(qx_results$qx))
#sum(is.na(qx_results$Ex_lastB / qx_results$qx) | is.nan(qx_results$Ex_lastB / qx_results$qx) | is.infinite(qx_results$Ex_lastB / qx_results$qx))

# Adjusted model fitting function to use basis splines
fit_spline_model <- function(data, knot_positions) {
  # print(knot_positions)
  if (length(knot_positions) > 2 && min(diff(sort(knot_positions))) <= 4) {
    # print(diff(sort(knot_positions)))
    return(Inf)
  }
  data$basis <- ns(data$x, knots = knot_positions)
  model <- lm(qx ~ basis, data = data, weights = data$Ex_lastB / data$qx)
  aic_value <- AIC(model)
  
  
  # Calculate AICc
  # k <- length(knot_positions) + 1 # Plus 1 for the intercept
  # aicc_value <- aic_value + (2 * k^2 / (min(diff(sort(knot_positions))^2)))
  
  return(aic_value)
}

# Optimization loop modified to store optimal model for each number of knots
for (num_knots in 1:6) {
  cat("Processing num_knots:", num_knots, "\n")
  interval_length <- 25
  possible_knots <- combn(seq(min(qx_results$x) + 5, max(qx_results$x) - 5, length.out = interval_length), num_knots, simplify = FALSE)
  
  # Initialize temporary optimal model for the current number of knots
  temp_optimal_model <- list(aic = Inf, knots = NULL)
  
  for (knot_positions in possible_knots) {
    current_aic <- fit_spline_model(qx_results, knot_positions)
    if (is.finite(current_aic) && current_aic < temp_optimal_model$aic) {
      temp_optimal_model$aic <- current_aic
      temp_optimal_model$knots <- knot_positions
    }
  }
  
  # Store the optimal model for the current number of knots
  optimal_models[[as.character(num_knots)]] <- temp_optimal_model
  
  cat("Completed num_knots:", num_knots, "with AIC:", temp_optimal_model$aic, "\n")
}

# temp_spline_df <- qx_results[, c("x", "qx")]

# IGNORE #######################################################################
reg_splines_df <- qx_results[, c("x", "qx")]

model_info1 <- optimal_models[[1]]
cubic_basis_optimal1 <- bs(qx_results$x, knots = model_info1$knots, degree = 3)
optimal_spline_model1 <- lm(qx ~ cubic_basis_optimal1, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
reg_splines_df$reg_splines_df_k1 <- fitted(optimal_spline_model1)

model_info2 <- optimal_models[[2]]
cubic_basis_optimal2 <- bs(qx_results$x, knots = model_info2$knots, degree = 3)
optimal_spline_model2 <- lm(qx ~ cubic_basis_optimal2, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
reg_splines_df$reg_splines_df_k2 <- fitted(optimal_spline_model2)

model_info3 <- optimal_models[[3]]
cubic_basis_optimal3 <- bs(qx_results$x, knots = model_info3$knots, degree = 3)
optimal_spline_model3 <- lm(qx ~ cubic_basis_optimal3, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
reg_splines_df$reg_splines_df_k3 <- fitted(optimal_spline_model3)

model_info4 <- optimal_models[[4]]
cubic_basis_optimal4 <- bs(qx_results$x, knots = model_info4$knots, degree = 3)
optimal_spline_model4 <- lm(qx ~ cubic_basis_optimal4, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
reg_splines_df$reg_splines_df_k4 <- fitted(optimal_spline_model4)

model_info5 <- optimal_models[[5]]
cubic_basis_optimal5 <- bs(qx_results$x, knots = model_info5$knots, degree = 3)
optimal_spline_model5 <- lm(qx ~ cubic_basis_optimal5, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
reg_splines_df$reg_splines_df_k5 <- fitted(optimal_spline_model5)

reg_splines_df

# Loop through each stored optimal model and create plots for them, including AICc value
for (num_knots in names(optimal_models)) {
  model_info <- optimal_models[[num_knots]]
  
  # Refit the m2odel with the optimal knots for the current number of knots
  cubic_basis_optimal <- bs(qx_results$x, knots = model_info$knots, degree = 3)
  optimal_spline_model <- lm(qx ~ cubic_basis_optimal, data = qx_results, weights = qx_results$Ex_lastB / qx_results$qx)
  
  # Generate the plot for the current optimal model
  p <- ggplot(qx_results, aes(x = x)) +
    geom_point(aes(y = qx), colour = "red") +
    geom_line(aes(y = fitted(optimal_spline_model)), colour = "blue") +
    geom_vline(xintercept = model_info$knots, linetype = "dashed", color = "green") +
    labs(title = paste("Optimal Spline Model with", num_knots, "Knots"),
         x = "Age",
         y = "Mortality Prob (qx)",
         subtitle = paste("Optimal Knots at Ages:", toString(model_info$knots))) +
    annotate("text", x = Inf, y = Inf, label = paste("AICc:", round(model_info$aic, 2)), hjust = 1.1, vjust = 1.1, size = 5) +
    theme_minimal()
  
  # Print the plot
  print(p)
}

# 3 - Smoothing Splines ########################################################
# Calculate the residuals from the optimal spline model
residuals_optimal_spline <- residuals(cubSpline_2k)

# Calculate the weighted residuals, as weights are used in the model fitting
weighted_residuals_optimal_spline <- residuals_optimal_spline * sqrt(qx_results$Ex_lastB / qx_results$qx)

# Calculate the RSS by summing the squares of the weighted residuals
rss_optimal_spline <- sum(weighted_residuals_optimal_spline^2)

# Output the RSS
rss_optimal_spline

# Predefine the RSS of the optimal spline model
optimal_rss <- rss_optimal_spline # Make sure this is defined from your previous optimal model's RSS calculation

# Create a vector to store smoothing parameters that result in RSS less than optimal_rss
optimal_spar_values <- numeric(0)

# Loop over smoothing parameters from 0.5 to 0.99 in steps of 0.01
for(spar_value in seq(0, 0.99, by = 0.01)) {
  # Fit the smoothing spline to your data
  smSpline <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = spar_value)
  
  # Calculate the weighted residuals and their RSS
  fitted_values <- predict(smSpline, qx_results$x)$y
  weights <- qx_results$Ex_lastB / qx_results$qx
  residuals <- qx_results$qx - fitted_values
  weighted_residuals <- residuals * sqrt(weights)
  spline_rss <- sum(weighted_residuals^2)
  
  # Compare the RSS with the optimal RSS
  if(spline_rss < optimal_rss) {
    # Save the spar value
    print(spline_rss)
    optimal_spar_values <- c(optimal_spar_values, spar_value)
  }
}

# Display the spar values that resulted in RSS less than the optimal RSS
optimal_spar_values

sm_splines_df <- qx_results[, c("x", "qx")]

smooth_spline65 <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = 0.63)
# smooth_spline63 <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = 0.63)
smooth_spline61 <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = 0.60)
# smooth_spline59 <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = 0.59)
smooth_spline57 <- smooth.spline(x = qx_results$x, y = qx_results$qx, spar = 0.57)

# Predict values for each smoothing spline
pred_spline65 <- predict(smooth_spline65, qx_results$x)
sm_splines_df$qx_ss_65 <- pred_spline65$y
# pred_spline63 <- predict(smooth_spline63, qx_results$x)
pred_spline61 <- predict(smooth_spline61, qx_results$x)
sm_splines_df$qx_ss_61 <- pred_spline61$y
# pred_spline59 <- predict(smooth_spline59, qx_results$x)
pred_spline57 <- predict(smooth_spline57, qx_results$x)
sm_splines_df$qx_ss_57 <- pred_spline57$y

# Create the initial ggplot with the original data points
ggplot_final <- ggplot(qx_results, aes(x = x)) +
  geom_point(aes(y = qx), colour = "red", size = 2, alpha = 0.6) + # Original data points
  
  # Add each set of predicted spline values as a separate line with unique color aesthetic
  geom_line(data = data.frame(x = qx_results$x, y = predict(smooth_spline65, qx_results$x)$y), aes(y = y, colour = "spar=0.63"), size = 0.75) +
  #geom_line(data = data.frame(x = qx_results$x, y = predict(smooth_spline63, qx_results$x)$y), aes(y = y, colour = "spar=0.63"), size = 0.5) +
  geom_line(data = data.frame(x = qx_results$x, y = predict(smooth_spline61, qx_results$x)$y), aes(y = y, colour = "spar=0.60"), size = 0.75) +
  #geom_line(data = data.frame(x = qx_results$x, y = predict(smooth_spline59, qx_results$x)$y), aes(y = y, colour = "spar=0.59"), size = 0.5) +
  #geom_line(data = data.frame(x = qx_results$x, y = predict(smooth_spline57, qx_results$x)$y), aes(y = y, colour = "spar=0.57"), size = 0.5) +
  
  # Add titles, labels, and theme
  labs(title = "Selected Smoothing Splines",
       subtitle = "Smooth splines at different spar values with original data points",
       x = "Age", y = "Mortality Probability (qx)",
       caption = "Red points: Observed. Colored lines: Fitted Smoothing Splines for spar values") +
  theme_minimal() +
  
  # Define colors for the lines and set the legend title
  scale_colour_manual("Spar Value", 
                      values = c("spar=0.63" = "blue", "spar=0.60" = "orange")) +
  
  # Position the legend at the bottom right
  theme(legend.position = "bottom")

# Display the final plot
print(ggplot_final)

# 3 - Graduated Probabilities ##################################################
# Final data frame
final_df <- qx_results[, c("x", "qx")]
final_df$Ex <- qx_results$Ex_lastB
final_df$dx <- qx_results$dx_lastB 

# Gompertz
final_df$qx_gompertz <- qx_from_mx_gompertz_results$qx

# Makeham
final_df$qx_makeham <- qx_from_mx_makeham_results$qx

# Regression Splines
final_df$qx_rs_k1 <- reg_splines_df$reg_splines_df_k1
final_df$qx_rs_k2 <- reg_splines_df$reg_splines_df_k2
final_df$qx_rs_k3 <- reg_splines_df$reg_splines_df_k3 
final_df$qx_rs_k4 <- reg_splines_df$reg_splines_df_k4
final_df$qx_rs_k5 <- reg_splines_df$reg_splines_df_k5

# Smoothing Splines
final_df$qx_ss_65 <- sm_splines_df$qx_ss_65
final_df$qx_ss_61 <- sm_splines_df$qx_ss_61
final_df$qx_ss_57 <- sm_splines_df$qx_ss_57

# Final data frame
final_df
write.csv(final_df, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/qx_graduated.csv", row.names = FALSE)
# 3 - Standardized Deviance Calc ####################################################
zx_df <- data.frame(x = final_df$x)  

# Add standardized deviations for each model to zx_df
zx_df$gompertz <- (final_df$dx - (final_df$Ex * final_df$qx_gompertz)) / sqrt(final_df$Ex * final_df$qx_gompertz)
zx_df$makeham <- (final_df$dx - (final_df$Ex * final_df$qx_makeham)) / sqrt(final_df$Ex * final_df$qx_makeham)

zx_df$rs_k1 <- (final_df$dx - (final_df$Ex * final_df$qx_rs_k1)) / sqrt(final_df$Ex * final_df$qx_rs_k1)
zx_df$rs_k2 <- (final_df$dx - (final_df$Ex * final_df$qx_rs_k2)) / sqrt(final_df$Ex * final_df$qx_rs_k2)
zx_df$rs_k3 <- (final_df$dx - (final_df$Ex * final_df$qx_rs_k3)) / sqrt(final_df$Ex * final_df$qx_rs_k3)
zx_df$rs_k4 <- (final_df$dx - (final_df$Ex * final_df$qx_rs_k4)) / sqrt(final_df$Ex * final_df$qx_rs_k4)
zx_df$rs_k5 <- (final_df$dx - (final_df$Ex * final_df$qx_rs_k5)) / sqrt(final_df$Ex * final_df$qx_rs_k5)

zx_df$ss_65 <- (final_df$dx - (final_df$Ex * final_df$qx_ss_65)) / sqrt(final_df$Ex * final_df$qx_ss_65)
zx_df$ss_61 <- (final_df$dx - (final_df$Ex * final_df$qx_ss_61)) / sqrt(final_df$Ex * final_df$qx_ss_61)
zx_df$ss_57 <- (final_df$dx - (final_df$Ex * final_df$qx_ss_57)) / sqrt(final_df$Ex * final_df$qx_ss_57)

zx_df
# 3 - Standardized Deviance Test ###############################################
parameters <- data.frame(x = final_df$x)  
std_df <- data.frame(Parameter = parameters)

stdTest <- function(zx, breaks = c(-Inf, -3, -2, -1, 0, 1, 2, 3, Inf)){ 
  observed <- table(cut(zx, breaks)) #count observation in each interval 
  expected.p <- diff(pnorm(breaks)) #expected probabilities for standard normal 
  chisq.test(observed, p = expected.p) #apply chisquare test 
}

results <- list(
  std_gompertz = stdTest(zx_df$gompertz),
  std_makeham = stdTest(zx_df$makeham),
  std_rs_k1 = stdTest(zx_df$rs_k1),
  std_rs_k2 = stdTest(zx_df$rs_k2),
  std_rs_k3 = stdTest(zx_df$rs_k3),
  std_rs_k4 = stdTest(zx_df$rs_k4),
  std_rs_k5 = stdTest(zx_df$rs_k5),
  std_ss_65 = stdTest(zx_df$ss_65),
  std_ss_61 = stdTest(zx_df$ss_61),
  std_ss_57 = stdTest(zx_df$ss_57)
)

p_values <- sapply(results, function(x) x$p.value)
std_df <- data.frame(P_Value = p_values)
print(std_df)

# 3 - Chi2 GOF Test ############################################################
parameters <- c("chi2", "c.value", "df", "p.value")
chi2_df <- data.frame(Parameter = parameters)

chi2Test <- function(O, E, npar, alpha = 0.05){ 
  chi2 <- sum((O - E)^2 / E) # Test statistic 
  df <- length(O) - npar 
  chi2_alpha <- qchisq(1 - alpha, df) # Critical value 
  p.value <- 1 - pchisq(chi2, df) # p.value 
  list(statistic = chi2, c.value = chi2_alpha,
       df = df, p.value = p.value) 
}

chi2_df$gompertz <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_gompertz, length(coef(gompertz_fit)))
chi2_df$makeham <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_makeham, length(coef(gompertz_fit)))

chi2_df$rs_k1 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_rs_k1, optimal_spline_model1$rank)
chi2_df$rs_k2 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_rs_k2, optimal_spline_model2$rank)
chi2_df$rs_k3 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_rs_k3, optimal_spline_model3$rank)
chi2_df$rs_k4 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_rs_k4, optimal_spline_model4$rank)
chi2_df$rs_k5 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_rs_k5, optimal_spline_model5$rank)

chi2_df$ss_65 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_ss_65, smooth_spline65$df)
chi2_df$ss_61 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_ss_61, smooth_spline61$df)
chi2_df$ss_57 <- chi2Test(final_df$dx, final_df$Ex * final_df$qx_ss_57, smooth_spline57$df)
chi2_df

# 3 - Signs Test ###############################################################
nages <- length(final_df$x) 

# Function to calculate proportion of positive signs
calc_proportion <- function(column) {
  sum(column > 0) / length(column)
}

# Perform sign tests and calculate proportions, storing results in lists
results <- list()
proportions <- list()

variables <- c("gompertz", "makeham", "rs_k1", "rs_k2", "rs_k3", "rs_k4", "rs_k5", "ss_65", "ss_61", "ss_57")

for (var in variables) {
  column <- zx_df[[var]]
  proportions[[var]] <- calc_proportion(column) # Calculate proportion
  results[[var]] <- binom.test(sum(column > 0), nages) # Perform binom.test
}

# Extract names, p-values, and proportions to create a summary dataframe
parameters <- names(results)
p_values <- sapply(results, function(x) x$p.value)
proportion_positives <- unlist(proportions)

# Create the dataframe
signTest_df <- data.frame(
  Parameter = parameters, 
  Proportion_Positive = proportion_positives, 
  P_Value = p_values
)

# Display the dataframe
print(signTest_df)

# 3 - CumDev Test ##############################################################
cumDevTest <- function(A, E, alpha = 0.05){ 
  cumDev <- sum(A - E) / sqrt(sum(E)) #Test statistic 
  #print(cumDev)
  z_alpha <- qnorm(1 - alpha/2) #Critical value 
  #print(z_alpha)
  pn <- 0 
  if (cumDev < 0) {
    pn <- 1 - pnorm(cumDev)
  } else {
    pn <- pnorm(cumDev)
  }
  p.value <- 2 *(1 - pn) #p.value (Note it is two-tailed) 
  #print(pnorm(cumDev))
  #print(p.value)
  list(statistic = cumDev, c.value = z_alpha, p.value = p.value) 
}

parameters <- c("CumDev", "c.value", "p.value")
cumDev_Df <- data.frame(Parameter = parameters)

cumDev_Df$gompertz <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_gompertz)
cumDev_Df$makeham <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_makeham)

cumDev_Df$rs_k1 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_rs_k1)
cumDev_Df$rs_k2 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_rs_k2)
cumDev_Df$rs_k3 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_rs_k3)
cumDev_Df$rs_k4 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_rs_k4)
cumDev_Df$rs_k5 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_rs_k5)

cumDev_Df$ss_65 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_ss_65)
cumDev_Df$ss_61 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_ss_61)
cumDev_Df$ss_57 <- cumDevTest(final_df$dx, final_df$Ex * final_df$qx_ss_57)

cumDev_Df

# 3 - Grouping of Signs Test #######################################################
groupSignTest <- function(zx, alpha = 0.05){ 
  #Count +'s and -'s 
  signs <- sign(zx) 
  n1 <- sum(signs == 1) 
  n2 <- sum(signs == -1) 
  
  #Count runs 
  y <- c(-1, sign(zx)) 
  G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1] != -1) # No Runs 
  
  #Normal approximation 
  mu <- n1 * (n2 + 1) / (n1 + n2) 
  s2 <- (n1 * n2)^2 / (n1 + n2)^3 
  G_alpha <- qnorm(alpha, mean = mu, sd = sqrt(s2)) #Critical value 
  p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) #p.value (one sided)  
  list(statistic = G, c.value = G_alpha, p.value = p.value) 
}

results <- list()
proportions <- list()

variables <- c("gompertz", "makeham", "rs_k1", "rs_k2", "rs_k3", "rs_k4", "rs_k5", "ss_65", "ss_61", "ss_57")

for (var in variables) {
  column <- zx_df[[var]]
  results[[var]] <- groupSignTest(column) # Perform binom.test
}

# Extract names, p-values, and proportions to create a summary dataframe
parameters <- names(results)
p_values <- sapply(results, function(x) x$p.value)
stat <- sapply(results, function(x) x$statistic)
C <- sapply(results, function(x) x$c.value)
# proportion_positives <- unlist(proportions)

# Create the dataframe
GroupSignTest_df <- data.frame(
  Statistic = stat,
  C_value = C,
  P_Value = p_values
)

# Display the dataframe
print(GroupSignTest_df)

# 3 - Serial Correlations Test #################################################
#acf(zx_df$gompertz)
#acf(zx_df$makeham)

#acf(zx_df$rs_k1)
acf(zx_df$rs_k2)
#acf(zx_df$rs_k3)
#acf(zx_df$rs_k4)
#acf(zx_df$rs_k5)
zx_df$ss_63 <- zx_df$ss_65
zx_df$ss_60 <- zx_df$ss_61
acf(zx_df$ss_63)
acf(zx_df$ss_60)

# 3 - Testing Smoothness #######################################################
# Define a function to calculate the nth difference of a vector
nth_difference <- function(vec, n) {
  diff_vec <- vec
  for (i in 1:n) {
    diff_vec <- diff(diff_vec)
  }
  return(diff_vec)
}

add_third_diffs <- function(data, column_name, third_diffs_df) {
  third_diffs <- c(NA, NA, NA, nth_difference(data[[column_name]], 3))
  print(third_diffs)
  third_diffs_df[[column_name]] <- third_diffs
}

third_diffs_df <- data.frame(x=final_df$x)

third_diffs_df <- data.frame(x=final_df$x[4:nrow(final_df)])

third_diffs_df$qx_gompertz_3rd_diff <- c(nth_difference(final_df$qx_gompertz, 3))
third_diffs_df$qx_makeham_3rd_diff <- c(nth_difference(final_df$qx_makeham, 3))
third_diffs_df$qx_rs_k1_3rd_diff <- c(nth_difference(final_df$qx_rs_k1, 3))
third_diffs_df$qx_rs_k2_3rd_diff <- c(nth_difference(final_df$qx_rs_k2, 3))
third_diffs_df$qx_rs_k3_3rd_diff <- c(nth_difference(final_df$qx_rs_k3, 3))
third_diffs_df$qx_rs_k4_3rd_diff <- c(nth_difference(final_df$qx_rs_k4, 3))
third_diffs_df$qx_rs_k5_3rd_diff <- c(nth_difference(final_df$qx_rs_k5, 3))
third_diffs_df$qx_ss_65_3rd_diff <- c(nth_difference(final_df$qx_ss_65, 3))
third_diffs_df$qx_ss_61_3rd_diff <- c(nth_difference(final_df$qx_ss_61, 3))
third_diffs_df$qx_ss_57_3rd_diff <- c(nth_difference(final_df$qx_ss_57, 3))

print(third_diffs_df)

#write.csv(third_diffs_df, "C:/Users/dhwan/OneDrive/Documents/MyFiles/ACTL3141_Assignment/third_diffs.csv", row.names = TRUE)

