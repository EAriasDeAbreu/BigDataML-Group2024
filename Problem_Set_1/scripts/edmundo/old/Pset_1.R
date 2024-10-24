
rm(list = ls())

# install and load required packages
packages <- c("tidyverse", "ggplot2", "stargazer", "rvest", "dplyr", "httr", "boot", "broom")
invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))


# function to scrape data for a single page dynamically
scrape_page <- function(url) {
  response <- GET(url)
  if (status_code(response) != 200) {
    stop(paste("Failed to retrieve page:", url))
  }
  
  content <- content(response, "text")
  page <- read_html(content)
  include_element <- html_node(page, "[w3-include-html]")
  include_url <- html_attr(include_element, "w3-include-html")
  full_include_url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/", include_url)
  
  included_content <- read_html(full_include_url)
  df_page <- html_node(included_content, "table") %>% html_table()
  
  return(df_page)
}

base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/page"
df_list <- lapply(1:10, function(i) {
  url <- paste0(base_url, i, ".html")
  df_page <- scrape_page(url)
  print(paste("Successfully scraped page", i))
  return(df_page)
})

# append dataframes
df <- do.call(rbind, df_list)

print(paste("Dimensions of the combined dataframe:", paste(dim(df), collapse = " x ")))



###### Age-Wage Profile Analysis ######
# df_2 <- df # create copy

# subset --> >18 & occupied
df <- df[df$age > 18 & df$ocu == 1, ]
names(df) <- make.names(names(df), unique = TRUE) # set df names

# drop missing wages, log transform
df <- df %>%
  filter(!is.na(y_ingLab_m), !is.na(age)) %>%
  mutate(log_wage = log(y_ingLab_m + 1))

# 1. fit model-- OLS
model <- lm(log_wage ~ age + I(age^2), data = df)
print(summary(model))

# 2. Bootstrap
boot_fn <- function(data, indices) {
  d <- data[indices,]
  fit <- lm(log_wage ~ age + I(age^2), data = d)
  coef <- coef(fit)
  peak_age <- -coef[2] / (2 * coef[3])
  return(c(coef, peak_age = peak_age))
}
# peak age
calc_peak_age <- function(data, indices) {
  d <- data[indices,]
  model <- lm(log_wage ~ age + I(age^2), data = d)
  coef <- coef(model)
  peak_age <- -coef[2] / (2 * coef[3])
  return(peak_age)
}

# 3. do bootstrap
set.seed(555)
boot_results <- boot(data = df, statistic = boot_fn, R = 1000)

# 4. b_se
boot_se <- apply(boot_results$t, 2, sd)

# 5. reg table
stargazer(model, se = list(boot_se), type = "text", 
          title = "Age-Wage Profile Regression Results",
          column.labels = c("Coefficients", "Bootstrap SE"))

# 6. peak age & CI's
original_peak_age <- -coef(model)[2] / (2 * coef(model)[3])
ci_peak <- quantile(boot_results$t[,4], c(0.025, 0.975))

cat("\nPeak Age Analysis:\n")
cat("Estimated Peak Age:", round(original_peak_age, 2), "\n")
cat("95% CI for Peak Age:", round(ci_peak[1], 2), "to", round(ci_peak[2], 2), "\n\n")

# 7. plot
new_data <- data.frame(age = seq(min(df$age), max(df$age), length.out = 100))
new_data$predicted_log_wage <- predict(model, newdata = new_data)

ggplot(df, aes(x = age, y = log_wage)) +
  geom_point(alpha = 0.1) +
  geom_line(data = new_data, aes(y = predicted_log_wage), color = "red") +
  geom_vline(xintercept = original_peak_age, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = ci_peak[1], color = "green", linetype = "dashed") +
  geom_vline(xintercept = ci_peak[2], color = "green", linetype = "dashed") +
  labs(title = "Age-Wage Profile",
       x = "Age", y = "Log Wage",
       caption = "Blue dashed line: Estimated peak age\nGreen dotted lines: 95% CI") +
  theme_minimal()

# 8. model fit stats
cat("Model Fit Statistics:\n")
cat("R-squared:", round(summary(model)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(model)$adj.r.squared, 4), "\n")
cat("F-statistic:", round(summary(model)$fstatistic[1], 2), 
    "on", summary(model)$fstatistic[2], "and", summary(model)$fstatistic[3], "DF\n")


###### Gender Earnings GAP ######
library(sandwich)
library(lmtest)
library(fixest)


# 1. Estimate unconditional wage wap
model_gap <- lm(log_wage ~ sex, data = df)
robust_se <- vcovHC(model_gap, type = "HC1")
robust_summary <- coeftest(model_gap, vcov = robust_se)
print(robust_summary)


# R/ Men earn on average 15.1% more than women in terms of wages. The effect is substantial, 
# suggesting a notable gender wage gap.

# 2. Conditional wage gap (By FWL)

# Define control variables
controls <- c("age", "clase", "college", "formal", "oficio", "depto")

# i. FWL: Nornal/standard SE's

# Create formulas
formula_controls <- as.formula(paste("log_wage ~", paste(controls, collapse = " + ")))
formula_controls_sex <- as.formula(paste("sex ~", paste(controls, collapse = " + ")))

# Step 1: Partial out the controls
residuals_y <- feols(formula_controls, data = df)$residuals
residuals_sex <- feols(formula_controls_sex, data = df)$residuals

# Step 2: Regress residuals
fwl_model <- lm(residuals_y ~ residuals_sex)

# stats
fwl_coef <- coef(fwl_model)[2]
fwl_se <- sqrt(vcov(fwl_model)[2,2])

# ii. FWL: bootstrap standard errors
boot_fwl <- function(data, indices) {
  resid_y <- residuals_y[indices]
  resid_sex <- residuals_sex[indices]
  boot_model <- lm(resid_y ~ resid_sex)
  return(coef(boot_model)[2])
}

# bootstrap
set.seed(555)
boot_results <- boot(data = df, statistic = boot_fwl, R = 1000)

# bootstrap standard error
boot_se <- sd(boot_results$t)

# Compare the estimates and the standard errors
cat("Frisch-Waugh-Lovell Theorem Results:\n")
cat("Coefficient:", fwl_coef, "\n")
cat("Standard Error (Normal):", fwl_se, "\n")
cat("Standard Error (Bootstrap):", boot_se, "\n")

## Table (added in Rmarkdown doc)

# plot

model <- lm(log_wage ~ sex * (age + I(age^2)), data = df)
print(summary(model))

# 2. Bootstrap function
boot_fn <- function(data, indices) {
  d <- data[indices,]
  fit <- lm(log_wage ~ sex * (age + I(age^2)), data = d)
  coef <- coef(fit)
  peak_age_male <- -coef["age"] / (2 * coef["I(age^2)"])
  peak_age_female <- -(coef["age"] + coef["sex:age"]) / (2 * (coef["I(age^2)"] + coef["sex:I(age^2)"]))
  return(c(peak_age_male, peak_age_female))
}

# 3. Do bootstrap
set.seed(555)
boot_results <- boot(data = df, statistic = boot_fn, R = 1000)

# Print the structure of boot_results to understand its content
print(str(boot_results))

# 4. Bootstrap SE for model coefficients
coef_boot_fn <- function(data, indices) {
  d <- data[indices,]
  fit <- lm(log_wage ~ sex * (age + I(age^2)), data = d)
  return(coef(fit))
}
coef_boot_results <- boot(data = df, statistic = coef_boot_fn, R = 1000)
boot_se <- apply(coef_boot_results$t, 2, sd)

# 5. Regression table
stargazer(model, se = list(boot_se), type = "text", 
          title = "Age-Wage Profile Regression Results",
          column.labels = c("Coefficients", "Bootstrap SE"))

# 6. Peak age & CI's
original_peak_age_male <- -coef(model)["age"] / (2 * coef(model)["I(age^2)"])
original_peak_age_female <- -(coef(model)["age"] + coef(model)["sex:age"]) / 
  (2 * (coef(model)["I(age^2)"] + coef(model)["sex:I(age^2)"]))

ci_peak_male <- quantile(boot_results$t[, 1], c(0.025, 0.975))
ci_peak_female <- quantile(boot_results$t[, 2], c(0.025, 0.975))

cat("\nPeak Age Analysis:\n")
cat("Estimated Peak Age (Male):", round(original_peak_age_male, 2), "\n")
cat("95% CI for Peak Age (Male):", round(ci_peak_male[1], 2), "to", round(ci_peak_male[2], 2), "\n")
cat("Estimated Peak Age (Female):", round(original_peak_age_female, 2), "\n")
cat("95% CI for Peak Age (Female):", round(ci_peak_female[1], 2), "to", round(ci_peak_female[2], 2), "\n\n")

# 7. Plot
new_data <- expand.grid(
  age = seq(min(df$age), max(df$age), length.out = 100),
  sex = c(0, 1)  # Keep as numeric
)
new_data$predicted_log_wage <- predict(model, newdata = new_data)

# labels for peak ages
male_peak_label <- paste("Male Peak:\n", round(original_peak_age_male, 1))
female_peak_label <- paste("Female Peak:\n", round(original_peak_age_female, 1))

ggplot(df, aes(x = age, y = log_wage, color = factor(sex))) +
  geom_point(alpha = 0.1) +
  geom_line(data = new_data, aes(y = predicted_log_wage, color = factor(sex))) +
  geom_vline(xintercept = original_peak_age_male, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = original_peak_age_female, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ci_peak_male[1], color = "lightblue", linetype = "dotted") +
  geom_vline(xintercept = ci_peak_male[2], color = "lightblue", linetype = "dotted") +
  geom_vline(xintercept = ci_peak_female[1], color = "pink", linetype = "dotted") +
  geom_vline(xintercept = ci_peak_female[2], color = "pink", linetype = "dotted") +
  # labels for peak ages
  annotate("text", x = original_peak_age_male, y = max(df$log_wage), 
           label = male_peak_label, color = "blue", vjust = 15, hjust = 1) +
  annotate("text", x = original_peak_age_female, y = max(df$log_wage), 
           label = female_peak_label, color = "red", vjust = 15, hjust = -0.1) +
  labs(title = "Age-Wage Profile by Sex",
       x = "Age", y = "Log Wage",
       color = "Sex",  # Change legend title
       caption = "Dashed lines: Estimated peak ages\nDotted lines: 95% CI") +
  scale_color_manual(values = c("0" = "red", "1" = "blue"), 
                     labels = c("0" = "Female", "1" = "Male")) +  
  theme_minimal() +
  theme(legend.position = "bottom")



