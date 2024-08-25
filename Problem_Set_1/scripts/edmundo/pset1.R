# Problem Set 1
# Coder: Edmundo Arias De Abreu
# Date: 2021-09-24


# clear environment and load libraries
rm(list = ls())
# nolint start

# install and load required packages
packages <- c(
    "tidyverse", "ggplot2", "stargazer",
    "rvest", "dplyr", "httr", "boot", "broom",
    "lmtest", "fixest", "sandwich", "lmtest"
)
invisible(lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
}))


# Part I: Data Scraping ------------------------------------------------------#

# %% Chunk 1: function
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

# %% Chunk 2: apply function
base_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/page"
df_list <- lapply(1:10, function(i) {
    url <- paste0(base_url, i, ".html")
    df_page <- scrape_page(url)
    print(paste("Successfully scraped page", i))
    return(df_page)
})

# append dataframes
df <- do.call(rbind, df_list)

# Part II: Age-Wage Profile Analysis -----------------------------------------#

# %% Chunk 3: prep data
df <- df[df$age > 18 & df$ocu == 1, ]
names(df) <- make.names(names(df), unique = TRUE) # set df names

# drop missing wages, log transform
df <- df %>%
    filter(!is.na(y_ingLab_m), !is.na(age)) %>%
    mutate(log_wage = log(y_ingLab_m + 1))


# %% Chunk 4: fit model
model <- lm(log_wage ~ age + I(age^2), data = df)
print(summary(model))

# %% Chunk 5: bootstrap standard errors

# define a function to apply bootstrap to regression estimates
boot_fn <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(log_wage ~ age + I(age^2), data = d)
    coef <- coef(fit)
    peak_age <- -coef[2] / (2 * coef[3])
    return(c(coef, peak_age = peak_age))
}

# define a function that calculates peak age (see model)
calc_peak_age <- function(data, indices) {
    d <- data[indices, ]
    model <- lm(log_wage ~ age + I(age^2), data = d)
    coef <- coef(model)
    peak_age <- -coef[2] / (2 * coef[3])
    return(peak_age)
}

# bootstrap
set.seed(555)
boot_results <- boot(data = df, statistic = boot_fn, R = 1000)

# 4. b_se
boot_se <- apply(boot_results$t, 2, sd)

# %% Chunk 6: table & results

stargazer(model,
    se = list(boot_se), type = "text",
    title = "Age-Wage Profile Regression Results",
    column.labels = c("Coefficients", "Bootstrap SE")
)

# peak age & CI's
original_peak_age <- -coef(model)[2] / (2 * coef(model)[3])
ci_peak <- quantile(boot_results$t[, 4], c(0.025, 0.975))

cat("\nPeak Age Analysis:\n")
cat("Estimated Peak Age:", round(original_peak_age, 2), "\n")
cat(
    "95% CI for Peak Age:", round(ci_peak[1], 2),
    "to", round(ci_peak[2], 2), "\n\n"
)
# %% Chunk 7: plot (won't appear & idk why)

new_data <- data.frame(age = seq(min(df$age), max(df$age), length.out = 100))
new_data$predicted_log_wage <- predict(model, newdata = new_data)

ggplot(df, aes(x = age, y = log_wage)) +
    geom_point(alpha = 0.1) +
    geom_line(data = new_data, aes(y = predicted_log_wage), color = "red") +
    geom_vline(
        xintercept = original_peak_age, color = "blue",
        linetype = "dashed"
    ) +
    geom_vline(xintercept = ci_peak[1], color = "green", linetype = "dashed") +
    geom_vline(xintercept = ci_peak[2], color = "green", linetype = "dashed") +
    labs(
        title = "Age-Wage Profile", x = "Age", y = "Log Wage",
        caption = "Blue dashed line: Estimated peak age
                   \nGreen dashed lines: 95% CI"
    ) +
    theme_minimal()

# Part III: Gender Earnings GAP  ----------------------------------------------#

# %% Chunk 8: unconditional earnings gap
model_gap <- lm(log_wage ~ sex, data = df)
robust_se <- vcovHC(model_gap, type = "HC1") # heteroskedasticity robust SE
robust_summary <- coeftest(model_gap, vcov = robust_se)
print(robust_summary)

# %% Chunk 9: conditional earnings gap

# define control variables
controls <- c("age", "oficio", "p6210", "relab", "hoursWorkUsual")
## --> "p6220" refers to education attainment

# i) FWL: Standard SE's
formula_controls <- as.formula(paste(
    "log_wage ~",
    paste(controls, collapse = " + ")
))
formula_controls_sex <- as.formula(paste(
    "sex ~",
    paste(controls, collapse = " + ")
))

# partial out the controls
residuals_y <- feols(formula_controls, data = df)$residuals
residuals_sex <- feols(formula_controls_sex, data = df)$residuals

# regress residuals
fwl_model <- lm(residuals_y ~ residuals_sex)

# stats
fwl_coef <- coef(fwl_model)[2]
fwl_se <- sqrt(vcov(fwl_model)[2, 2])




# %% Chunk 10: CEG-- FWL + Bootstrap

# define a function to apply bootstrap to regression estimates
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

# comparison
results_df <- data.frame(
    Term = c("Coefficient", "Standard Error (Normal)", "Standard Error (Bootstrap)"),
    Estimate = c(fwl_coef, fwl_se, boot_se)
)

stargazer(
    results_df,
    type = "text",
    summary = FALSE,
    rownames = FALSE,
    title = "Frisch-Waugh-Lovell Theorem Results",
    covariate.labels = results_df$Term,
    coef = list(results_df$Estimate),
    digits = 5,
    notes = "Bootstrap standard errors are based on 1000 replications",
    out = "results_table.txt"
)



# %% Chunk 11: Plot-- by sex

# 11.1 estimate model (unconditional -- ask)
model <- lm(log_wage ~ sex * (age + I(age^2)), data = df)

# 11.2 bootstrap function to apply to regression estimates (same as b4)
boot_fn <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(log_wage ~ sex * (age + I(age^2)), data = d)
    coef <- coef(fit)

    peak_age_male <- -coef["age"] / (2 * coef["I(age^2)"])
    peak_age_female <- -(coef["age"] + coef["sex:age"]) / (2 * (coef["I(age^2)"] + coef["sex:I(age^2)"]))

    return(c(peak_age_male, peak_age_female))
}

set.seed(555)
boot_results <- boot(data = df, statistic = boot_fn, R = 1000)

# 11.3 bootstrap standard errors
coef_boot_fn <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(log_wage ~ sex * (age + I(age^2)), data = d)
    return(coef(fit))
}

# bootstrap coefficients and standard errors
coef_boot_results <- boot(data = df, statistic = coef_boot_fn, R = 1000)
boot_se <- apply(coef_boot_results$t, 2, sd)

# 11.4 reg table
stargazer(model,
    se = list(boot_se), type = "text",
    title = "Age-Wage Profile Regression Results",
    column.labels = c("Coefficients", "Bootstrap SE")
)

# 11.5 calculate peak ages & respective CIs
original_peak_age_male <- -coef(model)["age"] / (2 * coef(model)["I(age^2)"])
original_peak_age_female <- -(coef(model)["age"] + coef(model)["sex:age"]) /
    (2 * (coef(model)["I(age^2)"] + coef(model)["sex:I(age^2)"]))

# Calculate the 95% confidence intervals for the peak ages
ci_peak_male <- quantile(boot_results$t[, 1], c(0.025, 0.975))
ci_peak_female <- quantile(boot_results$t[, 2], c(0.025, 0.975))

# Print the results
cat("\nPeak Age Analysis:\n")
cat("Estimated Peak Age (Male):", round(original_peak_age_male, 2), "\n")
cat("95% CI for Peak Age (Male):", round(ci_peak_male[1], 2), "to", round(ci_peak_male[2], 2), "\n")
cat("Estimated Peak Age (Female):", round(original_peak_age_female, 2), "\n")
cat("95% CI for Peak Age (Female):", round(ci_peak_female[1], 2), "to", round(ci_peak_female[2], 2), "\n\n")

# 11.6 plot
# gen data for prediction
new_data <- expand.grid(
    age = seq(min(df$age), max(df$age), length.out = 100),
    sex = factor(c(0, 1), levels = c(0, 1)) # Keep sex as a factor
)

# predict log_wage for new data
new_data$sex <- as.numeric(as.character(new_data$sex))
new_data$predicted_log_wage <- predict(model, newdata = new_data)

# change labels for clarity
male_peak_label <- paste("Male Peak:\n", round(original_peak_age_male, 1))
female_peak_label <- paste("Female Peak:\n", round(original_peak_age_female, 1))

# plot
ggplot(df, aes(x = age, y = log_wage, color = factor(sex))) +
    geom_point(alpha = 0.1) +
    geom_line(data = new_data, aes(y = predicted_log_wage, color = factor(sex))) +
    geom_vline(xintercept = original_peak_age_male, color = "blue", linetype = "dashed") +
    geom_vline(xintercept = original_peak_age_female, color = "red", linetype = "dashed") +
    geom_vline(xintercept = ci_peak_male[1], color = "lightblue", linetype = "dotted") +
    geom_vline(xintercept = ci_peak_male[2], color = "lightblue", linetype = "dotted") +
    geom_vline(xintercept = ci_peak_female[1], color = "pink", linetype = "dotted") +
    geom_vline(xintercept = ci_peak_female[2], color = "pink", linetype = "dotted") +
    # Labels for peak ages
    annotate("text",
        x = original_peak_age_male, y = max(df$log_wage),
        label = male_peak_label, color = "blue", vjust = -1, hjust = 1
    ) +
    annotate("text",
        x = original_peak_age_female, y = max(df$log_wage),
        label = female_peak_label, color = "red", vjust = -1, hjust = -0.1
    ) +
    labs(
        title = "Age-Wage Profile by Sex",
        x = "Age", y = "Log Wage",
        color = "Sex",
        caption = "Dashed lines: Estimated peak ages\nDotted lines: 95% CI"
    ) +
    scale_color_manual(
        values = c("0" = "red", "1" = "blue"),
        labels = c("0" = "Female", "1" = "Male")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
















# nolint end
# END ------------------------------------------------------------------------#
