
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



######
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


