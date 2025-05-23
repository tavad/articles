library(tidyverse)
library(readxl)

household_income_db <- read_csv("~/R/projects/t/data_37_2023.csv")

model_1 <- lm(totincome ~ year, data = household_income_db, weights = weight)
summary(model_1)

model_2 <- lm(totincome ~ as.factor(marz), data = household_income_db, weights = weight)
summary(model_2)

model_3 <- lm(totincome ~ settlement, data = household_income_db, weights = weight)
summary(model_3)

model_4 <- lm(totincome ~ year * as.factor(marz) * settlement, data = household_income_db, weights = weight)
summary(model_4)


inflation_row <- read_excel("~/R/newsletter/2025/2025_01_14_household_income/cpi_armenia_eng.xls", skip = 3)

inf_cumprod <- 
  inflation_row |> 
  rename(date = 1, cpi_m = 2) |> 
  select(date, cpi_m) |> 
  mutate(
    date = ym(date),
    cpi_m = cpi_m / 100,
    year = year(date)
  ) |> 
  filter(year <= 2023) |>
  select(-year) |> 
  arrange(desc(date)) |> 
  mutate(
    adj_price = cumprod(cpi_m),
    adj_price = lag(adj_price),
    adj_price = ifelse(is.na(adj_price), 1, adj_price)
  )

inf_cumprod

household_income_db_adj <- household_income_db |> 
  left_join(inf_cumprod, by = "date") |> 
  mutate(totincome_adj = totincome * adj_price) |> 
  mutate(month = month(date))

model_1 <- lm(totincome_adj ~ year, data = household_income_db_adj, weights = weight)
summary(model_1)

model_2 <- lm(totincome_adj ~ as.factor(marz), data = household_income_db_adj, weights = weight)
summary(model_2)

model_3 <- lm(totincome_adj ~ settlement, data = household_income_db_adj, weights = weight)
summary(model_3)

model_4 <- lm(totincome_adj ~ year * as.factor(marz) * settlement, data = household_income_db_adj, weights = weight)
summary(model_4)



#############################


household_income_db_adj <- household_income_db |> 
  left_join(inf_cumprod, by = "date") |> 
  mutate(
    totincome_adj = totincome * adj_price,
    month = month(date)
  ) |> 
  filter(totincome_adj != 0)

model_1 <- lm(log(totincome_adj) ~ year, data = household_income_db_adj, weights = weight)
summary(model_1)

model_2 <- lm(log(totincome_adj) ~ as.factor(marz), data = household_income_db_adj, weights = weight)
summary(model_2)

model_3 <- lm(log(totincome_adj) ~ settlement, data = household_income_db_adj, weights = weight)
summary(model_3)

model_4 <- lm(log(totincome_adj) ~ year * as.factor(marz) * settlement, data = household_income_db_adj, weights = weight)
summary(model_4)

model_5 <- lm(log(totincome_adj) ~ year * as.factor(marz) * settlement * as.factor(month), data = household_income_db_adj, weights = weight)
summary(model_5)

#############################


# Model 12: Combined Model (Adjusted, log-transformed)
model_12 <- lm(log(totincome_adj) ~ year * as.factor(marz) * settlement, data = household_income_db_adj, weights = weight)
summary(model_12)



###########################################


# 1. Detailed Residual Diagnostics
# Quantile analysis of residuals
summary(residuals(model_12))
# Distribution characteristics
library(moments)
skewness(residuals(model_12))
kurtosis(residuals(model_12))

# Check for spatial patterns in residuals
residuals_by_marz <- 
  data.frame(
    marz = household_income_db_adj$marz,
    residuals = residuals(model_12)
  ) |>
  group_by(marz) |>
  summarise(
    mean_resid = mean(residuals),
    sd_resid = sd(residuals),
    q1_resid = quantile(residuals, 0.25),
    q3_resid = quantile(residuals, 0.75)
  )

# 2. Region-Specific Effects
# Calculate average marginal effects for each marz
library(margins)
marz_effects <- margins(model_12, variables = "marz")
summary(marz_effects)

# Time trends by region
library(tidyverse)
region_trends <- 
  household_income_db_adj |>
  group_by(year, marz) |>
  summarise(
    mean_income = mean(log(totincome_adj)),
    sd_income = sd(log(totincome_adj))
  )

# 3. Inflation Adjustment Review
# Compare nominal vs adjusted values
inflation_comparison <- 
  household_income_db_adj |>
  group_by(year) |>
  summarise(
    mean_nominal = mean(totincome),
    mean_adjusted = mean(totincome_adj),
    adjustment_factor = mean_adjusted/mean_nominal,
    pct_difference = (mean_adjusted/mean_nominal - 1) * 100
  )

# Plot inflation adjustment impact over time
ggplot(inflation_comparison, aes(x = year)) +
  geom_line(aes(y = mean_nominal), color = "red") +
  geom_line(aes(y = mean_adjusted), color = "blue") +
  labs(title = "Nominal vs Adjusted Income Over Time")

# Additional heteroscedasticity tests
library(lmtest)
glejser_test <- lm(abs(residuals(model_12)) ~ fitted(model_12))
summary(glejser_test)