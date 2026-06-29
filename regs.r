################################################ Session Econometrics ###################################################

# deleting the memory
rm(list = ls())

# selecting our path ---> not needed if working from a cloned Git
setwd("YOURPATH")

# English Format and Avoiding scientific notation
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "en_US.utf8")
}

options(scipen = 999)

# Packages
library(dplyr)                  # data manipulation (filter, select, mutate, summarise)
library(mlogit)                 # multinomial logit models (discrete choice analysis)
library(tidyverse)              # collection of packages for data science (ggplot2, dplyr, tidyr, etc.)
library(stargazer)              # create regression tables (LaTeX, HTML, text)
library(rms)                    # regression modeling strategies (advanced modeling, validation)
library(readxl)                 # read Excel files (.xls, .xlsx)
library(magrittr)               # pipe operator %>% and functional helpers
library(broom)                  # tidy model outputs into data frames (tidy, glance, augment)
library(texreg)                 # format regression results for LaTeX/HTML
library(lmtest)                 # statistical tests for models (e.g. coeftest, hypothesis tests)
library(sandwich)               # robust / heteroskedasticity-consistent covariance matrices
library(Ecdat)                  # econometrics datasets (incl. Mroz dataset)
library(estimatr)               # fast estimation with built-in robust/clustered SEs
library(modelsummary)           # flexible regression tables and model summaries
library(giscoR)                 # geographic data from Eurostat GISCO (maps, shapefiles)
library(sf)                     # spatial data handling (simple features for GIS)
library(eurostat)               # Data from the Eurostat programm
library(vars)                   # Time Series Analysis VARS
library(lmtest)                 # Linear Models Testing


#### 6. Econometric Modelling ####

## OLS ##
# importing the data
wages = read.csv("wage1.csv", header = TRUE, sep = ",")
# wages = read.csv("FOLDERNAME/wage1.csv, header = T, sep = ","")  if working from a cloned Git

# regressing
ols_reg = lm(wage ~ educ + exper + nonwhite + female + married, data = wages)

summary(ols_reg)
modelsummary::modelsummary(ols_reg)
tidy(ols_reg, conf.int = TRUE) 
#%>% mutate(p.value = formatC(p.value, format = "f", digits = 5))


# Getting Stata's robust SE
ols_reg1 = coeftest(ols_reg, vcov = vcovHC(ols_reg, type = "HC1"))
tidy(ols_reg1, conf.int = TRUE)

# Heteroskedasticity-Consistent HCs
#, type = "HC0")                         # "robust" se w/o adjustment for heteroscedasticity -> DO NOT USE!
#, type = "HC1")                         # Stata's default ", robust", based on OLS residuals, also "stata"
#, type = "HC2")                         # HC1 w. small sample correction (DEFAULT)
#, type = "HC3")                         # based on squared residuals
#, type = "HC4")                         # HC3 w. small sample correction
#, type = "classical")                   # non-robust se -> same as lm(...)
# AND MANY MORE!!!!

## BEST ALTERNATIVE ## more on: https://www.youtube.com/watch?v=9TDCuN1Mzzo&t=212&ab_channel=Econometrics%2CCausality%2CandCodingwithDr.HK
# Packages: "Ecdat" and "estimatr"
model = lm_robust(wage ~ educ + exper + nonwhite + female + married, data = wages, se_type = "HC1")
summary(model)
tidy(model)
modelsummary(model)

# Interactions
model1 = lm_robust(wage ~ educ + exper * female,
                  data = wages, se_type = "HC1")
tidy(model1)
car::linearHypothesis(model1,
  c("female = 0", "exper:female = 0"),
  vcov. = vcov(model1))
lmtest::waldtest(model1, c("exper", "exper:female"))
waldtest(model1, . ~ . - exper - exper:female, test = "F")

## Logit ##
# importing the data -> female labor supply decisions MROZ
MROZ = read.csv("MROZ.csv")

# the reg
Logit = glm(
  formula = inlf ~ nwifeinc + educ + exper + age + kidslt6,
  family = binomial(link = "logit"),
  data = MROZ)

tidy(Logit,
  conf.int = TRUE,
  vcov = vcovHC(Logit, type = "HC1"))

modelsummary(Logit,
  vcov = vcovHC(Logit,
  type = "HC1"),
  output = "data.frame")


## Ordered (ordinal) logit ##
# importing the data
ordered = read_csv("ordered_health.csv")

# checking the unique values for health status
unique(ordered$healthstatus)

# Creating a numerical variable for health status
ordered %<>%
  mutate(
    health1 = as.integer(recode(healthstatus, 
    "fair" = "1", 
    "good" = "2", 
    "excellent" = "3")))

# estimating the ordered logit model -> rms package needed
ologit = lrm(health1 ~ age + logincome + numberdiseases, data = ordered)

# Extract SEs
se = sqrt(diag(vcov(ologit)))

# Exponentiate coefficients
coef_exp = exp(coef(ologit))

# viewing the output and showing the odds ratios
# the coefficients for 'y>=2' and 'y>=3' (intercepts) can be ignored
stargazer(
  ologit,
  coef = list(coef_exp),
  se   = list(se),
  star.cutoffs = c(0.1, 0.05, 0.01),
  type = "html",                                # "text" for console, "latex" for code, "html" as svg
  out = "ologit_table.html"
)

# Interpretation
# We compare the odds ratios to 1. A person is less likely to report good or excellent health as age and the nr.
# of diseases increase. A person is more likely to report good or excellent health when their income increases.

# ols with multi-cat variable -> NOT Recommended


## Basic Time Series Analysis ##
# Vectorial Autoregression VAR

# Simulating data
set.seed(42071)

n = 120

y1 = numeric(n)
y2 = numeric(n)

# starting values
y1[1] = 0
y2[1] = 0

# Subsequent values
for (t in 2:n) {
  e1 = rnorm(1, mean = 0, sd = 1)
  e2 = rnorm(1, mean = 0, sd = 1)
  
  y1[t] = 0.5 * y1[t-1] + 0.3 * y2[t-1] + e1
  y2[t] = 0.2 * y1[t-1] + 0.4 * y2[t-1] + e2
}

data_var = data.frame(
  time = 1:n,
  y1 = y1,
  y2 = y2
)

head(data_var)

# ploting the series
ggplot(data_var, aes(x = time)) +
  geom_line(aes(y = y1, color = "y1")) +
  geom_line(aes(y = y2, color = "y2")) +
  scale_color_manual(values = c("y1" = "blue", "y2" = "red")) +
  labs(title = "Two Time Series",
      x = "Time",
      y = "Value",
      color = "") +
  theme_minimal()

# Data prep for the VAR
var_data = data_var[, c("y1", "y2")]
head(var_data)

# Stationarity test -> HERE Innecessary, why?
adf_y1 = ur.df(var_data$y1, type = "drift", selectlags = "AIC")
summary(adf_y1)

adf_y2 = ur.df(var_data$y2, type = "drift", selectlags = "AIC")
summary(adf_y2)
# If both look stationary, continue with VAR in levels

# Choosing a lenght
VARselect(var_data, lag.max = 5, type = "const")

# Estimating
model_var = VAR(var_data, p = 1, type = "const")
summary(model_var)

# Forecasting
forecast_var = predict(model_var, n.ahead = 8)
forecast_var

# FC plot
plot(forecast_var)

# Impulse Response Function
set.seed(42071)

irf_y1_to_y2 = irf(model_var,
                    impulse = "y1",
                    response = "y2",
                    n.ahead = 10,
                    boot = TRUE)

plot(irf_y1_to_y2)

# Checking stability
roots(model_var)
stability(model_var)
plot(stability(model_var))
