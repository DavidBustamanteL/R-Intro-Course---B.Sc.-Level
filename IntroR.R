###################################### R for Absolute Beginners #####################################################
# FB 03 - Institute for Social Choice
# David Bustamante -> davidbustamantelazo@uni-mainz.de


# Let us begin as usual by errasing the memory and setting our wd
rm(list = ls())
setwd("YOURPATH")

# English Format and Avoiding scientific notation
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "en_US.utf8")
}

options(scipen = 999)


# Package Installation
# List of packages
#package_list = c("tidyverse", "dplyr", "stringr", "ggplot2", "ggthemes", "ggrepel", "ggpubr", "ggpp",
#"readxl", "readr", "eurostat", "giscoR", "viridis", "sf", "RColorBrewer", "reshape2", "jsonlite",
#"rjson", "rstatix", "lubridate", "gtsummary", "haven", "stargazer", "forcats", "magrittr", "gapminder")

# Installation
#for(i in package_list){
#  if(!require(i, character.only = T)){
#    install.packages(i, dependencies = T)
#    require(i, character.only = T)
#  }
#}

# Where to see the packages? -> extention to Positron

### Packages ###
# Data transformation, manipulation
library(tidyverse)                      # META-Package (ggplot, readr, tidyr, purrr, etc.)
library(dplyr)                          # mutate/filter/filter/join
library(stringr)                        # string operation
library(forcats)                        # factor handling
library(magrittr)                       # pipes
library(lubridate)                      # dates/times
library(reshape2)                       # wide/long formats

# Data import
library(readxl)                         # data from excel xlsx or xls
library(readr)                          # quick read/write csv files
library(haven)                          # stata, spss, sas files
library(jsonlite)                       # json data
library(rjson)                          # json data
library(eurostat)                       # Download of eurostat datasets
library(gapminder)                      # Socioecon. data

# Data Visualisation
library(ggplot2)                        # plotting
library(ggthemes)                       # extra themes
library(ggrepel)                        # non-overlapping text labels
library(ggpubr)                         # publication oriented annotations
library(ggpp)                           # annotation tooles
library(ggtext)                         # customizing plots
library(GGally)                         # facetwrap/grid plots
library(viridis)                        # uniform color scales
library(RColorBrewer)                   # sequential color palettes

# Spatial Data
library(giscoR)                         # Source for spatial EU data, to be used with sf
library(sf)                             # geoprocessing, modern spatial data classes
library(maps)                           # R maps package
library(mapproj)                        # working with latitude and longitude coordinates 
library(rnaturalearth)                  # adding geo data like rivers

# Statistics, tables & reporting
library(modelsummary)                   # best summary, etc. tables report
library(rstatix)                        # common stat test
library(gtsummary)                      # report-ready summaries
library(stargazer)                      # reg and summary tables for papers


#### 1. Importing data ####

## CSV ##
wages = read.csv("wage1.csv", header = TRUE, sep = ",")

## Excel ##
covid_gender_straints = read_excel("aufgabe1.1.xlsx", sheet = "Abb2.", range = "B4:D9")

## from packages ##
# eurostat
unemp_data = get_eurostat("une_rt_a")

# giscoR
nuts2_shapes = gisco_get_nuts(nuts_level = 2, resolution = "01")


#### 2. The very basics ####

## Some statistical analysis ##
colnames(wages)

# Finding specific variables
grep("inco", names(wages), value = T)

# Summary Stats
summarise(wages, mean(wage), mean(educ), median(wage), median(educ), var(wage), var(educ))
tidy(wages)


## intro to ggplot2 ##
ggplot(wages, aes(x = exper, y = wage, color = nonwhite)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Experience vs. Skin Color by Gender",
    x = "Experience (years)",
    y = "Hourly wage") +
  theme_minimal()

ggplot(wages, aes(x = factor(numdep), y = wage, fill = factor(female))) +
  geom_boxplot(alpha = 0.6, position = "dodge") +
  labs(
    title = "Wage by Number of Dependents and Gender",
    x = "Number of dependents",
    y = "Hourly wage") +
  theme_minimal() +
  theme(legend.position = "none")


## First steps when coding with R ## 

# type of data #
x = 1
typeof(x)
x = as.character(x)

x_a = as.integer(x)
x_b = as.double(x)

1 + x_a

y = "Hello World"
print(y)
typeof(y)

# Vectors, Lists, and Factors #
# Atomic Vectors -> one-type-of-element vectors
numbers_vector = c(1, 2, 3, 1000)

# Lists -> Collection of elements/objects with diff. structures
subjects_list = list(
  name = c("Nikol", "Mia", "Anna"),
  age = c(22, 31, 26),
  scores = list(
    c(90, 85, 88),
    c(89, 90, 90),
    c(100, 100, 100)))

# how to see an specific element in a list?
subjects_list[[3]]
subjects_list[[3]][1]   # the 1st value from all the components of the 3rd element

# Factors -> A categorical variable
humidity_factor = factor(c("low", "high", "medium", "low"))
female = factor(seq(1:2))
female1 = factor(c("1", "2"))
female2 = factor(1:2)


# Functions and Iterations #

# functions 
4 * 5

base1 = 4
height1 = 5
base1 * height1

# The idea is to optimize a process that will be repeated a bunch of time
area = function(x, y) {
  x * y
}

area(4, 5)
area(5, 8)

# function with an input request
area1 = function() {
  x = as.numeric(readline(prompt = "Insert x: "))
  y = as.numeric(readline(prompt = "Insert y: "))
  x * y
}

area1()


# If statements
sign = function(x){
  if (x >= 0){
    print("Positive")}
  else {
    print("Negative")}
}

sign(0)

# Other return values
test = function(x, y){
  if(length(x) != length(y)){
    return("ERROR: Both vectors HAVE to have the SAME amount of elements")}
  else {
    x * y}
}

a = c(1:3)
b = seq(1:6)
c = 4:6

test(a, b)
test(a, c)


# R loops
# Simulating a table with data
# TIBBLE -> build a wall by stacking columns of bricks
# SAME as with data.frame -> Tibbles are DFs
dp = tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5))

# This would be fine, BUT we do not want to copy and paste
stats = vector("double", nrow(dp))
for (i in 1:nrow(dp)){
  stats[[i]] = mean(as.numeric(dp[i,]))
}

stats

# so
opera_col = function(x, func){
  res = vector("double", ncol(x))
  for (j in 1:ncol(x)){
    res[[j]] = func(as.numeric(x[[j]]))
  }
  names(res) = colnames(x)
  return(res)
}

#opera_row = function(x, func){
#  res = vector("double", nrow(x))
#  for (i in 1:nrow(x)){
#    res[[i]] = func(as.numeric(x[i,]))
#  }
#  return(res)
#}

opera_col(dp, mean)
opera_col(dp, median)
opera_col(dp, var)
opera_col(dp, sd)

# Hardcore R coders do not really use iteration too much, how so?
# invoke function with purrr
operations = list(mean, median, var, sd)

purrr::map(operations, exec, x = as.numeric(dp[[1]]))
map_dbl(operations, .f = exec, x = as.numeric(dp[[1]]))

#### 3. Data Simulation ####

# Choosing a seed for later replication
set.seed(55131)

# The variables and tibble
x1 = rnorm(500, mean = 10, sd = 10)
x2 = rnorm(500, mean = 20, sd = 20)
x3 = rnorm(500, mean = 44, sd = 11)
x4 = rnorm(500, mean = 0, sd = 1)
x5 = factor(rbinom(500, size = 1, prob = 0.66))

stata_test = tibble(var1 = x1, var2 = x2, var3 = x3, var4 = x4, var5 = x5)
stata_test2 = data.frame(var1 = x1, var2 = x2, var3 = x3, var4 = x4, var5 = x5)

# Saving the tibble as a dta-file
write_dta(stata_test, "stata_test.dta")

# Importing do-files (stata files)
stata_test = read_dta("stata_test.dta")


#### 4. Data Manipulation/Transformation ####

## merging data ##
# TRIBBLE -> lay out the wall by writing each row one by one
x = tribble(
  ~id, ~sex, ~age,
  1, "M", "25",
  2, "F", "40",
  3, "M", "18")

y = tribble(
  ~id, ~income, ~expenses,
  1, "1750", "1100",
  2, "2540", "1760",
  4, "3000", "890")

# Joints #
# inner join: new df only has the obs included in BOTH old dfs
inner = inner_join(x, y, by = "id")

# outter join left: the obs from the column on the left is preserved, here x
left = left_join(x, y, by = "id")

# outter join right: the obs from the column on the right is preserved, here y
right = right_join(x, y, by = "id")

# full join: to have the entire obs from all to-be-merged dfs
full = full_join(x, y, by = "id")

# stacking # -> e.g., good for panel data
z = tribble(
  ~id, ~sex, ~age,
  14, "F", "95",
  25, "F", "34",
  36, "M", "48")

full_long = rbind(x, z)


## Mexican Survey 2010 ##
# importing the data
data = read_excel("mu_enoe.xlsx")

# Getting to know dplyr #
# renaming columns
data_new = data %>%
  rename(state = estado,
    age = edad,
    assistance = asiste,
    job_title = pos_ocu,
    income_minwage = ing_salarios,
    edu_level = niv_edu,
    years_school = anios_esc,
    hours_work = hrsocup,
    monthly_inc = ingreso_mensual,
    nr_jobs = num_trabajos,
    job_type = tipo_empleo)

# mutate and diff. R pipes
# dplyr simple pipe %>%
data_new %>%
  rename(female = sex) %>%
  mutate(
    female = as.character(female),
    female = fct_recode(female, "0" = "Hombre", "1" = "Mujer"))

# magrittr entlarged pipe %<>%
data_new %<>%
  rename(female = sex) %>%
  mutate(
    female = as.character(female),
    female = fct_recode(female, "0" = "Hombre", "1" = "Mujer"))

# select
data_short = data_new %>%
  select(state, age, female, edu_level, years_school, hours_work, monthly_inc, nr_jobs, job_type)

# filter
data_female = data_new %>%
  filter(female == 1)

# Subsetting
data_male = subset(data_new, female == 0)


#### 5. Plotting ####

## ggplot2 ##
# Importing from Gapminder
gm_2007 = gapminder %>%
  filter(year == 2007)

# First Steps
gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point()

ggsave("gapminder_2007.png", width = 7, height = 4)

# Let's make it a bit more professional #

# turning off the legends
gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent)) +
  geom_point(shape = 21, color = "white", show.legend = F) +
  scale_fill_manual(
    breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    values = c("#006400ff", "#e40101ff", "#0000ffff", "#ff7300ff", "#800080ff"))

ggsave("gapminder_2007.png", width = 5, height = 3)

# Formating the axis
gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent)) +
  geom_point(shape = 21, color = "white", show.legend = F) +
  scale_fill_manual(
    breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    values = c("#006400ff", "#e40101ff", "#0000ffff", "#ff7300ff", "#800080ff")) +
  labs(
    x = "GDP per Capita [in USD]",
    y = "Life Expectancy [in Years]",
    title = "Global Development in 2007") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave("gapminder_2007.png", width = 7, height = 4)

# logs in the x-axis
gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent)) +
  geom_point(shape = 21, color = "white", show.legend = F) +
  scale_fill_manual(
    breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    values = c("#006400ff", "#e40101ff", "#0000ffff", "#ff7300ff", "#800080ff")) +
  scale_x_log10(
    breaks = c(1e3, 1e4, 1e5),
    limits = c(200, 1e5),
    expand = c(0, 0),
    labels = c("1K", "10K", "100K")) +
  labs(
    x = "GDP per Capita [in USD]",
    y = "Life Expectancy [in Years]",
    title = "Global Development in 2007") +
  theme(
    plot.title = element_text(hjust = 0.5))

ggsave("gapminder_2007.png", width = 7, height = 4)

# Fixing the x-axis getting clipped & getting rid off some gridlines and tick marks
# As well as a source/note
gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent)) +
  geom_point(shape = 21, color = "white", show.legend = F) +
  scale_fill_manual(
    breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    values = c("#006400ff", "#e40101ff", "#0000ffff", "#ff7300ff", "#800080ff")) +
  scale_x_log10(
    breaks = c(1e3, 1e4, 1e5),
    limits = c(200, 1e5),
    expand = c(0, 0),
    labels = c("1K", "10K", "100K")) +
  coord_cartesian(clip = "off") +
  labs(
    x = "GDP per Capita [in USD]",
    y = "Life Expectancy [in Years]",
    title = "Global Development in 2007",
    caption = "<b>Source:</b> Gapminder & Riffomonas Project<br><b>Note:</b> Something") +
  annotate(
    "text",
    x = 1e5, y = 35,        # position near bottom-right
    label = "log x axis",
    hjust = 1, vjust = 0,   # align right horizontally, bottom vertically
    size = 3.2, color = "gray"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(r = 12, l = 5, t = 5, b = 5),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 9, color = "black"),
    plot.caption.position = "plot")

ggsave("gapminder_2007.png", width = 7, height = 4)

# Adding and adjusting labels in the plot for China, India, and the USA
gm_2007_labels = gm_2007 %>%
  filter(country %in% c("United States", "India", "China")) %>%
  mutate(
    country = if_else(country == "United States", "USA", country),
    gdpPercap = c(4000, 2200, 62000),
    lifeExp = c(77, 69, 78))

gm_2007 %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, fill = continent)) +
  geom_point(shape = 21, color = "white", show.legend = FALSE) +
  geom_text(
    data = gm_2007_labels,
    mapping = aes(label = country),
    size = 12, size.unit = "pt"
  ) +
  scale_fill_manual(
    breaks = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
    values = c("#006400ff", "#e01010ff", "#0000ffff", "#ff7300ff", "#800080ff")) +
  scale_x_log10(
    breaks = c(1e3, 1e4, 1e5),
    limits = c(200, 1e5),
    expand = c(0, 0),
    labels = c("1k", "10k", "100k")
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "GDP per Capita [in USD]",
    y = "Life Expectancy [in Years]",
    title = "Global Development in 2007",
    caption = "<b>Source:</b> Gapminder & Riffomonas Project<br><b>Note:</b> Something") +
    annotate(
    "text",
    x = 1e5, y = 35,        # position near bottom-right
    label = "log x axis",
    hjust = 1, vjust = 0,   # align right horizontally, bottom vertically
    size = 3.2, color = "gray"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(r = 12, l = 5, t = 5, b = 5),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 9, color = "black"),
    plot.caption.position = "plot")

ggsave("gapminder_2007.png", width = 7, height = 4)


## maps ##
# Getting the data
states = map_data("state")
arrests = USArrests
help(USArrests)

# Adjusting case for matching
names(arrests) = tolower(names(arrests))
arrests$region = tolower(rownames(USArrests))

# Merging and sorting (plots in order, sort ensures (no) alphab. order by a variable)
arrests.geo = merge(states, arrests, sort = F, by = "region")
arrests.geo = arrests.geo[order(arrests.geo$order), ]                  # crucial to sort for ggplot when using coordinates

## The Economist ##
arrests.geo %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault)) +
  scale_fill_continuous(name = "Assault\nRates",
                        low = "#e4d7d7ff",
                        high = "#CC0033") +                                    # Economist-like light to dark red JGU color scale
  theme_economist_white(base_size = 15) +                                        # Economist white theme
  theme(
    panel.background = element_blank(),                                          # Remove gray background
    legend.background = element_blank(),                                         # Remove legend background
    legend.title = element_text(size = 9, face = "bold"),                        # Bold legend title
    legend.text = element_text(size = 8),                                        # Make legend numbers smaller
    panel.grid = element_blank(),                                                # Remove grid lines
    axis.ticks = element_blank(),                                                # Remove tick marks
    axis.line = element_blank(),                                                 # Remove axis lines
    axis.text = element_blank(),                                                 # Remove axis values
    axis.title = element_blank(),                                                # Remove axis labels
    plot.title = element_text(size = 13,
                              face = "bold",
                              hjust = 0),                                        # Left-align title
    plot.subtitle = element_text(size = 11, hjust = 0),                          # Left-align subtitle
    plot.caption = element_markdown(size = 9,
                                    color = "#333333",
                                    hjust = 1)                                   # Right-align caption
  ) +
  coord_map() +                                                                  # Adjust for curvature of the Earth
  labs(
    title = "Assault Rates Across Regions 1973",                                 # Title
    subtitle = "An overview of assault rates in different\ngeographical areas",  # Subtitle
    caption = "**Source**: Maps R-package & USArrest Data, own Elaboration."     # Bold Source using Markdown
  )

ggsave("theecon_plot.png")


## Publication-quality plots ##
## From bscfiskalfoederalismuswt2526:
gdp.search = search_eurostat("Gross domestic product")                           # Eurostat code: nama_10r_2gdp

gdp = get_eurostat(id = gdp.search$code[4], select_time = "Y") %>% 
  filter(., unit %in% c("PPS_HAB_EU27_2020")) %>%
  filter(., TIME_PERIOD == "2024-01-01")

# GiscoR package for NUTS level data
nuts = gisco_get_nuts(resolution = "01")
nuts0 = nuts %>% filter(LEVL_CODE == 0)
nuts1 = nuts %>% filter(., LEVL_CODE == 1)

# starting
gdp_de = gdp %>%
  filter(unit == "PPS_HAB_EU27_2020", geo == "DE")

# GISCO geometries Germany at NUTS 2
de_nuts2 = nuts %>% 
  filter(LEVL_CODE == 2, CNTR_CODE == "DE")

# all countries at NUTS 0
countries0 = nuts %>% 
  filter(LEVL_CODE == 0)

# Germany country polygon
de0 = countries0 %>%
  filter(CNTR_CODE == "DE")

# countries sharing a border with Germany
touch_mat = st_touches(countries0, de0, sparse = F)

neighbors0 = countries0 %>%
  filter(as.vector(touch_mat))

# remove Germany itself from neighbors
neighbors0 = neighbors0 %>%
  filter(CNTR_CODE != "DE")

# Merging GDP with German NUTS 2
plot_data = de_nuts2 %>%
  left_join(gdp, by = "geo")

# Rivers crossing Germany
rivers = ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# making sure CRS matches
rivers = st_transform(rivers, st_crs(de0))

# clipping rivers to Germany
rivers_de = st_intersection(rivers, st_union(de0))

# Plot
ggplot() +
  geom_sf(data = neighbors0, fill = "grey85", color = "white", linewidth = 0.2) +    # neighboring countries in gray
  geom_sf(data = plot_data, aes(fill = values), color = "white", linewidth = 0.3) +  # Germany NUTS 2 colored by GDP
  geom_sf(data = de0, fill = NA, color = "black", linewidth = 0.5) +                 # Germany outer border stronger
  geom_sf(data = rivers_de, color = "deepskyblue3", linewidth = 0.5, alpha = 0.8) +  # rivers in blue
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey95",
    name = "GDP per capita\n(PPS)") +
  coord_sf(
    xlim = st_bbox(de0)[c("xmin", "xmax")] + c(-2, 2),
    ylim = st_bbox(de0)[c("ymin", "ymax")] + c(-1, 1),
    expand = FALSE) +
  theme_void()

ggsave("DEGDPpc.png")