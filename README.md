## So, why learn R?

- R is a free and powerful programming language designed for data analysis, statistics, and visualization.  
  It’s widely used in research, academia, and many professional fields — from economics and psychology to  
  biology and the social sciences.  

- Once you get familiar with it, you’ll see how much it can simplify your data work and help you later when  
  learning other languages, such as Python. Mastering tools like R also opens up many future job opportunities  
  in data-driven fields.  


---

## Installation

I would kindly ask you to download R and Positron on your computers.

### R
The latest version of R can be found at:  
https://cran.r-project.org/  
At this moment, it's called **“Because it was There”**.

### Positron
Please follow this link:  
https://positron.posit.co/download.html  

If you're using your private laptops, I would recommend the **"Setup.exe"** file.


---

## R Packages

We will be installing the following packages, among others, together in our first session:

- tidyverse  
- dplyr  
- stringr  
- ggplot2  
- ggthemes  
- ggrepel  
- ggpubr  
- ggpp  
- ggtext  
- GGally  
- readxl  
- readr  
- eurostat  
- giscoR  
- viridis  
- sf  
- RColorBrewer  
- reshape2  
- jsonlite  
- rjson  
- rstatix  
- lubridate  
- gtsummary  
- haven  
- stargazer  
- forcats  
- magrittr  
- gapminder  
- maps  
- mapproj  
- modelsummary  


---

## Optional: Install Packages in Advance

If you wish, you can already install the packages yourself by running the following code:

```r
# List of packages
package_list = c(
  "tidyverse", "dplyr", "stringr", "forcats", "magrittr", "lubridate", "reshape2",
  "readxl", "readr", "haven", "jsonlite", "rjson", "eurostat", "gapminder",
  "ggplot2", "ggthemes", "ggrepel", "ggpubr", "ggpp", "ggtext", "GGally",
  "viridis", "RColorBrewer",
  "giscoR", "sf", "maps", "mapproj", "rnaturalearth",
  "modelsummary", "rstatix", "gtsummary", "stargazer"
)

# Installation
for(i in package_list){
  if(!require(i, character.only = TRUE)){
    install.packages(i, dependencies = TRUE)
    require(i, character.only = TRUE)
  }
}

```
