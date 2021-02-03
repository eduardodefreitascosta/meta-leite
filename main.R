#Reset the envoronment
rm(list = ls())

#Packages to be used
packages<-c("readxl","here","tidyverse","ggplot2","meta","metafor","knitr")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Creadting dir.

dir.create(here("Figures"))
dir.create(here("Output"))


# Run the script
source(here("Script","meta_r.R"))
