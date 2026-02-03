library(rmarkdown)
library(knitr)
library(tidyverse)
library(dplyr)
library(emmeans)
library(multcomp)
library(lme4)
library(car)
library(purrr)
library(patchwork)
library(remotes)

#install_version('dabestr', versions = "0.3.0", repos = "http://cran.us.r-project.org") #Installs correct dabestr version
#use packageVersion("dabestr") to check if 0.3.0 is installed. You may need to first remove the new version of dabestr and restart r
library(dabestr)

wd <- "C:/Users/rowan/Documents/Research/Final_code_versons/OWA_CTmax_clean"
setwd(wd)
process_all_data = F #Runs data analysis for all files
make_report = T #Runs project summary

# Runs data analysis (data is read in during the for loop; nothing to read in here)
if (process_all_data==T){
  source(file = "Scripts/01_data_processing.R")
}else{
  full_data = read.csv(file = paste(wd, "/Data/processed/full_data.csv", sep = ""))
  ramp_record = read.csv(file = paste(wd, "/Data/processed/ramp_record.csv", sep = ""))
  temp_record = read.csv(file = paste(wd, "/Data/processed/temp_record.csv", sep = ""))

}

if(make_report == T){
  render(input = "Scripts/report.Rmd", #Input the path to your .Rmd file here
         output_format = "html_document")
}


