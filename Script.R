################################################################################
# This R projects analyzes the (very lage) Power Lifting dataset proived by the OpenPowerLifting Project
# This page uses data from the OpenPowerlifting project, https://www.openpowerlifting.org.
# You may download a copy of the data at https://data.openpowerlifting.org.
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################




################################################################################
#
# LIBRARIES
#
################################################################################

library(tidyverse)



################################################################################
#
# ACCESS DATA
#
################################################################################

temp_file <- tempfile()
data_url <-  "https://openpowerlifting.gitlab.io/opl-csv/files/openpowerlifting-latest.zip"
download.file(data_url,temp)
data <- read.table(unz(temp, "a1.dat"))
unlink(temp)
  
?unz

