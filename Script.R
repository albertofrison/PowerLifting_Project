################################################################################
# This R projects analyzes the (very lage) Power Lifting dataset proived by the OpenPowerLifting Project
# This page uses data from the OpenPowerlifting project, https://www.openpowerlifting.org.
# You may download a copy of the data at https://data.openpowerlifting.org.
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################

################################################################################
#
# LOAD LIBRARIES AND CLEANING ENVIRONMENT
#
################################################################################
#-------------------------------------------------------------------------------
# LIBRARIES
library(tidyverse)
library (caret)
# library (doSNOW)

#-------------------------------------------------------------------------------
# CLEANING ENVIRONMENT
rm (list = ls())


################################################################################
#
# ACCESS DATA
#
################################################################################
#-------------------------------------------------------------------------------
# ALTERNATIVE 01 - LOAD FILE SAVED LOCALLY
pl_data <- read.csv("./backup.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------
# ALTERNATIVE 02 - DOWNLOAD FILE IF NOT SAVED ALREADY LOCALLY
temp_file <- tempfile()

# OpenPowerLifting hosts a project for gathering PL competition data - there are 3M rows related to PL meetings participations! 
data_url <-  "https://openpowerlifting.gitlab.io/opl-csv/files/openpowerlifting-latest.zip"

# increases the timeout as otherwise will be insufficient to download large files
options(timeout = max(300, getOption("timeout")))
download.file(data_url,temp_file)

# I cannot assume that the file name of the zip file does not change so:
# 1) let's get the list of the files stored into the zip archive, and get the name of the biggest one
file_list <- unzip (temp_file, list = TRUE)
file_target_name <- file_list [which(file_list$Length == max(file_list$Length)),"Name"] 

# 2) now I can unzip and put into a df the "right" file
pl_data <- read.csv(unzip(temp_file, file_target_name), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# let's discard the file used to download the list
unlink(temp_file)

#-------------------------------------------------------------------------------
# BACKUP SECTION - SAVING
write.csv (pl_data, file = "./backup.csv", sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE, quote  = FALSE)
# will have to write code to zip the csv file

#-------------------------------------------------------------------------------
# DATA CHECK
ncol(pl_data) #41
nrow(pl_data) #3M rows!
class(pl_data) # data.frame
head(pl_data)


################################################################################
#
# DATA DICTIONARY
# Source: https://openpowerlifting.gitlab.io/opl-csv/bulk-csv-docs.html
################################################################################

str (pl_data)

#-------------------------------------------------------------------------------
# Name - Mandatory. The name of the lifter in UTF-8 encoding.
# Lifters who share the same name are distinguished by use of a # symbol followed by a unique number. For example, two lifters both named John Doe would have Name values John Doe #1 and John Doe #2 respectively.
# we can leave it aside for the moment...

#-------------------------------------------------------------------------------
# Sex - # Mandatory. The sex category in which the lifter competed, M, F, or Mx.
table (pl_data$Sex)
pl_data$Sex <- as.factor (pl_data$Sex)

# Mx (pronounced Muks) is a gender-neutral title — like Mr and Ms — originating from the UK. It is a catch-all sex category that is particularly appropriate for non-binary lifters.
# The Sex column is defined by crates/opltypes/src/sex.rs.

#-------------------------------------------------------------------------------
# Event - Mandatory. The type of competition that the lifter entered.
table (pl_data$Event)
pl_data$Event <- as.factor (pl_data$Event)

# Values are as follows:
# SBD: Squat-Bench-Deadlift, also commonly called "Full Power".
# BD: Bench-Deadlift, also commonly called "Ironman" or "Push-Pull".
# SD: Squat-Deadlift, very uncommon.
# SB: Squat-Bench, very uncommon.
# S: Squat-only.
# B: Bench-only.
# D: Deadlift-only.
# The Event column is defined by crates/opltypes/src/event.rs.

#-------------------------------------------------------------------------------
# Equipment - Mandatory. The equipment category under which the lifts were performed.
table (pl_data$Equipment)
pl_data$Equipment <- as.factor (pl_data$Equipment)

# Note that this does not mean that the lifter was actually wearing that equipment! For example, GPC-affiliated federations do not have a category that disallows knee wraps. Therefore, all lifters, even if they only wore knee sleeves, nevertheless competed in the Wraps equipment category, because they were allowed to wear wraps.
# Values are as follows:
# Raw: Bare knees or knee sleeves.
# Wraps: Knee wraps were allowed.
# Single-ply: Equipped, single-ply suits.
# Multi-ply: Equipped, multi-ply suits (includes Double-ply).
# Unlimited: Equipped, multi-ply suits or rubberized gear (like Bench Daddies).
# Straps: Allowed straps on the deadlift (used mostly for exhibitions, not real meets).
# The Equipment column is defined by crates/opltypes/src/equipment.rs.


#-------------------------------------------------------------------------------
# Age - Optional. The age of the lifter on the start date of the meet, if known.
sum(is.na (pl_data$Age)) # about 1M Age data is missing from the data set

pl_data %>%
  filter (!is.na (Age)) %>%
    ggplot (aes (x = Age)) +
    facet_wrap (~ Sex) +
    geom_histogram()

pl_data %>%
  filter (!is.na (Age)) %>%
  ggplot (aes (x = Age, color = Sex)) +
  facet_wrap (~ Sex) +
  geom_density(alpha = .5)

# Ages can be one of two types: exact or approximate. Exact ages are given as integer numbers, for example 23. Approximate ages are given as an integer plus 0.5, for example 23.5.
# Approximate ages mean that the lifter could be either of two possible ages. For an approximate age of n + 0.5, the possible ages are n or n+1. For example, a lifter with the given age 23.5 could be either 23 or 24 -- we don't have enough information to know.
# Approximate ages occur because some federations only provide us with birth year information. So another way to think about approximate ages is that 23.5 implies that the lifter turns 24 that year.
# The Age column is defined by crates/opltypes/src/age.rs.


#-------------------------------------------------------------------------------
# AgeClass - Optional. The age class in which the filter falls, for example 40-45. These classes are based on exact age of the lifter on the day of competition.
sum(is.na (pl_data$AgeClass)) # 0? for real?
table(pl_data$AgeClass) #816k are missing

# AgeClass is mostly useful because sometimes a federation will report that a lifter competed in the 50-54 division without providing any further age information.
# This way, we can still tag them as 50-54, even if the Age column is empty.
# The full range available to AgeClass is defined by crates/opltypes/src/ageclass.rs.



#-------------------------------------------------------------------------------
# BirthYearClass
# Optional. The birth year class in which the filter falls, for example 40-49. The ages in the range are the oldest possible ages for the lifter that year. For example, 40-49 means "the year the lifter turns 40 through the full year in which the lifter turns 49."
# BirthYearClass is used primarily by the IPF and by IPF affiliates. Non-IPF federations tend to use AgeClass instead.
# The full range available to BirthYearClass is defined by crates/opltypes/src/birthyearclass.rs.

# Division - Optional. Free-form UTF-8 text describing the division of competition, like Open or Juniors 20-23 or Professional.
sum(is.na (pl_data$Division)) # 0? for real?
table(pl_data$Division) #so many divisions!

# Some federations are configured in our database, which means that we have agreed on a limited set of division options for that federation, and we have rewritten their results to only use that set, and tests enforce that. Even still, divisions are not standardized between configured federations: it really is free-form text, just to provide context.
# Information about age should not be extracted from the Division, but from the AgeClass column.



#-------------------------------------------------------------------------------
# BodyweightKg -  Optional. The recorded bodyweight of the lifter at the time of competition, to two decimal places.
sum(is.na (pl_data$BodyweightKg)) # about 38k BodyWeights data is missing from the data set

pl_data %>%
  filter (!is.na (BodyweightKg)) %>%
  ggplot (aes (x = BodyweightKg, fill = Sex)) +
  facet_wrap (~ Sex) +
  geom_histogram()




#-------------------------------------------------------------------------------
# WeightClassKg - Optional. The weight class in which the lifter competed, to two decimal places.
sum(is.na (pl_data$WeightClassKg)) # 0? for real?
table(pl_data$WeightClassKg) #so many classes - some further string parsing can be done here to improve this feature (for the missing weights)
# Weight classes can be specified as a maximum or as a minimum. Maximums are specified by just the number, for example 90 means "up to (and including) 90kg." minimums are specified by a + to the right of the number, for example 90+ means "above (and excluding) 90kg."
# WeightClassKg is defined by crates/opltypes/src/weightclasskg.rs.



#-------------------------------------------------------------------------------
# Squat1Kg, Bench1Kg, Deadlift1Kg - Optional. First attempts for each of squat, bench, and deadlift, respectively. Maximum of two decimal places.
# Negative values indicate failed attempts.
# Not all federations report attempt information. Some federations only report Best attempts.

# Squat2Kg, Bench2Kg, Deadlift2Kg - Optional. Second attempts for each of squat, bench, and deadlift, respectively. Maximum of two decimal places.
# Negative values indicate failed attempts.
# Not all federations report attempt information. Some federations only report Best attempts.

# Squat3Kg, Bench3Kg, Deadlift3Kg - Optional. Third attempts for each of squat, bench, and deadlift, respectively. Maximum of two decimal places.
# Negative values indicate failed attempts.
# Not all federations report attempt information. Some federations only report Best attempts.

# Squat4Kg, Bench4Kg, Deadlift4Kg - Optional. Fourth attempts for each of squat, bench, and deadlift, respectively. Maximum of two decimal places.
# Negative values indicate failed attempts.
# Fourth attempts are special, in that they do not count toward the TotalKg. They are used for recording single-lift records.


#-------------------------------------------------------------------------------
# Best3SquatKg, Best3BenchKg, Best3DeadliftKg - Optional. Maximum of the first three successful attempts for the lift.
# Rarely may be negative: that is used by some federations to report the lowest weight the lifter attempted and failed.


#-------------------------------------------------------------------------------
# TotalKg - Optional. Sum of Best3SquatKg, Best3BenchKg, and Best3DeadliftKg, if all three lifts were a success. If one of the lifts was failed, or the lifter was disqualified for some other reason, the TotalKg is empty.
# Rarely, mostly for older meets, a federation will report the total but not any lift information.

pl_data %>%
  filter (!is.na (TotalKg)) %>%
  ggplot (aes (x = TotalKg, fill = Sex)) +
  facet_wrap (~ Sex + Event) +
  geom_histogram()


#-------------------------------------------------------------------------------
# Place - Mandatory. The recorded place of the lifter in the given division at the end of the meet.
# Values are as follows:
# Positive number: the place the lifter came in.
# G: Guest lifter. The lifter succeeded, but wasn't eligible for awards.
# DQ: Disqualified. Note that DQ could be for procedural reasons, not just failed attempts.
# DD: Doping Disqualification. The lifter failed a drug test.
# NS: No-Show. The lifter did not show up on the meet day.


# The Place column is defined by crates/opltypes/src/place.rs.
# Dots - Optional. A positive number if Dots points could be calculated, empty if the lifter was disqualified.
# Dots is very similar to (and drop-in compatible with) the original Wilks formula. It uses an updated, simpler polynomial and is built against data from drug-tested Raw lifters, as opposed to against data from drug-tested Single-ply lifters. The Dots formula was created by Tim Konertz of the BVDK in 2019.
# The calculation of Dots points is defined by crates/coefficients/src/dots.rs.


# Wilks - Optional. A positive number if Wilks points could be calculated, empty if the lifter was disqualified.
# Wilks is the most common formula used for determining Best Lifter in a powerlifting meet.
# The calculation of Wilks points is defined by crates/coefficients/src/wilks.rs.


# Glossbrenner - Optional. A positive number if Glossbrenner points could be calculated, empty if the lifter was disqualified.
# Glossbrenner was created by Herb Glossbrenner as an update of the Wilks formula. It is most commonly used by GPC-affiliated federations.
# The calculation of Glossbrenner points is defined by crates/coefficients/src/glossbrenner.rs.

# Goodlift - IPF GL Points. The successor to IPF Points (2019-01-01 through 2020-04-30).
# Optional. A positive number if IPF GL Points could be calculated, empty if the lifter was disqualified or IPF GL Points were undefined for the Event type.
# IPF GL Points roughly express relative performance to the expected performance of that weight class at an IPF World Championship event, as a percentage.
# The calculation of IPF GL points is defined by crates/coefficients/src/goodlift.rs.

# Tested - Optional. Yes if the lifter entered a drug-tested category, empty otherwise.
# Note that this records whether the results count as drug-tested, which does not imply that the lifter actually took a drug test. Federations do not report which lifters, if any, were subject to drug testing.

# Country- # Optional. The home country of the lifter, if known.
# The full list of valid Country values is defined by crates/opltypes/src/country.rs.

# State - Optional. The home state/province/oblast/division/etc of the lifter, if known.
# The full list of valid State values is defined by crates/opltypes/src/states.rs. Expanded names are given there in comments.

# Federation - Mandatory. The federation that hosted the meet.
# Note that this may be different than the international federation that provided sanction to the meet. For example, USPA meets are sanctioned by the IPL, but we record USPA meets as USPA.
# The full list of valid Federation values is defined by crates/opltypes/src/federation.rs. Comments in that file help explain what each federation value means.

# ParentFederation - Optional. The topmost federation that sanctioned the meet, usually the international body.
# For example, the ParentFederation for the USAPL and EPA is IPF.

# Date - Mandatory. The start date of the meet in ISO 8601 format. ISO 8601 looks like YYYY-MM-DD: as an example, 1996-12-04 would be December 4th, 1996.
# Meets that last more than one day only have the start date recorded.

# MeetCountry - Mandatory. The country in which the meet was held.
# The full list of valid Country values is defined by crates/opltypes/src/country.rs.

# MeetState - Optional. The state, province, or region in which the meet was held.
# The full list of valid State values is defined by crates/opltypes/src/state.rs.

# MeetName - Mandatory. The name of the meet.
# The name is defined to never include the year or the federation. For example, the meet officially called 2019 USAPL Raw National Championships would have the MeetName Raw National Championshps.


################################################################################
#
# FILTERING THE PL_DATA TABLE TO RETURN RECORDS HAVING ALL DATA (AGE, BW, EVENT, TOTAL KG AND SEX)
# 
################################################################################

#-------------------------------------------------------------------------------
nrow (pl_data)
pl_filtered <- pl_data %>%
  filter (Event == "SBD") %>%
  filter (Age != 0 & Age > 0 & !is.na (Age)) %>%
  filter (BodyweightKg != 0 & TotalKg > 0 & !is.na (BodyweightKg)) %>%
  filter (TotalKg != 0 & TotalKg > 0 & !is.na (TotalKg)) %>%
  filter (Sex != "Mx")

nrow(pl_filtered)

pl_filtered$Sex <- as.factor(as.character(pl_filtered$Sex))
levels(pl_filtered$Sex)

#-------------------------------------------------------------------------------
# we need to subset the pl_filtered df as it size would make the whole R session to crash
set.seed (12345)
# 3000 random rows from the 1M 
indexes <- sample (x = c(1:nrow(pl_filtered)), size = 3000, replace = FALSE)
pl_fitered_final <- pl_filtered[indexes,]


# I now use the createDataPartition of caret to split the pl_filtered_final df into a training and testing data sets
set.seed(12345)
inTrain = createDataPartition(y = pl_fitered_final$Sex, p = .75, list = FALSE)
training = pl_fitered_final[inTrain,]
testing = pl_fitered_final[-inTrain,] 


# I also reduce the weights by limiting the number of columns to the ones included into the features
features <- c("Sex", "Age", "BodyweightKg", "TotalKg")
training <- training [, features]
testing <- testing[, features]

nrow (pl_fitered_final) * 0.75
nrow (training) # it worked

# I now create the cross validation control 
# fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# let's now create the model
set.seed(12345)
# let's use some CPU cores...
# cl <- makeCluster(2, type = "SOCK") # 4 child processes with Socket Server 
# registerDoSNOW(cl) # register the cluster so Caret understands that will have to use it

# let's also monitor the time used
start.time <- Sys.time()

# caret rf training
rfFit1 <- train(Sex ~ ., data = training, method = "rf", verbose = FALSE)
rfFit1
# accuracy 0.8808832


end.time <- Sys.time()

# stopCluster(cl)
# time.taken <- round(end.time - start.time,2)
# time.taken

rfPrediction <- predict (rfFit1, testing)


test <- ifelse (rfPrediction == testing$Sex, "OK", "KO")
table (test) 
664/nrow(testing) #0.8865154


#-------------------------------------------------------------------------------
# recreate training and testing to test other features for prediction
head(pl_fitered_final)
training = pl_fitered_final[inTrain,]
testing = pl_fitered_final[-inTrain,] 

# I also reduce the weights by limiting the number of columns to the ones included into the features
features <- c("Sex", "Equipment" , "Age", "BodyweightKg", "TotalKg")
training <- training [, features]
testing <- testing[, features]

nrow (pl_fitered_final) * 0.75
nrow (training) # it worked

# I now create the cross validation control 
# fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# let's now create the model
set.seed(12345)

# caret rf training
rfFit2 <- train(Sex ~ ., data = training, method = "rf", verbose = FALSE)
rfFit2 # accuracy 0.8898891

rfPrediction <- predict (rfFit2, testing)

test <- ifelse (rfPrediction == testing$Sex, "OK", "KO")
table (test) 
657/nrow(testing) #0.8771696


#-------------------------------------------------------------------------------
# adding cross validation
# recreate training and testing to test other features for prediction
head(pl_fitered_final)
training = pl_fitered_final[inTrain,]
testing = pl_fitered_final[-inTrain,] 

# I also reduce the weights by limiting the number of columns to the ones included into the features
features <- c("Sex", "Equipment" , "Age", "BodyweightKg", "TotalKg")
training <- training [, features]
testing <- testing[, features]

# I now create the cross validation control 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# let's now create the model
set.seed(12345)

# caret rf training
rfFit3 <- train(Sex ~ ., data = training, method = "rf", trControl = fitControl, verbose = FALSE)
rfFit2 # accuracy 0.8898891 without CV
rfFit3 # accuracy 0.8925798 with CV

rfPrediction <- predict (rfFit3, testing)

test <- ifelse (rfPrediction == testing$Sex, "OK", "KO")
table (test) 
666/nrow(testing) #0.8891956

