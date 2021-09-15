#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(e1071)
library(ipred)
library(pROC)
## add other libraries

#### DIRECTORIES ####
working_dir = "C:/Users/Nabil-PC/Downloads/Progetto Digital"
data_dir = "C:/Users/Nabil-PC/Downloads/Progetto Digital"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  , 'D01_RFM.R'
  , 'D02_CHURN.R'
  )
# 
for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
  }
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
