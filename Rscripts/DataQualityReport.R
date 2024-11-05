# load the necessary packages
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)

library(tidyverse)
library(dplyr)
library(knitr)      # for kable
library(kableExtra) # for kable styling

############################## Dataset ############################################

## load the dataset
creditRisk <- read.csv("dataset/credit_risk_dataset.csv")

## divide the dataset into numerical and categorical variables
numData <- creditRisk %>%
  dplyr::select(is.numeric)

catData <- creditRisk %>%
  dplyr::transmute(across(where(is.character), as.factor)) # converts character variables to factors

## Take a look at these two datasets
glimpse(numData)
glimpse(catData)

#################### Data Quality Report - Numerical variables #####################

## Generate data quality report for numerical variables

## Create custom function for first and third quartiles
Q1 <- function(x, na.rm=TRUE){
  quantile(x, na.rm=na.rm)[2]  # first quartile
}

Q3 <- function(x, na.rm=TRUE){
  quantile(x, na.rm=na.rm)[4]  # third quartile
}

## Create custom function to define the summary statistics
numSummary <- function(x){
  return(c(length(x), n_distinct(x),sum(is.na(x)), mean(x, na.rm=TRUE), 
           min(x, na.rm=TRUE), Q1(x, na.rm=TRUE), median(x, na.rm=TRUE),
           Q3(x, na.rm=TRUE), max(x, na.rm=TRUE), sd(x, na.rm = TRUE)))
}

## Create the summary for numerical variables
numericSummary <- numData %>% dplyr::summarise(across(everything(), numSummary))

## Create labels for these summary statistics using cbind
numericSummary <- cbind(stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"), numericSummary)

## Tranform the data and add few more summary statistics
numericSummary <- numericSummary %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::mutate(missing_pct = 100*missing/n,                                                    # add new columns
                unique_pct = 100*unique/n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())


## Let's view/produce this data quality report using kable
options(digits=3)
options(scipen=99)
numericSummary %>% kable() %>% kable_styling(font_size = 12)

#################### Data Quality Report - Categorical variables #####################

## Generate data quality report for categorical/factorial variables

## Create custom defined functions to identify the first, second or least common modes
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

## Create custom defined functions to identify the frequencies of the first, second, or least common modes
getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

## Create a custom defined summary function for categorical variables
catSummary <- function(x){
  return(c(length(x), n_distinct(x), sum(is.na(x)),
           getmodes(x,type=1),  getmodesCnt(x,type=1),
           getmodes(x,type=2),  getmodesCnt(x,type=2),
           getmodes(x,type=-1), getmodesCnt(x,type=-1)))
}

## Create the summary for categorical variables
categoricalSummary <- catData %>% dplyr::summarise(across(everything(), catSummary))

## Generate labels for the summary statistics
categoricalSummary <- cbind(stat=c("n", "unique", "missing", "1st mode", "1st mode freq", "2nd mode", 
                                   "2nd mode freq", "least common", "least common freq"), categoricalSummary)

## Transform the data and add few more summary statistics
categoricalSummary <- categoricalSummary %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::mutate(across(c(n, missing, unique, `1st mode freq`, `2nd mode freq`, 
                         `least common freq`), as.numeric))  %>%           # converts character values to numeric
  dplyr::mutate(missing_pct = 100*missing/n,
                unique_pct = 100*unique/n,
                freqRatio = `1st mode freq`/ `2nd mode freq`) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, freqRatio, everything())

## Let's view/produce this data quality report using kable
options(digits=3)
options(scipen=99)
categoricalSummary %>% kable() %>% kable_styling(font_size = 12)
