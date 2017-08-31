#1.Loading data and packages
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  loans14 <- read.csv("LoanStats3c.csv")
  loans15 <- read.csv("LoanStats3d.csv")

  #Lending Club offers a table with definitions for each column header from
  #the loan dataset. This will be useful for renaming columns to make them more
  #readily understandable. Definitions have been imported below:
  definitions <- read.csv("LCDataDictionary.csv")
  definitions$LoanStatNew <- as.character(definitions$LoanStatNew)
  definitions$Description <- as.character(definitions$Description)

  #Create a simple function to research variable definitions (example: loan_status)
  define.var <- function(x) {
    definitions[definitions$LoanStatNew == x,]
  }
  define.var("loan_status")

  #Now let's look at the loan data to see how many defaults we have to work with:
  nrow(loans14[loans14$loan_status == "Default",]) #236
  nrow(loans15[loans15$loan_status == "Default",]) #793
  #Between the 2014 and 2015 data we have over 1,000 defaults. Now to combine
  #and clean the data.
  loans <- rbind(loans14, loans15)

#2.Wrangling the loan data.
  summary(loans)
  #Reviewing the output from the summary called above, it looks like some variables
  #may not contain any data. Let's remove them if so.
  emptycols <- c()
  for(i in 1:ncol(loans)) {
    if(sum(is.na(loans[,i]) == TRUE) == nrow(loans)) {
      emptycols <- c(emptycols, i)
    }
  }
  loans[,emptycols] <- NULL
  #This got rid of 14 empty columns
  summary(loans)
  #There are a number of other variables with missing values. Let's start by
  #looking at months since last delinquency:
  summary(loans$mths_since_last_delinq)
  #The number of months since borrower's last delinquency has over 115,000
  #missing values. Since a lower value for this variable most likely corresponds
  #to a higher potential for defaulting, we don't want to set this to zero, nor
  #do we want to assume these borrowers fall in the middle of the distribution
  #of available values. In all likelihood, a missing value indicates that the
  #borrower has never had a delinquency.
  summary(loans[is.na(loans$mths_since_last_delinq) == TRUE, "mo_sin_old_rev_tl_op"])
  summary(loans[loans$loan_status == "Default", "mths_since_last_delinq"])
  #lubridate
  
  #loans$mths_since_last_record
  #loans$mths_since_last_major_derog
  #loans$avg_cur_bal
  #loans$bc_open_to_buy
  #loans$bc_util
  #loans$mo_sin_old_il_acct
  #loans$mths_since_recent_bc
  #loans$mths_since_recent_bc_dlq
  #loans$mths_since_recent_inq
  #loans$mths_since_recent_revol_delinq
  #loans$num_tl_120dpd_2m
  #loans$percent_bc_gt_75
  #loans$deferral_term
  #loans$hardship_amount
  #loans$hardship_length
  #loans$hardship_dpd
  #loans$hardship_loan_status
  #loans$orig_projected_additional_accrued_interest
  #loans$hardship_payoff_balance_amount
  #loans$hardship_last_payment_amount
  #Create a "defaulted" column as an integer, where 1 indicates a default"
  loans <- loans %>%
    mutate(defaulted = as.integer(loan_status == "Default"))
  str(loans$defaulted)

#Statistical
  mean(loans$defaulted)
  mean(loans$dti)
  sd(loans$dti)
  loans %>% 
    group_by(defaulted)
  str(loans$dti)
#Logistic Regression Analysis
  defaultmod <- glm(defaulted ~ loan_amnt + 
                      annual_inc + 
                      dti + 
                      avg_cur_bal, data = loans, family = "binomial")
  summary(defaultmod)

  head(loans$issue_d)
