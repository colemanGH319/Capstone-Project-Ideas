#1.Loading data and packages
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(caTools)
  
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

  #Combine the data prior to wrangling
  loans <- rbind(loans14, loans15)

#2.Wrangling the loan data.
  summary(loans)
  #Reviewing the output from the summary called above, it looks like some variables
  #contain little or no data. Let's discard them any column that's less than 15%
  #populated with actual data.
  emptycols <- c()
  for(i in 1:ncol(loans)) {
    if(mean(is.na(loans[,i])) > 0.85) {
      emptycols <- c(emptycols, i)
    }
  }
  loans[,emptycols] <- NULL
  #This got rid of 14 empty columns. 
  summary(loans)
  #There are a number of other variables with missing values. Let's start by
  #looking at months since last delinquency:
  summary(loans$mths_since_last_delinq)
  #Roughly half of the rows in this column are missing values. Since a lower 
  #value for this variable most likely corresponds to a higher potential for 
  #defaulting, we don't want to set this to zero, nor do we want to assume these
  #borrowers fall in the middle of the distribution. In all likelihood, a missing 
  #value indicates that the borrower has never had a delinquency. Imputing values
  #here may be too complex a task, so instead I will create a new column that
  #simply denotes whether a borrower has any delinquencies in their history.
  loans$ever_delinq <- ifelse(is.na(loans$mths_since_last_delinq), 0, 1)
  #We can use the same logic for the number of months since most recent 90-day or
  #worse rating. This variable has 467,000 missing values.
  loans$derog_hist <- ifelse(is.na(loans$mths_since_last_major_derog), 0, 1)
  
  summary(loans$avg_cur_bal)
  #Average current balance only has 6 missing values. I will impute the value using
  #the median of all current balances
  loans <- loans %>% replace_na(list(avg_cur_bal = median(loans$avg_cur_bal, na.rm = TRUE)), 
                                avg_cur_bal)
  #I will use the same imputation method for bankcard open-to-buy and bankcard utilization
  loans <- loans %>% replace_na(list(bc_open_to_buy = median(loans$bc_open_to_buy, na.rm = TRUE)), 
                                bc_open_to_buy)
  loans <- loans %>% replace_na(list(bc_util = median(loans$bc_util, na.rm = TRUE)), bc_util)
  #Months since oldest bank installment account opened has 19,425 missing values.
  #Since the closest equivalent to a missing value here is zero months, I will replace
  #NA's with zero.
  loans <- loans %>% replace_na(list(mo_sin_old_il_acct = 0), mo_sin_old_il_acct)
  #Months since most recent bankcard account opened has 6,044 missing values. An NA
  #for this column most likely implies that they have never opened a bankcard account.
  #Since only 1% of the data is missing here, the most straight forward method would be
  #to use the median.
  loans <- loans %>% replace_na(list(mths_since_recent_bc = median(loans$mths_since_recent_bc, 
                                                                   na.rm = TRUE)), mths_since_recent_bc)
  
  summary(loans$mths_since_recent_bc_dlq)
  mean(is.na(loans$mths_since_recent_bc_dlq))
  #Months since most recent bankcard delinquency has NA's for 74% of its rows. This
  #should be converted to a factor indicating any history of delinquency on a bankcard.
  loans$bc_delinq_hist <- ifelse(is.na(loans$mths_since_recent_bc_dlq), 0, 1)
  loans$bc_delinq_hist <- as.factor(loans$bc_delinq_hist)
  
  summary(loans$mths_since_recent_inq)
  loans$recent_inq <- ifelse(is.na(loans$mths_since_recent_inq), 0, 1)
  
  summary(loans$mths_since_recent_revol_delinq)
  loans$revol_delinq_hist <- ifelse(is.na(loans$mths_since_recent_revol_delinq), 0, 1)
  
  summary(loans$num_tl_120dpd_2m)
  unique(loans$num_tl_120dpd_2m)
  #Number of accounts 120 days past due has only 5 unique values, not counting NA's.
  #We can replace this column with a factor with 2 levels.
  loans$acc_120dpd <- as.factor(ifelse(is.na(loans$num_tl_120dpd_2m), 0, 1))
  
  summary(loans$percent_bc_gt_75)
  #This is the percentage of bankcard accounts where spending has passed 75% of the
  #card's limit. A missing value most likely indicates that the borrower does not
  #have a bankcard. I will set these to zero.
  loans <- loans %>% 
    replace_na(list(percent_bc_gt_75 = 0), percent_bc_gt_75)
  
  loans$issue_year <- ifelse(grepl("14", loans$issue_d), 2014, 2015)
  loans$issue_month <- loans$issue_d
  loans$issue_month <- gsub(".*Jan.*", "January", loans$issue_month)
  loans$issue_month <- gsub(".*Feb.*", "February", loans$issue_month)
  loans$issue_month <- gsub(".*Mar.*", "March", loans$issue_month)
  loans$issue_month <- gsub(".*Apr.*", "April", loans$issue_month)
  loans$issue_month <- gsub(".*May.*", "May", loans$issue_month)
  loans$issue_month <- gsub(".*Jun.*", "June", loans$issue_month)
  loans$issue_month <- gsub(".*Jul.*", "July", loans$issue_month)
  loans$issue_month <- gsub(".*Aug.*", "August", loans$issue_month)
  loans$issue_month <- gsub(".*Sep.*", "September", loans$issue_month)
  loans$issue_month <- gsub(".*Oct.*", "October", loans$issue_month)
  loans$issue_month <- gsub(".*Nov.*", "November", loans$issue_month)
  loans$issue_month <- gsub(".*Dec.*", "December", loans$issue_month)
  unique(loans$issue_month)
  loans$issue_month <- as.factor(loans$issue_month)

  summary(loans$num_rev_accts)
  loans <- loans %>% 
    replace_na(list(num_rev_accts = 0), num_rev_accts)
  
  #Create a "defaulted" column as an integer, where 1 indicates a default"
  loans <- loans %>%
    mutate(defaulted = as.factor(loan_status == "Default"))
  mean(loans$defaulted)

  summary(loans)
  #The dataset is clean and we've created a response variable ("defaulted") that
  #will be used in the logistic regression model. Save it as a .csv

#Split into train and test datasets
  set.seed(72)
  split <- sample.split(loans$defaulted, SplitRatio = 0.6)
  loansTrain <- subset(loans, split == TRUE)
  loansTest <- subset(loans, split == FALSE)
  
  write.csv(loansTrain, file = "loansTrain.csv")
  write.csv(loansTest, file = "loansTest.csv")
#Statistical
  mean(as.integer(loansTrain$defaulted))
  mean(loansTrain$dti)
  sd(loansTrain$dti)
  loansTrain %>% 
    group_by(defaulted)
  str(loansTrain$dti)
#Logistic Regression Analysis
  defaultmod <- glm(defaulted ~ loan_amnt + 
                      annual_inc + 
                      dti + 
                      avg_cur_bal, data = loansTrain, family = "binomial")
  summary(defaultmod)

  head(loans$issue_d)
