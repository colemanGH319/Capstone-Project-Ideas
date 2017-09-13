#1.Loading data and packages
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(caTools)
  library(caret)
  
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
  sapply(loans, function(x) {mean(is.na(x))}) %>% 
    hist(breaks = 50)
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
  #This got rid of 37 columns. 
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
  #Months since oldest bank installment account opened has 19,425 missing
  #values. Since the credit report is looking back 7 years, an NA can be
  #replaced with 7*12 months = 84 months.
  loans <- loans %>% replace_na(list(mo_sin_old_il_acct = 84), mo_sin_old_il_acct)
  
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

  
  summary(loans$num_rev_accts)
  #An NA for number of revolving accounts presumably indicates that there are
  #no revolving accounts in that borrower's credit history.
  loans <- loans %>% 
    replace_na(list(num_rev_accts = 0), num_rev_accts)
  
  #Multiple columns exist to denote loans that have incurred a hardship. These
  #columns are nearly completely empty and can be removed.
  loans$hardship_flag <- NULL
  loans$hardship_type <- NULL
  loans$hardship_reason <- NULL
  loans$hardship_status <- NULL
  loans$hardship_start_date <- NULL
  loans$hardship_end_date <- NULL
  
  #Interest rates are stored as a factor w/ 150 levels. These need to be 
  #converted to integers but the % symbol is causing issues with the as.numeric 
  #formula. Below is the workaround:
  loans$int_rate <- as.character(loans$int_rate)
  loans$int_rate <- gsub("%$", "", loans$int_rate)
  loans$int_rate <- as.numeric(loans$int_rate)
  
  #Employment title to character vector
  loans$emp_title <- as.character(loans$emp_title)
  
  summary(loans$emp_length)
  #Employment length is a factor w/ 12 levels, one of which is "n/a". There are
  #35,836 borrowers with employment length n/a.
  summary(loans[loans$emp_length == "n/a", "annual_inc"])
  #The median annual income for borrowers who listed employment length as "n/a" 
  #is $45,000. This may suggest that certain borrowers are earning income other 
  #than wages or salary and therefore left their employment information blank. I
  #will collapse the employment length column from 12 levels to 11, converting 
  #the "n/a" level to true NA's. Then I will use this column in conjunction with
  #a new column, "employment_listed".
  loans$emp_length <- factor(loans$emp_length, levels = c("< 1 year", "1 year",
                                                          "2 years", "3 years",
                                                          "4 years", "5 years",
                                                          "6 years", "7 years",
                                                          "8 years", "9 years",
                                                          "10+ years"))
  loans$employment_listed <- ifelse(is.na(loans$emp_length), 0, 1)
  
  #Home ownership is currently a factor with 4 levels, one of which ("ANY") only
  #has 3 records. Collapse to 3 levels.
  loans$home_ownership <- factor(loans$home_ownership, levels = c("MORTGAGE", 
                                                                     "OWN",
                                                                     "RENT"))
  
  #The issue date of the loans is recorded in character format as abbreviated 
  #month and 2 digit year. I chose to split the year and month apart, replacing 
  #the year with 4-digit year (numeric) and the month with the full name of the
  #month (factor)
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
  loans$issue_month <- factor(loans$issue_month, levels = c("January", "February", "March", 
                                                            "April", "May", "June", "July", 
                                                            "August", "September", "October", 
                                                            "November", "December"))
  summary(loans$issue_month)
  
  #Convert loan descriptions to characters
  loans$desc <- as.character(loans$desc)
  
  #Create a "defaulted" column as a factor, where 1 indicates a default
  loans <- loans %>%
    mutate(defaulted = as.factor(loan_status == "Default" | loan_status == "Charged Off"))
  nrow(loans[loans$defaulted == TRUE,])/nrow(loans)
  summary(loans$defaulted)
  
  summary(loans)
  #The dataset is clean and we've created a response variable ("defaulted") that
  #will be used in the logistic regression model. Split into train and test data
  #and save it as a .csv

  set.seed(72)
  split <- sample.split(loans$defaulted, SplitRatio = 0.6)
  loansTrain <- subset(loans, split == TRUE)
  loansTest <- subset(loans, split == FALSE)
  
  write.csv(loansTrain, file = "loansTrain.csv")
  write.csv(loansTest, file = "loansTest.csv")

  #Clear original datasets from the environment
  remove(loans14)
  remove(loans15)  
  #remove(loans)
#2. Statistical Analysis
  
  #Begin by running univariate inferential tests using population proportions of
  #borrowers who defaulted. This requires conversion of "defaulted" variable to
  #a binomial integer.
  loansTrain$default_int <- as.integer(loansTrain$defaulted) - 1
  def_prop <- mean(loansTrain$default_int)
  #The population proportion for defaults in the training dataset is 12.4%.
  
  #Loan terms and default rate
  def_60mo <- mean(loansTrain[loansTrain$term == " 60 months", "default_int"])
  def_36mo <- mean(loansTrain[loansTrain$term == " 36 months", "default_int"])
  #Borrowers on a loan term of 60 months had a default rate of 15.9% vs 10.8%
  #for borrowers on 36 month loans.
  n1 <- nrow(loansTrain[loansTrain$term == " 60 months",])
  n2 <- nrow(loansTrain[loansTrain$term == " 36 months",])
  z <- (def_60mo - def_36mo) / sqrt(def_prop*(1 - def_prop)*((1/n1) + (1/n2)))
  z
  #Our large population contributes to a z-score of 45.16, which indicates that
  #we should firmly reject the null hypothesis of equal proportions. Borrowers
  #with 60 month loans are more likely to default.
  
  #Testing renters vs. total
  mean(loansTrain[loansTrain$home_ownership == "RENT", "default_int"])
  mean(loansTrain[loansTrain$home_ownership == "OWN", "default_int"])
  mean(loansTrain[loansTrain$home_ownership == "MORTGAGE", "default_int"])
  #Since borrowers who own their homes have a default rate roughly equivalent to
  #the overall default rate and borrowers with a mortgage are lower, I will test
  #the significance of renters having the higher default rate.
  def_rent <- mean(loansTrain[loansTrain$home_ownership == "RENT", "default_int"])
  n1 <- nrow(loansTrain[loansTrain$home_ownership == "RENT",])
  n2 <- nrow(loansTrain)
  z <- (def_rent - def_prop) / sqrt(def_prop * (1 - def_prop) * ((1/n1) + (1/n2)))
  z
  #We should reject the null hypothesis that renters default at the same rate as
  #the overall population.
  
  #Let's take a more in-depth look at each variable starting with loan amount:
  summary(loansTrain$loan_amnt)  
  loansTrain$loan_amnt %>% hist(breaks = 30)
  #The distribution of loan amount is slightly right-tailed, resulting in a mean
  #that is $2,300 greater than the median. Funded amount looks very similar to
  #loan amount since most of the loan requests were approved for the full amount
  #of the loan. Let's see if there's any difference at all:
  summary(loansTrain$funded_amnt)
  unfunded <- loansTrain$loan_amnt - loansTrain$funded_amnt
  sum(unfunded)
  #No difference. Repeat the exercise for variables representing total funded
  #amount and amount funded by investors:
  funded_diff <- loansTrain$funded_amnt - loansTrain$funded_amnt_inv
  sum(funded_diff)
  sum(as.numeric(loansTrain$funded_amnt))
  #Out of roughly $6B in loans issued, only $2M in funding appears to have come
  #from a source other than the investors.
  
  #Annual income
  summary(loansTrain$annual_inc)  
  ggplot(loansTrain, aes(x = annual_inc)) +
    geom_histogram(binwidth = 10000)
  
  #Default rate by employment Length
  loansTrain %>% 
    group_by(emp_length) %>% 
    summarise(default = mean(as.integer(defaulted)) - 1)
  #The rate of default is 16% for borrowers who have no employment length and 
  #13% for borrowers with less than 1 year in their current job. The default 
  #rate gradually decreases as employment length increases, which suggests that
  #stability in employment is associated with a lower risk of default.
  
  summary(loansTrain$bc_util)
  ggplot(loansTrain, aes(x = defaulted, y = bc_util)) +
    geom_boxplot() 
  #It is difficult to draw conclusions from the boxplot comparison of bankcard 
  #utilization percentage between defaulted and non-defaulted borrowers, however
  #it appears that median bankcard use is higher among borrowers who defaulted.
  #Perhaps a comparison of total current balance will be more revealing.
  ggplot(loansTrain, aes(x = defaulted, y = tot_cur_bal)) +
    geom_boxplot()
  #Still very hard to read. Let's transform the y axis and see if we can get a
  #better picture.
  ggplot(loansTrain, aes(x = defaulted, y = sqrt(tot_cur_bal))) +
    geom_boxplot()
  summary(loansTrain[loansTrain$defaulted == TRUE, "tot_cur_bal"])
  summary(loansTrain[loansTrain$defaulted == FALSE, "tot_cur_bal"])
  #The total current account balance is actually lower on average for the 
  #borrowers who defaulted. This may be a result of their credit limit.
  summary(loansTrain[loansTrain$defaulted == TRUE,"tot_hi_cred_lim"])
  summary(loansTrain[loansTrain$defaulted == FALSE,"tot_hi_cred_lim"])
  #The credit limit is lower for borrowers who defaulted, as expected.
  summary(nodefault_bc_util)
  summary(default_bc_util)

#3. Logistic Regression Analysis
  defaultmod <- glm(defaulted ~ loan_amnt + 
                      annual_inc + 
                      dti + 
                      avg_cur_bal, data = loansTrain, family = "binomial")
  summary(defaultmod)

  head(loans$issue_d)
