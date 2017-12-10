#Library Packages to be installed
install.packages("corrplot")
install.packages("rms")
install.packages("GGally")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("lubridate")
install.packages("corrplot")
install.packages("rms")
install.packages("GGally")
install.packages("corrplot")


#Library Packages
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(corrplot)
library(rms)
library(GGally)
library(corrplot)

#Setting up working directory & Loading Data 
#setwd("/home/asher/Desktop/pgdda/eda/")
loandata <- read.csv(file="loan.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)

#Dim of loandata
dim(loandata)
#39717 rows
#111 columns

#Column names
names(loandata)


#DATA CLEANING AND MANIPULATION
#=============================#
#Removing % sign from the interest rates
loandata$int_rate = (as.numeric(gsub(pattern = "%",replacement = "",x = loandata$int_rate)))
loandata$revol_util = (as.numeric(gsub(pattern = "%",replacement = "",x = loandata$revol_util)))

#Extract loan issued year
loandata$issue_d <- paste("01-", loandata$issue_d,sep="")
loandata$issue_d <- as.Date(loandata$issue_d, format="%d-%b-%y")
loandata$issue_m <-loandata$issue_m <- format(loandata$issue_d, format = "%b")
loandata$issue_y <- loandata$issue_y <- format(loandata$issue_d, format = "%Y")


#Discard irrelevant columns
#As many features do not appear at the issued time,id etc
#The aim is to go after drivers that will predict loan defaulters 
discard_column = c("collection_recovery_fee","emp_title",
                   "funded_amnt_inv","id",
                   "installment","last_credit_pull_d",
                   "last_pymnt_amnt","member_id",
                   "num_tl_120dpd_2m",
                   "num_tl_30dpd","out_prncp_inv","recoveries",
                   "total_pymnt","total_pymnt_inv",
                   "total_rec_int","total_rec_late_fee",
                   "total_rec_prncp","url",
                   "zip_code",
                   "policy_code",
                   "collections_12_mths_ex_med",
                   "pymnt_plan",
                   "desc",
                   "initial_list_status",
                   "application_type",
                   "acc_now_delinq",
                   "chargeoff_within_12_mths",
                   "delinq_amnt",
                   "tax_liens",
                   "title")
loandata = (loandata[,!(names(loandata) %in% discard_column)])

#Drop features that contain too many NA values
tmp = sort(sapply(loandata, function(x) sum(length(which(is.na(x)))))/nrow(loandata),decreasing = TRUE)

#Drop features that have more than 50% missing
discard_column = names(tmp[tmp>0.5])
loandata = (loandata[,!(names(loandata) %in% discard_column)])

#List out columns with missing values
tmp = sort(sapply(loandata, function(x) sum(length(which(is.na(x)))))/nrow(loandata),decreasing = TRUE)
tmp[tmp>0]

#Impute missing values with median
loandata$pub_rec_bankruptcies[is.na(loandata$pub_rec_bankruptcies)] <- median(loandata$pub_rec_bankruptcies,na.rm=TRUE)
loandata$revol_util[is.na(loandata$revol_util)] <- median(loandata$revol_util,na.rm=TRUE)

#Check for NA values
sort(sapply(loandata, function(x) sum(length(which(is.na(x)))))/nrow(loandata),decreasing = TRUE)


#cleaning employee length column
loandata$emp_length<-gsub("[^0-9]", "", loandata$emp_length)
loandata$emp_length[loandata$emp_length==''] <- 0


#Dim after clearing columns
#39717 rows
#29 columns
dim(loandata)

#Outlier correction for loan amount and annual income
y <- loandata

x <- y$loan_amnt
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
y <- filter(y, y$loan_amnt <= (qnt[2] + H))                        
y <- filter(y, y$loan_amnt >= (qnt[1] - H))


x <- y$annual_inc
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
y <- filter(y, y$annual_inc <= (qnt[2] + H))
y <- filter(y, y$annual_inc >= (qnt[1] - H))

loandata <- y

#EXPLORATORY DATA ANALYSIS
#========================#

#Loan amount distribution based on grades 
#Those with higher grades (A, B, C and D) have received more loans compared to those with lower grades (E, F and G)
plot <- ggplot(loandata, aes(loan_amnt, col = grade)) 
plot + geom_histogram(bins = 50) + facet_grid(grade ~ .)  + labs(title ="Plot Of Loan amount distribution to LC grade", x="Loan Amount",y="Count",color="LC Grade") 

#boxplot showing outlier distribution for loan amount
plot + geom_boxplot(aes(y=loan_amnt), outlier.shape=NA) +  facet_grid(.~grade) + labs(title ="Distribution of Loan amount grouped by loan grade")

#Loan amount distribution based on interest rates by grade
#Grades are assigned based on risk, and so interest rates go up as the risk goes up.
ggplot(loandata, aes(int_rate, fill = grade)) + geom_density() + facet_grid(grade ~ .)  + labs(title="Loan amount distribution based on interest rates by grade", y="Rate Density",x="Interest Rate") 

#Total loans issued over the years [2007-2011]
loan_amnt_by_month <- aggregate(loan_amnt ~ issue_d, data = loandata, sum) 
ggplot(loan_amnt_by_month, aes(issue_d, loan_amnt)) + geom_bar(stat = "identity") +  labs( title="Total loans issued over the years [2007-2011]",x="Year",y="Loan Amount") 

#Total loans issued per month [2007-2011]
loandata$issue_m = factor(loandata$issue_m,levels=c("Jan","Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"),ordered=TRUE)
ggplot(loandata,aes(issue_m)) + geom_bar(stat = "count") + labs(title="Total loans issued per month [2007-2011]",x="Month",y="Loan Amount") 


#UNIVARIATE ANALYSIS
#Take a look at the loan status

#current status loans may end up getting defaulted/fully paid, can be ignored if needed
loandata_without_current <-filter(loandata,!loandata$loan_status =='Current')

#Total Loan Amounts getting Charged Off and Fully Paid
ggplot(loandata_without_current, aes(loan_status, loan_amnt, fill = loan_status)) + geom_bar(stat = "identity") + labs(title="Loans getting Chargged Off or Fully Paid with repsect to Loan Amount", x="Loan Status",y="Loan Amount",fill="Loan Status") 

#variation of loan default with dti
ggplot(loandata_without_current, aes(dti,fill=loan_status)) + geom_bar( stat = "count", binwidth = 1,position = "fill") +labs(title="Variation of probability to default w.r.t dti", x="dti",y="proprotion of default",fill="Loan Status") 
#probability to default increases slightly as dti increases

#variation of loan default with revol_util
ggplot(loandata_without_current, aes(revol_util,fill=loan_status)) + geom_bar( stat = "count", binwidth = 1,position = "fill") + labs(title="Variation of probability to default w.r.t revol_util")
#probability to default rises as revol_util rises

#Segmented Univariate Analysis
#Default rate Vs Inquiries made in the last 6 months
ggplot(loandata_without_current, aes(inq_last_6mths, loan_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90)) + labs(title="Proportion of loan default based on Number of Inquiries", x="Loan Inquiries",y="Proportion of Fully Paid/Charged Off",fill="Loan Status")

#Paid vs Default across the grades
loan_by_grade <- aggregate(loan_amnt ~ sub_grade + loan_status, data = loandata_without_current, sum)
gbar <- ggplot(loan_by_grade, aes(sub_grade, loan_amnt, fill = loan_status)) 
gbar + geom_bar(stat = "identity") + theme(axis.text.x=element_text(size=7)) +  labs(title="Loans getting Chargged Off or Fully Paid with repsect to Loan Amount",x="Sub Grade",y="Loan Amount",fill="Loan Status") 

#Proportion of paid vs default over the grades
#It is very obvious that as the grade goes down, the proportion of the unpaid loan increases
gbar + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=7)) + labs(title="Percentage of Loans getting Chargged Off or Fully Paid with repsect to Loan Amount",x="Sub Grade",y="Proportion of Fully Paid/Charged Off",fill="Loan Status") 

#Distribution of loans based on purposes
ggplot(loandata_without_current, aes(purpose, loan_amnt, fill = loan_status)) + geom_boxplot(outlier.shape = NA) + theme(axis.text.x=element_text(size=8, angle = 90))  + labs(title="Box Plot Loans Getting Fully Paid/Charged Off by Purpose",x="Purpose",y="Loan Amount",fill="Loan Status") 

#Proportion of Paid vs Default loan amount by different purposes
Loan_by_purpose <- aggregate(loan_amnt ~ purpose + loan_status, data = loandata_without_current, sum)
### Code block for arraging plots as per proportion
sub1 <- subset(Loan_by_purpose, loan_status == "Charged Off")
sub2 <- subset(Loan_by_purpose, !loan_status == "Charged Off")
r <- sub1$loan_amnt/(sub2$loan_amnt+sub1$loan_amnt) # finding proportion of Charged Off
Purpose <- factor(Loan_by_purpose$purpose, levels = sub1$purpose[order(r, decreasing = F)]) # Alloting levels for purpose in the increasing order of r
ggplot(Loan_by_purpose, aes(Purpose, loan_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90)) + labs(title="Percentage of Loans getting Chargged Off or Fully Paid with repsect to Purpose",x="Purpose",y="Proportion of Fully Paid/Charged Off",fill="Loan Status") #plot using the ordered vector f1
###
#The probability of the loans for educational and small business being unpaid is nearly 25%.

#Table of distribution of loans based on home ownership
mort_df <- loandata %>% select(home_ownership, grade, sub_grade)
table(mort_df$home_ownership)


# Other catergories have only a few data
mort_df <- mort_df[mort_df$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ]  
g_mort <- ggplot(mort_df, aes(grade))
g_mort + geom_bar(aes(fill = grade)) + facet_wrap(~home_ownership) + labs(x = "Grade",  y = "Number of Loans", title = "Issued Loans of Different Home Ownership") + theme_bw() 


#People in MORTGAGE and RENT have much more demands of borrowing money than people in OWN based on the bar chart. 
#Thats because people who own a house usually have better financial situation than others.
tmp <- loandata_without_current[loandata_without_current$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ]  
Loan_by_HomeOwnership <- aggregate(loan_amnt ~ home_ownership + loan_status, data = tmp, sum)
ggplot(Loan_by_HomeOwnership, aes(home_ownership, loan_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90)) + labs(x = "House Ownership",  y = "",fill="Loan Status", title = "Probablity of Issued Loans of Different Home Ownership getting Charged Off/Fully Paid")
#All 3 categories of home owners have nearly 13-15% chance of defaulting.


#Volume and Loan Amount in each states
locVol_df <- select(loandata, addr_state)
locVol_df <- locVol_df %>% na.omit() %>% group_by(addr_state) %>% dplyr::summarise(value = n())
locAmt_df <- select(loandata, addr_state, loan_amnt)
locAmt_df$loan_amnt <- as.numeric(locAmt_df$loan_amnt)  
locAmt_df <- locAmt_df %>% na.omit() %>% group_by(addr_state) %>% dplyr::summarise(value = sum(loan_amnt,na.rm = TRUE))

#Consider only the top 12 states with volume count > 1000
tmp <- loandata_without_current[loandata_without_current$addr_state %in% c("CA", "NY", "FL","TX","NJ","IL","PA","VA","GA","MA","OH","MD"), ]  
Loan_by_State <- aggregate(loan_amnt ~ addr_state + loan_status, data = tmp, sum)
ggplot(Loan_by_State, aes(addr_state, loan_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90)) + labs(x = "State",  y = "",fill="Loan Status", title = "Probablity of Issued Loans in Different State getting Charged Off/Fully Paid")
#Almost all states (top 12) has 15-20% chance to default

#Employee experience with loan status
loandata_without_current$emp_length <- as.numeric(loandata_without_current$emp_length)
Loan_by_EmpLength <- aggregate(loan_amnt ~ emp_length + loan_status, data = loandata_without_current, sum)
ggplot(Loan_by_EmpLength, aes(factor(emp_length), loan_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90)) + labs(x = "Work Ex",  y = "",fill="Loan Status", title = "Probablity of Issued Loans with Employee Experience getting Charged Off/Fully Paid")
#People who have been employed for less than 1 year have 22-23% of defaulting

#Loan term with loan status
ggplot(loandata_without_current, aes(term, fill = loan_status)) + geom_bar(position = "fill", stat = "count") + labs(x = "Term",  y = "",fill="Loan Status", title = "Probablity of Issued Loans getting Charged Off/Fully Paid compared to loan term")
#5 year have more tendency to get charged off

#Public Record of Bankruptcies with loan status
ggplot(loandata_without_current, aes(pub_rec_bankruptcies, fill = loan_status)) + geom_bar(position = "fill", stat = "count") + labs(x = "Public Record of Bankruptcies",  y = "",fill="Loan Status", title = "Probablity of Issued Loans getting Charged Off/Fully Paid compared to No. of Public Record of Bankruptcies ")
# As Public Record of Bankruptcies inscreases the chances of charged off also increases


#preparing data to plot a correlation matrix using all the significant variables
#as analysed previously 
loan_var <- loandata[, 4:5]
loan_var$loan_status <- loandata$loan_status
loan_var$purpose <- loandata$purpose
loan_var$dti <- loandata$dti
loan_var$revol_util <- loandata$revol_util
loan_var$term <- loandata$term
loan_var$pub_rec_bankruptcies <- loandata$pub_rec_bankruptcies
loan_var$emp_length<-loandata$emp_length
loan_var$emp_length <- as.numeric(loan_var$emp_length)
loan_var$home_ownership <- loandata$home_ownership

#adding a new binary variable to reflect good and bad loans based on 
#categorical variable status
loan_var$LB <- 0
loan_var$LB[which(loan_var$loan_status == "Charged Off")] <- 1

#adding new binary variables to reflect five year and three year term loans based on 
#categorical variable term
loan_var$T5 <- 0
loan_var$T3 <- 0
loan_var$T5[which(loan_var$term == " 60 months")] <- 1
loan_var$T3[which(loan_var$term == " 36 months")] <- 1


#adding binary variables to reflect the purpose of loans availed based on 
#top 4 categorical variables in purpose namely credit_card,small_business,debt_consolidation,home_improvement
loan_var$PC <- 0
loan_var$PB <- 0
loan_var$PD <- 0
loan_var$PH <- 0
loan_var$PC[which(loan_var$purpose == "credit_card")] <- 1
loan_var$PB[which(loan_var$purpose == "small_business")] <- 1
loan_var$PD[which(loan_var$purpose == "debt_consolidation")] <- 1
loan_var$PH[which(loan_var$purpose == "home_improvement")] <- 1


#adding binary variables to reflect the grade of loans availed based on 
#categorical variables in grade namely A,B,C,D,E,F,G
loan_var$GA <- 0
loan_var$GB <- 0
loan_var$GC <- 0
loan_var$GD <- 0
loan_var$GE <- 0
loan_var$GF <- 0
loan_var$GG <- 0
loan_var$GA[which(loan_var$grade == "A")] <- 1
loan_var$GB[which(loan_var$grade == "B")] <- 1
loan_var$GC[which(loan_var$grade == "C")] <- 1
loan_var$GD[which(loan_var$grade == "D")] <- 1
loan_var$GE[which(loan_var$grade == "E")] <- 1
loan_var$GF[which(loan_var$grade == "F")] <- 1
loan_var$GG[which(loan_var$grade == "G")] <- 1


#adding binary variables to reflect the home ownership status based on 
#top categorical variables in home_ownership namely RENT,OWN,MORTGAGE
loan_var$HR <- 0
loan_var$HO <- 0
loan_var$HM <- 0
loan_var$HR[which(loan_var$home_ownership == "RENT")] <- 1
loan_var$HO[which(loan_var$home_ownership == "OWN")] <- 1
loan_var$HM[which(loan_var$home_ownership == "MORTGAGE")] <- 1

#now removing the non numeric variables from prepared data for plotting correlation
loan_var$grade <- NULL
loan_var$purpose <- NULL
loan_var$loan_status <- NULL
loan_var$term <- NULL
loan_var$home_ownership <- NULL

#plotting the significant variables 
ggcorr(loan_var)

#plotting the significant variables using corrplot package
# transfer to matrix
M <- cor(loan_var)   
corrplot(M, method = "number", title = "Correlation Map of loans",type = "lower", order = "FPC", number.cex = 0.5, tl.cex = 0.8)
#we can infer that bad loans have a higher correlation with revol_util,int_rate and subsequently increase with grade D and higher

#we can further confirm these findings with hypothesis testing as below

#HYPOTHESIS TESTING
#=============================================
#good loans versus defaults
# revol_util: good loan versus default
t.test(subset(loan_var, LB == 0)$revol_util, subset(loan_var, LB == 1)$revol_util, conf.level = 0.95)
# int_rate: good loan versus default
t.test(subset(loan_var, LB == 0)$int_rate, subset(loan_var, LB == 1)$int_rate, conf.level = 0.95)
# term 5 years: good loan versus default
t.test(subset(loan_var, LB == 0)$T5, subset(loan_var, LB == 1)$T5, conf.level = 0.95)

#grade A comparison with others
# dti: grade A loan versus others
t.test(subset(loan_var, GA == 1)$dti, subset(loan_var, GA == 0)$dti, conf.level = 0.95)
# revol_util: grade A loan versus others
t.test(subset(loan_var, GA == 1)$revol_util, subset(loan_var, GA == 0)$dti, conf.level = 0.95)
# int_rate: grade A loan versus others
t.test(subset(loan_var, GA == 1)$int_rate, subset(loan_var, GA == 0)$dti, conf.level = 0.95)


#grade D comparison with others
# dti: grade D loan versus others
t.test(subset(loan_var, GD == 1)$dti, subset(loan_var, GD == 0)$dti, conf.level = 0.95)
# revol_util: grade D loan versus others
t.test(subset(loan_var, GD == 1)$revol_util, subset(loan_var, GD == 0)$dti, conf.level = 0.95)
# int_rate: grade D loan versus others
t.test(subset(loan_var, GD == 1)$int_rate, subset(loan_var, GD == 0)$dti, conf.level = 0.95)





