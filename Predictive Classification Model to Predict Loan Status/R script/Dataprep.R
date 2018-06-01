Sys.getenv()
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre-9.0.4")
install.packages("csv")

library("csv")

# FOR TRAINING DATASET
#df = read.csv(file.choose(), sep = ",", header = T, quote = "")
df = read.csv(file.choose(), header = T)
df = df[,!apply(is.na(df), 2, all)]

# having a look at first set of values in data frame
head(df)


# taking backup of original dataset
dfb <- df


# structure of the datframe
str(dfb)




# summary and structure of the first variable: Gender --------------------------------------------- 
summary(dfb$Gender)
str(dfb$Gender)

# getting the indices of the rows where data is missing in Gender column
toberemoved <- which(dfb$Gender=="")
k <- dfb[dfb$Gender=="",]    # k is a dataframe with 13 observations
class(toberemoved)   # integer
length(toberemoved)  # 13


# remomving the rows 
dfb <- dfb[-toberemoved,]
## dfb1 <- df
## dfb1 <- dfb1[-k,]

# reconfiguring coulmn
dfb$Gender <- as.character(dfb$Gender)
dfb$Gender <- as.factor(dfb$Gender)


# summary and structure of the second variable: Married -------------------------------------------
summary(dfb$Married)
str(dfb$Married) # factor variable
 
# getting the indices of the rows where data is missing in Married column
toberemoved <- which(dfb$Married=="")
k <- dfb[dfb$Married=="",] # k is dataframe with three observations
class(toberemoved)  # integer
length(toberemoved)  # 3

# removing the rows with blank Married status
dfb <- dfb[-toberemoved,]

# reconfiguring columns
dfb$Married <- as.character(dfb$Married)
dfb$Married <- as.factor(dfb$Married)






# summary and structure of the third variable: Dependents -----------------------------------------
summary(dfb$Dependents)
str(dfb$Dependents) # factor variable

# getting the indices of the rows where data is missing in Dependents column
toberemoved <- which(dfb$Dependents=="")
k <- dfb[dfb$Dependents=="",] # k is dataframe with 12 observations
class(toberemoved)  # integer
length(toberemoved)  # 12

# removing the rows with blank dependents status
dfb <- dfb[-toberemoved,]

# reconfiguring columns
dfb$Dependents <- as.character(dfb$Dependents)
dfb$Dependents <- as.factor(dfb$Dependents)






# summary and structure of the fourth variable: Educations -----------------------------------------
summary(dfb$Education)
str(dfb$Education) # factor variable
## NOTE : NO ANOMALY OR INCONSISTENCIES FOUUND IN THE EDUCATION COLUMN


# summary and structure of the fifth variable: Self Employed---------------------------------------
summary(dfb$Self_Employed)
str(dfb$Self_Employed) # factor variable

# getting the indices of the rows where data is missing in self employed column
toberemoved <- which(dfb$Self_Employed=="")
k <- dfb[dfb$Self_Employed=="",] # k is dataframe with 32 observations
class(toberemoved)  # integer
length(toberemoved)  # 32

# removing the rows with blank self employed status
dfb <- dfb[-toberemoved,]

#reconfiguring column
dfb$Self_Employed <- as.character(dfb$Self_Employed)
dfb$Self_Employed <- as.factor(dfb$Self_Employed)








# summary and structure of the sixth variable: Applicant Income-----------------------------------
summary(dfb$ApplicantIncome)
str(dfb$ApplicantIncome) # integer

# getting the indices of the rows where data is missing in Applicant Income column
toberemoved <- which(dfb$ApplicantIncome < 0)
k <- dfb[dfb$ApplicantIncome < 0,] # k is dataframe with 0 observations
class(toberemoved)  # integer
length(toberemoved)  # 0
## NOTE : NO ANOMALY FOUND IN APPLICANT INCOME




# summary and structure of the seventh variable: Co Applicant Income-------------------------------
summary(dfb$CoapplicantIncome)
str(dfb$CoapplicantIncome) 
# numeric variable (needs to be changed as income should not be same as applicants incomw type)
dfb$CoapplicantIncome <- as.integer(dfb$CoapplicantIncome)

# getting the indices of the rows where data is missing in self employed column
toberemoved <- which(dfb$CoapplicantIncome < 0)
k <- dfb[dfb$CoapplicantIncome < 0,] # k is dataframe with 0 observations
class(toberemoved)  # integer
length(toberemoved)  # 0

## NOTE : NO DATA ANOMALY FOUND IN THE CO APPLICANTS INCOME 








# summary and structure of the Eighth variable: Loan Amount-------------------------------
summary(dfb$LoanAmount) # Na's value present
str(dfb$LoanAmount) # integer 

# getting the indices of the rows where data is missing in self employed column
toberemoved <- which(is.na(dfb$LoanAmount))
k <- dfb[is.na(df$LoanAmount),] # k is dataframe with 19 observations
class(toberemoved)  # integer
length(toberemoved)  # 19

# removing the rows with loan applicants
dfb <- dfb[-toberemoved,]










# summary and structure of the ninth variable: Loan Amount term-------------------------------
summary(dfb$Loan_Amount_Term) # Na's value present
str(dfb$Loan_Amount_Term) # integer 

# getting the indices of the rows where data is missing in self employed column
toberemoved <- which(is.na(dfb$Loan_Amount_Term))
k <- dfb[is.na(dfb$Loan_Amount_Term),] # k is dataframe with 12 observations
class(toberemoved)  # integer
length(toberemoved)  # 12

# removing the rows with Na's Laon aount term
dfb <- dfb[-toberemoved,]










# summary and structure of the tenth variable: Credit History-------------------------------
summary(dfb$Credit_History) # Na's value present
str(dfb$Credit_History) # integer 

# getting the indices of the rows where data is missing in credit history column
toberemoved <- which(is.na(dfb$Credit_History))
k <- dfb[is.na(dfb$Credit_History),] # k is dataframe with 43 observations
class(toberemoved)  # integer
length(toberemoved)  # 43

# removing the rows with Na's credit term 
dfb <- dfb[-toberemoved,]










# summary and structure of the eleventh variable: Loan status-------------------------------
summary(dfb$Loan_Status) # fine
str(dfb$Loan_Status) # factor


## NOTE : NO DATA ANOMALY FOUND IN THE LOAN STATUS 











# summary and structure of the twelveth variable: property area------------------------------
summary(dfb$Property_Area)
str(dfb$Property_Area)

##  NOTE : NO DATA ANOMALY FOUND IN THE PROPERTY AREA




write.csv(dfb,file = "prepareddata.csv")
write.csv(dfb,file = file.choose(new = T), row.names = F)

getwd()





