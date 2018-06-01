load(file = file.choose())
df <- as.data.frame(read.csv(file = file.choose(), header = T))
dfb <- df
str(df)
df <- dfb

pre_Defined_Values <- df[,-c(1,13)]

levels(df$Gender)
levels(df$Married)
levels(df$Dependents)
levels(df$Education)
levels(df$Self_Employed)
range(df$ApplicantIncome)
range(df$CoapplicantIncome)
range(df$LoanAmount)
range(df$Loan_Amount_Term)
range(df$Credit_History)
levels(df$Property_Area)

# enter Values Here

Gender <- "Male" # "Male"  or "Female"
Married <- "Yes" # "Yes" or "No"
Dependents <- 1 #  "0", "1", "2" or "3+"
Education <- "Graduate" # "Graduate" or "Not Graduate"
Self_Employed <- "No" # "Yes" or "No"
Applicant_Income <- 4853 # [150, 81000]
Coapplicant_Income <- 1580 # [0, 33837]
Loan_Amount <- 128 # [9, 600]
Loan_Amount_Term <- 360 #[36, 480]
Credit_History <- 1 # {0, 1}
Property_Area <- "Rural" #  "Rural", "Semiurban" or "Urban"



Gender <- as.factor(Gender)
Married <- as.factor(Married)
Dependents <- as.factor(Dependents)
Education <- as.factor(Education)
Self_Employed <- as.factor(Self_Employed)
Applicant_Income <- as.integer(Applicant_Income)
Coapplicant_Income <- as.integer(Coapplicant_Income)
Loan_Amount <- as.integer(Loan_Amount)
Loan_Amount_Term <- as.integer(Loan_Amount_Term)
Credit_History <- as.integer(Credit_History)
Property_Area <- as.factor(Property_Area)


user_Defined <- data.frame("Gender" = Gender, "Married" = Married, "Dependents" = Dependents, "Education" = Education,
                     "Self_Employed" = Self_Employed,
                     "ApplicantIncome" = Applicant_Income,
                     "CoapplicantIncome" = Coapplicant_Income,
                     "LoanAmount" = Loan_Amount,
                     "Loan_Amount_Term" = Loan_Amount_Term,
                     "Credit_History" = Credit_History,
                     "Property_Area" = Property_Area
                     )
dfn <- rbind(df,user_Defined)


# prediction from decison tree model
dtpr <- predict(pmod, pre_Defined_Values[nrow(dfn),], type = "class")
dtpr
ifelse(dtpr == "Y", "Loan will be Passed !", "Loan Will not be Passed !")
# prediction from logistic Regression model

logRegpr <- predict(mod2, pre_Defined_Values[nrow(dfn)], type = "response")
ifelse(logRegpr > 0.5, "Loan will be Passed !", "Loan Will not be Passed !")



