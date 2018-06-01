Sys.getenv()
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre-9.0.4")


df <- read.csv(file = file.choose(), header = T)
rm(df)

dftb <- df
dftrain <- df
str(dftrain)


# partitioning Train : Valid :: 60 : 40
partidx <- sample(1:nrow(dftrain), 0.6*nrow(dftrain), replace = F)
dftrain <- dftb[partidx,]
dfvalid <- dftb[-partidx,]

dftrain <- dftrain[,-c(1)]
dfvalid <- dfvalid[,-c(1)]

################################################################################ Decision Tree
library(rpart)
library(rpart.plot)

# rel error is the ratio of incorrectly classified traiining records
# after doing a split to incorrectly classified training records
# at the root node (naive  rule)

# xval (default value = 10)
# pruning using rpart's prune
mod1 <- rpart(Loan_Status ~ ., method = "class", data = dftrain, 
              control = rpart.control(cp = 0, minsplit = 2, minbucket = 1, 
                                      maxcomplete = 0, maxsurrogate = 0,
                                      xval = 10)
              )
prp(mod1)

mod1$cptable

mod1$cptable[which.min(mod1$cptable[,"xerror"]),]
summary(mod1)$variable.importance

#  xerror  contains estimates of cross-validated prediction error
# for different numbers of splits (nsplit)
# below is the cp value corresponding to minimum xerror value
cp1 <- mod1$cptable[which.min(mod1$cptable[,"xerror"]),"CP"]
plotcp(mod1)

pmod <- prune(mod1, cp = cp1)
prp(pmod)


# performance on training partition
bmodtr <- predict(pmod, dftrain, type = "class")
#classification accuracy #0.79
mean(bmodtr == dftrain$Loan_Status)
# misclassification error #0.20
mean(bmodtr != dftrain$Loan_Status)


# performance on validation partition
bmodvr <- predict(pmod, dfvalid, type = 'class')
#classification accuracy #0.82
mean(bmodvr == dfvalid$Loan_Status)
# misclassification error #0.17
mean(bmodvr != dfvalid$Loan_Status)


#################################################################### Logistic regression model

# cormat
mod2 <- glm(Loan_Status ~ . , family = binomial(link = "logit"),
            data = dftrain)
summary(mod2)

# on training partition
lrmodrt <- predict(mod2, dftrain, type = "response")
lrmodrt <- ifelse(lrmodrt > 0.5,"Y","N")
lrmodrt <- as.factor(lrmodrt)
#classification accuracy #0.80
mean(lrmodrt == dftrain$Loan_Status)
# misclassification error #0.19
mean(lrmodrt != dftrain$Loan_Status)


# on validation partition
lrmodrv <- predict(mod2, dfvalid, type = "response")
lrmodrv <- ifelse(lrmodrv > 0.5,"Y","N")
lrmodrv <- as.factor(lrmodrv)
#classification accuracy #0.79
mean(lrmodrv == dfvalid$Loan_Status)
# misclassification error #0.20
mean(lrmodrv != dfvalid$Loan_Status)


#------------------------------------------------------------------------------
#saving models
save(pmod, file = file.choose(new = T))
save(mod2, file = file.choose(new = T))

# generating PMML
library("pmml")
xmlmodel1 <- pmml(pmod, model.name="Decision Tree",
                 app.name="Rattle/PMML",
                 description="RPart Decision Tree Model",
                 copyright=NULL, transforms=NULL,
                 unknownValue=NULL, dataset=NULL)
xmlmodel2 <- pmml(mod2, model.name="Logistic Regression Model",
                  app.name="Rattle/PMML",
                  description="Logistic regression model",
                  copyright=NULL, transforms=NULL,
                  unknownValue=NULL, dataset=NULL)

# Save to an external file 
savePMML(xmlmodel1, "DecisionTreeModel.pmml")
savePMML(xmlmodel2, "LogRegModel.pmml")













