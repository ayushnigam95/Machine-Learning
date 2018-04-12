Sys.getenv()
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre-9.0.4")

library(xlsx)

# dataset in xlsx format file 
# choose the file : Promotional offer.xlsx

df <- read.xlsx(file.choose(), 1, header = T)
df <- df[,!apply(is.na(df), 2, all)]
head(df)

# taking backup of original dataframe
df1 <- df
df1[1:20,]

str(df1)

# about pin code :
# - typically it is a categorical variable
# - but latitudes and longitudes are not categorical but numeric

# pincode are large in number and thus are needed to be categorised
tpin <- table(as.factor(df1$PIN.Code))
str(tpin)
tpin

Pinnames <- dimnames(tpin)[[1]]

# count of success cases for each pincode
c_pincode <- NULL
for (x in Pinnames) {
  c_pincode <- c(c_pincode, length(which(as.character(df1$PIN.Code)==x & df1$Promoffer == 1)))
  
}

range(c_pincode)
par(mar = c(5.1,5.1,5.1,5.1) + 0.9)
barplot(c_pincode, names.arg = Pinnames, xlab = "Pin codes", ylab = "# offers accepted", las = 3,
        ylim = c(0,12), cex.names = 0.7)

table(as.factor(c_pincode))

# Assign count of pin code as its label
# pin codes having same count will have same lable and will be grouped togethr
for (x in Pinnames) {
  index <- which(as.character(df$PIN.Code) == x)
  df1[index, ]$PIN.Code <- rep(c_pincode[which(Pinnames == x)], length(index)) 
  
}

df1$PIN.Code <- as.factor(df1$PIN.Code)
df1$Education <- as.factor(df1$Education)
df1$Promoffer <- as.factor(df1$Promoffer)
df$Online <- as.factor(df1$Online)

str(df1)



# partitioning : Training : validation : testing -> 2500 : 1500 : 1000  
# not taking 60 : 20 : 20 rule

partidx <- sample(1:nrow(df1), 2500, replace = F)
df1train <- df1[partidx,]

partidx1 <- sample((1:nrow(df1))[-partidx], 1500, replace = F)
intersect(partidx,partidx1)
df1valid <- df1[partidx1,]
df1test <- df1[-c(partidx,partidx1),]



# model creation : first attempt


library(rpart)
library(rpart.plot)
mod1 <- rpart(Promoffer ~ ., method = "class", data = df1train, 
              control = rpart.control(cp = 0, minsplit = 2, minbucket = 1, 
                                      maxcomplete = 0, maxsurrogate = 0,
                                      xval = 0),
              parms = list(split = "gini")
              
              )

par(mar = c(0,0,0,0), oma = c(0,0,0,0), xpd = NA)
plot(mod1, uniform = T, branch = 0.1, compress = T, margin = 0, nspace = 1)

text(mod1, splits = T, use.n = F, all = F, minlength = 0, cex = 0.7)

prp(mod1, varlen =0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0) 


# First four levels of full grown tree
prp(mod1, varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0,
    nn = T, nn.cex = 0.6)

toss1 <- as.integer(row.names(mod1$frame))
toss2 <- sort(toss1)
toss3 <- toss2[which(toss2==16):length(toss2)]
mod1sub <- snip.rpart(mod1, toss = toss3)
prp(mod1sub, varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0, nn = T)


# Desciption of each splitting step of the full grown tree
# no. of decision nodes
nrow(mod1$splits)
# no of terminal nodes
nrow(mod1$frame)-nrow(mod1$splits)
nrow(mod1$frame)
nrow(mod1$splits)



#----------------------------------------logic testing------(just checking something out)--------
nrow(mod1sub$frame)
nrow(mod1sub$splits)

# for counting number of leaf nodes
o <- mod1sub$frame$var=="<leaf>"
oo <- mod1sub$frame[o,]
nrow(oo)
#------------------------------------------------------------------------------------------------



# j is the counter for split variables
# i is counter for split values
splitvalue <- NULL
j <- 1
i <- 1
for(x in mod1$frame$var){
  if (as.character(x) != "<leaf>") {
    if(!is.factor(df1[,as.character(x)])){
      splitvalue[i] <- mod1$splits[j,"index"]
    }
    else{
      cl <- NULL
      
      # split variable is a factor
      # k <- {1, largest number of levels in the factors}
      for (k in 1:ncol(mod1$csplit)) {
        temp <- mod1$csplit[mod1$splits[j,"index"],k]
        # if level temp goes to the left
        if (temp == 1L){
          cl <- paste(cl, levels(df1[,as.character(x)])[k], sep = ",")
        }
      }
      splitvalue[i] <- substr(cl, start = 2, stop = nchar(cl))
    }
    j <- j + 1
    }
    else{
    splitvalue[i] <- NA
    }
    
    i <- i + 1
}

data.frame("Node Number" = row.names(mod1$frame), "Split var" = mod1$frame$var,
           "Split value" = splitvalue, "cases" = mod1$frame$n,
           check.names = F)


mod1train <- predict(mod1, df1train[,-c(3)], type = "class")
table("Actual value" = df1train$Promoffer, "Predicted value" = mod1train)


# classification accuracy
mean(mod1train==df1train$Promoffer)
# misclassification error
mean(mod1train!=df1train$Promoffer)



mod1valid <- predict(mod1, df1valid[,-c(3)], type = "class")
table("Actual value" = df1valid$Promoffer, "Predicted value" = mod1valid)

# classification accuracy
mean(mod1valid==df1valid$Promoffer)
# misclassification error
mean(mod1valid!=df1valid$Promoffer)




mod1test <- predict(mod1, df1test[,-c(3)], type = "class")
table("Actual value" = df1test$Promoffer, "Predicted value" = mod1test)

# classification accuracy
mean(mod1test==df1test$Promoffer)
# misclassification error
mean(mod1test!=df1test$Promoffer)


summary(mod1)$variable.importance


# Pruning ------------------------------------------------------------------------------------
# ---------------------based on node numbering 
# (ie. ignoring the original sequence in which decision tree was created)

# validation partition : misclassification error vs. no. of decision nodes
# total no of nodes in a full grown tree
nrow(mod1$frame)

#number of decision nodes
nrow(mod1$splits)

# number of terminal nodes
o <- mod1$frame$var=="<leaf>"
oo <- mod1$frame[o,]
nrow(oo)

# node numbers
toss1 <- as.integer(row.names(mod1$frame))
toss2 <- sort(toss1)

# counter of nides to be snipped off
i <- 1
mod1splitv <- NULL
mod1strainv <- NULL
mod1svalidv <- NULL
ErrTrainv <- NULL
Errvalidv <- NULL

for (x in mod1$frame$var) {
  if(as.character(x) != "<leaf>" & i < length(toss2)){
    toss3 <- toss2[(i+1):length(toss2)]
    
    mod1split <- snip.rpart(mod1, toss = toss3)
    mod1splitv <- c(mod1splitv, mod1split)
    
    mod1strain <- predict(mod1split, df1train[,-c(3)], type = "class")
    mod1strainv <- c(mod1strainv, mod1strain)
    mod1svalid <- predict(mod1split, df1valid[,-c(3)], type = "class")
    mod1svalidv <- c(mod1svalidv, mod1svalid)
    
    ErrTrain <- mean(mod1strain != df1train$Promoffer)
    ErrTrainv <- c(ErrTrainv, ErrTrain)
    Errvalid <- mean(mod1svalid != df1valid$Promoffer)
    Errvalidv <- c(Errvalidv, Errvalid)
  }
  i <- i + 1
}



# error rates vs no. of splits
DF <- data.frame("#Decision Nodes" = 1:159, "Error Training" = ErrTrainv,
                 "Error Validation" = Errvalidv, check.names = F)

DF


# Tree after last snip
prp(mod1split, varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0)
nrow(mod1split$frame)


# plot of error rates vs. number of splits
nsplits <- 1:159
par(mar = c(1.5,1.5,1.5,1.5)+3)
plot(smooth.spline(nsplits,100*ErrTrainv), type = "l", xlab = "Number of splits",
     ylab = "Error rates")

lines(smooth.spline(nsplits,100*Errvalidv))

margin(0,0,0,0)


# minimum error tree and best pruned tree
min(Errvalidv)
METree <- min(nsplits[which(Errvalidv==min(Errvalidv))])
METree #6

# std. err
sqrt(var(Errvalidv)/length(Errvalidv))

# best pruned tree near first minima : within  1 std. err.
met1std <- min(Errvalidv)+sqrt(var(Errvalidv)/length(Errvalidv))
met1std

BPT <- DF[which(Errvalidv > min(Errvalidv) &
                Errvalidv < met1std &
                  nsplits < METree),]

BPT # na

toss3 <- toss2[(METree+1) : length(toss2)]
mod1best <- snip.rpart(mod1, toss = toss3)
prp(mod1best, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0, 
    split.cex = 0.8, under.cex = 0.8)


bmodtrain1 <- predict(mod1best, df1train[,-c(3)], type = "class")
table("Predicted value" = bmodtrain1, "Actual Value" = df1train$Promoffer)


# classification accuracy # 0.9108
mean(bmodtrain1 == df1train$Promoffer)
# misclassification error # 0.0892
mean(bmodtrain1 != df1train$Promoffer)


# on validation partition
bmodvalid1 <- predict(mod1best, df1valid[,-c(3)], type = "class")
table("Predicted value" = bmodvalid1, "Actual Value" = df1valid$Promoffer)

# classification accuracy # 0.898
mean(bmodvalid1 == df1valid$Promoffer)
# misclassification error # 0.102
mean(bmodvalid1 != df1valid$Promoffer)


# on test partition
bmodtest1 <- predict(mod1best, df1test[,-c(3)], type = "class")
table("Predicted value" = bmodtest1, "Actual Value" = df1test$Promoffer)

# classification accuracy # 0.902
mean(bmodtest1 == df1test$Promoffer)
# misclassification error # 0.098
mean(bmodtest1 != df1test$Promoffer)

###################################################################################################
###################################################################################################


# Pruning ------------------------------------------------------------------------------------
# ---------------------based on sequence in which nodes are actually created using Rpart 
# (ie. considering the original sequence in which decision tree was created)

# validation partition : misclassification error vs. no. of decision nodes
# total no of nodes in a full grown tree
nrow(mod1$frame)

#number of decision nodes
# nrow(mod1$splits) # this statement is wrong

# number of terminal nodes
o <- mod1$frame$var=="<leaf>"
oo <- mod1$frame[o,]
nrow(oo) # number of terminal node

# number of decision nodes
nrow(mod1$frame) - nrow(oo) # number of terminal node


# node numbers
toss1 <- as.integer(row.names(mod1$frame))
DFP <- data.frame(toss1, mod1$frame$var, mod1$frame$complexity)
DFP
DFP1 <- DFP[DFP$mod1.frame.var != "<leaf>",]
DFP1

# nested sequence of splits based on complexity
DFP2 <- DFP1[order(DFP1$mod1.frame.complexity, decreasing = T),]
DFP2
rownames(DFP2) <- 1 : nrow(DFP2)
DFP2

toss2 <- DFP2$toss1

# counter of nodes to be sniffed off
i <- 1
mod1splitv <- NULL
mod1strainv <- NULL
mod1svalidv <- NULL
ErrTrainv <- NULL
Errvalidv <- NULL
length(DFP2$mod1.frame.var)

for (x in DFP2$mod1.frame.var){
  if(i <= length(toss2)){
    toss3 <- toss2[i : length(toss2)]
    
    mod1split <- snip.rpart(mod1, toss = toss3)
    
    # now cut down the CP table
    temp <- pmax(mod1$cptable[,1], DFP2$mod1.frame.complexity[i])
    keep <- match(unique(temp), temp)
    mod1split$cptable <- mod1$cptable[keep, ,drop = F]
    mod1split$cptable[max(keep), 1] <- DFP2$mod1.frame.complexity[i]
    
    # reset variable importance
    mod1split$variable.importance <- importance(mod1split)
    # NOTE : DEFINITION OF impportance FUNCTION IS DEFINED BELOW IN THE FILE
    # (not available to be directly used by the programmer that's why source code is to be copied)
    
    mod1splitv <- list(mod1splitv, mod1split)
    
    mod1strain <- predict(mod1split, df1train[,-c(3)], type = "class")
    mod1strainv <- list(mod1strainv, mod1strain)
    mod1svalid <- predict(mod1split, df1valid[,-c(3)], type = "class")
    mod1svalidv <- list(mod1svalidv, mod1svalid)
    
    ErrTrain <- mean(mod1strain != df1train$Promoffer)
    ErrTrainv <- c(ErrTrainv, ErrTrain)
    ErrValid <- mean(mod1svalid != df1valid$Promoffer)
    Errvalidv <- c(Errvalidv, ErrValid)
  }
  i <- i + 1
}

# Error rates vs no of splits
DF <- data.frame("# decision nodes" = 1: 159 , "Error Training" = ErrTrainv,
                 "Error Validation" = Errvalidv, check.names = F)
DF

# Tree after last snip
prp(mod1split, varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0)
nrow(mod1split$frame)
nrow(mod1split$frame[mod1split$frame$var=="<leaf>",])
nrow(mod1split$frame) - nrow(mod1split$frame[mod1split$frame$var=="<leaf>",])



# plot of error rates vs. number of splits
nsplits <- 1:159
plot(smooth.spline(nsplits, 100*ErrTrainv), type = "l",
     xlab = "Number of splits", ylab = "Error rate")
lines(smooth.spline(nsplits, 100*Errvalidv))


# minimum error tree and best pruned tree
min(Errvalidv)
METfinal <- min(nsplits[which(Errvalidv == min(Errvalidv))]) 
METfinal

# std err
sqrt(var(Errvalidv)/length(Errvalidv))

# Best pruned tree near first minima : within 1 std error
METfinalstd <- min(Errvalidv) + sqrt(var(Errvalidv)/length(Errvalidv))

BPT <- DF[which(Errvalidv > min(Errvalidv) &
                  Errvalidv < METfinalstd &
                  nsplits < METfinal),][1,1]

if(is.na(BPT)) BPT <- METfinal

toss3 <- toss2[(BPT + 1):length(toss2)]
mod1best <- snip.rpart(mod1, toss = toss3)
#prp(mod1best, type = 0, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T, Margin = 0, 
#    digits = 0)

prp(mod1best, type = 0, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T, Margin = 0,
    digits = 0, split.cex = 0.8, under.cex = 0.8)


bmodtrain1 <- predict(mod1best, df1train[,-c(3)], type = "class")
table("predicted value" = bmodtrain1, "Actual value" = df1train$Promoffer)

#classification accuracy #0.926
mean(bmodtrain1 == df1train$Promoffer)
# misclassification error #0.0724
mean(bmodtrain1 != df1train$Promoffer)


bmodvalid1 <- predict(mod1best, df1valid[,-c(3)], type = "class")
table("predicted value" = bmodvalid1, "Actual value" = df1valid$Promoffer)

#classification accuracy #0.917
mean(bmodvalid1 == df1valid$Promoffer)
# misclassification error #0.082
mean(bmodvalid1 != df1valid$Promoffer)


bmodtest1 <- predict(mod1best, df1test[,-c(3)], type = "class")
table("predicted value" = bmodtest1, "Actual value" = df1test$Promoffer)

#classification accuracy #0.91
mean(bmodtest1 == df1test$Promoffer)
# misclassification error #0.09
mean(bmodtest1 != df1test$Promoffer)



# complexity value for best pruned tree
cpbest <- DFP2$mod1.frame.complexity[BPT]
cpbest
DFP2[DFP2$mod1.frame.complexity == cpbest,]


# pruning through default function from Rpart
bmod <- prune(mod1, cp = cpbest)
prp(bmod, type = 0, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T, Margin = 0,
    digits = 0, split.cex = 0.8, under.cex = 0.8)

# complexity value form minimum error tree
cpmet <- DFP2$mod1.frame.complexity[METfinal]
cpmet 
DFP2[DFP2$mod1.frame.complexity == cpmet,]

mmod <- prune(mod1, cp = cpmet)
prp(mmod, type = 0, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T, Margin = 0,
    digits = 0, split.cex = 0.8, under.cex = 0.8)





# Pruning ------------------------------------------------------------------------------------
# ---------------------based on sequence in which nodes are actually created using Rpart 
# ( but also rectifying the flaws in the rpart algo from analysis point of view )

# validation partition : misclassification error vs. no. of decision nodes
# total no of nodes in a full grown tree
nrow(mod1$frame)

#number of decision nodes
# nrow(mod1$splits) # this statement is wrong

# number of terminal nodes
o <- mod1$frame$var=="<leaf>"
oo <- mod1$frame[o,]
nrow(oo) # number of terminal node

# number of decision nodes
nrow(mod1$frame) - nrow(oo) # number of terminal node


# node numbers
toss1 <- as.integer(row.names(mod1$frame))
DFP <- data.frame("toss" = toss1, "Svar" = mod1$frame$var, "CP" =  mod1$frame$complexity)
DFP
DFP1 <- DFP[DFP$Svar != "<leaf>",]
DFP1

# Nested sequence of splits based on complexity
DFP2 <- DFP1[order(DFP1$CP, -DFP1$toss, decreasing = T),]
length(DFP2$Svar)

rownames(DFP2) <- 1 : nrow(DFP2)

toss2 <- DFP2$toss

# counter of nodes to be snipped off
i <- 1
mod1splitv <- list()
mod1strainv <- list()
mod1svalidv <- list()
ErrTrainv <- NULL
Errvalidv <- NULL


for (x in DFP2$Svar) {
  
  if (i <= length(toss2)) {
    toss3 <- toss2[i:length(toss2)]
    
    mod1split <- snip.rpart(mod1, toss = toss3)
    
    ## Now cutting down the CP table
    temp <- pmax(mod1$cptable[,1], DFP2$CP[i])
    keep <- match(unique(temp), temp)
    mod1split$cptable <- mod1$cptable[keep, ,drop = F]
    mod1split$cptable[max(keep), 1] <- DFP2$CP[i]
    
    # resetting the variable importance
    mod1split$variable.importance <- importance(mod1split)
    
    mod1splitv[i] <- list(mod1split)
    
    mod1strain <- predict(mod1split, df1train[,-c(3)], type = "class")
    mod1strainv[i] <- list(mod1strain)
    mod1svalid <- predict(mod1split, df1valid[,-c(3)], type = "class")
    mod1svalidv[i] <- list(mod1svalid)
    
    ErrTrain <- mean(mod1strain != df1train$Promoffer)
    ErrTrainv <- c(ErrTrainv, ErrTrain)
    ErrValid <- mean(mod1svalid != df1valid$Promoffer)
    Errvalidv <- c(Errvalidv, ErrValid)
    
    
    
    
  }
  i <- i + 1
  
}

DF <- data.frame("# decision nodes" = 0: 158 , "Error Training" = ErrTrainv,
                 "Error Validation" = Errvalidv, check.names = F)
DF


AUX <- data.frame("# decision nodes" = 159 , "Error Training" = mean(mod1train != df1train$Promoffer),
                 "Error Validation" = mean(mod1valid != df1valid$Promoffer), check.names = F)

# addition of last node info to DF ie for full grown tree
AUX <- data.frame("# decision nodes" = 159 , "Error Training" = mean(mod1train != df1train$Promoffer),
                  "Error Validation" = mean(mod1valid != df1valid$Promoffer), check.names = F)


DF <- rbind(DF, AUX)
DF


# Tree after last snip
prp(mod1split, varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0)
nrow(mod1split$frame)
nrow(mod1split$frame[mod1split$frame$var=="<leaf>",])
nrow(mod1split$frame) - nrow(mod1split$frame[mod1split$frame$var=="<leaf>",])


# Tree after first snip
prp(mod1splitv[[1]], varlen = 0, cex = 0.7, extra = 0, compress = T, Margin = 0, digits = 0)
nrow(mod1splitv[[1]]$frame)
nrow(mod1splitv[[1]]$frame[mod1splitv[[1]]$frame$var=="<leaf>",])
nrow(mod1splitv[[1]]$frame) - nrow(mod1splitv[[1]]$frame[mod1splitv[[1]]$frame$var=="<leaf>",])


# plot of error rate vs no of splits
range(100 * DF[,2])
range(100 * DF[,3])

plot(smooth.spline(DF[,1], 100 * DF[,2]), type = "l",
     xlab = "Number of splits", ylab = "Error rate")
lines(smooth.spline(DF[,1], 100 * DF[,3]))



# Minimum error tree and best pruned tree
min(DF[,3])
MET <- min(DF[which(DF[,3] == min(DF[,3])), 1])

# std err
sqrt(var(DF[,3]) / length(DF[,3]))

# best pruned tree near first minima : within 1 std err

met1std <- min(DF[,3]) + sqrt(var(DF[,3]) / length(DF[,3]))

BPT <- DF[which(DF[,3] > min(DF[,3]) &
                  DF[,3] < met1std &
                  DF[,1] < MET),][1,1]

if (is.na(BPT)) BPT = MET

toss3 <- toss2[(BPT + 1):length(toss2)]

##############################################################################################
mod1best <- snip.rpart(mod1, toss = toss3) ######### THIS IS THE BEST PERFORMING MODEL #######
##############################################################################################

prp(mod1best, type = 0, extra = 1, under = T, varlen = 0, cex = 0.5, compress = T, Margin = 0,
    digits = 0, split.cex = 1.0, under.cex = 1.0)


bmodtrain1 <- predict(mod1best, df1train[,-c(3)], type = "class")
table("predicted value" = bmodtrain1, "Actual value" = df1train$Promoffer)

#classification accuracy #0.9276
mean(bmodtrain1 == df1train$Promoffer)
# misclassification error #0.0724
mean(bmodtrain1 != df1train$Promoffer)


bmodvalid1 <- predict(mod1best, df1valid[,-c(3)], type = "class")
table("predicted value" = bmodvalid1, "Actual value" = df1valid$Promoffer)

#classification accuracy #0.917
mean(bmodvalid1 == df1valid$Promoffer)
# misclassification error #0.082
mean(bmodvalid1 != df1valid$Promoffer)


bmodtest1 <- predict(mod1best, df1test[,-c(3)], type = "class")
table("predicted value" = bmodtest1, "Actual value" = df1test$Promoffer)

#classification accuracy #0.91
mean(bmodtest1 == df1test$Promoffer)
# misclassification error #0.09
mean(bmodtest1 != df1test$Promoffer)





#------------------------------------------------comment are for my future reference

# possible complexity parameter based prunings based on 
# cross validated predictions
printcp(mod1)

# rel error is the ratio of incorrectly classified traiining records
# after doing a split to incorrectly classified training records
# at the root node (naive  rule)

# change xval (default value = 10)
# pruning using rpart's prune

mod2 <- rpart(Promoffer ~ ., method = "class", data = df1train,
              control = rpart.control(cp = 0, minsplit = 2, minbucket = 1,
                                      maxcompete = 0, maxsurrogate = 0,
                                      xval = 10))

mod2$cptable
#  xerror  contains estimates of cross-validated prediction error
# for different numbers of splits (nsplit)
# below is the cp value corresponding to minimum xerror value
mod2$cptable[which.min(mod2$cptable[,"xerror"]),]

par(mar = c(5.1,5.1,5.1,5.1) - 5)
cp1 <- mod2$cptable[which.min(mod2$cptable[,"xerror"]),"CP"]
plotcp(mod2)

pmod <- prune(mod2, cp = cp1) 


prp(pmod, type = 0, extra = 1, under = T, varlen = 0, cex = 0.5, compress = T, Margin = 0,
    digits = 0, split.cex = 1.0, under.cex = 1.0)
# not useful in this case



#=================================================================================================
#-------------------------------------------------------------------------------------------------
#.................................................................................................





#saving best performing model
save(mod1best, file = file.choose(new = T))

# generating PMML
library("pmml")
xmlmodel <- pmml(mod1best, model.name="RPart_Model",
     app.name="Rattle/PMML",
     description="RPart Decision Tree Model",
     copyright=NULL, transforms=NULL,
     unknownValue=NULL, dataset=NULL)
# Save to an external file 
savePMML(xmlmodel, "RpartModel.pmml")




#.................................................................................................
#-------------------------------------------------------------------------------------------------
#=================================================================================================
##################################################################################################


#---------------------------------------------------------------------------------------------
# this part is the source code from github repository of Rpart
# (not available to be directly used by the programmer that's why source code is to be copied)
#---------------------------------------------------------------------------------------------
# Caclulate variable importance
# Each primary split is credited with the value of splits$improve
# Each surrogate split gets split$adj times the primary split's value
#
# Called only internally by rpart (not available to be directly used by the programmer tha)
#
importance <- function(fit)
{
  ff <- fit$frame
  fpri <- which(ff$var != "<leaf>")  # points to primary splits in ff
  spri <- 1 + cumsum(c(0, 1 + ff$ncompete[fpri] + ff$nsurrogate[fpri]))
  spri <- spri[seq_along(fpri)] # points to primaries in the splits matrix
  nsurr <- ff$nsurrogate[fpri]  # number of surrogates each has
  
  sname <- vector("list", length(fpri))
  sval <- sname
  
  ## The importance for primary splits needs to be scaled
  ## It was a printout choice for the anova method to list % improvement in
  ##  the sum of squares, an importance calculation needs the total SS.
  ## All the other methods report an unscaled change.
  scaled.imp <- if (fit$method == "anova")
    fit$splits[spri, "improve"] * ff$dev[fpri]
  else fit$splits[spri, "improve"]
  
  sdim <- rownames(fit$splits)
  for (i in seq_along(fpri)) {
    ## points to surrogates
    if (nsurr[i] > 0L) {
      indx <- spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
      sname[[i]] <- sdim[indx]
      sval[[i]] <- scaled.imp[i] * fit$splits[indx, "adj"]
    }
  }
  
  import <- tapply(c(scaled.imp, unlist(sval)),
                   c(as.character(ff$var[fpri]), unlist(sname)),
                   sum)
  sort(c(import), decreasing = TRUE) # a named vector
}















































































