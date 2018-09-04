# hypothetical example
FatScore = c(0.2,0.1,0.2,0.2,0.4,0.3)
SaltScore = c(0.9,0.1,0.4,0.5,0.5,0.8)
Acceptance = c(1,0,0,0,1,1)

# Neural Network Structure
# Input layer: two nodes (for two predictors) - nodes 1 and 2
# Hidden Layer: one node - node 3,4,5and 6
# Output Layer: one node - node 6

# i,j and Wij initialization(random number in the range: 0.00 +- 0.05)
bias = matrix(runif(4, -0.05, 0.05), ncol = 1, dimnames = list(c(3,4,5,6), c("THeTA,")))
bias

weightsIH = matrix(runif(6, -0.05, 0.05), ncol = 3, dimnames = list(c(1,2), c(3,4,5)))
weightsHO = matrix(runif(3, -0.05, 0.05), ncol = 1, dimnames = list(c(3,4,5), c(6)))

# computing output value for first record
# (FastScore = 0.2, Saltscore = 0.9)
output = NULL
k = 1 # first observation
for(i in 1:length(bias)-1){
  x = bias[i,1] + weightsIH[1,i] * FatScore[k] + weightsIH[2,i] * SaltScore[k]
  output[i] = 1/(1 + exp(-x))
}
i = i + 1 # node increment
j = 1

x = bias[i,1] + weightsHO[1,j] * output[1] + weightsHO[2,j] * output[2] + weightsHO[3,j] * output[3]
output[4] = 1/(1 + exp(-x))
output


# classify first record using cutoff value = 0.5
ifelse(output[4]>0.5, 1,0) #predicted value
Acceptance[1] # actual value

# model for hypothetocal data using library
library(neuralnet)
install.packages("neuralnet")


df = data.frame(FatScore,SaltScore,Acceptance)
str(df)


# Neural Network model
# startweights vector: no. of all bias (4) and connection weight values (9)
# linear.outputs = T for predction
# linear.outputs  = F for classification

mod = neuralnet(Acceptance ~ FatScore + SaltScore, df,hidden = c(3), startweights = runif(13, -0.05, 0.05),
                rep = 1,  algorithm = "backprop", learningrate = 0.1, err.fct = "sse", linear.output = F)
mod$result.matrix[1:3,1]

mod$result.matrix

# inter layer connection weights
# input layer to hidden layer connections
dimnames(mod$weights[[1]][[1]]) = list(c("bias", "node1:fat", "node2:salt"), c("node3","node4","node5"))

# hidden layer to output layer connections
dimnames(mod$weights[[1]][[2]]) = list(c("bias", "node3", "node4", "node5"), c("node6:accept"))

# classify training records
modtrainc = ifelse(mod$net.result[[1]][,1]>0.5, 1, 0)
modtrainc = unname(modtrainc)

data.frame("predicted class" = modtrainc, "Actual class" = df$Acceptance, 
           "predicted value" = mod$net.result[[1]][,1], "fat" = df$FatScore, "salt" = df$SaltScore)

# classification matrix
table("Actual class" = df$Acceptance, "predicted class" = factor(modtrainc, levels = c("0","1")))

# classification accuracy
mean(modtrainc == df$Acceptance)
# misclassification accuracy
mean(modtrainc != df$Acceptance)

# network diagram
k = plot(mod)


