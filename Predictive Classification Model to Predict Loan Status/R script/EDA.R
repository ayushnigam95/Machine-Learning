Sys.getenv()
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre-9.0.4")
load(file = file.choose())


df <- read.csv(file = file.choose(), header = T)
rm(df)

dftrain <- df
dftest <- df
str(dftest)
str(dftrain)




# bar plot for gender
countgender <- table(dftrain$Gender)
countgender
gendernames <- dimnames(countgender)
gendernames
par(mar = c(1.5,1.5,1.5,1.5)+3)
barplot(countgender, names.arg = gendernames[[1]], xlab = "Gender", ylab = "Number", ylim = c(0,250),las = 1,
        cex.names = 0.7)

fcountg <- as.data.frame(countgender)
fcountg

library(ggplot2)
genderplot <- ggplot(fcountg, aes(Var1, Freq)) + geom_bar(stat = "identity",width = 0.5, fill ="steelblue") + theme(plot.margin = margin(2,2,2,2,"cm")) +labs(title = "plot (gender)", x = "Gender", y = "Count")
genderplot


# bar plot for married
countmarried <- table(dftrain$Married)
as.data.frame(countmarried)

marriedplot <- ggplot(as.data.frame(countmarried), aes(Var1, Freq)) + geom_bar(stat = "identity",width = 0.5, fill ="steelblue") + theme(plot.margin = margin(2,2,2,2,"cm")) +labs(title = "plot (Married)", x = "Married", y = "Count")
marriedplot


# bar plot for Education
counteducation <- table(dftrain$Education)
as.data.frame(counteducation)

educationplot <- ggplot(as.data.frame(counteducation), aes(Var1, Freq)) + geom_bar(stat = "identity",width = 0.5, fill ="steelblue") + theme(plot.margin = margin(2,2,2,2,"cm")) +labs(title = "plot (education)", x = "Education", y = "Count")
educationplot


# bar plot for self employed
countemployed <- table(dftrain$Self_Employed)
as.data.frame(counteducation)

employmentplot <- ggplot(as.data.frame(countemployed), aes(Var1, Freq)) + geom_bar(stat = "identity",width = 0.5, fill ="steelblue") + theme(plot.margin = margin(2,2,2,2,"cm")) +labs(title = "plot (self employment)", x = "Employment", y = "Count")
employmentplot

# bar plot for Property_Area
countPropertyArea <- table(dftrain$Property_Area)
as.data.frame(countPropertyArea)

PropertyAreaplot <- ggplot(as.data.frame(countPropertyArea), aes(Var1, Freq)) + geom_bar(stat = "identity",width = 0.5, fill ="steelblue") + theme(plot.margin = margin(2,2,2,2,"cm")) +labs(title = "plot (Property Area)", x = "Property Area", y = "Count")
PropertyAreaplot

# bar plot for loan Amount
LoanAmountplot <- ggplot(data=dftrain, aes(dftrain$LoanAmount)) + geom_histogram(fill ="steelblue", binwidth = 7) + theme(plot.margin = margin(2,2,2,2,"cm")) + theme(plot.margin = margin(1,1,1,1,'cm')) + labs(title = "Histogram for Loan Amount", x = "Loan Amount")
LoanAmountplot

# converting data frame to numeric for pca and correlation analysis
str(dftrain)
dfnum <- dftrain
dfnum$Gender <- as.integer(dfnum$Gender) 
dfnum$Married <- as.integer(dfnum$Married)
dfnum$Education <- as.integer(dfnum$Education)
dfnum$Loan_Status <- as.integer(dfnum$Loan_Status)
dfnum$Property_Area <- as.integer(dfnum$Property_Area)
dfnum$Self_Employed <- as.integer(dfnum$Self_Employed)
str(dfnum)

# principle component Analysis

pcaResult <- prcomp(dfnum[,c(2:13)])
pcaResult$rotation

###############################################################################################
#                           Results from principal component analysis
###############################################################################################
#                         PC1           PC2   
#Gender            -1.879542e-06 -2.391847e-05
#Married           -2.818853e-06 -2.004374e-05
#Dependents        -2.348916e-05 -7.272243e-06
#Education          9.091475e-06  1.451416e-05
#Self_Employed     -1.033185e-05 -3.048763e-06
#ApplicantIncome   -9.978190e-01 -6.558862e-02
#CoapplicantIncome  6.564553e-02 -9.978134e-01
#LoanAmount        -6.914984e-03 -8.154964e-03
#Loan_Amount_Term   1.219407e-04  1.845990e-04
#Credit_History     3.464203e-06  2.284859e-06
#Property_Area      7.269117e-06  3.171926e-07
#Loan_Status        3.379295e-06  9.881395e-06
#################################################################################################

# correalation matrix
str(dfnum[,c(2:13)])
cormatelements <- dfnum[,c(2:13)]
str(cormatelements)
cormat <- cor(cormatelements, use = 'everything', method = "pearson")
cormatround <- round(cormat,2)
cormatround

# reordering the correlation matrix elementa
cormatround <- reorder_correlation_matrix(cormatround)


cormatround_upper <- get_upper_tri(cormatround)
cormatround_upper

# melting the cormat round upper
library(reshape2)
melted_cormatround_upper <- melt(cormatround_upper, na.rm = T) 
melted_cormatround

correlation_plot <- ggplot(data = melted_cormatround_upper, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()


correlation_plot  + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



###########
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# function for reordering correlation matrix
reorder_correlation_matrix <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


