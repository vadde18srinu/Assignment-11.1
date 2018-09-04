1. Use the given link and locate the bank marketing dataset. Data Set Link
   Perform the below operations:
   library(ggplot2)
   library(knitr)
   library(cluster)
   library(HSAUR)
   library(fpc)
   library(lattice)
   library(rpart)
   library(kernlab)
   library(randomForest)
     
     
setwd("F:/AcadGild/Workings")   
  
library(readr)   
   
#bank full data
Bank_full<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank/bank-full.csv",delim=";",
                 escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
View(Bank_full)
str(Bank_full)
class(Bank_full)

#bank data
Bank<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank/bank.csv",delim=";",
                 escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
View(Bank)
str(Bank)
class(Bank)


#bank additional full data
bank_addl_full<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank-additional/bank-additional/bank-additional-full.csv",delim=";",
                           escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)    

head(bank_addl_full)
str(bank_addl_full)
names(bank_addl_full)

#bank additional data
bank_addl<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank-additional/bank-additional/bank-additional.csv",delim=";",
                    escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)    
head(bank_addl)
str(bank_addl)



(a). Create a visual for representing missing values in the dataset.

library(stringr)
library(reshape2)
library(readr)
library(dplyr)

is.na(bank_addl_full)

sapply(bank_addl_full, function(x)all(is.na(x)))

library(mice)
#missing values as visualized. 
md.pattern(bank_addl_full)

# no missing values
md.pattern(bank_addl)

#no missing values
md.pattern(Bank_full)

#no missing values
md.pattern(Bank)


# missing values frequecy
aggr_plot <- aggr(bank_addl_full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bank_addl_full), cex.axis=.7, 
                                                            gap=3, ylab=c("Histogram of missing data","Pattern"))

library(VIM)
# missing values count
counts <- table(bank_addl_full$y)
barplot(counts,col=c("darkblue","red"),legend = rownames(counts), main = "missing")


(b). Show a distribution of clients based on a Job.

## The observation categories of the bank_additional_full dataset:
head(bank_addl_full)
str(bank_addl_full)
names(bank_addl_full)
class(bank_addl_full)

summary(bank_addl_full)
dim(bank_addl_full)

# Selecting observations to determine cluster parameters(charecter to numeric)
JC<- data.frame(as.numeric(as.factor(bank_addl_full$job)),
                                as.numeric(as.factor(bank_addl_full$campaign)))
JC

# Rename the columns
colnames(JC) <- c("Job", "campaign")

# Reduce the amount of dataset records for legibility within clusters
JC1 <- JC[sample(nrow(JC),500),]
JC1

# Kmeans clustering to create 5 clusters
set.seed(12345)
JC2 <- kmeans(JC1, centers=5)
JC2

library(dplyr)
library(MASS)

#chisqtest
df<-table(JC$default,JC$job)
df
chisq.test(df)




(c). Check whether is there any relation between Job and Marital Status?
  
# Selecting observations to determine cluster parameters(charecter to numeric)
MJ<- data.frame(as.numeric(as.factor(bank_addl_full$marital)),
                    as.numeric(as.factor(bank_addl_full$job)))
MJ

# Rename the columns
colnames(MJ) <- c("marital", "job")

# Reduce the amount of dataset records for legibility within clusters
MJ1 <- MJ[sample(nrow(BankAdditional2),500),]
MJ1

# Kmeans clustering to create 5 clusters
set.seed(12345)
MJ2 <- kmeans(MJ1, centers=5)
MJ2

#chisqtest
df<-table(MJ$default,MJ$job)
df
chisq.test(df)


(d). Check whether is there any association between Job and Education?

# Selecting observations to determine cluster parameters(charecter to numeric)
EJ <- data.frame(as.numeric(as.factor(bank_addl_full$education)),
                                as.numeric(as.factor(bank_addl_full$job)))
EJ

# Rename the columns
colnames(EJ) <- c("education", "job")

# Reduce the amount of dataset records for legibility within clusters
EJ1 <- EJ[sample(nrow(EJ),500),]
EJ1

# Kmeans clustering to create 5 clusters
set.seed(12345)
EJ2 <- kmeans(EJ1, centers=5)
EJ2

#chisqtest
df<-table(EJ$default,EJ$job)
df
chisq.test(df)  
