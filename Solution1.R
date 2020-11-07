# Import required libraries
library(data.table)
library(ggplot2)
library(DMwR)
library(dplyr)

# Read all data
data <- fread("Financial_Data.csv")

#Renaming V1 to ticker
setnames(data, "V1", "Ticker")
setnames(data, "PRICE VAR[%]", "Price Variation")

# Convert Class to categorical variable
data$Class <- factor(data$Class)

# Filter only technology stocks
data <- data[`Sector` == "Technology"]

# Create vector of column names that we require
useful.columns <- c("Ticker",
                    "Book Value per Share",
                    "PE ratio",
                    "PB ratio",
                    "Enterprise Value over EBITDA",
                    "priceEarningsToGrowthRatio",
                    "returnOnEquity",
                    "assetTurnover",
                    "Dividend Yield",
                    "interestCoverage",
                    "Free Cash Flow Yield",
                    "EPS Diluted Growth",
                    "debtEquityRatio",
                    "currentRatio",
                    "Market Cap",
                    "EBITDA Margin",
                    "Profit Margin",
                    'operatingCashFlowSalesRatio',
                    'Revenue Growth',
                    "Price Variation",
                    "Class")

# Keep only useful columns
data <- subset(data, select = useful.columns)
# Left with 3126 rows and 21 columns


# Data Cleaning -----------------------------------------------------------------------------------------------------------

# Checking number of NA values row-wise & visualize it on a bar plot
NA_row <- data.table(seq(nrow(data)), rowSums(is.na(data)))[order(-V2)]
NA_row
ggplot(NA_row[V2 > 0], aes(V2)) + geom_bar(fill= "red") + 
        labs(y = "Number of rows affected", x = "Number of NAs", title = "Number of NAs vs Row") + theme_classic()

# We remove rows that have more than 50% NA column values as 
# there is insufficient information regarding that particular stock to help the model
# In this cleaning step, we removed 81 row entries (3126 -> 3045)
data <- data[which(rowMeans(is.na(data)) <= 0.5), ]

# Checking number of NA values column-wise
NA_col <- data.table(colnames(data), colSums(is.na(data)))[-(1),][order(-V2)] #Removing the column name for ticker, reorder the number of NA in descending order
NA_col
ggplot(NA_col, aes(V1, V2)) + geom_col(fill = "blue")  + 
        labs(x = "Column Names", y = "Number of NAs", title = "Number of NAs vs Columns") + 
        coord_flip()

# We remove columns that have more than 20% NA values as 
# there is insufficient information regarding that particular stock to help the model
# In this cleaning step, we removed 1 column (priceEarningsToGrowthRatio)
data <- subset(data, select = colSums(is.na(data)) < 0.2 * nrow(data))

# Removing illogical data
# Ticker "IMOS" is removed as it has a market cap of 0.0 but it has a EPS of >$50/share which does not make sense
data <- data[`Market Cap` != 0 | is.na(`Market Cap`)]

# Remove duplicate data
data <- distinct(data)

#Removing extreme data points that can skew our models
boxplot(data$`Book Value per Share`, main = "Book Value per Share", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- data[`Book Value per Share` > 20000] # Removed Ticker: INPX

boxplot(data$`PE ratio`, main = "PE ratio", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE) #No extreme values

boxplot(data$`PB ratio`, main = "PB ratio", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE) # No extreme values

boxplot(data$`Enterprise Value over EBITDA`, main = "Enterprise Value over EBITDA", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Enterprise Value over EBITDA` > 2500]) # Removed Ticker: SYNC

boxplot(data$`returnOnEquity`, main = "returnOnEquity", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`returnOnEquity` > 1300]) # Removed Ticker: MOXC

boxplot(data$`assetTurnover`, main = "assetTurnover", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`assetTurnover` > 10]) # Removed Ticker: HMI

boxplot(data$`Dividend Yield`, main = "Dividend Yield", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE)
outliers <- rbind(outliers, data[`Dividend Yield` > 5]) # Removed Ticker: RENN

boxplot(data$`interestCoverage`, main = "interestCoverage", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`interestCoverage` < -100000]) # Removed Ticker: MOXC

boxplot(data$`Free Cash Flow Yield`, main = "Free Cash Flow Yield", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Free Cash Flow Yield` < -80]) # Removed Ticker: BW, ABIL

boxplot(data$`EPS Diluted Growth`, main = "EPS Diluted Growth", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`EPS Diluted Growth` < -1000]) #Removed Ticker: ICHR

boxplot(data$`debtEquityRatio`, main = "debtEquityRatio", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`debtEquityRatio` < -150]) # Removed Tickers: SGH, MOXC, INAP

boxplot(data$`currentRatio`, main = "currentRatio", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`currentRatio` > 60]) # Removed Tickers: MSN, LORL

boxplot(data$`Market Cap`, main = "Market Cap", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Market Cap` > 1e+12]) # Removed Tickers: AAPL

boxplot(data$`EBITDA Margin`, main = "EBITDA Margin", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`EBITDA Margin` < -800]) # Removed Ticker: PRKR, COCP
outliers <- rbind(outliers, data[`EBITDA Margin` > 200]) # Removed Ticker: CCUR

boxplot(data$`Profit Margin`, main = "Profit Margin", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Profit Margin` < -800]) # Removed Ticker: PRKR
outliers <- rbind(outliers, data[`EBITDA Margin` > 200]) # Removed Ticker: CCUR

boxplot(data$`operatingCashFlowSalesRatio`, main = "operatingCashFlowSalesRatio", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`operatingCashFlowSalesRatio` < -200]) # Removed Tickers: MOXC, PRKR, VHC

boxplot(data$`Revenue Growth`, main = "Revenue Growth", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Revenue Growth` > 100]) # Removed Tickers: HEAR,PRKR

boxplot(data$`Price Variation`, main = "Price Variation", ylab = "Value",
        las = 1, border = "brown", horizontal = FALSE, notch = TRUE)
outliers <- rbind(outliers, data[`Price Variation` < -60000]) # Removed Ticker: BW
outliers <- rbind(outliers, data[`Price Variation` > 10000]) # Removed Ticker: LN, BKI, RUN, AMRH, HMI

outliers <- distinct(outliers) 

cleaned.columns <- c("Book Value per Share",
                    "PE ratio",
                    "PB ratio",
                    "Enterprise Value over EBITDA",
                    "returnOnEquity",
                    "assetTurnover",
                    "Dividend Yield",
                    "interestCoverage",
                    "Free Cash Flow Yield",
                    "EPS Diluted Growth",
                    "debtEquityRatio",
                    "currentRatio",
                    "Market Cap",
                    "EBITDA Margin",
                    "Profit Margin",
                    'operatingCashFlowSalesRatio',
                    'Revenue Growth',
                    "Price Variation")
data_cleaned <- anti_join(data, outliers, by = cleaned.columns)


# Data Exploration -----------------------------------------------------------------------------------------------------

# Check data and do some data exploration
summary(data_cleaned)
str(data_cleaned)

# Distribution of Class
ggplot(data_cleaned, aes(Class)) + geom_bar(fill= "red") + 
        labs(y = "Number of stocks", x = "Class", title = "Distribution of Class") + theme_bw()
round(summary(data_cleaned$Class)/nrow(data_cleaned) * 100)
# 57% of stocks with class "1" and 43% of stocks with class "0"

#Distribution of Class based on Market Capitalization
market_cap_cat <-cut(data_cleaned$`Market Cap`,breaks = c(0,2e+9,1e+10,1e+15),labels =c("Small","Mid","Large") )
data_cleaned$market_cap_cat = market_cap_cat
ggplot(subset(data_cleaned, !is.na(market_cap_cat)), aes(market_cap_cat)) + geom_bar(fill= "red") + 
        labs(y = "Number of stocks", x = "Market Cap", title = "Distribution of Market Cap") + theme_bw()

ggplot(subset(data_cleaned, !is.na(market_cap_cat)), aes(x=market_cap_cat,fill=Class)) + 
        labs(y = "Percentage of Class", x = "Market Cap", title = "Distribution of Class based on Market Cap") + 
        geom_bar(position = 'fill') + theme_bw()

# Distribution of Price Variation based on Market Capitalization
ggplot(subset(data_cleaned, !is.na(market_cap_cat)), aes(x=market_cap_cat, y= `Price Variation`, fill=market_cap_cat)) + 
        geom_violin() + geom_boxplot(width=.1, fill="white") + labs(title="Price Variation based on Market Cap") 

#Distribution of Analyst Rating based on Market Capitalization
summary(data_cleaned$`Price Variation`)
quantile(data_cleaned$`Price Variation`, c(.20, .40,  .60, .80))
AnalystRating<-cut(data_cleaned$`Price Variation`, breaks = c(-100, -27.7, -3.12, 19.1, 46.7, 120000),
                   labels =c("Strong Sell","Sell","Hold","Buy","Strong Buy"))
data_cleaned$AnalystRating = AnalystRating
ggplot(subset(data_cleaned, !is.na(market_cap_cat) & !is.na(AnalystRating)), aes(x=market_cap_cat,fill=AnalystRating)) + 
        labs(y = "Percentage of Analyst Rating", x = "Market Cap", title = "Distribution of Analyst Rating based on Market Cap") + 
        geom_bar(position = 'fill') + theme_bw()


# Splitting data into train set and test set --------------------------------------------------------------------------
library(caTools)
set.seed(30)
train <- sample.split(Y = data_cleaned$Class, SplitRatio = 0.7)
trainset <- subset(data_cleaned, train == T)
testset <- subset(data_cleaned, train == F)

# Initial CART Model------------------------------------------------------------------------------------------------------
library(rpart)
library(rpart.plot)

#Removing unnecessary columns for analysis
trainset.cart <- copy(trainset)
trainset.cart[, c("Ticker", "Price Variation", "market_cap_cat", "AnalystRating"):= NULL] 
testset.cart <- copy(testset)
testset.cart[, c("Ticker", "Price Variation", "market_cap_cat", "AnalystRating"):= NULL]
data_cart <- copy(data_cleaned)
data_cart[, c("Ticker", "Price Variation", "market_cap_cat", "AnalystRating"):= NULL]

set.seed(30)
initial.cart <- rpart(Class~. , data = data_cart, method = 'class', 
                    control = rpart.control(minsplit = 2, cp = 0))
initial.cart$variable.importance
diff(initial.cart$variable.importance)

# CART Model 1------------------------------------------------------------------------------------------------------
set.seed(30)
model.cart <- rpart(Class~ `Free Cash Flow Yield`+ `operatingCashFlowSalesRatio` +`returnOnEquity` + 
                            `Book Value per Share`,
                    data = trainset.cart, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

# Plotting of maximal tree
rpart.plot(model.cart, nn= T, main = "Maximal Tree")

# Printing  pruning sequence and respective 10-fold CV errors
printcp(model.cart)

# Display the pruning sequence and 10-fold CV errors, as a graph
plotcp(model.cart, main = "Subtrees")

# Compute min CV Error + 1SE in maximal tree model.cart
CVerror.cap <- model.cart$cptable[which.min(model.cart$cptable[,"xerror"]), "xerror"] + model.cart$cptable[which.min(model.cart$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CV Error.cap in maximal tree model.cart
i <- 1; j<- 4
while (model.cart$cptable[i,j] > CVerror.cap) {
        i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(model.cart$cptable[i,1] * model.cart$cptable[i-1,1]), 1)
cp.opt

#Pruning tree
model.cart.pruned <- prune(model.cart, cp = cp.opt)

printcp(model.cart.pruned)

# Pruned Decision Tree
rpart.plot(model.cart.pruned, nn= T, main = "Pruned Tree with optimal cp with model 1")

#Prediction on Train & Test Set
test.predict <- predict(model.cart.pruned, newdata = testset.cart, type = "class")
train.predict <- predict(model.cart.pruned, newdata = trainset.cart, type = "class")

# Confusion Matrix for test set
round(prop.table(table(testset.cart$Class, test.predict, deparse.level = 2)), 3)
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y <- c(27.0, 27.6, 3.7, 41.7)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Confusion matrix for train set
round(prop.table(table(trainset.cart$Class, train.predict, deparse.level = 2)),3)
Y <- c(28.8, 25.9, 3.0, 42.3)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Classification Accuracy
mean(trainset.cart$Class == train.predict) * 100 
mean(testset.cart$Class ==test.predict) * 100
summary(model.cart.pruned)

# Cart Model 2------------------------------------------------------------------------------------------------------------------------
set.seed(30)
model.cart2 <- rpart(Class ~ `Free Cash Flow Yield`+ `operatingCashFlowSalesRatio` +`returnOnEquity` + 
                             `Book Value per Share` + `EBITDA Margin` + `Profit Margin`, data = trainset.cart,
                     method = 'class', control = rpart.control(minsplit = 2, cp = 0))

# Compute min CV Error + 1SE in maximal tree model.cart2
CVerror.cap2 <- model.cart2$cptable[which.min(model.cart2$cptable[,"xerror"]), "xerror"] + model.cart2$cptable[which.min(model.cart2$cptable[,"xerror"]), "xstd"]

plotcp(model.cart2, main = "Subtrees")

# Find the optimal CP region whose CV error is just below CV Error.cap in maximal tree model.cart2
i <- 1; j<- 4
while (model.cart2$cptable[i,j] > CVerror.cap2) {
        i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt2 = ifelse(i > 1, sqrt(model.cart2$cptable[i,1] * model.cart2$cptable[i-1,1]), 1)
cp.opt2

#Pruning tree
model.cart.pruned2 <- prune(model.cart2, cp = cp.opt2)

# Pruned Decision Tree
rpart.plot(model.cart.pruned2, nn= T, main = "Pruned Tree with optimal cp with model 2")

printcp(model.cart.pruned2)

#Prediction on Train & Test Set
test.predict2 <- predict(model.cart.pruned2, newdata = testset.cart, type = "class")
train.predict2 <- predict(model.cart.pruned2, newdata = trainset.cart, type = "class")

# Confusion Matrix for test set
round(prop.table(table(testset.cart$Class, test.predict2, deparse.level = 2)), 3)
Y <- c(27.0, 27.6, 3.7, 41.7)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Confusion Matrix for train set
round(prop.table(table(trainset.cart$Class, train.predict2, deparse.level = 2)),3)
Y <- c(28.8, 25.9, 3.0, 42.3)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()


#Classification Accuracy
mean(trainset.cart$Class == train.predict2) * 100 
mean(testset.cart$Class ==test.predict2) * 100
summary(model.cart.pruned2)

# Cart Model 3------------------------------------------------------------------------------------------------------------------------
set.seed(30)
model.cart3 <- rpart(Class~ `Free Cash Flow Yield`+ `operatingCashFlowSalesRatio` +`returnOnEquity` + 
                             `Book Value per Share` + `EBITDA Margin` + `Profit Margin` + `assetTurnover` + 
                             `Market Cap` + `debtEquityRatio` + `PE ratio` + `EPS Diluted Growth` + `interestCoverage` +
                             `Revenue Growth` + `PB ratio`,
                        data = trainset.cart, method = 'class', control = rpart.control(minsplit = 2, cp = 0))

# Compute min CV Error + 1SE in maximal tree model.cart3
CVerror.cap3 <- model.cart3$cptable[which.min(model.cart3$cptable[,"xerror"]), "xerror"] + model.cart3$cptable[which.min(model.cart3$cptable[,"xerror"]), "xstd"]

plotcp(model.cart3, main = "Subtrees")

# Find the optimal CP region whose CV error is just below CV Error.cap in maximal tree model.cart3
i <- 1; j<- 4
while (model.cart3$cptable[i,j] > CVerror.cap3) {
        i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt3 = ifelse(i > 1, sqrt(model.cart3$cptable[i,1] * model.cart3$cptable[i-1,1]), 1)
cp.opt3

#Pruning tree
model.cart.pruned3 <- prune(model.cart3, cp = cp.opt3)

# Pruned Decision Tree
rpart.plot(model.cart.pruned3, nn= T, main = "Pruned Tree with optimal cp with model 3")

printcp(model.cart.pruned3)

#Prediction on Train & Test Set
test.predict3 <- predict(model.cart.pruned3, newdata = testset.cart, type = "class")
train.predict3 <- predict(model.cart.pruned3, newdata = trainset.cart, type = "class")

# Confusion Matrix for test set
round(prop.table(table(testset.cart$Class, test.predict3, deparse.level = 2)), 3)
Y <- c(27.9, 27.6, 3.7, 41.7)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Confusion Matrix for train set
round(prop.table(table(trainset.cart$Class, train.predict3, deparse.level = 2)),3)
Y <- c(29.8, 24.8, 3.0, 42.3)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Classification Accuracy
mean(trainset.cart$Class == train.predict3) * 100 
mean(testset.cart$Class ==test.predict3) * 100
summary(model.cart.pruned3)


# Logistic Regression model-----------------------------------------------------------------------------------------------------------
library(car)

# Remove all NA values
data_cleaned.glm <- na.omit(data_cleaned) 
# Train-Test split ----------------------------------------------------------------------------------------------------------
set.seed(30)
train.glm <- sample.split(Y = data_cleaned.glm$Class, SplitRatio = 0.7)
trainset.glm <- subset(data_cleaned.glm, train.glm == T)
testset.glm <- subset(data_cleaned.glm, train.glm == F)

# Model 1 with the 17 important variables ------------------------------------------------------------------------------------
m1.glm <- glm(Class~ .-`Price Variation` -`Ticker` -`market_cap_cat` -`AnalystRating` , family = binomial, data =trainset.glm)
summary(m1.glm)
vif(m1.glm)

#Model 2 after removing `Profit Margin` due to high VIF -----------------------------------------------------------------------
m2.glm <- glm(Class~ .-`Price Variation` -`Ticker` -`market_cap_cat` -`AnalystRating` - `Profit Margin`, family = binomial, data =trainset.glm)
summary(m2.glm)
vif(m2.glm)

#Model 3 after removing `Profit Margin` and `operatingCashFlowSales` due to high VIF ---------------------------------------------
m3.glm <- glm(Class~ .-`Price Variation` -`Ticker` -`market_cap_cat` -`AnalystRating` - `Profit Margin` - operatingCashFlowSalesRatio, family = binomial, data =trainset.glm)
summary(m3.glm)
vif(m3.glm)

#Final model with only statistically important variables -----------------------------------------------------------------------------
m4.glm <- glm(Class~ `Book Value per Share` + `PE ratio` + `Enterprise Value over EBITDA` + `Dividend Yield` + `Free Cash Flow Yield` + `EPS Diluted Growth`  + `Revenue Growth`, family = binomial, data =trainset.glm)
summary(m4.glm)

# Confusion Matrix on Trainset
threshold1 <- 0.5
prob.train <- predict(m4.glm, type = 'response')
m4.predict.train <- ifelse(prob.train > threshold1, 1 , 0)
table.glmtrain <- table(Trainset.Actual = trainset.glm$Class, m4.predict.train, deparse.level = 2)
table.glmtrain
round(prop.table(table.glmtrain),3)
Y <- c(35.1, 17.0, 16.9, 30.9)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Overall Trainset Accuracy
mean(m4.predict.train == trainset.glm$Class) * 100

# Confusion Matrix on Testset
prob.test <- predict(m4.glm, newdata = testset.glm, type = 'response')
m4.predict.test <- ifelse(prob.test > threshold1, 1 , 0)
table.glmtest <- table(Testset.Actual = testset.glm$Class, m4.predict.test, deparse.level = 2)
table.glmtest
round(prop.table(table.glmtest), 3)
Y <- c(32.6, 19.5, 17.3, 30.6)
df <- data.frame(TClass, PClass, Y)

ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
        geom_tile(aes(fill = Y), colour = "white") +
        geom_text(aes(label = sprintf("%.1f", Y)), vjust = 1, colour = "white") +
        labs(y = "Predicted Class", x = "Actual Class") +
        scale_fill_gradient()

# Overall Testset Accuracy
mean(m4.predict.test == testset.glm$Class) * 100


# Analysis of Models-------------------------------------------------------------------------------------------------------------
summary(data_cleaned$`Free Cash Flow Yield`)
ggplot(subset(data_cleaned, `Free Cash Flow Yield` < 0.5 & `Free Cash Flow Yield` > -0.5 ), aes(x = Class, y = `Free Cash Flow Yield`, fill = Class)) + 
        geom_violin() + geom_boxplot(width=.1, fill="white") + coord_flip()
summary(data_cleaned[`Class` == 0])

summary(data_cleaned$`operatingCashFlowSalesRatio`)
ggplot(subset(data_cleaned, `operatingCashFlowSalesRatio` > -1), aes(x = Class, y = `operatingCashFlowSalesRatio`, fill = Class)) + 
        geom_violin() + geom_boxplot(width=.1, fill="white") + coord_flip()
summary(data_cleaned[`Class` == 1])

summary(data_cleaned$`returnOnEquity`)
ggplot(subset(data_cleaned, `returnOnEquity` < 2 & `returnOnEquity` > -2), aes(x = Class, y = `returnOnEquity`, fill = Class)) + 
        geom_violin() + geom_boxplot(width=.1, fill="white") + coord_flip()
summary(data_cleaned[`Class` == 0])

