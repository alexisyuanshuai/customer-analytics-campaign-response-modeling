#### 1.Loading the CSV file and data cleaning ####
# 1.Shape:(2240,29)
# 2.Missing values: Income-24. New shape after deleting missing values: (2216,29)
# 3.Duplicated value: 0
# 4.Convert the categorical variable into factor variable
# 5.Convert the time variable
# 6.Compute totalmnt and age, then add them to the dataset (New shape:(2216,31))
# 7.Perform IQR outlier detection on the Income (8) and TotalMnt(3)
# 8.Remove the rows with outliers and the unnecessary columns (New shape:(2205,22))

## 1 ##
df=read.csv("marketing_campaign.csv",header=T,na.strings="?")
attach(df)
names(df)
dim(df)

## 2 ##
colSums(is.na(df))
df<-na.omit(df)
dim(df)
sum(is.na(df))

## 3 ##
sum(duplicated(df))

## 4 ##
str(df)
df$Education <- factor(df$Education)
df$Marital_Status <- factor(df$Marital_Status)
df$Kidhome <- factor(df$Kidhome)
df$Teenhome <- factor(df$Teenhome)
df$AcceptedCmp1 <- factor(df$AcceptedCmp1)
df$AcceptedCmp2 <- factor(df$AcceptedCmp2)
df$AcceptedCmp3 <- factor(df$AcceptedCmp3)
df$AcceptedCmp4 <- factor(df$AcceptedCmp4)
df$AcceptedCmp5 <- factor(df$AcceptedCmp5)
df$Response <- factor(df$Response)
df$Complain <- factor(df$Complain)
str(df)

## 5 ##
df$Dt_Customer <- as.Date(df$Dt_Customer, format = "%d-%m-%Y")

## 6 ##
df$TotalMnt <- df$MntWines + df$MntFruits + df$MntMeatProducts +df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds
df$Age <- 2014 - df$Year_Birth


## 7 ##
Q1_inc <- quantile(df$Income, .25)
Q3_inc <- quantile(df$Income, .75)
IQR_inc <- IQR(df$Income)
lower_inc <- Q1_inc - 1.5 * IQR_inc
upper_inc <- Q3_inc + 1.5 * IQR_inc
outliers_inc <- which(df$Income < lower_inc | df$Income > upper_inc);outliers_inc

Q1_tomn <- quantile(df$TotalMnt, .25)
Q3_tomn <- quantile(df$TotalMnt, .75)
IQR_tomn <- IQR(df$TotalMnt)
lower_tomn <- Q1_tomn - 1.5 * IQR_tomn
upper_tomn <- Q3_tomn + 1.5 * IQR_tomn
outliers_tomn<- which(df$TotalMnt < lower_tomn | df$TotalMnt > upper_tomn);outliers_tomn

# 8 #
df<-df[-outliers_inc, ]
df<-df[-outliers_tomn, ]
cols_to_remove <- c("MntWines", "MntFruits", "MntMeatProducts",
                    "MntFishProducts", "MntSweetProducts", "MntGoldProds",
                    "Z_CostContact","Z_Revenue",
                    "Year_Birth")
df <- df[, !names(df) %in% cols_to_remove]
dim(df)


#### 2.Descriptive Statistics ####
# 1.Obtain basic statistical information using the summary function and skimr
# 2.histogram of TotalMnt,Age and Income
# 3.Pie chart of educational level

# 1 #
summary(df)
library(skimr)
skim(df)

# 2 #
par(mfrow = c(1, 3))
hist(df$TotalMnt, col = "#6BAED6", border = "white")
abline(v = mean(df$TotalMnt), col = "#08519C", lwd = 2)

hist(df$Age, col = "#FDAE6B", border = "white")
abline(v = mean(df$Age), col = "#D94801", lwd = 2)


hist(df$Income, col = "#74C476", border = "white")
abline(v = mean(df$Income), col = "#238B45", lwd = 2)

# 3 #
par(mfrow = c(1, 1))
tbl_edu <- table(df$Education)
pct_edu <- round(tbl_edu / sum(tbl_edu ) * 100, 1)
pie(tbl_edu,labels = paste(names(tbl_edu), pct_edu, "%"),main = "Distribution of Education Levels")

#### 3.linear model ####
model1 <- lm(TotalMnt ~ ., data = df)
summary(model1)

data1<- df[, c("TotalMnt", "Income", "Kidhome", "Teenhome",
               "NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases",
               "NumStorePurchases", "NumWebVisitsMonth",
               "Education","Dt_Customer","AcceptedCmp5","AcceptedCmp1")]

model2 <- lm(TotalMnt ~ ., data = data1)
summary(model2)

# Check multicollinearity among predictors using VIF.
# GVIF values substantially larger than 5 (or 10) would indicate multicollinearity.

library(car)
vif(model2)

#Draw a Normal plot of the residuals to assess the normality assumption.
#If points roughly follow the straight line, the residuals are approximately normal.
qqnorm(residuals(model2))

# Add the theoretical normal line for visual comparison in the Q–Q plot.
qqline(residuals(model2))

#Scale–Location (Spread–Location) plot.
# A random scatter around a horizontal line suggests roughly constant variance.
plot(model2, which = 3)

#Residuals vs Fitted values
#Check linearity and to detect non-constant variance or systematic patterns.
# Ideally, residuals should be randomly scattered with no clear structure.
plot(model2, which = 1)

# Residuals vs Leverage with Cook's distance.
# Used to identify influential observations that have a large impact on the fitted model.
# Points with high leverage and large Cook’s distance may need further investigation.
plot(model2, which = 5)

df$log_TotalMnt <- log(df$TotalMnt + 1)

model3 <- lm(log_TotalMnt ~ Income + Kidhome + Teenhome +
               NumDealsPurchases + NumWebPurchases + NumCatalogPurchases +
               NumStorePurchases + NumWebVisitsMonth + Education +
               Dt_Customer + AcceptedCmp5 + AcceptedCmp1,
             data = df)

summary(model3)

qqnorm(residuals(model3))
qqline(residuals(model3))

plot(model3, which = 1)
plot(model3, which = 3)
plot(model3, which = 5)

#The residual diagnostics continued to show substantial non-linearity. 
#This indicates that the underlying relationship cannot be adequately represented by linear or polynomial structures.

##############################################################################
# the linear model does not fit well, we switch to clustering

# Select variables used for clustering (features)
vars <- c("Recency", "Income",
          "NumDealsPurchases", "NumWebPurchases",
          "NumCatalogPurchases", "NumStorePurchases",
          "NumWebVisitsMonth", "Kidhome", "Teenhome")

# Extract these variables to construct x for clustering
x <- df[ , vars]

# Keep only numeric columns (to avoid issues with factor/character variables)
x <- x[ , sapply(x, is.numeric)]

# Standardize all features (k-means requires scaling; also emphasized in class)
x_scaled <- scale(x)

# Use the Elbow Plot to help determine the optimal number of clusters (K)
set.seed(123)
wss <- c()
for (k in 1:10) {km_tmp <- kmeans(x_scaled, centers = k, nstart = 20)
  wss[k] <- km_tmp$tot.withinss}

plot(1:10, wss, type = "b",
     xlab = "Number of clusters (K)",
     ylab = "Within-cluster sum of squares",
     main = "Elbow Plot")

# Based on the Elbow Plot, choose K = 3 and run k-means (styled after class example km.out)
set.seed(123)
km.out <- kmeans(x_scaled, centers = 3, nstart = 50)

km.out          # Print clustering results (centers, WSS, etc.)
km.out$cluster  # Cluster assignment for each customer

# Add cluster labels back to the original dataset
df$cluster <- factor(km.out$cluster)

# Show the number of observations in each cluster
table(df$cluster)

# Compute the mean of each feature within each cluster (customer profile analysis)
cluster_means <- aggregate(x, by = list(cluster = df$cluster), FUN = mean)
cluster_means
# This allows us to analyze the three customer groups; results are very clear

km.out$size        
km.out$withinss     
km.out$tot.withinss  
km.out$centers      

# Graph 1: Income vs NumWebPurchases (colored by cluster)
plot(x$Income,
     x$NumWebPurchases,
     col = km.out$cluster,
     pch = km.out$cluster,
     xlab = "Income",
     ylab = "NumWebPurchases",
     main = "Clusters on Income vs Web Purchases")

# Graph 2: NumStorePurchases vs NumWebPurchases
plot(x$NumStorePurchases,
     x$NumWebPurchases,
     col = km.out$cluster,
     pch = km.out$cluster,
     xlab = "NumStorePurchases",
     ylab = "NumWebPurchases",
     main = "Clusters on Store vs Web Purchases")

# Boxplot: Income by cluster
boxplot(Income ~ cluster, data = df,
        xlab = "Cluster",
        ylab = "Income",
        main = "Income by Cluster")

# Boxplot: Recency by cluster
boxplot(Recency ~ cluster, data = df,
        xlab = "Cluster",
        ylab = "Recency",
        main = "Recency by Cluster")

# Boxplot: NumWebPurchases by cluster
boxplot(NumWebPurchases ~ cluster, data = df,
        xlab = "Cluster",
        ylab = "NumWebPurchases",
        main = "Web Purchases by Cluster")

# Compare total spending across clusters (identify high-value customers)
aggregate(TotalMnt ~ cluster, data = df, FUN = mean)
boxplot(TotalMnt ~ cluster, data = df,
        xlab = "Cluster",
        ylab = "TotalMnt",
        main = "Total Spending by Cluster")

### Cluster profile summary
centers_original_scale <- sweep(km.out$centers, 2, attr(x_scaled, "scaled:scale"), "*")
centers_original_scale <- sweep(centers_original_scale, 2, attr(x_scaled, "scaled:center"), "+")
centers_original_scale

################################################
#Marketing Campaign Effectiveness Response Model
logit1 <- glm(Response ~ Income + Recency + NumWebVisitsMonth +
                AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + 
                AcceptedCmp4 + AcceptedCmp5,
              data = df, family = binomial)
summary(logit1)

#Calculate the predicted probability
df$pred_prob <- predict(logit1, type = "response")

#hard classification
df$pred_class <- ifelse(df$pred_prob > 0.5,1,0)

#generate confusion matrix
table(df$pred_class, df$Response)

library(pROC)
roc_obj <- roc(df$Response, df$pred_prob)
#A ROC curve that bows toward the upper-left corner indicates strong model performance.
plot(roc_obj)
auc(roc_obj)
#AUC= 0.8242
#The model can correctly distinguish between respond and non-respond in 82.4% of random pairs.

#####################################
#Decision tree of the response model

df$Response <- factor(df$Response, levels = c(0, 1))

library(tree)   
df_tree <- df[, c("Response",
                  "Income",
                  "Kidhome", "Teenhome",
                  "NumDealsPurchases", "NumWebPurchases",
                  "NumCatalogPurchases", "NumStorePurchases",
                  "NumWebVisitsMonth",
                  "Education", "Dt_Customer",
                  "AcceptedCmp1", "AcceptedCmp2",
                  "AcceptedCmp3", "AcceptedCmp4",
                  "AcceptedCmp5")]


set.seed(1) 
n <- nrow(df_tree)
train_idx <- sample(1:n, size = round(0.7 * n)) 

train_data <- df_tree[train_idx, ]
test_data <- df_tree[-train_idx, ]

# Fit classification tree on training data
tree_resp <- tree(Response ~ ., data = train_data)

plot(tree_resp)
text(tree_resp, pretty = 1)

# Cross-validation to choose optimal tree size
set.seed(100)
cv_resp <- cv.tree(tree_resp, FUN = prune.misclass)  

# View CV results for different tree sizes
cv_resp$size       
cv_resp$dev   

# Select the tree size with the smallest deviance
best_size <- cv_resp$size[which.min(cv_resp$dev)]
best_size

# Pruning Based on Optimal Size
pruned_resp <- prune.misclass(tree_resp, best = best_size)

plot(pruned_resp)
text(pruned_resp, pretty = 0)

#  Prediction on test set
pred_class <- predict(pruned_resp, newdata = test_data, type = "class")

table(pred_class, test_data$Response)
mean(pred_class == test_data$Response)

#Heterogeneity analysis
#1 
aggregate(TotalMnt ~ cluster, data = df, FUN = mean)
aggregate(Income ~ cluster, data = df, FUN = mean)
aggregate(NumWebPurchases ~ cluster, data = df, FUN = mean)
aggregate(NumStorePurchases ~ cluster, data = df, FUN = mean)


#2
df$Response_numeric <- as.numeric(as.character(df$Response))
aggregate(Response_numeric ~ cluster, data = df, FUN = mean)
# cluster2 are more easily influenced by marketing campaigns.

#3
logit_hetero <- glm(Response ~ Income + Recency + NumWebVisitsMonth +
                      cluster + Recency:cluster,
                    data = df, family = binomial)

summary(logit_hetero)

#4这个感觉可以放到前面

logit_c1 <- glm(Response ~ Income + Recency + NumWebVisitsMonth,
                data = df[df$cluster == 1,], family = binomial)

logit_c2 <- glm(Response ~ Income + Recency + NumWebVisitsMonth,
                data = df[df$cluster == 2,], family = binomial)

logit_c3 <- glm(Response ~ Income + Recency + NumWebVisitsMonth,
                data = df[df$cluster == 3,], family = binomial)

summary(logit_c1)
summary(logit_c2)
summary(logit_c3)

#5 education
heteromodel1 <- glm(
  Response ~ Income*Education + Recency*Education + NumWebVisitsMonth*Education,
  data = df,
  family = binomial
)

summary(heteromodel1)
#6 Age
df$Age_group <- cut(
  df$Age,
  breaks = c(-Inf, 35, 55, Inf),
  labels = c("Young", "Middle", "Senior")
)

model_age <- glm(
  Response ~ NumWebVisitsMonth * Age_group + Income* Age_group + Recency* Age_group, 
  family = binomial, 
  data = df
)

summary(model_age)

