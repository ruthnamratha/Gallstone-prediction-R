install.packages("readxl")
install.packages("corrplot")
library(tidyverse)
library(ggplot2)
library(readxl)
library(forecast)
library(car)
library(leaps)
library(caret)
library(corrplot)
library(ggcorrplot)
gall1<-read_excel(file.choose())

#Collapse rare categories (Comorbidity 0:3, Fatty Liver  0:3)
gall1 <- gall1 %>%
  mutate(
    Comorbidity = case_when(
      Comorbidity == 0 ~ 0,
      TRUE ~ 1))
 #Fatty Liver
gall1$`Gallstone Status`<-factor(gall1$`Gallstone Status`, levels = 0:1)
unique(gall1$Hypothyroidism )
#Rename columns names
gall1 <- gall1 %>%
  rename(
    `Coronary Artery Disease` = `Coronary Artery Disease (CAD)`,
    Diabetes  = `Diabetes Mellitus (DM)`)
    

#Convert binary variables to factors with lapply()
binary_cols <- c("Coronary Artery Disease", "Hypothyroidism", "Hyperlipidemia", "Diabetes")

gall1[binary_cols] <- lapply(gall1[binary_cols], function(x) {
  factor(x, levels = c(0, 1), labels = c("No", "Yes"))
})

# Collapse and rename fatty liver
gall1 <- gall1 %>%
  mutate(
    `Fatty Liver` = case_when(
      `Hepatic Fat Accumulation (HFA)` == 0 ~ "None",
      `Hepatic Fat Accumulation (HFA)` %in% c(1, 2) ~ "Mild-Moderate",
      `Hepatic Fat Accumulation (HFA)` >= 3 ~ "Severe"),
      `Fatty Liver` = factor(`Fatty Liver`, levels = c("None", "Mild-Moderate", "Severe")) ) %>%
                              select(-`Hepatic Fat Accumulation (HFA)`)      #Removes the column after collapse 
gall1$Gender <- factor(gall1$Gender,levels = c(0, 1),labels = c("Male", "Female"))

# Removing Outliers 

gall1 <- gall1[gall1$`Obesity (%)` <= 100, ] #unrealistic values
gall1 <- gall1[gall1$Triglyceride != 1.39, ]
gall1 <- gall1[gall1$`Glomerular Filtration Rate (GFR)`>= 20, ]



######Correlation Matrix to remove Multicollinearity##############
col_nums <- gall1 %>% select(where(is.numeric))
corr_matrix <- cor(col_nums)
round(corr_matrix, 2)


#correlation heatmap using corrplot


pdf("correlation_heatmap8.png", width = 12, height = 12)

corrplot(corr_matrix,
         method = "color",
         type = "upper",
         tl.cex = 0.45,
         tl.col = "black",        # <— forces label color
         cl.cex = 0.8,            # color legend text size
         addCoef.col = "black",   # correlation numbers
         number.cex = 0.45,
         insig = "blank",
         sig.level = 0.5,
         diag = FALSE)

dev.off()


# Identify highly correlated variables (≥ |-0.8|)
to_drop <- findCorrelation(corr_matrix, cutoff = 0.8, names = TRUE)
gall1_drop <- gall1 %>% select(-all_of(to_drop))
write.csv(gall1_drop, "gall_drop.csv", row.names = FALSE)
install.packages("GWalkR")
library(GWalkR)

gwalkr(gall1_drop)


#Logistic Regression with dropped dataset
mod1<-glm(data=gall1_drop,`Gallstone Status`~.,family="binomial")
summary(mod1)
#VIF Check 
vif_values <- vif(mod1)
vif_values # Values are below 5 not much multicollinearity so did not remove any further variables 

#####################################################################################################
#Creating training Data and Validation Data 

set.seed(123)
gall_index <- createDataPartition(gall1_drop$`Gallstone Status`, p = 0.6, list = FALSE)# Data Split 60:40
train.gall  <- gall1_drop[gall_index,] 
test.gall  <- gall1_drop[-gall_index,]

gall.glm_0 <- glm(`Gallstone Status` ~ ., data = train.gall, family = 'binomial')
summary(gall.glm_0)

gall_pred <- predict(gall.glm_0,test.gall, type="response")
summary(gall_pred)

############################### Confusion Matrix -set cut off to 0.5#################
pclass <- ifelse(gall_pred >=0.5,1,0)

predicted_factor <- factor(pclass, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))
actual_factor <- factor(test.gall$`Gallstone Status`, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))

# Confusion matrix
confusionMatrix(predicted_factor, actual_factor,positive = "Gallstone")
############################### Confusion Matrix -set cut off to ROC 0.3298577########################
pclass1 <- ifelse(gall_pred >=0.3298577,1,0)

predicted_factor <- factor(pclass1, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))
actual_factor <- factor(test.gall$`Gallstone Status`, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))

# Confusion matrix for ROC 0.3298577
confusionMatrix(predicted_factor, actual_factor,positive = "Gallstone")

#-------------------------------Step wise Selection---------------------------------
options(scipen=999)
library(MASS)
step_model <- stepAIC(gall.glm_0, direction = "both", trace = FALSE)
summary(step_model)



gall_pred_step <- predict(step_model, test.gall, type = "response")
#---------------Confusion matric with step predict ---------------------------
pclass3 <- ifelse(gall_pred_step  >=0.5,1,0)

predicted_factor <- factor(pclass3, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))
actual_factor <- factor(test.gall$`Gallstone Status`, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))

# Confusion matrix
confusionMatrix(predicted_factor, actual_factor,positive = "Gallstone")
##########################################ROC###############################################

# Load the required package
library(pROC)

# Predict   Step-wise logistic model
gall_pred_step <- predict(step_model, test.gall, type = "response")

#  Calculating ROC curve 
roc_step <- roc(test.gall$`Gallstone Status`, gall_pred_step)
auc_step<-auc(roc_step)
auc_step
plot(roc_step, print.auc = TRUE, col = "blue", main = "ROC Curve - Gallstone Prediction")

# t cutoff (Youden’s J)
best_cutoff <- coords(roc_step, "best", ret = c("threshold", "sensitivity", "specificity"))
print(best_cutoff)

# Classify based on best cutoff
pclass_best <- ifelse(gall_pred_step >= 0.3298577, 1, 0)

# Convert to factors for confusion matrix
predicted_factor <- factor(pclass_best, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))
actual_factor <- factor(test.gall$`Gallstone Status`, levels = c(0, 1), labels = c("No Gallstone", "Gallstone"))

# Confusion matrix with optimal threshold cutoff
library(caret)
confusionMatrix(predicted_factor, actual_factor, positive = "Gallstone")



#----------------------------------------# Cross-validated LASSO-------------------------------------------
library(glmnet)
X_train <- model.matrix(`Gallstone Status` ~ . - 1, data = train.gall)
Y_train <- as.numeric(train.gall$`Gallstone Status`) - 1

X_test <- model.matrix(`Gallstone Status` ~ . - 1, data = test.gall)
Y_test <- as.numeric(test.gall$`Gallstone Status`) - 1

#````````````````````````````````````````````````````````````````````````
lasso_cv <- cv.glmnet(X_train, Y_train, 
                      family = "binomial",   
                      alpha = 1,
                      nfolds = 10) #

plot(lasso_cv, main = "LASSO Cross-Validation")

# Lasso model 
lasso_model <- glmnet(X_train, Y_train, 
                      family = "binomial", 
                      alpha = 1, 
                      lambda = lasso_cv$lambda.min)

# Variables selected
lasso_coefs <- coef(lasso_model)
lasso_vars <- rownames(lasso_coefs)[lasso_coefs[,1] != 0][-1]
#Variables left after the penalty 
print(lasso_vars)

# Predictions
pred_lasso <- predict(lasso_model, newx = X_test, 
                      type = "response", s = "lambda.min")[,1]     # type="prob"
#############################ROC-AUC############################################

roc_lasso <- roc(Y_test, pred_lasso)

best_cutoff_lasso <- coords(
  roc_lasso,
  "best",
  ret = c("threshold", "sensitivity", "specificity")
)


thr_lasso <- best_cutoff_lasso["threshold"]
thr_lasso

roc_lasso <- roc(Y_test, pred_lasso)
auc_lasso <- auc(roc_lasso)

best_cutoff_lasso <- coords(
  roc_lasso,
  "best",
  ret = c("threshold", "sensitivity", "specificity")
)
thr_lasso <- as.numeric(best_cutoff_lasso["threshold"])

# Class labels
pclass_lasso <- ifelse(pred_lasso >= thr_lasso, 1, 0)

predicted_factor1 <- factor(pclass_lasso,
                            levels = c(0, 1),
                            labels = c("No Gallstone", "Gallstone"))

actual_factor1 <- factor(Y_test,
                         levels = c(0, 1),
                         labels = c("No Gallstone", "Gallstone"))

# Confusion matrix via table
cm_tab <- table(predicted_factor1, actual_factor1)
cm_tab

confusionMatrix(cm_tab, positive = "Gallstone")




#===================================================END==========================================================




#######################################
refit_model <- glm(
  `Gallstone Status` ~ 
    `Coronary Artery Disease` +
    Hypothyroidism +
    Hyperlipidemia +
    Diabetes +
    `Extracellular Fluid/Total Body Water (ECF/TBW)` +
    `Bone Mass (BM)` +
    `Total Fat Content (TFC)` +
    `High Density Lipoprotein (HDL)` +
    `C-Reactive Protein (CRP)` +
    `Hemoglobin (HGB)` +
    `Vitamin D`,
  data = train.gall,
  family = "binomial"
)

summary(refit_model)
or_vec <- exp(coef(refit_model))

lasso_table <- data.frame(
  Variable   = names(or_vec),
  Odds_Ratio = as.numeric(or_vec)
)

# Remove intercept and any weird values
lasso_plot <- lasso_table %>%
  filter(Variable != "(Intercept)") %>%
  filter(is.finite(Odds_Ratio), Odds_Ratio > 0)


library(ggplot2)

ggplot(lasso_plot,
       aes(x = reorder(Variable, Odds_Ratio),
           y = Odds_Ratio)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(trans = "log10") +  # OK now because we filtered
  labs(
    title = "Odds Ratios from Refit Logistic Model (LASSO Variables)",
    x = "Predictor",
    y = "Odds Ratio (log10 scale)"
  ) +
  theme_minimal(base_size = 12)
library(ggplot2)

library(ggplot2)
library(scales)

ggplot(gall1_drop,
       aes(x = `Gallstone Status`, fill = Hyperlipidemia)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Hyperlipidemia- Gallstone Status",
    x = "Gallstone Status",
    y = "Percentage of Patients",
    fill = "Hyperlipidemia"
  ) +
  theme_minimal(base_size = 13)

#----------------------------------Decision Tree -----------------------------------
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tree_model <- rpart(
  `Gallstone Status` ~ .,
  data = train.gall,
  method = "class"
)

rpart.plot(tree_model, type = 2, extra = 104)
print(tree_model)

printcp(tree_model)   # shows complexity table
plotcp(tree_model)    # visualize error vs complexity
#prune
pruned_tree <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
rpart.plot(pruned_tree, type = 2, extra = 104)

library(caret)

tree_pred <- predict(pruned_tree, test.gall, type = "class")

confusionMatrix(
  tree_pred,
  factor(test.gall$`Gallstone Status`),
  positive = "1"
)

tree_model$variable.importance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Random Forest~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("randomForest")
library(randomForest)
colnames(train.gall) <- make.names(colnames(train.gall))
colnames(test.gall)  <- make.names(colnames(test.gall))
set.seed(123)

rf_model <- randomForest(
  Gallstone.Status ~ .,
  data = train.gall,
  ntree = 500,        # number of trees
  importance = TRUE
)

rf_model


library(caret)

rf_pred <- predict(rf_model, test.gall)

confusionMatrix(
  rf_pred,
  factor(test.gall$Gallstone.Status),
  positive = "1"
)
varImpPlot(rf_model)

rf_prob <- predict(rf_model, test.gall, type = "prob")[, 2]

library(pROC)

roc_rf <- roc(test.gall$Gallstone.Status, rf_prob)

plot(roc_rf,
     main = "ROC Curve – Random Forest",
     col = "blue",
     lwd = 2)

auc(roc_rf)

coords_rf <- coords(
  roc_rf,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

coords_rf
# Ensure test set has no missing values
test_clean <- test.gall[complete.cases(test.gall), ]

rf_prob <- predict(rf_model, test_clean, type="prob")[,2]
rf_class_tuned <- ifelse(rf_prob >= coords_rf["threshold"], 1, 0)

confusionMatrix(
  factor(rf_class_tuned, levels=c(0,1)),
  factor(test_clean$Gallstone.Status, levels=c(0,1)),
  positive="1"
)
thr <- as.numeric(coords_rf["threshold"])
rf_class_tuned <- ifelse(rf_prob >= thr, 1, 0)

pred <- factor(rf_class_tuned, levels=c(0,1))
actual <- factor(test_clean$Gallstone.Status, levels=c(0,1))

confusionMatrix(pred, actual, positive="1")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~XG Boost~~~~~~~~~~~~~~~~~~~~~~~~

train_y <- as.numeric(as.character(train_clean$Gallstone.Status))
test_y  <- as.numeric(as.character(test_clean$Gallstone.Status))
graphics.off()
table(train_y)
unique(train_y)


base <- mean(train_y)

dtrain <- xgb.DMatrix(data = train_x, label = train_y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  learning_rate = 0.05,
  base_score = base
)

set.seed(123)
xgb_booster <- xgb.train(params = params, data = dtrain, nrounds = 200, verbose = 0)

# STEP 1: Get SHAP contributions from XGBoost
# SHAP contributions on TRAIN data
shap_contrib <- predict(
  xgb_booster,
  train_x,
  predcontrib = TRUE
)

# STEP 2: Remove the bias column
# Remove last column (bias / intercept)
shap_contrib_no_bias <- shap_contrib[, -ncol(shap_contrib)]

# Prepare SHAP data for plotting
library(SHAPforxgboost)

# 1) Convert to data.frame (this fixes setDT error)
shap_df  <- as.data.frame(shap_contrib_no_bias)
X_df     <- as.data.frame(train_x)

# 2) Make sure column names match exactly
stopifnot(identical(colnames(shap_df), colnames(X_df)))

# 3) Prepare + plot
shap_long <- shap.prep(
  shap_contrib = shap_df,
  X_train = X_df
)

shap.plot.summary(shap_long)


















