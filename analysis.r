# loading in the pacakges
library(magrittr)  
library(tidyverse)
library(tree)
library(readxl)
library(glmnet)    # LASSO regression
library(caret)     # Data splitting and preprocessing

############################################Finding correaltions using plots
# loading in the PSID data
psid <- read_excel("/Users/user/Desktop/ECON491project/data/J347114.xlsx")

# filtering and cleaning the data
# we are log transforming the family income and childhood economic situation to reduce the skewness
df <- psid %>%
  # we sleect people who are around age 30 - 38 in 2021
  filter(ER34904 >= 30, ER34904 <= 38) %>%       
  
  filter(!is.na(ER81775)) %>%              
  # we select features of data that are avaliable to use
  # PSID contains thoudsands of predictors, and variables.
  mutate(
    # we measure the financial outcome using family_income at 2021
    family_income_35   = ER81775,                
    log_family_income_35 = log(abs(family_income_35) + 1) * sign(family_income_35),
    
    #we log transform the childhood family incomes
    family_inc_89      = V17533,                 
    family_inc_90      = V18875,                 
    log_family_inc_89  = log(abs(family_inc_89) + 1) * sign(family_inc_89),
    log_family_inc_90  = log(abs(family_inc_90) + 1) * sign(family_inc_90),
    
    #we create a categorical value to generalize the chilhood family incomes
    childhood_inc_cat = case_when(
      family_inc_89 < 20000 ~ "Low (<$20k)",
      family_inc_89 >= 20000 & family_inc_89 < 50000 ~ "Middle ($20k-$50k)",
      family_inc_89 >= 50000 ~ "High (>$50k)",
      TRUE ~ "Unknown"
    ),
    
    # demo graphic information
    sex         = factor(ER32000, levels=c(1,2), labels=c("Male","Female")),
    years_educ  = ER34952,
    parents_poor = factor(ER81141, levels=c(1,3,5), labels=c("Poor","Average","WellOff"))
  ) %>%
  select(
    family_income_35, log_family_income_35,
    family_inc_89, log_family_inc_89, 
    family_inc_90, log_family_inc_90,
    childhood_inc_cat,
    sex, years_educ, parents_poor
  ) %>%
  filter(!is.na(family_inc_89) & !is.na(family_income_35))  # remove rows with missing values

#set up plotting area to show multiple plots
par(mfrow=c(2,2))
#boxplot by years of education (binned for better visualization)
df$educ_cat <- cut(df$years_educ, 
                   breaks=c(0, 12, 16, 30), 
                   labels=c("HS or less", "Some college/Bachelor's", "Graduate"))
boxplot(log_family_income_35  ~ educ_cat, data=df, 
        main="Income by Education Level", 
        xlab="Education Level", 
        ylab="Family Income at Age ~35")

#boxplot by childhood economic status
boxplot(log_family_income_35 ~ childhood_inc_cat, data=df, 
        main="Income by Childhood Economic Status", 
        xlab="Childhood Economic Category", 
        ylab="Family Income at Age ~35")

#boxplot by gender
boxplot(log_family_income_35 ~ sex, data=df, 
        main="Income by Gender", 
        xlab="Gender", 
        ylab="Family Income at Age ~35")



# We generate a density plot of  childhood income categories.
# the x axis is the family_income_level at age 35
# y axis is the density of family. income_leve at childhood
ggplot(df, aes(x = family_income_35, fill = childhood_inc_cat)) +
  geom_density(alpha = 0.5) +
  labs(title = "Adult Income Density by Childhood Income Category",
       x = "Family Income at Age 35",
       y = "Density",
       fill = "Childhood Income") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  xlim(0, 200000) 

# Log-transformed scatter plot
ggplot(df, aes(x = log_family_inc_89, y = log_family_income_35)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship Between Log Childhood Income and Log Adult Income",
       x = "Log Family Income in 1989",
       y = "Log Family Income at Age 35") +
  theme_minimal()
# Reset plotting parameters
par(mfrow=c(1,1))

################################################################################Feature Selection and Data cleaning

# filtering and cleaning the data
# we are log transforming the family income and childhood economic situation to reduce the skewness
df <- psid %>%
  # we sleect people who are around age 30 - 38 in 2021
  
  filter(ER34904 >= 30, ER34904 <= 38) %>%
  
  # select the data with family income reported
  filter(!is.na(ER81775)) %>%
  
  # we select features of data that are avaliable to use
  # PSID contains thoudsands of predictors, and variables.
  mutate(
    # we measure the financial outcome using family_income at 2021
    family_income     = ER81775,                  # family income
    # we take log
    log_family_income = log(abs(family_income) + 1) * sign(family_income),
    
    # demo graphic information
    sex                = factor(ER32000, levels=c(1,2), labels=c("Male","Female")),
    years_educ         = ER34952,
    parents_poor       = factor(ER81141, levels=c(1,3,5), labels=c("Poor","Average","WellOff")),
    parent_college     = factor(V11956, levels=c(0,1), labels=c("No","Yes")),
    
    # log transform the childhood environment
    log_family_inc_90  = log(abs(V18875) + 1) * sign(V18875),
  ) %>%
  select(
    individual_id      = ER30002, # unique identifier for each individual
    famid              = ER34901, # family identifier
    age_2021           = ER34904, # age in 2021
    log_family_income,
    sex,
    years_educ,
    parents_poor,
    parent_college,
    log_family_inc_90,
  ) %>%
  filter(!is.na(log_family_income))               # we drop unreported family income

# drop rows with unreported predictors
df_clean <- df %>%
  drop_na(years_educ, sex, parents_poor, parent_college, log_family_inc_90)


# split the data into traning and testing sets
set.seed(123)
train_idx <- createDataPartition(df_clean$log_family_income, p=0.7, list=FALSE)
df_train <- df_clean[train_idx, ]
df_test  <- df_clean[-train_idx, ]

############################################################Decision Tree model

# the model formula here excludes identifiers
model_formula <- log_family_income ~
  sex +
  years_educ +
  parents_poor +
  parent_college +
  log_family_inc_90 
# fit a tree model on it
library(tree)
tree_model <- tree(model_formula, data = df_train)
summary(tree_model)

#print out the tree plot
plot(tree_model)
text(tree_model, pretty = 0)

# cross validation on tree model, then find the optimal size of the tree model we use
cv_tree   <- cv.tree(tree_model)
best_size <- cv_tree$size[which.min(cv_tree$dev)]
pruned    <- prune.tree(tree_model, best = best_size)

# plot the pruned tree
plot(pruned)
text(pruned, pretty = 0)

# check test set performance of our tree prediction
tree_pred     <- predict(pruned, newdata = df_train)
tree_test_mse <- mean((tree_pred - df_train$log_family_income)^2)

cat("Pruned tree test MSE:", tree_test_mse, "\n")

############################################## Lasso regression
## We run a lassol regression with a 10 fold CV to dicided which predictors are significant in determining the adult economic outcomes


# we are using all predictors in our df except from the identifiers
x_train <- model.matrix(log_family_income ~ . - individual_id - famid - age_2021, df_train)[, -1]
y_train <- df_train$log_family_income
x_test  <- model.matrix(log_family_income ~ . - individual_id - famid - age_2021, df_test)[, -1]
y_test  <- df_test$log_family_income

#We run lasso regression with 10 fold cv
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
# extract the best lamba values
best_lambda <- cv_lasso$lambda.min

# some predictors is d
# now we fit the lasso model with the minimal lamba
lasso_min <- glmnet(x_train, y_train, alpha=1, lambda=best_lambda)

# Extract coefficients
lasso_coefs_min <- coef(lasso_min)

cat("LASSO coefficients at optimal lambda (minimum CV error)")
print(lasso_coefs_min[lasso_coefs_min != 0])


#predict and compute MSE for this modal
pred_lasso_min <- predict(lasso_min, s=best_lambda, newx=x_test)
mse_lasso_min <- mean((pred_lasso_min - y_test)^2)


cat("\nTest MSE (lambda.min):", round(mse_lasso_min, 4), "\n")
cat("Test RMSE (lambda.min):", round(sqrt(mse_lasso_min), 4), "\n")



library(gridExtra)

# prepare a data.frame for plotting
preds <- as.numeric(pred_lasso_min)
plot_df <- data.frame(
  actual    = y_test,
  predicted = preds,
  residual  = preds - y_test
)
threshold = 0.5
# 1) plot actual data points vs predicted datapoints
p1 <- ggplot(plot_df, aes(x = actual, y = predicted)) +
  geom_point(aes(color = abs(residual) < threshold), size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs. Predicted (LASSO)",
    x     = "Actual Log Family Income",
    y     = "Predicted Log Family Income"
  )
p1
# 2) plotting the relationship between the predicted values (fitted values) and the residuals from the LASSO regression model.
p2 <- ggplot(plot_df, aes(x = predicted, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted",
    x     = "Predicted Log Family Income",
    y     = "Residual (Predicted – Actual)"
  ) 

p2
