### Quarterly Inflation Forecasting using FRED-QD Dataset####

rm(list=ls())

#### LOADING DATA FROM FRED-QD DATASET ####
library(devtools)
#devtools::install_github("cykbennie/fbi")
library(fbi) #we use FRED fbi package to load and prepare the FRED-QD dataset 
library(tidyverse)
data_raw = fredqd("https://files.stlouisfed.org/files/htdocs/fred-md/quarterly/current.csv", transform = FALSE) 
varlist = fredqd_description #To get FRED-QD dataset Description
dim(data_raw)
names(data_raw)

#### SUBSETTING AND TRANSFORMING THE RAW DATA ####
#fred column contains the list of variables, let's find the common variable names between data and varlist
vars = intersect(colnames(data_raw),varlist$fred) 
data_1 <- data_raw[, !(colnames(data_raw) %in% vars)] #to know which variables were left out.

#we need those columns with date and those specified in "vars"
data = data_raw %>% as_tibble()%>%
  select(all_of(c("date",vars)))

#updates the var list with the variable names in data
varlist = varlist %>% filter(fred %in% vars)

#we need the determinants of prices in tcode = 6, exchange rates in tcode = 11 & expectations in others, tcode = 1
prices_varlist = varlist %>% filter(group == "Prices", tcode == 6) 
exchange_rate_varlist = varlist %>% filter(group == "Exchange Rates", tcode == 5)
others_varlist = varlist %>% filter(group == "Other", tcode == 1)

#extracting only required data
data = data %>% as_tibble() %>%
  select(date, c(prices_varlist$fred, exchange_rate_varlist$fred, others_varlist$fred)) 

#Note that prices also include information on the sub-components of PCE, PPI and so on. #We include only the required variables as stated before - 
keep <- c("PCECTPI", "CPIAUCSL", "CPILFESL", "PPIACO")

df = data[, (colnames(data) %in% keep)] %>% 
  mutate(across(everything(), ~ 100 * ( . - lag(.,4))/lag(.,4))) %>% #we calculate the percentage change from year ago
  bind_cols(data %>% select(date)) %>% select(date, everything())

#Checking if there are NAs in the dataset
na_cols <- colMeans(is.na(df)) * 100 
barplot(sort(na_cols, decreasing = TRUE),
        ylim = c(0, 25),
        main = "NAs per feature"
)
box()
grid()
na_cols[na_cols>0]

df <- df %>% na.omit() #This leaves us with 199 observations for 7 variables

#since all the column names are in codified format, let us change them to readable format
colnames(df) <- c("date","PCE","CPI","Core_CPI","PPI")

#since date is in character format, let us collect it separately 
dates <- df$date
inflation <- df %>% as_tibble() %>% select(-date) 

#To perform random forest over time series data, I need lagged terms, so I create it here
inflation_1 <- inflation %>% 
  mutate(lag1_CPI = lag(CPI),
         lag2_PCE = lag(PCE),
         lag3_Core = lag(Core_CPI),
         lag4_PPI = lag(PPI),
         dif1 = lag1_CPI - lag2_PCE,
         dif2 = lag2_PCE - lag3_Core) %>% 
  filter(complete.cases(.))
dates <-dates[-1] #excluding the first one since lag variable doesn't exist

### Some plots ####
#The data is in wide format, let us convert it to long format to plot the time series
my_data_long <- tidyr::gather(df, key = "Variable", value = "Value", -date)
ggplot(my_data_long, aes(x = date, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~Variable, scales = 'free_y') +
  labs(title = "Time series of major macroeconomic indicators", x = "Time", y = "Value") +
  theme(legend.position = "none") 

#Correlation plots
library(corrplot)
custom_red_palette <- colorRampPalette(c("lightcoral", "darkred"))(100)
correlation_matrix <- round(cor(inflation, use = "complete.obs"),2)
corrplot(correlation_matrix, method = "color", col = custom_red_palette)

#ACF & PACF
library(ggplot2)
library(forecast)
acf_plot <- acf(inflation$CPI, main = "Autocorrelation Function (ACF)")
pacf_plot <- pacf(inflation$CPI, main = "Partial Autocorrelation Function (PACF)")

#### ORGANISING PREDICTORS ####
y <- inflation_1$CPI[1:nrow(inflation)-1] # this is our target
x <- inflation_1[1:(nrow(inflation)-1),] %>% select(-CPI) #these are our predictors

# Create training and testing dataframes.
# We will use the last 40 quarters to test.
pred_df <- cbind(y,x) #254 observations in total
pred_df_train <- pred_df[1:(nrow(pred_df)-40),] #214 obs is my training set
pred_df_test <- pred_df[(nrow(pred_df)-39):nrow(pred_df),] #40 obs is my testing set
ts_data <- pred_df_train$CPI
dates_test <- dates[215:254]
pred_test <- cbind.data.frame('dates'= dates_test)
pred_test$realized <- pred_df_test$y

#prediction for Q3 2023
pred_df_next <- inflation_1[nrow(inflation_1),]

#### DIFFERENT MODELS ####
### 1. Ar(1) Model
mdl.ar <- lm(y ~ lag1_CPI,pred_df_train)
yhat.ar <- predict(mdl.ar,pred_df_test)
loss.ar  <- mean( (pred_df_test$y - yhat.ar)^2 )
yhat.next.ar <- predict(mdl.ar, pred_df_next)
pred_test$ar_model <- yhat.ar

### Visualizing
ggplot(pred_test, aes(x = dates)) +
  # Add the observed CPI growth line
  geom_line(aes(y = ar_model, color = "Observed CPI growth")) +
  # Add the actual CPI growth line
  geom_line(aes(y = realized, color = "Actual CPI growth")) +
  # Customize the plot
  labs(title = "AR(1) Regression",
       x = "Date",
       y = "CPI Growth") +
  theme_minimal()

### 2. Linear Regression  
mdl.full <- lm( y ~ .,pred_df_train)
yhat.full <- predict(mdl.full,pred_df_test)
loss.full  <- mean( (pred_df_test$y - yhat.full)^2 )
yhat.next.full <- predict(mdl.full, pred_df_next)
pred_test$full_model <- yhat.full
### Visualizing
ggplot(pred_test, aes(x = dates)) +
  # Add the observed CPI growth line
  geom_line(aes(y = full_model, color = "Observed CPI growth")) +
  # Add the actual CPI growth line
  geom_line(aes(y = realized, color = "Actual CPI growth")) +
  # Customize the plot
  labs(title = "Linear Regression",
       x = "Date",
       y = "CPI Growth") +
  theme_minimal()

### 3. Ridge Model
y.train <- as.numeric(pred_df_train$y)
X.train <- as.matrix(pred_df_train[,-c(1)]) #Excluding y column 
X.test <- as.matrix(pred_df_test[,-c(1)]) #Excluding y column 
pred_df_1 <- pred_df[,-c(1)] #Excluding y column 
X.next <- as.matrix(pred_df_1[254,]) #prediction for Q3 2023

set.seed(123)

library(glmnet)
mdl.ridge <- cv.glmnet(y=y.train,x=X.train,alpha = 0, lambda.min.ratio=0.0001)
ridge_coef <- coef(mdl.ridge)
yhat.ridge <- predict(mdl.ridge,X.test)
loss.ridge  <- mean( (pred_df_test$y - yhat.ridge)^2 )
yhat.next.ridge <- predict(mdl.ridge,X.next)
pred_test$ridge <- yhat.ridge

### Visualizing
ggplot(pred_test, aes(x = dates)) +
  # Add the observed CPI growth line
  geom_line(aes(y = ridge, color = "Observed CPI growth")) +
  # Add the actual CPI growth line
  geom_line(aes(y = realized, color = "Actual CPI growth")) +
  # Customize the plot
  labs(title = "Ridge Regression",
       x = "Date",
       y = "CPI Growth") +
  theme_minimal()

### FINDING THE BEST MODEL ###
MSE <- c('Full Model'=loss.full,'AR'=loss.ar,'Ridge' = loss.ridge)
names(MSE)[which.min(MSE)]
Predictions_Q3_2023 <- c('Full' = unname(yhat.next.full), 'AR' = unname(yhat.next.ar), 
                          'Ridge' = yhat.next.ridge)

#The best model with the least mean square error in predicting test data is linear regression 
#The linear model predicts 3.06% inflation for Q3 of FY23 while the actual was 3.57%.