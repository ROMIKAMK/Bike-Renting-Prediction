# Setting working directory
setwd("F:/Edwisor/Bike_Rental")

#get working directory
getwd()

# importing all required library
required_library <- c('ggplot2', 'corrgram', 'corrplot', 'randomForest',
                      'caret', 'class', 'e1071', 'rpart', 'mlr','grid',
                      'DMwR','usdm','dplyr','caTools','LiblineaR')

# checking for each library whether installed or not
# if not install then installing it first and then attaching to file
for (lib in required_library){
  if(!require(lib, character.only = TRUE))
  {
    install.packages(lib)
    require(lib, character.only = TRUE)
  }
}

# removing extra variable
rm(required_library,lib)


# reading csv file
df = read.csv("day.csv")
df
############################################
#                                          #
#     2.1 Exploratory Data Analysis        #
#                                          #
############################################

# checking datatypes of all columns
str(df)

continuous_columns <- c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt')
categorical_columns <- c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')

### checking numerical variables ###
# Checking numerical statistics of numerical columns (Five point summary + mean of all column)
summary(df[,continuous_columns])

### Checking categorical variable ###
# unique values in each category
lapply(df[,categorical_columns], function(feat) length(unique(feat)))

# counting of each unique values in categorical columns
lapply(df[,categorical_columns], function(feature) table(feature))

###################################
#  2.1.2 Missing value analysis   #
###################################

# checking missing value for each column and storing counting in dataframe with column name
missing_val <- data.frame(lapply(df, function(feat) sum(is.na(feat))))

###################################
#  2.1.3 outlier analysis         #
###################################

# box_plot function to plot boxplot of numerical columns
box_plot <- function(column, dataset){
  dataset$x = 1
  ggplot(aes_string(x= 'x', y = column), data = dataset)+
    stat_boxplot(geom = 'errorbar', width = 0.5)+
    geom_boxplot(outlier.size = 2, outlier.shape = 18)+
    labs(y = "", x = column)+
    ggtitle(paste(" BP :",column))
}

# hist_plot function to plot histogram of numerical variable
hist_plot <- function(column, dataset){
  ggplot(aes_string(column), data = dataset)+
    geom_histogram(aes(y=..density..), fill = 'skyblue2')+
    geom_density()+
    labs(x = gsub('\\.', ' ', column))+
    ggtitle(paste(" Histogram :",gsub('\\.', ' ', column)))
}

# calling box_plot function and storing all plots in a list
all_box_plots <- lapply(c('temp', 'atemp', 'hum', 'windspeed'),box_plot, dataset = df)

# calling hist_plot function and storing all plots in a list
all_hist_plots <- lapply(c('temp', 'atemp', 'hum', 'windspeed'),hist_plot, dataset = df)

# printing all plots
gridExtra::grid.arrange(all_box_plots[[1]],all_box_plots[[2]],all_box_plots[[3]],all_box_plots[[4]],
                        all_hist_plots[[1]],all_hist_plots[[2]],all_hist_plots[[3]],all_hist_plots[[4]],ncol=4,nrow=2)


###################################
#  2.1.4 Feature Engineering      #
###################################

# plotting barplot of a columns with respect to other column
plot_bar <- function(cat, y, fun){
  gp = aggregate(x = df[, y], by=list(cat=df[, cat]), FUN=fun)
  ggplot(gp, aes_string(x = 'cat', y = 'x'))+
    geom_bar(stat = 'identity')+
    labs(y = y, x = cat)+
    ggtitle(paste("Bar plot for",y,"wrt to",cat))
}

# plotting cnt with respect to month
plot_bar('mnth', 'cnt', 'sum')

# plotting cnt with respect to yr
plot_bar('yr', 'cnt', 'sum')

# plotting cnt with respect to yr
plot_bar('weekday', 'cnt', 'sum')

# making bins for columns mnth and weekday
# changing values of month 5th to 10th as 1 and others 0
df = transform(df, mnth = case_when(
  mnth <= 4 ~ 0, 
  mnth >= 11 ~ 0,
  TRUE   ~ 1 
))
colnames(df)[5] <- 'month_feat'

# changing values of weekday for day 0 and 1 the value will be 0 and 1 for rest
df = transform(df, weekday = case_when(
  weekday < 2 ~ 0, 
  TRUE   ~ 1 
))
colnames(df)[7] <- 'week_feat'

###################################
#  2.1.5 Feature Selection        #
###################################

# correlation plot for numerical feature
corrgram(df[,continuous_columns], order = FALSE,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlation Plot for the df data set")

# heatmap plot for continuous variables
corrplot(cor(df[,continuous_columns]), method = 'color', type = 'lower')

categorical_columns <- c('season', 'yr', 'month_feat', 'holiday', 'week_feat', 'workingday', 'weathersit')

# making every combination from categorical_columns
combined_cat <- combn(categorical_columns, 2, simplify = F)

# doing chi-square test for every combination
for(i in combined_cat){
  print(i)
  print(chisq.test(table(df[,i[1]], df[,i[2]])))
}

#Important variables
important_feat <- randomForest(cnt ~ ., data = df[,-c(1,2,14,15)],
                               ntree = 200, keep.forest = FALSE, importance = TRUE)
importance_feat_df <- data.frame(importance(important_feat, type = 1))

# checking vif of continuous column without dropping multicollinear column
vif(df[,c(10,11,12,13)])

# Checking VIF values of continuous columns after dropping multicollinear column i.e. atemp
vif(df[,c(10,12,13)])

# Making factor datatype to each category
df[,categorical_columns] <- lapply(df[,categorical_columns], as.factor)

# releasing memory of R, removing all variables except the main dataset
rm(list = setdiff(ls(),"bike_rental_dataset"))

###################################
#  2.1.7 Data after EDA           #
###################################
# creating another dataset with dropping outliers i.e. df1
df1 <- df
df1
df
# removing outliers from hum(humidity) and windspeed columns
for (i in c('hum', 'windspeed')){
  out_value = df1[,i] [df1[,i] %in% boxplot.stats(df1[,i])$out]
  df1 = df1[which(!df1[,i] %in% out_value),]
}

# checking dimension of both the datasets
dim(df)
dim(df1)

# dropping unwanted columns
drop_col <- c('instant', 'dteday', 'holiday', 'atemp', 'casual', 'registered')
df[,drop_col]<- NULL
df1[,drop_col] <- NULL
df
############################################
#                                          #
#                                          #
#   2.2.2 Building models                  #
#                                          #
#                                          #
############################################
set.seed(123)
split = sample.split(df$cnt, SplitRatio = 0.80)
train_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

split = sample.split(df1$cnt, SplitRatio = 0.80)
train_set1 = subset(df1, split == TRUE)
test_set1 = subset(df1, split == FALSE)

# making a function which will train model on training data and would be detecting K-fold R2 score , R2 score for test dataset and train dataset
fit.predict.show.performance <- function(method, train_data, test_data){
  reg_fit <- caret::train(cnt~., data = train_data, method = method)
  
  y_pred <- predict(reg_fit, test_data[,-10])
  print("R2 on test dataset")
  print(caret::R2(y_pred, test_data[,10])) 
  
  y_pred <- predict(reg_fit, train_data[,-10])
  print("R2 on train dataset")
  print(caret::R2(y_pred, train_data[,10]))
  
  # creating 10 folds of data
  ten_folds = createFolds(train_data$cnt, k = 10)
  ten_cv = lapply(ten_folds, function(fold) {
    training_fold = train_data[-fold, ]
    test_fold = train_data[fold, ]
    reg_fit <- caret::train(cnt~., data = training_fold, method = method)
    
    y_pred <- predict(reg_fit, test_fold[,-10])
    return(as.numeric(caret::R2(y_pred, test_fold[,10]))) 
  })
  sum = 0
  for(i in ten_cv){
    sum = sum + as.numeric(i)
  }
  print("K-fold (K =10) explained variance")
  print(sum/10)
}


#########################
#   Linear Regression   #
#########################

# building model for dataset df
fit.predict.show.performance('lm', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('lm', train_set1, test_set1)

#########################
#         KNN           #
#########################

# building model for dataset df
fit.predict.show.performance('knn', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('knn', train_set1, test_set1)

#########################
#        SVM            #
#########################

# Building model for dataset df
fit.predict.show.performance('svmLinear3', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('svmLinear3', train_set1, test_set1)

#############################
# Decision Tree Regression  #
#############################

# building model for dataset df
fit.predict.show.performance('rpart2', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('rpart2', train_set1, test_set1)

#########################
#  Random Forest        #
#########################

# Building model for dataset df
fit.predict.show.performance('rf', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('rf', train_set1, test_set1)

#########################
#     XGBRegressor      #
#########################

# building model for dataset df
fit.predict.show.performance('xgbTree', train_set, test_set)

# building model for dataset df1 i.e. without  outliers
fit.predict.show.performance('xgbTree', train_set1, test_set1)


############################################
#                                          #
#                                          #
#        Hyperparameter tuning             #
#                                          #
#                                          #
############################################
###############################################
#                                             #
# tuning Random Forest for df dataset  #
#                                             #
###############################################

control <- trainControl(method="repeatedcv", number=10, repeats=3)
reg_fit <- caret::train(cnt~., data = train_set, method = "rf",trControl = control)
reg_fit$bestTune
y_pred <- predict(reg_fit, test_set[,-10])
print(caret::R2(y_pred, test_set[,10]))

###############################################
#                                             #
#      tuning XGB for df dataset       #
#                                             #
###############################################

control <- trainControl(method="repeatedcv", number=10, repeats=3)
reg_fit <- caret::train(cnt~., data = train_set, method = "xgbTree",trControl = control)
reg_fit$bestTune
y_pred <- predict(reg_fit, test_set[,-10])
print(caret::R2(y_pred, test_set[,10]))


