#myAPI.R
library(tidyverse)
library(tidymodels)
library(ranger)

raw_data <- read_csv(file='diabetes_data.csv',show_col_types=FALSE)
data <- raw_data %>%
  select(Diabetes_binary,BMI,HighBP,HighChol,Smoker,Stroke,HvyAlcoholConsump,GenHlth,Sex) %>%
  mutate(Diabetes_binary = as.factor(Diabetes_binary),
         HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol),
         Smoker = as.factor(Smoker),
         Stroke = as.factor(Stroke),
         HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
         GenHlth = factor(GenHlth,levels=seq(1:5),labels=c('Excellent','Very Good','Good','Fair','Poor')),
         Sex = as.factor(Sex)
  )

cat_cols <- names(data)[!names(data) %in% c('Diabetes_binary','BMI')]

getMode <- function(column_name){
  group_sym <- sym(column_name)
  group_name <- as_label(enquo(column_name))
  grouped_data <- data %>%
    group_by(!!group_sym) %>%
    summarize(count=n()) %>%
    arrange(desc(count)) %>%
    slice(1) %>%
    pull(!!group_sym)
  return(grouped_data)
}

defaults <- lapply(cat_cols,FUN=getMode)
defaults$BMI <- mean(data$BMI)
names(defaults) <- c(cat_cols,"BMI")

## Model

set.seed(123)

data <- data[1:10000,]
data_split <- initial_split(data,prop=0.70)
data_train <- training(data_split)
data_test <- testing(data_split)
data_folds <- vfold_cv(data_train,5)

recipe <- recipe(Diabetes_binary ~ BMI + HighBP + HighChol + GenHlth + Sex + Smoker + Stroke + HvyAlcoholConsump
                       , data = data_train) |>
  step_normalize(BMI) |>
  step_dummy(GenHlth)

rf_model <- rand_forest(mtry = tune()) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_wfl <- workflow() |>
  add_recipe(recipe) |>
  add_model(rf_model)

rf_fit <- rf_wfl |>
  tune_grid(resamples = data_folds,
            grid = 5,
            metrics = metric_set(mn_log_loss))

rf_best_params <- select_best(rf_fit, metric = "mn_log_loss")

rf_final_wfl <- rf_wfl |>
  finalize_workflow(rf_best_params)

rf_final_model <- rf_final_wfl |>
  fit(data)

rf_final_model |>
  predict(data.frame(BMI=40,HighBP="1",HighChol="1",Smoker="0",Stroke="0",HvyAlcoholConsump="0",GenHlth="Good",Sex="1"))
  
#query with http://localhost:PORT/ln?num=1

#* Find multiple of two numbers
#* param num1 1st number
#* param num2 2nd number
#* get /pred
test <- function(BMI=40,HighBP="1",HighChol="1",Smoker="0",Stroke="0",HvyAlcoholConsump="0",GenHlth="Good",Sex="1"){
  df <- data.frame(BMI=BMI,
             HighBP=HighBP,
             HighChol=HighChol,
             Smoker=Smoker,
             Stroke=Stroke,
             HvyAlcoholConsump=HvyAlcoholConsump,
             GenHlth=GenHlth,
             Sex=Sex)
  prediction <- rf_final_model |>
    predict(df)
  if(prediction$.pred_class==1){
    print("Diabetes")
  } else {
    print("Not Diabetic")
  }
}

