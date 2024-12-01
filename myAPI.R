#myAPI.R
library(tidyverse)
library(tidymodels)
library(ranger)
library(ggplot2)

raw_data <- read_csv(file='diabetes_data.csv',show_col_types=FALSE)
data <- raw_data %>%
  mutate(Diabetes_binary = factor(Diabetes_binary,levels=c(0,1),labels=c('Not Diabetic','Diabetic')),
         GenHlth = factor(GenHlth,levels=seq(1:5),labels=c('Excellent','Very Good','Good','Fair','Poor')),
         Age = factor(Age,levels=seq(1:13),labels=c('18-24','25-29','30-34','35-39','40-44','45-49'
                                                    ,'50-54','55-59','60-64','65-69','70-74','75-79','80+')
         ),
         Education = factor(Education,levels=seq(1:6),labels=c('Never attended school or only kindergarten','Grades 1-8','Grades 9-11','Grade 12 or GED','College 1-3 years','College 4+ years')),
         Income = factor(Income,levels=seq(1:8),labels=c('<$10k','<$15k','<$20k','<$25k','<$35k','<$50k','<$75k','$75k+'))
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
#data_split <- initial_split(data,prop=0.70)
#data_train <- training(data_split)
#data_test <- testing(data_split)
#data_folds <- vfold_cv(data_train,5)

recipe <- recipe(Diabetes_binary ~ BMI + HighBP + HighChol + GenHlth + Sex + Smoker + Stroke + HvyAlcoholConsump + DiffWalk + HeartDiseaseorAttack
                       , data = data) |>
  step_normalize(BMI) |>
  step_dummy(GenHlth)

## Using our optimal parameter from our modeling file.

rf_model <- rand_forest(mtry = 4) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_wfl <- workflow() |>
  add_recipe(recipe) |>
  add_model(rf_model)

rf_fit <- rf_wfl |>
  fit(data)
  #tune_grid(resamples = data_folds,
  #          grid = 5,
  #          metrics = metric_set(mn_log_loss))

#rf_best_params <- select_best(rf_fit, metric = "mn_log_loss")

#rf_final_wfl <- rf_wfl |>
#  finalize_workflow(rf_best_params)

#rf_final_model <- rf_final_wfl |>
#  fit(data)

## Below could be final model

#rf_fit |>
#  predict(data.frame(BMI=defaults$BMI,HighBP=1,HighChol=1,Smoker=0,Stroke=0,HvyAlcoholConsump=1,GenHlth=as.character(defaults$GenHlth),Sex=1))

#matrix <- conf_mat(data |> 
#                    mutate(estimate = rf_fit |> predict(data) |> pull()), #data
#                   Diabetes_binary, #truth
#                   estimate)

#* Find out if a subject is classified as diabetic based on our model parameters
#* @param BMI Body Mass Index
#* @param HighBP High Blood Pressure flag
#* @param HighChol High Cholesterol flag
#* @param Smoker Smoker flag
#* @param Stroke Stroke flag
#* @param HvyAlcoholConsump Heavy Alcohol Consumption flag
#* @param GenHlth General Health Category
#* @param Sex Sex flag
#* @param DiffWalk Diff Walk flag
#* @param HeartDiseaseorAttack Heart disease or attack flag
#* @get /pred
function(BMI=defaults$BMI,HighBP=defaults$HighBP,HighChol=defaults$HighChol,Smoker=defaults$Smoker,Stroke=defaults$Stroke,
         HvyAlcoholConsump=defaults$HvyAlcoholConsump,GenHlth=as.character(defaults$GenHlth),Sex=defaults$Sex,
         DiffWalk=defaults$DiffWalk,HeartDiseaseorAttack=defaults$HeartDiseaseorAttack){
  df <- data.frame(BMI=as.double(BMI),
             HighBP=as.numeric(HighBP),
             HighChol=as.numeric(HighChol),
             Smoker=as.numeric(Smoker),
             Stroke=as.numeric(Stroke),
             HvyAlcoholConsump=as.numeric(HvyAlcoholConsump),
             GenHlth=GenHlth,
             Sex=as.numeric(Sex),
             DiffWalk = as.numeric(DiffWalk),
             HeartDiseaseorAttack = as.numeric(HeartDiseaseorAttack)
  )
  prediction <- rf_fit |>
    predict(df)
  if(prediction$.pred_class==1){
    print("Diabetes")
  } else {
    print("Not Diabetic")
  }
}

#http://127.0.0.1:4829/pred?BMI=40&HighBP=1&HighChol=1&Smoker=0&Stroke=0&HvyAlcoholConsump=0&GenHlth=Good&Sex=1
#http://127.0.0.1:4829/pred?BMI=19&HighBP=1&HighChol=1&Smoker=1&Stroke=1&HvyAlcoholConsump=1&GenHlth=Fair&Sex=0
#http://127.0.0.1:4829/pred?HighBP=1&HighChol=1


#* Retrieve information about the author and github pages
#* @get /info
function(){
  info <- list(author="Tyler Hunt",pages=c('https://tah96.github.io/ds_final_proj/EDA.html','https://tah96.github.io/ds_final_proj/Modeling.html'))
  return(info)
}

#* Retrieve confusion matrix from the model
#* @serializer png
#* @get /confusion
function(){
  matrix <- conf_mat(data |> 
                       mutate(estimate = rf_fit |> predict(data) |> pull()), #data
                     Diabetes_binary, #truth
                     estimate)
  plot <- ggplot2::autoplot(matrix,type="heatmap")
  print(plot)
}







