library(tidyverse)
library(tidymodels)
library(ranger)
library(ggplot2)

## Read in data

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

## Retrieve categorical predictor columns and the most frequent category 
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

## Recipe copied from modeling file

set.seed(123)

recipe <- recipe(Diabetes_binary ~ BMI + HighBP + HighChol + GenHlth + Sex + Smoker + Stroke + HvyAlcoholConsump + DiffWalk + HeartDiseaseorAttack
                       , data = data) |>
  step_normalize(BMI) |>
  step_dummy(GenHlth)

## Using our optimal parameter from our modeling file.

rf_model <- rand_forest(mtry = 5) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rf_wfl <- workflow() |>
  add_recipe(recipe) |>
  add_model(rf_model)

rf_fit <- rf_wfl |>
  fit(data)

#* Find out if a subject is classified as diabetic based on our model parameters
#* @param BMI Body Mass Index
#* @param HighBP High Blood Pressure flag. 0 - Not High BP. 1 - High BP
#* @param HighChol High Cholesterol flag.  0 - Not High Cholesterol. 1 - High Cholesterol
#* @param Smoker Smoker flag. 0 - Has not smoked 100 cigarettes in lifetime. 1 - Has smoked >= 100 cigarettes in lifetime
#* @param Stroke Stroke flag. 0 - Not had a stroke. 1 - Had a stroke
#* @param HvyAlcoholConsump Heavy Alcohol Consumption flag. 0 - < 14 drinks per week (Male) OR < 7 drinks per week (Female). 1 >= 14 (7) drinks per week Male (Female)
#* @param GenHlth General Health Category
#* @param Sex Sex flag. 0 - Female. 1 - Male.
#* @param DiffWalk Diff Walk flag. 0 - No difficulty climbing stairs or walking. 1 - Difficulty climbing stairs or walking.
#* @param HeartDiseaseorAttack Heart disease or attack flag. 0 - Not Heart Disease or Attack. 1 - Had or has a heart disease or had an attack
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
  prediction_df <- rf_fit |>
    predict(df)
  
  if(prediction_df$.pred_class=='Diabetic'){
    prediction <- "Diabetic"
  } else {
    prediction <- "Not Diabetic"
  }
  
  return(prediction)
}

## User may have to enter in appropriate port numbers

## Default values API call
#http://.0.0.0:8000/pred?BMI=28.3824&HighBP=0&HighChol=0&Smoker=0&Stroke=0&HvyAlcoholConsump=0&GenHlth=Very%20Good&Sex=0&DiffWalk=0&HeartDiseaseorAttack=0

## High BMI with High Blood Pressure and Cholesterol
#http://0.0.0.0:8000/pred?BMI=31&HighBP=1&HighChol=1&Smoker=0&Stroke=0&HvyAlcoholConsump=0&GenHlth=Very%20Good&Sex=0&DiffWalk=0&HeartDiseaseorAttack=0

## High BMI with High Blood Pressure and Cholesterol in poor health and documented health issues
#http://0.0.0.0:8000/pred?BMI=37&HighBP=1&HighChol=1&Smoker=1&Stroke=1&HvyAlcoholConsump=0&GenHlth=Poor&Sex=1&DiffWalk=1&HeartDiseaseorAttack=1


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


