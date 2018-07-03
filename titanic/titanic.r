library(tidyverse)

one_way_plot <- function(data,x_var) {
  data %>%
    group_by(!!x_var) %>%
    summarise(response=sum(response * weight) / sum(weight),
              weight = sum(weight)) %>%
    rename("x" = !!x_var) %>%
    ggplot(aes(x=x,y=response,size=weight)) + geom_point()
}

two_way_plot <- function(data,x_var,group_var) {
  data %>%
    group_by(!!x_var, !!group_var) %>%
    summarise(response=sum(response * weight) / sum(weight),
              pred = sum(pred * weight) / sum(weight),
              weight = sum(weight)) %>%
    rename("x" = !!x_var,
           "grp" = !!group_var) %>%
    gather(key=key,value=value,-x,-grp,-weight) %>% 
    ggplot(aes(x=x,y=value,colour=grp, group = interaction(grp,key))) + 
    geom_point(aes(size=weight)) +
    geom_line(aes(linetype=key))
}

training_data <- readr::read_csv("train.csv")

# first step: bin age by 10y
# second step: factorise rating factors
# third step: remove unratable things 

factored_data <- training_data %>%
  mutate(Age = ntile(Age,10)) %>%
  mutate(Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Age = as.factor(Age), 
         SibSp = as.factor(SibSp),
         Parch = as.factor(Parch),
         Embarked = as.factor(Embarked)) %>%
  select(-Name, -Fare, -Ticket, -Cabin) %>%
  mutate(Age.Variate = as.double(Age),
         Age.Variate = ifelse(is.na(Age.Variate),-1,Age.Variate),
         Age.Cardinal = is.na(Age),
         SibSp.Cardinal = as.integer(SibSp) >= 3)

# split into training and holdout data for model validation 

holdout_data <- factored_data %>%
  filter(PassengerId %% 10 > 6)

building_data <- setdiff(factored_data, holdout_data)

model_formula_2 <- "Survived ~ Sex * Pclass 
+ Age.Variate * Sex 
+ (Age.Variate^2)* Sex
+ Age.Variate * Sex
+ Age.Cardinal 
+ Age.Variate * Pclass * Sex"

model_formula_3 <- "Survived ~ Sex * Pclass 
+ Age.Variate * Sex 
+ (Age.Variate^2)* Sex
+ Age.Variate * Sex
+ Age.Cardinal
+ SibSp.Cardinal"

model_formula_4 <- "Survived ~ Sex * Pclass 
+ Age.Variate * Sex 
+ (Age.Variate^2)* Sex
+ (Age.Variate^3) * Sex
+ Age.Cardinal * Sex
+ SibSp.Cardinal * Sex
+ Embarked"

model_formula <- model_formula_4

simple_model <- glm(model_formula, 
                    building_data, family = binomial)

modelled_dataset <- broom::augment_columns(simple_model,building_data, type.predict = "response")
holdout_dataset <- broom::augment_columns(simple_model, building_data, holdout_data,
                                          type.predict = "response")

grouped_data <- modelled_dataset %>%
  group_by(Pclass,Sex,Age,SibSp,Parch,Embarked) %>%
  summarise(response=mean(Survived),
            weight=n(),
            pred=mean(.fitted)) %>%
  ungroup

grouped_holdout_data <- holdout_dataset %>%
  group_by(Pclass,Sex,Age,SibSp,Parch,Embarked) %>%
  summarise(response=mean(Survived),
            weight=n(),
            pred=mean(.fitted)) %>%
  ungroup

grouped_data %>% two_way_plot(quo(Pclass),quo(Sex)) 
grouped_data %>% two_way_plot(quo(Age),quo(Sex)) 
grouped_data %>% two_way_plot(quo(SibSp),quo(Sex)) 
grouped_data %>% two_way_plot(quo(Embarked),quo(Sex)) 
grouped_data %>% two_way_plot(quo(Parch),quo(Sex)) 
grouped_data %>% two_way_plot(quo(Age),quo(Pclass))

grouped_holdout_data %>% two_way_plot(quo(Pclass),quo(Sex)) 
grouped_holdout_data %>% two_way_plot(quo(Age),quo(Sex)) 
grouped_holdout_data %>% two_way_plot(quo(SibSp),quo(Sex)) 
grouped_holdout_data %>% two_way_plot(quo(Embarked),quo(Sex)) 
grouped_holdout_data %>% two_way_plot(quo(Parch),quo(Sex)) 
grouped_holdout_data %>% two_way_plot(quo(Age),quo(Pclass))

# apply model to full data for final predictors

full_model <- glm(model_formula, 
                    factored_data, family = binomial)
full_modelled_dataset <- broom::augment_columns(full_model,factored_data, type.predict = "response")

full_grouped_data <- full_modelled_dataset %>%
  group_by(Pclass,Sex,Age,SibSp,Parch,Embarked) %>%
  summarise(response=mean(Survived),
            weight=n(),
            pred=mean(.fitted)) %>%
  ungroup

full_grouped_data %>% two_way_plot(quo(Pclass),quo(Sex)) 
full_grouped_data %>% two_way_plot(quo(Age),quo(Sex)) 
full_grouped_data %>% two_way_plot(quo(SibSp),quo(Sex)) 
full_grouped_data %>% two_way_plot(quo(Embarked),quo(Sex)) 
full_grouped_data %>% two_way_plot(quo(Parch),quo(Sex)) 
full_grouped_data %>% two_way_plot(quo(Age),quo(Pclass))

# load test data and use P>0.5 to determine survival 

test_data <- readr::read_csv("test.csv")

factored_test_data <- test_data %>%
  mutate(Age = ntile(Age,10)) %>%
  mutate(Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Age = as.factor(Age), 
         SibSp = as.factor(SibSp),
         Parch = as.factor(Parch),
         Embarked = as.factor(Embarked)) %>%
  select(-Name, -Fare, -Ticket, -Cabin) %>%
  mutate(Age.Variate = as.double(Age),
         Age.Variate = ifelse(is.na(Age.Variate),-1,Age.Variate),
         Age.Cardinal = is.na(Age),
         SibSp.Cardinal = as.integer(SibSp) >= 3)

predictions <- broom::augment_columns(full_model,factored_data,factored_test_data,type.predict = "response")

predictions %>%
  select(PassengerId,.fitted) %>%
  rename("Survived" = .fitted) %>%
  mutate(Survived = ifelse(Survived >= 0.25, 1, 0)) %>%
  write_csv("submission-model-4-p-0.25.csv")

predictions %>%
  select(PassengerId,.fitted) %>%
  rename("Survived" = .fitted) %>%
  mutate(Survived = ifelse(Survived >= 0.5, 1, 0)) %>%
  write_csv("submission-model-4-p-0.50.csv")

predictions %>%
  select(PassengerId,.fitted) %>%
  rename("Survived" = .fitted) %>%
  mutate(Survived = ifelse(Survived >= 0.75, 1, 0)) %>%
  write_csv("submission-model-4-p-0.75.csv")

broom::glance(full_model)
