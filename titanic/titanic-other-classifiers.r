library(tidyverse)
library(caret)

set.seed(19*13-1)

training_data_raw <- readr::read_csv("train.csv")
competition_data <- readr::read_csv("test.csv")

TitleMapper <- function(to_map)
{
  if(to_map %in% c("Capt", 
                   "Col",
                   "Don",
                   "Dr",
                   "Jonkheer",
                   "Major",
                   "Rev",
                   "Sir")) "Mr"
  else if(to_map %in% c("Lady",
                        "Mme",
                        "the Countess",
                        "Ms",
                        "Dona")) "Mrs"
  else if(to_map %in% c("Mlle")) "Miss"
  else to_map
}

training_data_processed <- training_data_raw %>%
  extract(Name,
          into = c("First","Title","Rest"),
          regex = c("^(.*),(.*?)\\.(.*)$")) %>%
  mutate(Age = ifelse(is.na(Age),
                      mean(Age,na.rm=TRUE),
                      Age)) %>%
  mutate(RandomInt = PassengerId %% 10) %>% 
  mutate_if(is.character,str_trim) %>% 
  mutate(Pclass = as.factor(Pclass)) %>% 
  mutate(Title = map_chr(Title,TitleMapper))

competition_data_processed <- competition_data %>%
  extract(Name,
          into = c("First","Title","Rest"),
          regex = c("^(.*),(.*?)\\.(.*)$")) %>%
  mutate(Age = ifelse(is.na(Age),
                      mean(Age,na.rm=TRUE),
                      Age)) %>%
  mutate(RandomInt = PassengerId %% 10) %>% 
  mutate_if(is.character,str_trim) %>% 
  mutate(Pclass = as.factor(Pclass)) %>% 
  mutate(Title = map_chr(Title,TitleMapper))

training_filter <- caret::createDataPartition(training_data_processed$Survived,
                                              p = 0.75,
                                              list = TRUE)[[1]]

training_data <- training_data_processed[ training_filter,]
testing_data  <- training_data_processed[-training_filter,]

the_glm <- glm(Survived ~
                 Title + Sex + Pclass + Pclass:Sex + Age + SibSp,
               family = binomial(link="logit"),
               data = training_data)
full_glm <- glm(Survived ~
                  Title + Sex + Pclass + Pclass:Sex + Age + SibSp,
                family = binomial(link="logit"),
                data = training_data_processed)

fit_glm <- function(threshold)
{
  tryCatch(
    {
      broom::augment_columns(the_glm,
                             data = training_data,
                             newdata = testing_data,
                             type.predict = "response") %>%
        select(PassengerId,Survived,.fitted) %>%
        rename("Predicted"=.fitted) %>%
        mutate(Predicted = ifelse(Predicted > threshold,1,0)) %>%
        mutate(status = ifelse(Survived == 1 & Predicted == 1, "TP",
                               ifelse(Survived == 1 & Predicted == 0, "FN",
                                      ifelse(Survived == 0 & Predicted == 0, "TN", "FP")))) %>%
        group_by(status) %>%
        summarise(n=n()) %>%
        spread(key=status,value=n) %>%
        mutate(sensitivity = TP / (FN + TP),
               specificity = TN / (FP + TN)) %>%
        select(sensitivity, specificity)
    },
    error = function(e) NULL
  )
}

thresholds <- seq(0.01,0.99,by=0.01)
names(thresholds) <- thresholds

roc_glm <- map_df(thresholds,
                  fit_glm,
                  .id="id")

roc_glm %>%
  ggplot(aes(x=1-specificity,
             y=sensitivity))+
  geom_line() + 
  geom_point() + 
  geom_abline() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

roc_glm %>%
  filter(sensitivity > 0.75,
         specificity > 0.75)

# select 0.35 as the threshold

broom::augment_columns(full_glm,
                       newdata=competition_data_processed,
                       type.predict = "response") %>%
  select(PassengerId,.fitted) %>%
  mutate(.fitted = ifelse(.fitted > 0.35, 1, 0)) %>%
  rename("Survived"=.fitted) %>%
  readr::write_csv("glm-2.0.csv")
                       
