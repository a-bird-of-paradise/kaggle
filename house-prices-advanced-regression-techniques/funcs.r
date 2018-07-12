library(party) 
library(tree)
library(rpart)
library(tidyverse)
library(tweedie)
library(statmod)

LoadDataFile <- function(the_file)
{
  sample_data <- readr::read_csv(the_file) %>%
    mutate_if(is.character, factor)  %>%
    mutate_if(is.factor,forcats::fct_explicit_na) %>%
    mutate_if(is.numeric,NA_to_minus_1) %>%
    mutate(HoldoutIndicator = Id %% 10 %in% c(3,5,9)) %>%
    mutate(MSSubClass = as.factor(MSSubClassMapper(MSSubClass))) %>%
    mutate(TotRmsAbvGrd.Grp = pmin(pmax(TotRmsAbvGrd,3),10)) %>%
    mutate(GarageArea.Grp = pmin(750,50 * (as.integer(GarageArea/50)))) %>%
    mutate(LotArea.Grp = pmax(5000,pmin(1000 * (as.integer(LotArea/1000)), 15000))) %>%
    mutate(LotFrontage.Grp = pmin(100,5 * (as.integer(LotFrontage/5)))) %>%
    mutate(YearBuilt.Grp = pmax(1900,10 * (as.integer(YearBuilt/10)))) %>%
    mutate(YearRemodAdd.Grp = pmax(1900,10 * (as.integer(YearRemodAdd/10)))) %>%
    mutate(MasVnrArea.Grp = pmin(750,pmax(0,50 * (as.integer(MasVnrArea/50))))) %>%
    mutate(BsmtFinSF1.Grp = pmin(1500,pmax(0,100 * as.integer(BsmtFinSF1/100))))%>%
    mutate(BsmtFinSF2.Grp = pmin(1500,pmax(0,100 * as.integer(BsmtFinSF2/100))))%>%
    mutate(BsmtUnfSF.Grp = pmin(1500,pmax(0,100 * as.integer(BsmtUnfSF/100))))%>%
    mutate(TotalBsmtSF.Grp = pmin(2000,pmax(0,100 * as.integer(TotalBsmtSF/100)))) %>%
    mutate(GrLivArea.Grp = pmin(2500,pmax(0,100 * as.integer(GrLivArea/100)))) %>%
    mutate(YrMoSold = 12 * (YrSold - min(YrSold)) + MoSold - 1) %>%
    mutate(YrSold = as.factor(YrSold)) %>%
    mutate(GrLivArea.Grp = pmin(2500,pmax(0,100 * as.integer(GrLivArea/100)))) %>%
    mutate(GarageYrBlt.Grp = 10 * as.integer(GarageYrBlt/10)) %>%
    mutate(GarageArea.Grp = pmin(700,50 * as.integer(GarageArea/50))) %>%
    mutate(WoodDeckSF.Grp = pmin(300,25 * as.integer(WoodDeckSF/25))) %>%
    mutate(OpenPorchSF.Grp = pmin(250, 25 * as.integer(OpenPorchSF/25))) %>%
    mutate(EnclosedPorch.Grp = pmin(250, 25 * as.integer(EnclosedPorch/25))) %>%
    mutate(ScreenPorch.Grp = pmin(250, 25 * as.integer(ScreenPorch/25))) %>%
    mutate(MiscVal.Grp = pmin(2500, 100 * as.integer(MiscVal/100))) %>%
    mutate(BedroomAbvGr.Grp = pmax(1,pmin(BedroomAbvGr,4))) %>%
    rename("FirstFlrSF" = `1stFlrSF`,
           "SecondFlrSF" = `2ndFlrSF`) %>%
    mutate(TotalSF = FirstFlrSF+SecondFlrSF) %>%
    mutate(FirstFlrSF.Grp = pmin(2000, 100 * as.integer(FirstFlrSF/100))) %>%
    mutate(SecondFlrSF.Grp = pmin(2000,100 * as.integer(SecondFlrSF/ 100))) %>%
    mutate(TotalSF.Grp = pmin(3000, 100 * as.integer(TotalSF/100))) %>%
    mutate(FootPrintRatio.Grp = as.integer(pmin(15,(LotArea / FirstFlrSF))))
  
  return(sample_data)
}

OneWayPlot <- function(ungrouped, group_by_var, response_var,
                       holdout_var = "HoldoutIndicator")
{
  group_by_var_sym <- rlang::sym(group_by_var)
  response_var_sym <- rlang::sym(response_var)
  holdout_var_sym <- rlang::sym(holdout_var)
  
  ungrouped %>%
    select(!! group_by_var_sym,!! response_var_sym, .fitted,
           !! holdout_var_sym) %>%
    rename("Modelled" = .fitted) %>%
    gather(key=key,value=value,-!! group_by_var_sym,-!! holdout_var_sym) %>%
    group_by(!! group_by_var_sym, key, !! holdout_var_sym) %>%
    summarise(mean = mean(value),
              n = n()) %>%
    rename("x" = !! group_by_var_sym) %>%
    rename("HoldoutIndicator" = !! holdout_var) %>%
    ggplot(aes(x=x,y=mean,group = key, colour = key)) +
    geom_point(aes(size=n)) + 
    geom_line(aes(linetype = key)) +
    ggtitle(group_by_var_sym) + 
    facet_grid(HoldoutIndicator ~ .)
}

TwoWayPlot <- function(ungrouped, x_var, y_var, response_var,
                       holdout_var = "HoldoutIndicator")
{
  
  x_var_sym <- rlang::sym(x_var)
  y_var_sym <- rlang::sym(y_var)
  response_var_sym <- rlang::sym(response_var)
  holdout_var_sym <- rlang::sym(holdout_var)
  
  ungrouped %>%
    select(!! x_var_sym,
           !! y_var_sym,
           !! response_var_sym,
           !! holdout_var_sym,
           "Modelled" = .fitted) %>%
    gather(key=key,value = value,
           -!! x_var_sym, -!! y_var_sym, -!! holdout_var_sym) %>%
    group_by(!! x_var_sym,
             !! y_var_sym,
             !! holdout_var_sym,
             key) %>%
    summarise(mean=mean(value),
              n=n()) %>%
    ggplot(aes(x = !! x_var_sym,y=mean,colour=key,group=key))+
    geom_point(aes(size=n,alpha=0.5)) + 
    geom_line() +
    facet_grid(vars(!!holdout_var_sym), vars(!!y_var_sym))
}


TimePlot <- function(ungrouped, x_var, facet_var, response_var)
{
  x_var_sym <- rlang::sym(x_var)
  facet_var_sym <- rlang::sym(facet_var)
  response_var_sym <- rlang::sym(response_var)
  
  ungrouped%>%
    select(!! x_var_sym,
           !! response_var_sym, 
           .fitted,
           !! facet_var_sym) %>%
    rename("Modelled" = .fitted) %>%
    gather(key=key,value=value,-!! x_var_sym, -!! facet_var_sym) %>%
    group_by(!! x_var_sym, key, !! facet_var_sym) %>%
    summarise(mean = mean(value),
              n = n()) %>%
    rename("x" = !! x_var_sym,
           "facet" = !! facet_var_sym) %>%
    ggplot(aes(x=x,y=mean,group = key, colour = key)) +
    geom_point(aes(size=n)) + 
    geom_line(aes(linetype = key)) +
    facet_wrap(~facet)
}

MSSubClassMapper <- function(x)
{
  mapper_table <- c(
    "20" = "1-STORY 1946 & NEWER ALL STYLES",
    "30" = "1-STORY 1945 & OLDER",
    "40" = "1-STORY W/FINISHED ATTIC ALL AGES",
    "45" = "1-1/2 STORY - UNFINISHED ALL AGES",
    "50" = "1-1/2 STORY FINISHED ALL AGES",
    "60" = "2-STORY 1946 & NEWER",
    "70" = "2-STORY 1945 & OLDER",
    "75" = "2-1/2 STORY ALL AGES",
    "80" = "SPLIT OR MULTI-LEVEL",
    "85" = "SPLIT FOYER",
    "90" = "DUPLEX - ALL STYLES AND AGES",
    "120" = "1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
    "150" = "1-1/2 STORY PUD - ALL AGES",
    "160" = "2-STORY PUD - 1946 & NEWER",
    "180" = "PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
    "190" = "2 FAMILY CONVERSION - ALL STYLES AND AGES")
  
  return(mapper_table[as.character(x)])
}

LinkFunction <- Gamma(link = "log")

InvestigateModel <- function(ModelFunction,
                             ModelData,
                             RatingFactors,
                             ModelName = "default",
                             ResponseVar = "Response",
                             ModelFamily = "Gamma",
                             HoldoutIndicator = "HoldoutIndicator")
{
  output_dir <- file.path(getwd(),ModelName)
  
  dir.create(output_dir)
  
  the_model <- glm(ModelFunction,
                   family = ModelFamily,
                   ModelData %>% filter(!HoldoutIndicator))
  
  augmented_data <- broom::augment_columns(the_model,
                                           ModelData %>% filter(!HoldoutIndicator),
                                           ModelData,
                                           type.predict = "response")
  
  deparse(formula(the_model)) %>%
    write(file.path(output_dir,"model-formula.txt"))
  
  if(F)
  {
    purrr::map(RatingFactors,function(x)
    {
      print(x)
      
      the_plot <- OneWayPlot(augmented_data,x,ResponseVar,HoldoutIndicator)
      
      ggsave(filename = file.path(output_dir,
                                  paste0(x,".png")),
             width = 8,
             height = 6,
             dpi = 100,
             plot = the_plot)
      
      return(x)
    })
  }
  
  return(the_model)
  
}

NA_to_minus_999 <- function(x)
{
  return(ifelse(is.na(x),-999,x))
}
NA_to_minus_1 <- function(x)
{
  return(ifelse(is.na(x),-1,x))
}
