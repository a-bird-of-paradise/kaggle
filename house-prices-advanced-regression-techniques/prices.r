setwd("~/Documents/Kaggle/house-prices-advanced-regression-techniques")
source("funcs.r")

sample_data <- LoadDataFile("train.csv")

test_data <- LoadDataFile("test.csv") %>%
  mutate(KitchenQual = forcats::fct_collapse(KitchenQual,TA = c("TA","(Missing)")),
         MSSubClass = forcats::fct_collapse(MSSubClass,
                                            `1-1/2 STORY FINISHED ALL AGES` = 
                                              c("1-1/2 STORY FINISHED ALL AGES",
                                                "1-1/2 STORY PUD - ALL AGES")),
         MSZoning = forcats::fct_collapse(MSZoning, RL = c("RL","(Missing)")),
         SaleType = forcats::fct_collapse(SaleType,WD = c("WD","(Missing)")))


rating_factors <- sample_data %>%
  select_if(is.factor) %>%
  names %>%
  union(
    sample_data %>%
      names %>%
      grep("\\.Grp$",.,value = TRUE)
  ) %>%
  union(c("GarageCars","Fireplaces","OverallQual","OverallCond",
          "FullBath","HalfBath", "YrMoSold", "LowQualFinSF",
          "BsmtFullBath","BsmtHalfBath","BedroomAbvGr","KitchenAbvGr",
          "TotRmsAbvGrd","PoolArea","MoSold")) %>%
  sort

sample_data %>% names %>% setdiff(rating_factors)

models <- c(
  dumb_model = SalePrice ~ 
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + YrSold,
  dumber_model = SalePrice ~ 
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + YrSold + BsmtFinType1,
  consistent_holdout_model = SalePrice ~
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + Neighborhood + BsmtFinType1 +
    Alley + BldgType + BsmtCond + BsmtExposure + BsmtUnfSF.Grp +
    CentralAir + ExterQual + Fireplaces + GarageArea.Grp + HeatingQC +
    KitchenQual + MasVnrType + MSSubClass + MSZoning + OverallQual +
    PavedDrive + YearBuilt.Grp + OverallCond,
  final_main_effects_model = SalePrice ~
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + Neighborhood + BsmtFinType1 +
    Alley + BldgType + BsmtCond + BsmtExposure + BsmtUnfSF.Grp +
    CentralAir + ExterQual + Fireplaces + GarageArea.Grp + HeatingQC +
    KitchenQual + MasVnrType + MSSubClass + MSZoning + OverallQual +
    PavedDrive + YearBuilt.Grp + OverallCond + BsmtFinType2 + GarageQual +
    Heating + LotConfig,
  first_interactions_model = SalePrice ~
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + Neighborhood + BsmtFinType1 +
    Alley + BldgType + BsmtCond + BsmtExposure + BsmtUnfSF.Grp +
    CentralAir + ExterQual + Fireplaces + GarageArea.Grp + HeatingQC +
    KitchenQual + MasVnrType + MSSubClass + MSZoning + OverallQual +
    PavedDrive + YearBuilt.Grp + OverallCond + BsmtFinType2 + GarageQual +
    Heating + LotConfig + FullBath:BedroomAbvGr.Grp + 
    TotalSF.Grp:SaleType,
  footprint_ratio_model = SalePrice ~
    TotRmsAbvGrd.Grp + TotRmsAbvGrd.Grp^2 + LotArea.Grp + GarageCars + 
    BsmtFinSF1.Grp + FullBath + HalfBath + Neighborhood + BsmtFinType1 +
    Alley + BldgType + BsmtCond + BsmtExposure + BsmtUnfSF.Grp +
    CentralAir + ExterQual + Fireplaces + GarageArea.Grp + HeatingQC +
    KitchenQual + MasVnrType + MSSubClass + MSZoning + OverallQual +
    PavedDrive + YearBuilt.Grp + OverallCond + BsmtFinType2 + GarageQual +
    Heating + LotConfig + FullBath:BedroomAbvGr.Grp + 
    TotalSF.Grp:SaleType + FootPrintRatio.Grp)

glms <- purrr::map2(models, names(models), function(x,y)
{
  InvestigateModel(x,
                   sample_data,
                   rating_factors,
                   y,
                   "SalePrice",
                   LinkFunction,
                   "HoldoutIndicator")
})

glms_no_holdout <- purrr::map2(models,names(models),function(x,y)
{
  output_dir <- file.path(getwd(),y)
  
  dir.create(output_dir)
  
  full_glm <- glm(x,sample_data,family = LinkFunction)
  
  broom::augment(full_glm,type.predict = "response", newdata = test_data) %>%
    select(Id,SalePrice=.fitted) %>%
    write_csv(file.path(output_dir,"predicitons.csv"))
  broom::augment(full_glm,type.predict = "response", newdata = test_data) %>%
    select(Id,SalePrice=.fitted) %>%
    mutate(SalePrice = round(SalePrice/500)*500) %>%
    write_csv(file.path(output_dir,"predicitons_nearest_500.csv"))
  
  return(full_glm)
}
)

purrr::map(glms,broom::glance) %>% bind_rows(.id="id")
purrr::map(glms_no_holdout,broom::glance) %>% bind_rows(.id="id")

the_glm <- glms_no_holdout[[6]]

broom::augment(the_glm, 
               sample_data,
               type.predict = "response") %>%
  ggplot(aes(x = pmin(LotArea,15000), 
             y = .resid,
             colour = HoldoutIndicator)) + 
  geom_point(size=1) +
  facet_grid(HoldoutIndicator ~ .)

glm_formula <- models[[6]]

glm_model <- glm(glm_formula,
                 family = Gamma(link = log),
                 sample_data %>% filter(!HoldoutIndicator))

augmented_data <- broom::augment_columns(glm_model,
                                         sample_data %>% filter(!HoldoutIndicator),
                                         sample_data,
                                         type.predict = "response")

TwoWayPlot(augmented_data %>% filter(SaleType %in% c("WD","New")),
           "TotalSF.Grp",
           "SaleType",
           "SalePrice")

OneWayPlot(augmented_data,"FootPrintRatio.Grp","SalePrice")
