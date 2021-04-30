# Uncomment the following two lines to install packages, if necessary
#options(needs.promptUser = FALSE)
#install.packages('tidyverse')
#install.packages('sjPlot')

# Load packages
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# Change working directory to folder containing dataset
dir <- "/Users/benposnick/Documents/Spring 2021/AEM 4510/Empirical Project"
setwd(dir)

# Load dataset and create a ln(wage) variable
vsldata <- read.csv("vsldata.txt", sep="")
vsldata <- vsldata %>%
  mutate(lnwage = log(wage))
mean_wage <- vsldata %>%
  summarise(mean_wage = mean(wage, na.rm = FALSE))

# Model (1)
model1a <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople, data=vsldata)
model1b <- lm(lnwage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople, data=vsldata)

# Model (2)
vsldata <- vsldata %>%
  mutate(age_sq = age^2)
model2a <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq, data=vsldata)
model2b <- lm(lnwage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq, data=vsldata)

# Model (3)
model3a <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed), data=vsldata)
model3b <- lm(lnwage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed), data=vsldata)

# Model (4): leaving out 'other' race dummy variable because of collinearity (i.e., 'dummy variable trap')
model4a <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan, data=vsldata)
model4b <- lm(lnwage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan, data=vsldata)

# Model (5)
vsldata <- vsldata %>%
  mutate(higher_ed = ifelse(ed >= 3, 1, 0)) %>%
  # Interactions between occupational attributes and higher education
  mutate(int_nonfatal_higher_ed = higher_ed * nonfatal) %>% 
  mutate(int_complex_higher_ed = higher_ed * complex) %>%
  mutate(int_mskill_higher_ed = higher_ed * mskill) %>%
  mutate(int_phydds_higher_ed = higher_ed * phydds) %>%
  mutate(int_worcon_higher_ed = higher_ed * worcon) %>%
  mutate(int_cskill_higher_ed = higher_ed * cskill) %>%
  # Interactions between occupational attributes and age
  mutate(int_nonfatal_age = age * nonfatal) %>%
  mutate(int_complex_age = age * complex) %>%
  mutate(int_mskill_age = age * mskill) %>%
  mutate(int_phydds_age = age * phydds) %>%
  mutate(int_worcon_age = age * worcon) %>%
  mutate(int_cskill_age = age * cskill)

model5a <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan +
               factor(region) + public + marital + union + msa + fulltime +
               int_nonfatal_higher_ed + int_complex_higher_ed + int_mskill_higher_ed +
               int_phydds_higher_ed + int_worcon_higher_ed + int_cskill_higher_ed +
               int_nonfatal_age + int_complex_age + int_mskill_age + int_phydds_age +
               int_worcon_age + int_cskill_age, data=vsldata)
model5b <- lm(lnwage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan +
               factor(region) + public + marital + union + msa + fulltime +
               int_nonfatal_higher_ed + int_complex_higher_ed + int_mskill_higher_ed +
               int_phydds_higher_ed + int_worcon_higher_ed + int_cskill_higher_ed +
               int_nonfatal_age + int_complex_age + int_mskill_age + int_phydds_age +
               int_worcon_age + int_cskill_age, data=vsldata)

# Model (6)
vsldata <- vsldata %>%
  mutate(int_fatal_marital = fatal * marital)

model6 <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan +
               factor(region) + public + marital + union + msa + fulltime +
               int_nonfatal_higher_ed + int_complex_higher_ed + int_mskill_higher_ed +
               int_phydds_higher_ed + int_worcon_higher_ed + int_cskill_higher_ed +
               int_nonfatal_age + int_complex_age + int_mskill_age + int_phydds_age +
               int_worcon_age + int_cskill_age + int_fatal_marital, data=vsldata)

# Model (7)
vsldata <- vsldata %>%
  mutate(int_fatal_age = fatal * age) %>%
  mutate(int_fatal_age_sq = fatal * age_sq)

model7 <- lm(wage ~ fatal + nonfatal + complex + mskill + phydds + worcon +
               cskill + intpeople + age + age_sq + factor(ed) + white + black + hispan +
               factor(region) + public + marital + union + msa + fulltime +
               int_nonfatal_higher_ed + int_complex_higher_ed + int_mskill_higher_ed +
               int_phydds_higher_ed + int_worcon_higher_ed + int_cskill_higher_ed +
               int_nonfatal_age + int_complex_age + int_mskill_age + int_phydds_age +
               int_worcon_age + int_cskill_age + int_fatal_age + int_fatal_age_sq, data=vsldata)

pred_labels <- c(
  `(Intercept)` = "Intercept",
  fatal = "Fatality risk (deaths per 100 workers)",
  nonfatal = "Nonfatal risk (average days missed per yaer due to injury)",
  complex = "Complexity of occupation",
  mskill = "Motor skill requirements",
  phydds = "Physical demands",
  worcon = "Unpleasant working conditions",
  cskill = "Creative skills requirements",
  intpeople = "Worker interactions with others in same occupation",
  age = "Age",
  age_sq = "Age squared",
  `factor(ed)2` = "High school graduate",
  `factor(ed)3` = "Completed some college",
  `factor(ed)4` = "Four-year college degree",
  white = "White",
  black = "Black",
  `factor(region)2` = "Mid-Atlantic Census Division",
  `factor(region)3` = "East-North Central Census Division",
  `factor(region)4` = "West-North Central Census Division",
  `factor(region)5` = "South Atlantic Census Division",
  `factor(region)6` = "East-South Central Census Division",
  `factor(region)7` = "West-South Central Census Division",
  `factor(region)8` = "Mountain Census Division",
  `factor(region)9` = "Pacific Census Division",
  public = "Public sector",
  marital = "Married",
  union = "Union member",
  msa = "Metropolital region",
  fulltime = "Fulltime worker",
  hispan = "Hispanic originc",
  int_nonfatal_higher_ed = "Nonfatal risk X higher education",
  int_complex_higher_ed = "Complexity of occupation X higher education",
  int_mskill_higher_ed = "Motor skill requirements X higher education",
  int_phydds_higher_ed = "Physical demands X higher education",
  int_worcon_higher_ed = "Unpleasant working conditions X higher education",
  int_cskill_higher_ed = "Creative skills requirements X higher education",
  int_nonfatal_age = "Nonfatal risk X age",
  int_complex_age = "Complexity of occupation X age",
  int_mskill_age = "Motor skill requirements X age",
  int_phydds_age = "Physical demands X age",
  int_worcon_age = "Unpleasant working conditions X age",
  int_cskill_age = "Creative skills requirements X age",
  int_fatal_marital = "Fatality risk X married",
  int_fatal_age = "Fatality risk X age",
  int_fatal_age_sq = "Fatality risk X age squared"
)
dv_labels <- c(
  "Model 1",
  "Model 2",
  "Model 3",
  "Model 4",
  "Model 5",
  "Model 6",
  "Model 7"
)
dv_labels_log <- c(
  "Model 1",
  "Model 2",
  "Model 3",
  "Model 4",
  "Model 5"
)

tab_model(
  model1a, model2a, model3a, model4a, model5a, model6, model7,
  pred.labels = pred_labels,
  string.pred = "Coeffcient",
  string.p = "p-Value",
  show.ci = FALSE,
  show.reflvl = TRUE,
  p.style = "stars",
  dv.labels = dv_labels
)

tab_model(
  model1b, model2b, model3b, model4b,model5b,
  pred.labels = pred_labels,
  string.pred = "Coeffcient",
  string.p = "p-Value",
  show.ci = FALSE,
  show.reflvl = TRUE,
  p.style = "stars",
  dv.labels = dv_labels_log
)

