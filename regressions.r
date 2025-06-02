library(tidyverse)
library(dplyr)
library(caret)
library(sandwich)
library(lmtest)
library(stargazer)
library(car)
library(plm)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(data.table)
library(lmtest)
library(lubridate)
library(stringr)
library(fixest)

conference_calls_list100 <- read.csv("conference_calls_sample100.csv")
eventstudy02 <- read.csv("MM Permno CAR[0,2].csv")
eventstudy01 <- read.csv("MM Permno CAR[0,1].csv")
eventstudy03 <- read.csv("MM Permno CAR[0,3].csv")
eventstudy05 <- read.csv("MM Permno CAR[0,5].csv")
eventstudy10 <- read.csv("MM Permno CAR[0,10].csv")
ezprompt <- read.csv("ezprompt.csv")
goodprompt <- read.csv("goodprompt.csv")
claritytest <- read.csv("claritytest.csv")

vagueness_scores <- read.csv("vagueness_scores.csv")

# Create a key in both data frames for matching
eventstudy02 <- eventstudy02 %>%
  mutate(key = paste(permno, evtdate, sep = "_")) %>%
  select(key, car)  # keep only necessary columns

#rename eventstudy$car to car02
eventstudy02 <- eventstudy02 %>%
  rename(car02 = car)

# Create a key in both data frames for matching
eventstudy01 <- eventstudy01 %>%
  mutate(key = paste(permno, evtdate, sep = "_")) %>%
  select(key, car)  # keep only necessary columns

#rename eventstudy$car to car01
eventstudy01 <- eventstudy01 %>%
  rename(car01 = car)

# Create a key in both data frames for matching
eventstudy05 <- eventstudy05 %>%
  mutate(key = paste(permno, evtdate, sep = "_")) %>%
  select(key, car)  # keep only necessary columns

#rename eventstudy$car to car05
eventstudy05 <- eventstudy05 %>%
  rename(car05 = car)

conference_calls_list100 <- conference_calls_list100 %>%
  mutate(key = paste(PERMNO_merged, new_date, sep = "_"))

# Create a key in both data frames for matching
eventstudy03 <- eventstudy03 %>%
  mutate(key = paste(permno, evtdate, sep = "_")) %>%
  select(key, car)  # keep only necessary columns

#rename eventstudy$car to car03
eventstudy03 <- eventstudy03 %>%
  rename(car03 = car)

# Create a key in both data frames for matching
eventstudy10 <- eventstudy10 %>%
  mutate(key = paste(permno, evtdate, sep = "_")) %>%
  select(key, car)  # keep only necessary columns

#rename eventstudy$car to car10
eventstudy10 <- eventstudy10 %>%
  rename(car10 = car)

# Left join to add car and cret
conference_calls_list100 <- conference_calls_list100 %>%
  left_join(eventstudy02, by = "key")

conference_calls_list100 <- conference_calls_list100 %>%
  left_join(eventstudy01, by = "key")

conference_calls_list100 <- conference_calls_list100 %>%
  left_join(eventstudy03, by = "key")

conference_calls_list100 <- conference_calls_list100 %>%
  left_join(eventstudy05, by = "key")

conference_calls_list100 <- conference_calls_list100 %>%
  left_join(eventstudy10, by = "key")

# Optionally remove the key column
conference_calls_list100 <- conference_calls_list100 %>%
  select(-key)

# Join ezprompt
conference_calls_list100 <- conference_calls_list100 %>%
  left_join(ezprompt, by = "Doc_id")

# Join goodprompt
conference_calls_list100 <- conference_calls_list100 %>%
  left_join(goodprompt, by = "Doc_id")

# remove na from conference_call_list
conference_calls_list100 <- conference_calls_list100 %>%
  filter(!is.na(Vagueness_Score.x) & !is.na(Vagueness_Score.y))
conference_calls_list100 <- pdata.frame(conference_calls_list100)

model_ez01 <- lm(car01 ~ Vagueness_Score.x + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_ez02 <- lm(car02 ~ Vagueness_Score.x + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_ez03 <- lm(car03 ~ Vagueness_Score.x + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_ez05 <- lm(car05 ~ Vagueness_Score.x + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_ez10 <- lm(car10 ~ Vagueness_Score.x + log(DlyCap) + DlyPrc, data = conference_calls_list100)

summary(model_ez01)
summary(model_ez02)
summary(model_ez03)
summary(model_ez05)
summary(model_ez10)

model_good01 <- lm(car01 ~ Vagueness_Score.y + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_good02 <- lm(car02 ~ Vagueness_Score.y + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_good03 <- lm(car03 ~ Vagueness_Score.y + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_good05 <- lm(car05 ~ Vagueness_Score.y + log(DlyCap) + DlyPrc, data = conference_calls_list100)
model_good10 <- lm(car10 ~ Vagueness_Score.y + log(DlyCap) + DlyPrc, data = conference_calls_list100)

summary(model_good01)
summary(model_good02)
summary(model_good03)
summary(model_good05)
summary(model_good10)

# Investigate Clarity Score
conference_calls_list2000 <- read.csv("conference_calls_sample2000.csv")
conference_calls_list2000 <- conference_calls_list2000 %>%
  mutate(key = paste(PERMNO_merged, new_date, sep = "_"))

conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(eventstudy02, by = "key")
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(eventstudy01, by = "key")
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(eventstudy03, by = "key")
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(eventstudy05, by = "key")
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(eventstudy10, by = "key")

# Join claritytest
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(claritytest, by = "Doc_id")
conference_calls_list2000 <- conference_calls_list2000 %>%
  left_join(vagueness_scores, by = "Doc_id")

model_clarity01 <- lm(car01 ~ Clarity_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity02 <- lm(car02 ~ Clarity_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity03 <- lm(car03 ~ Clarity_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity05 <- lm(car05 ~ Clarity_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity10 <- lm(car10 ~ Clarity_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)

summary(model_clarity01)
summary(model_clarity02)
summary(model_clarity03)
summary(model_clarity05)
summary(model_clarity10)

model_vagueness01 <- lm(car01 ~ Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_vagueness02 <- lm(car02 ~ Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_vagueness03 <- lm(car03 ~ Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_vagueness05 <- lm(car05 ~ Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_vagueness10 <- lm(car10 ~ Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)

summary(model_vagueness01)
summary(model_vagueness02)
summary(model_vagueness03)
summary(model_vagueness05)
summary(model_vagueness10)

model_clarity_vagueness01 <- lm(car01 ~ Clarity_Score + Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity_vagueness02 <- lm(car02 ~ Clarity_Score + Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity_vagueness03 <- lm(car03 ~ Clarity_Score + Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity_vagueness05 <- lm(car05 ~ Clarity_Score + Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)
model_clarity_vagueness10 <- lm(car10 ~ Clarity_Score + Vagueness_Score + log(DlyCap) + DlyPrc, data = conference_calls_list2000)

summary(model_clarity_vagueness01)
summary(model_clarity_vagueness02)
summary(model_clarity_vagueness03)
summary(model_clarity_vagueness05)
summary(model_clarity_vagueness10)

#check for autocorrelation model_ez01
# Create a function to check for autocorrelation
check_autocorrelation <- function(model) {
  # Get the residuals
  residuals <- resid(model)
  
  # Perform the Durbin-Watson test
  dw_test <- dwtest(model)
  
  # Print the results
  print(dw_test)
  
  # Plot the residuals
  plot(residuals, main = "Residuals Plot", ylab = "Residuals", xlab = "Index")
  abline(h = 0, col = "red")
}
# Check for autocorrelation in the models
check_autocorrelation(model_ez01)
check_autocorrelation(model_good01)
check_autocorrelation(model_ez02)
check_autocorrelation(model_good02)
check_autocorrelation(model_ez03)
check_autocorrelation(model_good03)
check_autocorrelation(model_ez05)
check_autocorrelation(model_good05)
check_autocorrelation(model_ez10)
check_autocorrelation(model_good10)

# Check for heteroskedasticity
check_heteroskedasticity <- function(model) {
  # Perform the Breusch-Pagan test
  bp_test <- bptest(model)
  
  # Print the results
  print(bp_test)
  
  # Plot the residuals vs fitted values
  plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", ylab = "Residuals", xlab = "Fitted Values")
  abline(h = 0, col = "red")
}

# Check for heteroskedasticity in the models
check_heteroskedasticity(model_ez01)
check_heteroskedasticity(model_good01)
check_heteroskedasticity(model_ez02)
check_heteroskedasticity(model_good02)
check_heteroskedasticity(model_ez03)
check_heteroskedasticity(model_good03)
check_heteroskedasticity(model_ez05)
check_heteroskedasticity(model_good05)
check_heteroskedasticity(model_ez10)
check_heteroskedasticity(model_good10)

# Check for multicollinearity
check_multicollinearity <- function(model) {
  # Calculate the Variance Inflation Factor (VIF)
  vif_values <- vif(model)
  
  # Print the VIF values
  print(vif_values)
  
  # Check for high VIF values
  if (any(vif_values > 5)) {
    cat("Warning: High VIF values detected. Possible multicollinearity.\n")
  }
}

# Check for multicollinearity in the models
check_multicollinearity(model_ez01)
check_multicollinearity(model_good01)
check_multicollinearity(model_ez02)
check_multicollinearity(model_good02)
check_multicollinearity(model_ez03)
check_multicollinearity(model_good03)
check_multicollinearity(model_ez05)
check_multicollinearity(model_good05)
check_multicollinearity(model_ez10)
check_multicollinearity(model_good10)
check_multicollinearity(model_clarity01)

###################################
conference_calls_original <- read.csv("conference_calls.csv")

# Remove 'GMT', parse datetime, and reformat
conference_calls_original$DateTime <- conference_calls_original$Date |>
  str_remove(" GMT") |>
  dmy_hm() |>
  format("%Y-%m-%d %H-%M")

conference_calls <- read.csv("updated_filtered_conference_calls.csv")

#left join datetime of on conference_calls by Doc_id
conference_calls <- conference_calls %>%
  left_join(conference_calls_original %>% select(Doc_id, DateTime), by = "Doc_id")

# Drop columns 11 12 from conference_calls2
conference_calls <- conference_calls %>%
  select(-c(11, 12))

# Add yearquarter dummy (format YYYYQX) from new_date currently formatted YYYY-MM-DD
conference_calls <- conference_calls %>%
  mutate(new_date = as.Date(new_date)) %>%
  mutate(YearQuarter = paste0(format(new_date, "%Y"), "Q", quarter(new_date)))

conference_calls <- conference_calls %>%
  mutate(key = paste(PERMNO_merged, new_date, sep = "_"))

# Add industry classification FF48
sic <- read.csv("SIC.csv")
colnames(sic)[1] <- "PERMNO_merged"
colnames(sic)[5] <- "sic"
sic <- sic %>%
  select(PERMNO_merged, sic)
sic <- distinct(sic)
sic <- sic %>%
  group_by(PERMNO_merged) %>%
  summarise(sic = first(sic))

sic_to_ff48 <- function(sic_code) {
  # Ensure sic_code is numeric
  sic <- suppressWarnings(as.numeric(sic_code))
  
  if (is.na(sic)) {
    return(list(FF48_Code = NA_integer_, FF48_ShortName = "Invalid SIC", FF48_LongName = "Input SIC code is not a number"))
  }
  
  # 1 Agric Agriculture
  if ((sic >= 100 && sic <= 299) || (sic >= 700 && sic <= 799) || (sic >= 910 && sic <= 919) || sic == 2048) {
    return(list(FF48_Code = 1, FF48_ShortName = "Agric", FF48_LongName = "Agriculture"))
  }
  # 2 Food Food Products
  else if ((sic >= 2000 && sic <= 2039) || (sic >= 2040 && sic <= 2046) || (sic >= 2050 && sic <= 2059) || (sic >= 2060 && sic <= 2063) || (sic >= 2070 && sic <= 2079) || (sic >= 2090 && sic <= 2092) || sic == 2095 || (sic >= 2098 && sic <= 2099)) {
    return(list(FF48_Code = 2, FF48_ShortName = "Food", FF48_LongName = "Food Products"))
  }
  # 3 Soda Candy & Soda
  else if ((sic >= 2064 && sic <= 2068) || (sic >= 2086 && sic <= 2087) || (sic >= 2096 && sic <= 2097)) {
    return(list(FF48_Code = 3, FF48_ShortName = "Soda", FF48_LongName = "Candy & Soda"))
  }
  # 4 Beer Beer & Liquor
  else if (sic == 2080 || (sic >= 2082 && sic <= 2085)) {
    return(list(FF48_Code = 4, FF48_ShortName = "Beer", FF48_LongName = "Beer & Liquor"))
  }
  # 5 Smoke Tobacco Products
  else if (sic >= 2100 && sic <= 2199) {
    return(list(FF48_Code = 5, FF48_ShortName = "Smoke", FF48_LongName = "Tobacco Products"))
  }
  # 6 Toys Recreation
  else if ((sic >= 920 && sic <= 999) || (sic >= 3650 && sic <= 3652) || sic == 3732 || (sic >= 3930 && sic <= 3931) || (sic >= 3940 && sic <= 3949)) {
    return(list(FF48_Code = 6, FF48_ShortName = "Toys", FF48_LongName = "Recreation"))
  }
  # 7 Fun Entertainment
  else if ((sic >= 7800 && sic <= 7833) || (sic >= 7840 && sic <= 7841) || sic == 7900 || (sic >= 7910 && sic <= 7911) || (sic >= 7920 && sic <= 7929) || (sic >= 7930 && sic <= 7933) || (sic >= 7940 && sic <= 7949) || sic == 7980 || (sic >= 7990 && sic <= 7999)) {
    return(list(FF48_Code = 7, FF48_ShortName = "Fun", FF48_LongName = "Entertainment"))
  }
  # 8 Books Printing and Publishing
  else if ((sic >= 2700 && sic <= 2749) || (sic >= 2770 && sic <= 2771) || (sic >= 2780 && sic <= 2799)) {
    return(list(FF48_Code = 8, FF48_ShortName = "Books", FF48_LongName = "Printing and Publishing"))
  }
  # 9 Hshld Consumer Goods
  else if (sic == 2047 || (sic >= 2391 && sic <= 2392) || (sic >= 2510 && sic <= 2519) || (sic >= 2590 && sic <= 2599) || (sic >= 2840 && sic <= 2844) || (sic >= 3160 && sic <= 3161) || (sic >= 3170 && sic <= 3172) || (sic >= 3190 && sic <= 3199) || sic == 3229 || (sic >= 3230 && sic <= 3231) || sic == 3260 || (sic >= 3262 && sic <= 3263) || sic == 3269 || (sic >= 3630 && sic <= 3639) || (sic >= 3750 && sic <= 3751) || sic == 3800 || (sic >= 3860 && sic <= 3861) || (sic >= 3870 && sic <= 3873) || (sic >= 3910 && sic <= 3911) || (sic >= 3914 && sic <= 3915) || (sic >= 3960 && sic <= 3962) || sic == 3991 || sic == 3995) {
    return(list(FF48_Code = 9, FF48_ShortName = "Hshld", FF48_LongName = "Consumer Goods"))
  }
  # 10 Clths Apparel
  else if ((sic >= 2300 && sic <= 2390) || (sic >= 3020 && sic <= 3021) || (sic >= 3100 && sic <= 3111) || (sic >= 3130 && sic <= 3131) || (sic >= 3140 && sic <= 3151) || (sic >= 3963 && sic <= 3965)) {
    return(list(FF48_Code = 10, FF48_ShortName = "Clths", FF48_LongName = "Apparel"))
  }
  # 11 Hlth Healthcare
  else if (sic >= 8000 && sic <= 8099) {
    return(list(FF48_Code = 11, FF48_ShortName = "Hlth", FF48_LongName = "Healthcare"))
  }
  # 12 MedEq Medical Equipment
  else if (sic == 3693 || (sic >= 3840 && sic <= 3851)) {
    return(list(FF48_Code = 12, FF48_ShortName = "MedEq", FF48_LongName = "Medical Equipment"))
  }
  # 13 Drugs Pharmaceutical Products
  else if ((sic >= 2830 && sic <= 2831) || (sic >= 2833 && sic <= 2836)) {
    return(list(FF48_Code = 13, FF48_ShortName = "Drugs", FF48_LongName = "Pharmaceutical Products"))
  }
  # 14 Chems Chemicals
  else if (((sic >= 2800 && sic <= 2829) || (sic >= 2850 && sic <= 2879) || (sic >= 2890 && sic <= 2899)) &&
           !((sic >= 2830 && sic <= 2836) || (sic >= 2840 && sic <= 2844))) {
    return(list(FF48_Code = 14, FF48_ShortName = "Chems", FF48_LongName = "Chemicals"))
  }
  # 15 Rubbr Rubber and Plastic Products
  else if ((sic == 3031 || sic == 3041 || (sic >= 3050 && sic <= 3099)) &&
           !(sic >= 3020 && sic <= 3021)) {
    return(list(FF48_Code = 15, FF48_ShortName = "Rubbr", FF48_LongName = "Rubber and Plastic Products"))
  }
  # 16 Txtls Textiles
  else if (((sic >= 2200 && sic <= 2284) || (sic >= 2290 && sic <= 2295) || (sic >= 2297 && sic <= 2299) || (sic >= 2393 && sic <= 2395) || (sic >= 2397 && sic <= 2399)) &&
           sic != 2296) {
    return(list(FF48_Code = 16, FF48_ShortName = "Txtls", FF48_LongName = "Textiles"))
  }
  # 17 BldMt Construction Materials
  else if ((sic >= 800 && sic <= 899) || (sic >= 2400 && sic <= 2439) || (sic >= 2450 && sic <= 2459) || (sic >= 2490 && sic <= 2499) || (sic >= 2660 && sic <= 2661) || (sic >= 2950 && sic <= 2952) || sic == 3200 || (sic >= 3210 && sic <= 3211) || sic == 3241 || (sic >= 3250 && sic <= 3259) || sic == 3261 || sic == 3264 || (sic >= 3270 && sic <= 3275) || (sic >= 3280 && sic <= 3281) || (sic >= 3290 && sic <= 3293) || (sic >= 3295 && sic <= 3299) || (sic >= 3420 && sic <= 3429) || (sic >= 3430 && sic <= 3433) || (sic >= 3440 && sic <= 3442) || sic == 3446 || sic == 3448 || sic == 3449 || (sic >= 3450 && sic <= 3452) || (sic >= 3490 && sic <= 3499) || sic == 3996) {
    return(list(FF48_Code = 17, FF48_ShortName = "BldMt", FF48_LongName = "Construction Materials"))
  }
  # 18 Cnstr Construction
  else if ((sic >= 1500 && sic <= 1799)) {
    return(list(FF48_Code = 18, FF48_ShortName = "Cnstr", FF48_LongName = "Construction"))
  }
  # 19 Steel Steel Works Etc
  else if (sic == 3300 || (sic >= 3310 && sic <= 3379) || (sic >= 3390 && sic <= 3399)) {
    return(list(FF48_Code = 19, FF48_ShortName = "Steel", FF48_LongName = "Steel Works Etc"))
  }
  # 20 FabPr Fabricated Products
  else if (sic == 3400 || (sic >= 3443 && sic <= 3444) || (sic >= 3460 && sic <= 3479)) {
    return(list(FF48_Code = 20, FF48_ShortName = "FabPr", FF48_LongName = "Fabricated Products"))
  }
  # 21 Mach Machinery
  else if (((sic >= 3510 && sic <= 3536) || sic == 3538 || (sic >= 3540 && sic <= 3569) || sic == 3580 || (sic >= 3581 && sic <= 3582) || sic == 3585 || sic == 3586 || sic == 3589 || (sic >= 3590 && sic <= 3599)) &&
           !((sic >= 3570 && sic <= 3579) || sic == 3537) ) {
    return(list(FF48_Code = 21, FF48_ShortName = "Mach", FF48_LongName = "Machinery"))
  }
  # 22 ElcEq Electrical Equipment
  else if ((sic == 3600 || (sic >= 3610 && sic <= 3613) || (sic >= 3620 && sic <= 3621) || (sic >= 3623 && sic <= 3629) || (sic >= 3640 && sic <= 3646) || (sic >= 3648 && sic <= 3649) || sic == 3660 || sic == 3690 || (sic >= 3691 && sic <= 3692) || sic == 3699) &&
           !((sic >= 3650 && sic <= 3652) || sic == 3693 || sic == 3647 || sic == 3694 ||
             (sic >= 3680 && sic <= 3689) || sic == 3695 || sic == 3622 ||
             (sic >= 3661 && sic <= 3669) || (sic >= 3670 && sic <= 3679) ||
             (sic >= 3630 && sic <= 3639))) {
    return(list(FF48_Code = 22, FF48_ShortName = "ElcEq", FF48_LongName = "Electrical Equipment"))
  }
  # 23 Autos Automobiles and Trucks
  else if (sic == 2296 || sic == 2396 || (sic >= 3010 && sic <= 3011) || sic == 3537 || sic == 3647 || sic == 3694 || sic == 3700 || (sic >= 3710 && sic <= 3711) || (sic >= 3713 && sic <= 3716) || (sic >= 3790 && sic <= 3792) || sic == 3799 ) {
    return(list(FF48_Code = 23, FF48_ShortName = "Autos", FF48_LongName = "Automobiles and Trucks"))
  }
  # 24 Aero Aircraft
  else if ((sic >= 3720 && sic <= 3721) || (sic >= 3723 && sic <= 3725) || (sic >= 3728 && sic <= 3729)) {
    return(list(FF48_Code = 24, FF48_ShortName = "Aero", FF48_LongName = "Aircraft"))
  }
  # 25 Ships Shipbuilding, Railroad Equipment
  else if (((sic >= 3730 && sic <= 3731) || (sic >= 3740 && sic <= 3743)) &&
           !(sic == 3732 || (sic >= 3750 && sic <= 3751)) ) {
    return(list(FF48_Code = 25, FF48_ShortName = "Ships", FF48_LongName = "Shipbuilding, Railroad Equipment"))
  }
  # 26 Guns Defense
  else if ((sic >= 3480 && sic <= 3489) || (sic >= 3760 && sic <= 3769) || sic == 3795) {
    return(list(FF48_Code = 26, FF48_ShortName = "Guns", FF48_LongName = "Defense"))
  }
  # 27 Gold Precious Metals
  else if (sic >= 1040 && sic <= 1049) {
    return(list(FF48_Code = 27, FF48_ShortName = "Gold", FF48_LongName = "Precious Metals"))
  }
  # 28 Mines Non-Metallic and Industrial Metal Mining
  else if (((sic >= 1000 && sic <= 1039) || (sic >= 1050 && sic <= 1119) || (sic >= 1400 && sic <= 1499)) &&
           !(sic >= 1040 && sic <= 1049)) {
    return(list(FF48_Code = 28, FF48_ShortName = "Mines", FF48_LongName = "Non-Metallic and Industrial Metal Mining"))
  }
  # 29 Coal Coal
  else if (sic >= 1200 && sic <= 1299) {
    return(list(FF48_Code = 29, FF48_ShortName = "Coal", FF48_LongName = "Coal"))
  }
  # 30 Oil Petroleum and Natural Gas
  else if (((sic >= 1300 && sic <= 1339) || (sic >= 1370 && sic <= 1389) || (sic >= 2900 && sic <= 2912) || (sic >= 2990 && sic <= 2999)) &&
           !(sic >= 2950 && sic <= 2952)) {
    return(list(FF48_Code = 30, FF48_ShortName = "Oil", FF48_LongName = "Petroleum and Natural Gas"))
  }
  # 31 Util Utilities
  else if (sic == 4900 || (sic >= 4910 && sic <= 4911) || (sic >= 4920 && sic <= 4925) || (sic >= 4930 && sic <= 4942)) {
    return(list(FF48_Code = 31, FF48_ShortName = "Util", FF48_LongName = "Utilities"))
  }
  # 32 Telcm Communication
  else if (sic == 4800 || (sic >= 4810 && sic <= 4841) || (sic >= 4880 && sic <= 4892) || sic == 4899) {
    return(list(FF48_Code = 32, FF48_ShortName = "Telcm", FF48_LongName = "Communication"))
  }
  # 33 PerSv Personal Services
  else if (((sic >= 7020 && sic <= 7021) || (sic >= 7030 && sic <= 7033) || sic == 7200 || (sic >= 7210 && sic <= 7212) || sic == 7214 || (sic >= 7215 && sic <= 7217) || sic == 7219 || (sic >= 7220 && sic <= 7221) || (sic >= 7230 && sic <= 7241) || (sic >= 7250 && sic <= 7299) || sic == 7395 || sic == 7500 || (sic >= 7510 && sic <= 7515) || (sic >= 7520 && sic <= 7549) || sic == 7600 || sic == 7620 || sic == 7622 || sic == 7623 || sic == 7629 || (sic >= 7630 && sic <= 7631) || (sic >= 7640 && sic <= 7641) || (sic >= 7690 && sic <= 7699) || (sic >= 8100 && sic <= 8499) || (sic >= 8600 && sic <= 8699) || (sic >= 8800 && sic <= 8899)) &&
           !((sic >= 7800 && sic <= 7999) || (sic >= 8000 && sic <= 8099) ||
             (sic >= 7000 && sic <= 7019) || (sic >= 7040 && sic <= 7049) || sic == 7213 ||
             sic == 7218 || (sic >= 7300 && sic <= 7389 && !(sic >= 7370 && sic <= 7379) && sic != 7395)|| # Adjusted to not exclude all of 73xx
             (sic >= 7391 && sic <= 7394 ) || (sic >= 7396 && sic <= 7399 && sic != 7395) ||
             sic == 7519 || (sic >= 8700 && sic <= 8748) || (sic >= 8900 && sic <= 8999) )) {
    return(list(FF48_Code = 33, FF48_ShortName = "PerSv", FF48_LongName = "Personal Services"))
  }
  # 34 BusSv Business Services
  else if (((sic >= 2750 && sic <= 2759) || sic == 3993 || (sic >= 4220 && sic <= 4229) || sic == 7218 || sic == 7300 || (sic >= 7310 && sic <= 7339) || (sic >= 7340 && sic <= 7342) || sic == 7349 || (sic >= 7350 && sic <= 7369) || (sic >= 7370 && sic <= 7372) || (sic >= 7374 && sic <= 7379) || (sic >= 7380 && sic <= 7385) || (sic >= 7389 && sic <= 7394 && sic != 7395) || (sic >= 7396 && sic <= 7397) || sic == 7399 || sic == 7519 || (sic >= 8700 && sic <= 8734) || (sic >= 8740 && sic <= 8748) || (sic >= 8900 && sic <= 8911) || (sic >= 8920 && sic <= 8999)) &&
           !(sic == 7373 || sic == 7395 )) { # Kept Comps exclusion, ensure 7395 (PerSv) is excluded
    return(list(FF48_Code = 34, FF48_ShortName = "BusSv", FF48_LongName = "Business Services"))
  }
  # 35 Comps Computers
  else if ((sic >= 3570 && sic <= 3579) || (sic >= 3680 && sic <= 3689) || sic == 3695 || sic == 7373) {
    return(list(FF48_Code = 35, FF48_ShortName = "Comps", FF48_LongName = "Computers"))
  }
  # 36 Chips Electronic Equipment
  else if (sic == 3622 || (sic >= 3661 && sic <= 3679) || sic == 3810 || sic == 3812 ) {
    return(list(FF48_Code = 36, FF48_ShortName = "Chips", FF48_LongName = "Electronic Equipment"))
  }
  # 37 LabEq Measuring and Control Equipment
  else if (sic == 3811 || (sic >= 3820 && sic <= 3827) || sic == 3829 || (sic >= 3830 && sic <= 3839)) {
    return(list(FF48_Code = 37, FF48_ShortName = "LabEq", FF48_LongName = "Measuring and Control Equipment"))
  }
  # 38 Paper Business Supplies
  else if (((sic >= 2520 && sic <= 2549) || (sic >= 2600 && sic <= 2639) || (sic >= 2670 && sic <= 2699) || (sic >= 2760 && sic <= 2761) || (sic >= 3950 && sic <= 3955)) &&
           !((sic >= 2660 && sic <= 2661) || (sic >= 2640 && sic <= 2659))) {
    return(list(FF48_Code = 38, FF48_ShortName = "Paper", FF48_LongName = "Business Supplies"))
  }
  # 39 Boxes Shipping Containers
  else if ((sic >= 2440 && sic <= 2449) || (sic >= 2640 && sic <= 2659) || (sic >= 3220 && sic <= 3221) || (sic >= 3410 && sic <= 3412)) {
    return(list(FF48_Code = 39, FF48_ShortName = "Boxes", FF48_LongName = "Shipping Containers"))
  }
  # 40 Trans Transportation
  else if (((sic >= 4000 && sic <= 4013) || (sic >= 4040 && sic <= 4049) || sic == 4100 || (sic >= 4110 && sic <= 4151) || (sic >= 4170 && sic <= 4173) || (sic >= 4190 && sic <= 4199) || (sic >= 4200 && sic <= 4219) || (sic >= 4230 && sic <= 4231) || (sic >= 4240 && sic <= 4249) || (sic >= 4400 && sic <= 4789)) &&
           !(sic >= 4220 && sic <= 4229)) {
    return(list(FF48_Code = 40, FF48_ShortName = "Trans", FF48_LongName = "Transportation"))
  }
  # 41 Whlsl Wholesale
  else if ((sic >= 5000 && sic <= 5199)) {
    return(list(FF48_Code = 41, FF48_ShortName = "Whlsl", FF48_LongName = "Wholesale"))
  }
  # 42 Rtail Retail
  else if ((sic >= 5200 && sic <= 5999) && !(sic >= 5800 && sic <= 5899) ) {
    return(list(FF48_Code = 42, FF48_ShortName = "Rtail", FF48_LongName = "Retail"))
  }
  # 43 Meals Restaurants, Hotels, Motels
  else if ((sic >= 5800 && sic <= 5899) || (sic >= 7000 && sic <= 7019) || (sic >= 7040 && sic <= 7049) || sic == 7213) {
    return(list(FF48_Code = 43, FF48_ShortName = "Meals", FF48_LongName = "Restaurants, Hotels, Motels"))
  }
  # 44 Banks Banking
  else if ((sic >= 6000 && sic <= 6199)) {
    return(list(FF48_Code = 44, FF48_ShortName = "Banks", FF48_LongName = "Banking"))
  }
  # 45 Insur Insurance
  else if ((sic >= 6300 && sic <= 6411)) {
    return(list(FF48_Code = 45, FF48_ShortName = "Insur", FF48_LongName = "Insurance"))
  }
  # 46 RlEst Real Estate
  else if ((sic >= 6500 && sic <= 6611) && !(sic >= 6700 && sic <= 6799) ) {
    return(list(FF48_Code = 46, FF48_ShortName = "RlEst", FF48_LongName = "Real Estate"))
  }
  # 47 Fin Trading
  else if ((sic >= 6200 && sic <= 6299) || (sic >= 6700 && sic <= 6799)) {
    return(list(FF48_Code = 47, FF48_ShortName = "Fin", FF48_LongName = "Trading"))
  }
  # 48 Other Almost Nothing
  else if ((sic >= 4950 && sic <= 4971) || (sic >= 4990 && sic <= 4991)) {
    return(list(FF48_Code = 48, FF48_ShortName = "Other", FF48_LongName = "Almost Nothing"))
  }
  # No match
  else {
    return(list(FF48_Code = NA_integer_, FF48_ShortName = "NoMatch", FF48_LongName = "SIC code not in Fama-French 48 Definition"))
  }
}

sic <- sic %>%
  mutate(
    ff48_mapping = lapply(sic, sic_to_ff48),
    ff48 = sapply(ff48_mapping, function(x) x$FF48_LongName)
  ) %>%
  select(-ff48_mapping)

sic <- sic %>%
  select(PERMNO_merged, ff48)

conference_calls <- conference_calls %>%
  left_join(sic, by = "PERMNO_merged")

# Left join to add Book to Market
booktomarket <- read.csv("BMData.csv")

# 1. Ensure both are data.tables and dates are Date type
setDT(booktomarket)[, public_date := as.Date(public_date)]
setDT(conference_calls)[, new_date     := as.Date(new_date)]

# 2. Rename for consistency
setnames(booktomarket, "permno", "PERMNO_merged")

# 3. Set key on booktomarket for rolling join
setkey(booktomarket, PERMNO_merged, public_date)

# 4. Perform the rolling (as-of) join, keeping only PERMNO_merged, new_date, and bm_lagged
bm_lagged_dt <- booktomarket[conference_calls,
                             on = .(PERMNO_merged, public_date <= new_date),  # non-equi roll
                             mult = "last",                                   # pick the latest available
                             .(PERMNO_merged,
                               new_date,
                               bm_lagged = bm)                                # only these cols
]

bm_lagged_dt <- as.data.frame(bm_lagged_dt) %>%
  mutate(key = paste(PERMNO_merged, new_date, sep = "_"))

bm_lagged_dt <- bm_lagged_dt[,3:4]

# 5. Add bm_lagged_dt$bm_lagged to conference_calls by key
conference_calls <- conference_calls %>%
  left_join(bm_lagged_dt, by = "key")

# Left join to add car
conference_calls <- conference_calls %>%
  left_join(eventstudy01, by = "key")
conference_calls <- conference_calls %>%
  left_join(eventstudy02, by = "key")
conference_calls <- conference_calls %>%
  left_join(eventstudy03, by = "key")
conference_calls <- conference_calls %>%
  left_join(eventstudy05, by = "key")
conference_calls <- conference_calls %>%
  left_join(eventstudy10, by = "key")

# Add vagueness score
conference_calls <- conference_calls %>%
  left_join(vagueness_scores, by = "Doc_id")

# Remove rows with NA anywhere
conference_calls <- conference_calls %>%
  filter(complete.cases(.))

# Remove duplicates
conference_calls <- conference_calls %>%
  distinct(PERMNO_merged, DateTime, .keep_all = TRUE)

# Winsorize conference_calls$car
winsorize <- function(x, lower = 0.01, upper = 0.99) {
  quantiles <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

# Apply winsorize
# conference_calls$car01 <- winsorize(conference_calls$car01)

# Add other NLP scores
gunning_fog <- read.csv("doc_gunning_fog_scores_combined_fix.csv")
conference_calls <- conference_calls %>%
  left_join(gunning_fog, by = "Doc_id")

lexicon_scores <- read.csv("doc_lexicon_based_scores_fix.csv")
conference_calls <- conference_calls %>%
  left_join(lexicon_scores, by = "Doc_id")

vader_sentiment <- read.csv("doc_vader_sentiment_scores_fix.csv")
conference_calls <- conference_calls %>%
  left_join(vader_sentiment, by = "Doc_id")

cols_to_move <- conference_calls %>%
  select(c(DlyPrc, DlyCap, DlyVol, DlyPrcVol, bm_lagged))

conference_calls <- conference_calls %>%
  select(-c(DlyRet, DlyClose, DlyPrc, DlyCap, DlyVol, DlyPrcVol, bm_lagged, total_words_for_lex_analysis))

#add cols_to_move to conference_calls right side
conference_calls <- cbind(conference_calls, cols_to_move)

# Correlation matrix conference_calls column 23 onwards by making a matrix using ggplot2
correlation_matrix <- conference_calls %>%
  select(10:ncol(conference_calls)) %>%
  cor(use = "pairwise.complete.obs")
ggcorrplot(correlation_matrix,
           lab = TRUE)

# Investigate CAR plot histogram
hist(conference_calls$car01,
     breaks = 100,
     main = "Histogram of CAR [0,1]",
     xlab = "CAR [0,1]",
     col = "lightblue",
     border = "black")
     
# grab car01 values

# Drop highly correlated variables and other unnecessary ones
conference_calls <- conference_calls %>%
  select(-c(VADER_Neutral, lm_weak_modal_ratio))

# Make conference_calls a panel data frame
conference_calls$ff48 <- as.factor(conference_calls$ff48)
conference_calls <- pdata.frame(conference_calls, index = c("PERMNO_merged", "DateTime"))
conference_calls$YearQuarter <- as.factor(conference_calls$YearQuarter)
conference_calls$Doc_id <- as.factor(conference_calls$Doc_id)
conference_calls$PERMNO_merged <- as.factor(conference_calls$PERMNO_merged)

# Summary data
stargazer(conference_calls, type = "text", 
          title = "Summary Statistics of Complete Conference Calls Dataset",
          digits = 2, 
          summary.stat = c("mean", "sd", "min", "max", "n"))

# count unique instances of conference_calls$Vagueness_Score
vagueness_score_counts <- conference_calls %>%
  group_by(Vagueness_Score) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
head(vagueness_score_counts, 10)

# Create the binned variable 'Vagueness_Bin'
conference_calls <- conference_calls %>%
  mutate(
    Vagueness_Bin = case_when(
      Vagueness_Score <= 0.55 ~ "Lo",
      Vagueness_Score > 0.55 & Vagueness_Score <= 0.70 ~ "Me",
      Vagueness_Score > 0.70 ~ "Hi",
      TRUE ~ NA_character_
    )
  )

# Convert the new column to an ordered factor
conference_calls$Vagueness_Bin <- factor(
  conference_calls$Vagueness_Bin,
  levels = c("Lo", "Me", "Hi"),
  ordered = TRUE
)

model_car01_fe <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
           data = conference_calls, model = "within")
model_car01_pooled <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
           data = conference_calls, model = "pooling")
model_car01_re <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
           data = conference_calls, model = "random")

# Check which model
phtest(model_car01_fe, model_car01_re)
pFtest(model_car01_fe, model_car01_pooled)

# Robustness check
pbgtest(model_car01_fe)

# Robust standard error
coeftest(model_car01_fe, vcov = vcovHC(model_car01_fe, type = "HC1", cluster = "group"))

# Create a two-way fixed effects model
model_car01_fe_twoway <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged, 
           data = conference_calls, model = "within", effect = "twoways")
pFtest(model_car01_fe_twoway, model_car01_fe)

# Create fe models for each CAR period
model_car01 <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_car02 <- plm(car02 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
#model_car03 <- plm(car03 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_car05 <- plm(car05 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_car10 <- plm(car10 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")

model_car01cluster <- plm(car01 ~ Vagueness_Bin + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")

# Summarize the models
summary(model_car01, vcov = vcovHC(model_car01, type = "HC1", cluster = "group"))
summary(model_car02, vcov = vcovHC(model_car02, type = "HC1", cluster = "group"))
#summary(model_car03, vcov = vcovHC(model_car03, type = "HC1", cluster = "group"))
summary(model_car05, vcov = vcovHC(model_car05, type = "HC1", cluster = "group"))
summary(model_car10, vcov = vcovHC(model_car10, type = "HC1", cluster = "group"))

summary(model_car01cluster, vcov = vcovHC(model_car01cluster, type = "HC1", cluster = "group"))

# Create a summary table for the models with robust standard errors
stargazer(model_car01, model_car02, model_car05, model_car10,
          type = "text",
          title = "Fixed Effects Models for CAR Periods",
          dep.var.labels = c("CAR [0,1]", "CAR [0,2]", "CAR [0,5]", "CAR [0,10]"),
          covariate.labels = c("Vagueness Score", "Log Daily Market Cap", "Log Daily Price Volume", "Book to Market Ratio"),
          model.numbers = FALSE,
          omit.stat = c("f", "ser"),
          add.lines = list(c("YearQuarter FE", "Yes", "Yes", "Yes", "Yes")),
          se = list(sqrt(diag(vcovHC(model_car01, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(model_car02, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(model_car05, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(model_car10, type = "HC1", cluster = "group"))))
)

# Compare Vagueness_Score against other NLP scores
model_gunning_fog <- plm(car01 ~ Gunning_Fog + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_negative_ratio <- plm(car01 ~ lm_negative_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_positive_ratio <- plm(car01 ~ lm_positive_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_uncertainty_ratio <- plm(car01 ~ lm_uncertainty_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_litigious_ratio <- plm(car01 ~ lm_litigious_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_strong_modal_ratio <- plm(car01 ~ lm_strong_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_lm_constraining_ratio <- plm(car01 ~ lm_constraining_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_custom_weak_modal_ratio <- plm(car01 ~ custom_weak_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_custom_hedging_word_ratio <- plm(car01 ~ custom_hedging_word_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_custom_hedging_phrase_ratio <- plm(car01 ~ custom_hedging_phrase_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_VADER_Negative <- plm(car01 ~ VADER_Negative + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_VADER_Positive <- plm(car01 ~ VADER_Positive + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_VADER_Compound <- plm(car01 ~ VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")

summary(model_gunning_fog, vcov = vcovHC(model_gunning_fog, type = "HC1", cluster = "group")) # non significant
summary(model_lm_negative_ratio, vcov = vcovHC(model_lm_negative_ratio, type = "HC1", cluster = "group"))
summary(model_lm_positive_ratio, vcov = vcovHC(model_lm_positive_ratio, type = "HC1", cluster = "group"))
summary(model_lm_uncertainty_ratio, vcov = vcovHC(model_lm_uncertainty_ratio, type = "HC1", cluster = "group")) #non-significant
summary(model_lm_litigious_ratio, vcov = vcovHC(model_lm_litigious_ratio, type = "HC1", cluster = "group"))# non-significant
summary(model_lm_strong_modal_ratio, vcov = vcovHC(model_lm_strong_modal_ratio, type = "HC1", cluster = "group")) # non-significant
summary(model_lm_constraining_ratio, vcov = vcovHC(model_lm_constraining_ratio, type = "HC1", cluster = "group")) # non-significant
summary(model_custom_weak_modal_ratio, vcov = vcovHC(model_custom_weak_modal_ratio, type = "HC1", cluster = "group")) # non-significant
summary(model_custom_hedging_word_ratio, vcov = vcovHC(model_custom_hedging_word_ratio, type = "HC1", cluster = "group")) # non-significant
summary(model_custom_hedging_phrase_ratio, vcov = vcovHC(model_custom_hedging_phrase_ratio, type = "HC1", cluster = "group"))
summary(model_VADER_Negative, vcov = vcovHC(model_VADER_Negative, type = "HC1", cluster = "group"))
summary(model_VADER_Positive, vcov = vcovHC(model_VADER_Positive, type = "HC1", cluster = "group"))
summary(model_VADER_Compound, vcov = vcovHC(model_VADER_Compound, type = "HC1", cluster = "group")) #non-significant

model_vagueness_gunning_fog <- plm(car01 ~ Vagueness_Score + Gunning_Fog + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_negative_ratio <- plm(car01 ~ Vagueness_Score + lm_negative_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_positive_ratio <- plm(car01 ~ Vagueness_Score + lm_positive_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_uncertainty_ratio <- plm(car01 ~ Vagueness_Score + lm_uncertainty_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_litigious_ratio <- plm(car01 ~ Vagueness_Score + lm_litigious_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_strong_modal_ratio <- plm(car01 ~ Vagueness_Score + lm_strong_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_lm_constraining_ratio <- plm(car01 ~ Vagueness_Score + lm_constraining_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_custom_weak_modal_ratio <- plm(car01 ~ Vagueness_Score + custom_weak_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_custom_hedging_word_ratio <- plm(car01 ~ Vagueness_Score + custom_hedging_word_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_custom_hedging_phrase_ratio <- plm(car01 ~ Vagueness_Score + custom_hedging_phrase_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_VADER_Negative <- plm(car01 ~ Vagueness_Score + VADER_Negative + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_VADER_Positive <- plm(car01 ~ Vagueness_Score + VADER_Positive + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")
model_vagueness_VADER_Compound <- plm(car01 ~ Vagueness_Score + VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls, model = "within")

summary(model_vagueness_gunning_fog, vcov = vcovHC(model_vagueness_gunning_fog, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_negative_ratio, vcov = vcovHC(model_vagueness_lm_negative_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_positive_ratio, vcov = vcovHC(model_vagueness_lm_positive_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_uncertainty_ratio, vcov = vcovHC(model_vagueness_lm_uncertainty_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_litigious_ratio, vcov = vcovHC(model_vagueness_lm_litigious_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_strong_modal_ratio, vcov = vcovHC(model_vagueness_lm_strong_modal_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_lm_constraining_ratio, vcov = vcovHC(model_vagueness_lm_constraining_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_custom_weak_modal_ratio, vcov = vcovHC(model_vagueness_custom_weak_modal_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_custom_hedging_word_ratio, vcov = vcovHC(model_vagueness_custom_hedging_word_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_custom_hedging_phrase_ratio, vcov = vcovHC(model_vagueness_custom_hedging_phrase_ratio, type = "HC1", cluster = "group"))
summary(model_vagueness_VADER_Negative, vcov = vcovHC(model_vagueness_VADER_Negative, type = "HC1", cluster = "group"))
summary(model_vagueness_VADER_Positive, vcov = vcovHC(model_vagueness_VADER_Positive, type = "HC1", cluster = "group"))
summary(model_vagueness_VADER_Compound, vcov = vcovHC(model_vagueness_VADER_Compound, type = "HC1", cluster = "group"))

# Model with all NLP scores
model_vagueness_all_nlp_01 <- plm(car01 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                   lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                   lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                   custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                   VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
                                 data = conference_calls, model = "within")
model_vagueness_all_nlp_02 <- plm(car02 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                   lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                   lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                   custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                   VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
                                 data = conference_calls, model = "within")
model_vagueness_all_nlp_05 <- plm(car05 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                   lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                   lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                   custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                   VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
                                 data = conference_calls, model = "within")
model_vagueness_all_nlp_10 <- plm(car10 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                   lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                   lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                   custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                   VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, 
                                 data = conference_calls, model = "within")
summary(model_vagueness_all_nlp_01, vcov = vcovHC(model_vagueness_all_nlp_01, type = "HC1", cluster = "group"))
summary(model_vagueness_all_nlp_02, vcov = vcovHC(model_vagueness_all_nlp_02, type = "HC1", cluster = "group"))
summary(model_vagueness_all_nlp_05, vcov = vcovHC(model_vagueness_all_nlp_05, type = "HC1", cluster = "group"))
summary(model_vagueness_all_nlp_10, vcov = vcovHC(model_vagueness_all_nlp_10, type = "HC1", cluster = "group"))

### Industy Fixed Effects
# Make conference_calls a panel data frame
conference_calls$ff48 <- as.factor(conference_calls$ff48)
conference_calls_i <- pdata.frame(conference_calls, index = c("ff48", "DateTime"))
conference_calls_i$YearQuarter <- as.factor(conference_calls$YearQuarter)
conference_calls_i$Doc_id <- as.factor(conference_calls$Doc_id)
conference_calls_i$PERMNO_merged <- as.factor(conference_calls$PERMNO_merged)

# Create fe models for each CAR period
i_model_car01 <- plm(car01 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_car02 <- plm(car02 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
#i_model_car03 <- plm(car03 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged + YearQuarter, data = conference_calls_i, model = "within")
i_model_car05 <- plm(car05 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_car10 <- plm(car10 ~ Vagueness_Score + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")

# Summarize the models
summary(i_model_car01, vcov = vcovHC(i_model_car01, type = "HC1", cluster = "group"))
summary(i_model_car02, vcov = vcovHC(i_model_car02, type = "HC1", cluster = "group"))
#summary(i_model_car03, vcov = vcovHC(i_model_car03, type = "HC1", cluster = "group"))
summary(i_model_car05, vcov = vcovHC(i_model_car05, type = "HC1", cluster = "group"))
summary(i_model_car10, vcov = vcovHC(i_model_car10, type = "HC1", cluster = "group"))

# Create a summary table for the models with robust standard errors
stargazer(i_model_car01, i_model_car02, i_model_car05, i_model_car10,
          type = "text",
          title = "Fixed Effects Models for CAR Periods",
          dep.var.labels = c("CAR [0,1]", "CAR [0,2]", "CAR [0,5]", "CAR [0,10]"),
          covariate.labels = c("Vagueness Score", "Log Daily Market Cap", "Log Daily Price Volume", "Book to Market Ratio"),
          model.numbers = FALSE,
          omit.stat = c("f", "ser"),
          add.lines = list(c("YearQuarter FE", "Yes", "Yes", "Yes", "Yes")),
          se = list(sqrt(diag(vcovHC(i_model_car01, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(i_model_car02, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(i_model_car05, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(i_model_car10, type = "HC1", cluster = "group"))))
)

# Compare Vagueness_Score against other NLP scores
i_model_gunning_fog <- plm(car01 ~ Gunning_Fog + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_negative_ratio <- plm(car01 ~ lm_negative_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_positive_ratio <- plm(car01 ~ lm_positive_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_uncertainty_ratio <- plm(car01 ~ lm_uncertainty_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_litigious_ratio <- plm(car01 ~ lm_litigious_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_strong_modal_ratio <- plm(car01 ~ lm_strong_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_lm_constraining_ratio <- plm(car01 ~ lm_constraining_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_custom_weak_modal_ratio <- plm(car01 ~ custom_weak_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_custom_hedging_word_ratio <- plm(car01 ~ custom_hedging_word_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_custom_hedging_phrase_ratio <- plm(car01 ~ custom_hedging_phrase_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_VADER_Negative <- plm(car01 ~ VADER_Negative + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_VADER_Positive <- plm(car01 ~ VADER_Positive + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_VADER_Compound <- plm(car01 ~ VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")

summary(i_model_gunning_fog, vcov = vcovHC(i_model_gunning_fog, type = "HC1", cluster = "group")) # non significant
summary(i_model_lm_negative_ratio, vcov = vcovHC(i_model_lm_negative_ratio, type = "HC1", cluster = "group"))
summary(i_model_lm_positive_ratio, vcov = vcovHC(i_model_lm_positive_ratio, type = "HC1", cluster = "group"))
summary(i_model_lm_uncertainty_ratio, vcov = vcovHC(i_model_lm_uncertainty_ratio, type = "HC1", cluster = "group")) #non-significant
summary(i_model_lm_litigious_ratio, vcov = vcovHC(i_model_lm_litigious_ratio, type = "HC1", cluster = "group"))# non-significant
summary(i_model_lm_strong_modal_ratio, vcov = vcovHC(i_model_lm_strong_modal_ratio, type = "HC1", cluster = "group")) # non-significant
summary(i_model_lm_constraining_ratio, vcov = vcovHC(i_model_lm_constraining_ratio, type = "HC1", cluster = "group")) # non-significant
summary(i_model_custom_weak_modal_ratio, vcov = vcovHC(i_model_custom_weak_modal_ratio, type = "HC1", cluster = "group")) # non-significant
summary(i_model_custom_hedging_word_ratio, vcov = vcovHC(i_model_custom_hedging_word_ratio, type = "HC1", cluster = "group")) # non-significant
summary(i_model_custom_hedging_phrase_ratio, vcov = vcovHC(i_model_custom_hedging_phrase_ratio, type = "HC1", cluster = "group"))
summary(i_model_VADER_Negative, vcov = vcovHC(i_model_VADER_Negative, type = "HC1", cluster = "group"))
summary(i_model_VADER_Positive, vcov = vcovHC(i_model_VADER_Positive, type = "HC1", cluster = "group"))
summary(i_model_VADER_Compound, vcov = vcovHC(i_model_VADER_Compound, type = "HC1", cluster = "group")) #non-significant

i_model_vagueness_gunning_fog <- plm(car01 ~ Vagueness_Score + Gunning_Fog + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_negative_ratio <- plm(car01 ~ Vagueness_Score + lm_negative_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_positive_ratio <- plm(car01 ~ Vagueness_Score + lm_positive_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_uncertainty_ratio <- plm(car01 ~ Vagueness_Score + lm_uncertainty_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_litigious_ratio <- plm(car01 ~ Vagueness_Score + lm_litigious_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_strong_modal_ratio <- plm(car01 ~ Vagueness_Score + lm_strong_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_lm_constraining_ratio <- plm(car01 ~ Vagueness_Score + lm_constraining_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_custom_weak_modal_ratio <- plm(car01 ~ Vagueness_Score + custom_weak_modal_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_custom_hedging_word_ratio <- plm(car01 ~ Vagueness_Score + custom_hedging_word_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_custom_hedging_phrase_ratio <- plm(car01 ~ Vagueness_Score + custom_hedging_phrase_ratio + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_VADER_Negative <- plm(car01 ~ Vagueness_Score + VADER_Negative + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_VADER_Positive <- plm(car01 ~ Vagueness_Score + VADER_Positive + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")
i_model_vagueness_VADER_Compound <- plm(car01 ~ Vagueness_Score + VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, data = conference_calls_i, model = "within")

summary(i_model_vagueness_gunning_fog, vcov = vcovHC(i_model_vagueness_gunning_fog, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_negative_ratio, vcov = vcovHC(i_model_vagueness_lm_negative_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_positive_ratio, vcov = vcovHC(i_model_vagueness_lm_positive_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_uncertainty_ratio, vcov = vcovHC(i_model_vagueness_lm_uncertainty_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_litigious_ratio, vcov = vcovHC(i_model_vagueness_lm_litigious_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_strong_modal_ratio, vcov = vcovHC(i_model_vagueness_lm_strong_modal_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_lm_constraining_ratio, vcov = vcovHC(i_model_vagueness_lm_constraining_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_custom_weak_modal_ratio, vcov = vcovHC(i_model_vagueness_custom_weak_modal_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_custom_hedging_word_ratio, vcov = vcovHC(i_model_vagueness_custom_hedging_word_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_custom_hedging_phrase_ratio, vcov = vcovHC(i_model_vagueness_custom_hedging_phrase_ratio, type = "HC1", cluster = "group"))
summary(i_model_vagueness_VADER_Negative, vcov = vcovHC(i_model_vagueness_VADER_Negative, type = "HC1", cluster = "group"))
summary(i_model_vagueness_VADER_Positive, vcov = vcovHC(i_model_vagueness_VADER_Positive, type = "HC1", cluster = "group"))
summary(i_model_vagueness_VADER_Compound, vcov = vcovHC(i_model_vagueness_VADER_Compound, type = "HC1", cluster = "group"))


# Model with all NLP scores
i_model_vagueness_all_nlp_01 <- plm(car01 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                    lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                    lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                    custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                    VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, 
                                  data = conference_calls_i, model = "within")
i_model_vagueness_all_nlp_02 <- plm(car02 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                    lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                    lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                    custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                    VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, 
                                  data = conference_calls_i, model = "within")
i_model_vagueness_all_nlp_05 <- plm(car05 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                    lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                    lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                    custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                    VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, 
                                  data = conference_calls_i, model = "within")
i_model_vagueness_all_nlp_10 <- plm(car10 ~ Vagueness_Score + Gunning_Fog + lm_negative_ratio + lm_positive_ratio + 
                                    lm_uncertainty_ratio + lm_litigious_ratio + lm_strong_modal_ratio + 
                                    lm_constraining_ratio + custom_weak_modal_ratio + custom_hedging_word_ratio + 
                                    custom_hedging_phrase_ratio + VADER_Negative + VADER_Positive + 
                                    VADER_Compound + log(DlyCap) + log(DlyPrcVol) + bm_lagged, 
                                  data = conference_calls_i, model = "within")
summary(i_model_vagueness_all_nlp_01, vcov = vcovHC(i_model_vagueness_all_nlp_01, type = "HC1", cluster = "group"))
summary(i_model_vagueness_all_nlp_02, vcov = vcovHC(i_model_vagueness_all_nlp_02, type = "HC1", cluster = "group"))
summary(i_model_vagueness_all_nlp_05, vcov = vcovHC(i_model_vagueness_all_nlp_05, type = "HC1", cluster = "group"))
summary(i_model_vagueness_all_nlp_10, vcov = vcovHC(i_model_vagueness_all_nlp_10, type = "HC1", cluster = "group"))
