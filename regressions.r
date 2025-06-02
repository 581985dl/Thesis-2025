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

# Add industry classification NAICS where permno = PERMNO_merged
naics <- read.csv("NAICS.csv")
colnames(naics)[1] <- "PERMNO_merged"
conference_calls <- conference_calls %>%
  left_join(naics, by = "PERMNO_merged")

# Shorten NAICS to the first three digits
conference_calls <- conference_calls %>%
  mutate(HNAICS = substr(HNAICS, 1, 2)) %>%
  mutate(HNAICS = as.factor(HNAICS))
n_distinct(conference_calls$HNAICS)

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
conference_calls$HNAICS <- as.factor(conference_calls$HNAICS)
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
conference_calls$HNAICS <- as.factor(conference_calls$HNAICS)
conference_calls_i <- pdata.frame(conference_calls, index = c("HNAICS", "DateTime"))
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
