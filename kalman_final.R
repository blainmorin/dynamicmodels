
###########################################
######################
##################
# This code sets up data for stan and executes the stan model
# For years 1974 - 2013
###########################################
##############################################################################
#######################################################################
###################################################################
### Required Libraries
library(knitr)
library(tidyverse)
library(rstan)
library(parallel)
library(pscl)
library(kableExtra)
library(shinystan)
library(ggcorrplot)
##############################################################################
#######################################################################
###################################################################
### Read in data
fed = read_csv("fed_agency_capacity_autonomy.csv")
##############################################################################
#######################################################################
###################################################################
# Model for reference
# capacity =~ logn + logMA_pct + LOSavg + logAIN_pct + logLST_pct + med_sal_ + gini

##############################################################################
#######################################################################
###################################################################
# Data cleaning
# Get rid of agencies with med salary = 0 and employees less than 5
# Make log transforms
set.seed(9876)
df = fed %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(med_sal_ > 0) %>% 
  filter(n > 5) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(!grepl("NET", agy_full)) %>%
  mutate(logMA_pct = log(ma_pct + 1)) %>%
  mutate(logLST_pct = log(LST_pct + 1)) %>%
  mutate(logAIN_pct = log(AIN_pct + 1)) %>%
  mutate(logPHDpct = log(doc_pct + 1)) %>%
  mutate(logn = log(n)) 

years = 1974:2013

regressors = c("logn", "med_sal_", "LOSavg", "logMA_pct", "logAIN_pct", "logLST_pct", "gini")

# Select regressors and scale them
# Drop missing years temporarily
dff = df %>%
  filter(yr %in% years) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(yr = as.factor(yr)) %>%
  drop_na()

# This gets agency data start and end years
dff = dff %>%
  mutate(year = as.integer(as.factor(yr))) %>%
  group_by(AGYSUB) %>%
  mutate(minyear = min(year)) %>%
  mutate(maxyear = max(year)) %>%
  ungroup()


# This section removes agencies where a random year in the middle is missing
dff = dff %>%
  arrange(AGYSUB)

dff = dff %>%
  group_by(AGYSUB) %>%
  mutate(nobs = n()) %>%
  ungroup()

dff = dff %>%
  mutate(missingyear = ifelse(nobs != maxyear - minyear + 1, 1, 0)) %>% 
  filter(missingyear == 0) ### Removes 16 agencies 

dff = dff %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  complete(nesting(AGYSUB), year) %>%
  mutate(agency = as.integer(AGYSUB))

##############################################################################
#######################################################################
###################################################################
##############################################################################
#######################################################################
###################################################################
# The following section makes the stan data
# z puts the puts dff into an array form
# Basically, for the 180 agencies, there is a time by indicator matrix


z = array(0, dim=c(length(unique(dff$AGYSUB)), length(years), length(regressors))) 
# array[agency, time, indicator]

for (i in 1:length(unique(dff$AGYSUB))) {
  for (t in 1:length(years)) {
    for (j in 1:length(regressors)) {
      
      value = as.double(dff %>% filter(agency == i, year == t) %>% select(regressors[j]))
      z[i, t, j] = value
      
    }
    
  }
  
}

# Turn missing years to 0 because stan doesnt accept na (these won't actually get used)
z[is.na(z)] = 0

cap.data = list(M = length(unique(dff$AGYSUB)), Time = length(years), z = z)

cap.data$N = nrow(cap.data$y)

cap.data$J = length(regressors)

cap.data$which_pos = 1

# Create vectors with agency starting and ending times
cap.data$start_time = dff %>%
  group_by(AGYSUB) %>%
  drop_na() %>%
  slice(1) %>%
  ungroup() %>%
  select(minyear)

cap.data$start_time = cap.data$start_time$minyear

cap.data$end_time = dff %>%
  group_by(AGYSUB) %>%
  drop_na() %>%
  slice(1) %>%
  ungroup() %>%
  select(maxyear)

cap.data$end_time = cap.data$end_time$maxyear

##############################################################################
#######################################################################
###################################################################

# Run the model

options(mc.cores = detectCores())
kalman.test = stan(file = "kalman.stan", data = cap.data, iter = 5000, seed = 9495)

save(kalman.test, file = "kalmanfinal")

##############################################################################
#######################################################################
###################################################################


#### Plot capacity faceted by agency type

load("kalmanfinal")

check=as.data.frame(summary(kalman.test)$summary)
check$rowname = rownames(check)
check2 = check %>%
  filter(str_detect(rowname, "x_sam"))
check2 = check2[, c(1,4, 6, 8, 9, 10)]
check2$agency = rep(1:cap.data$M, each = cap.data$Time)
check2$time = rep(1:cap.data$Time, cap.data$M)

dff = dff %>%
  mutate(agency = as.integer(AGYSUB)) %>%
  mutate(time = as.integer(yr))

dfff = left_join(dff, check2) 
dfff = dfff %>%
  rename(Ave_Capacity = mean, lower = "2.5%", upper = "97.5%")

dfff %>%
  drop_na(agy_typ) %>%
  mutate(Ave_Capacity = -1*Ave_Capacity) %>%
  ggplot(aes(x = time, y = Ave_Capacity)) +
  geom_line(aes(color = agy_typ, group = AGYSUB), size = 1.4, alpha = .6) +
  facet_wrap(~agy_typ) +
  geom_smooth(color = "black", se = FALSE) +
  ylab("Capacity") +
  xlab("Time") +
  theme_minimal()
