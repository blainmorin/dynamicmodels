### Packages
library(tidyverse)
library(ctsem)
library(rstan)
library(kableExtra)

### Raw Data
fed = read.csv("https://www.dropbox.com/s/xtt4emmz6txtbun/fed_agency_capacity_autonomy.csv?dl=1")

### Set Seed
set.seed(3710)


# Choose Year Range
startyear = 1981 # Needs to be >=1974
endyear = 2010 # Needs to be <=2019

# Choose Agency Type
agencytype = c("Natural Resources and Environment",
               "Health")

# Choose Manifest Variables 
regressors = c("ma_pct", "med_sal_", "LOSavg", "appt_pct", "advs_pct", "expert_pct")


# Choose Minimum Employee Size
minemployee = 3000


# Data Clean
df = fed %>%
  filter(yr >= 1974) %>%
  filter(med_sal_ > 0) %>% 
  filter(n > minemployee) %>%
  drop_na(med_sal_) %>% ### Am dropping NA here, but may not need to with ctsem
  filter(AGYSUB != "TOTL") %>% ### Removes Total Rows 
  filter(!grepl("NET", agy_full)) ### Removes any total agency counts, ie only individual agencies are left

# Data Process
years = startyear:endyear
dff = df %>%
  filter(agy_typ %in% agencytype) %>%
  filter(yr %in% startyear:endyear) %>%
  select(regressors, yr, AGYSUB, agy_full) %>%
  mutate_at(regressors, scale) %>%
  drop_na()


## Make Strings for the model
## based on the number of regressors
lambdas = c(1)
manifest = c(0)
for (i in 2:length(regressors)) {
  
  temp = paste0("lambda", i)
  temp2 = paste0("manifestmean", i)
  lambdas[i] = temp
  manifest[i] = temp2
}

dff$yr = dff$yr - 1980


manifest = c(0)
for (i in 2:length(regressors)) {
  temp2 = paste0("manifestmean", i)
  manifest[i] = temp2
}

lambda = matrix(c(1,"lam21", "lam31", 0, 0, "lam61",
                  0, 0, 0, "lam42", "lam52", "lam62"), nrow = 6, ncol = 2)

model = ctModel(type='stanct', 
                
                LAMBDA=lambda, # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                MANIFESTMEANS = matrix(manifest,
                                       nrow = length(regressors), 
                                       ncol = 1), # Forcing the first mean to be 0
                
                n.latent=2,
                
                latentNames=c('capactiy', "autonomy"),
                
                CINT = matrix(c('cint1', "cint2"), nrow = 2),
                
                id = "AGYSUB",
                
                time = "yr")

cap.auto.fit = ctStanFit(datalong = dff,
                  ctstanmodel = model,
                  chains = 4,
                  cores = 4,
                  iter = 8000,
                  optimize = FALSE,
                  nopriors = FALSE,
                  control = list(max_treedepth = 11, adapt_delta = .9))

save(cap.auto.fit, file = "capautofit")

ctKalman(cap.auto.fit, 
         plot = TRUE,
         subjects = (1:length(unique(dff$AGYSUB))), # Plots all agencies
         kalmanvec = c("etasmooth", "y", "ysmooth"),
         timestep = .5) 
