# d

### Packages
library(tidyverse)
library(ctsem)
library(rstan)
library(kableExtra)
library(bayesplot)

### Raw Data
fed = read.csv("https://www.dropbox.com/s/xtt4emmz6txtbun/fed_agency_capacity_autonomy.csv?dl=1")

### Set Seed
set.seed(3710)


# Choose Year Range
startyear = 1981 # Needs to be >=1974
endyear = 2010 # Needs to be <=2019

# Choose Agency Type
agencytype = c("Natural Resources and Environment")
# agencytype = c("Natural Resources and Environment",
#                "Health")

# Choose Manifest Variables 
regressors = c("med_sal_", "ma_pct")
# names(fed)

# Choose Minimum Employee Size
minemployee = 5000


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


# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 1), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                MANIFESTMEANS = matrix(manifest,
                                       nrow = length(regressors), 
                                       ncol = 1), # Forcing the first mean to be 0
                
                n.latent=1,
                
                latentNames=c('capacity'),
                
                CINT = matrix('cint'),
                
                id = "agy_full",
                
                time = "yr")


fit = ctStanFit(datalong = dff,
                ctstanmodel = model,
                chains = 4,
                cores = 4,
                iter = 2000,
                optimize = FALSE,
                nopriors = FALSE,
                control = list(max_treedepth = 16, adapt_delta = 0.999))

save(fit, file = "tree3")

ctKalman(fit, 
         plot = TRUE,
         subjects = (1:length(unique(dff$AGYSUB))), # Plots all agencies
         kalmanvec = c("etasmooth", "y", "ysmooth"),
         timestep = .5) 
