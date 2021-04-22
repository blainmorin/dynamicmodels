
### Required Packages
library(tidyverse)
library(ctsem)

### Data
fed = read.csv("https://www.dropbox.com/s/xtt4emmz6txtbun/fed_agency_capacity_autonomy.csv?dl=1")


###############################################################
############ After Loading data and packages, ##############
#############   Run the Script from here      ###############
###########################################################

set.seed(123444421112) # Important!

##########################
### Choose Inputs Here ###
##########################

# Choose Year Range
startyear = 1980 # Needs to be >=1974
endyear = 1990 # Needs to be <=2019

# Choose Agency Type
agencytype = c("Natural Resources and Environment")
# agencytype = c("Natural Resources and Environment",
#                "Health")

# Choose Manifest Variables 
regressors = c("b18_roll", "ma_pct", "LOSavg", "med_sal_")
# names(fed)

# Choose Minimum Employee Size
minemployee = 5

# Choose Chains and Iterations
chains = 2
iterations = 2000

# Want to output a pdf of model results?
wantpdf = TRUE

# Want summary of model?
wantsummary = TRUE

# Want the kalman plot?
wantplot = TRUE

###############################################################
###################################################
###########################################

# Data Clean
df = fed %>%
  filter(yr >= 1974) %>%
  filter(med_sal_ > 0) %>% 
  drop_na(med_sal_) %>% ### Am dropping NA here, but may not need to with ctsem
  filter(AGYSUB != "TOTL")%>% ### Removes Total Rows 
  filter(!grepl("NET", agy_full)) ### Removes any total agency counts, ie only individual agencies are left

# Data Process
years = startyear:endyear

dff = df %>%
  filter(agy_typ %in% agencytype) %>%
  filter(yr %in% startyear:endyear) %>%
  select(regressors, yr, AGYSUB, agy_full) %>%
  mutate_at(regressors, scale) %>%
  drop_na() ### Again, dropping NA here, but may not need to
  
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
model<-ctModel(type='stanct', # Specify continuous time (can also be discrete)
               
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), # Specify the Factor Loading Vector
                              ncol = 1), # Forcing the first factor load to be 1
               
                n.manifest=length(regressors),
               
                manifestNames = regressors,
               
                MANIFESTMEANS = matrix(manifest,
                                       nrow = length(regressors), # Specify the mean vector
                                       ncol = 1), # Forcing the first mean to be 0
               
                n.latent=1,
               
                latentNames=c('capacity'),
               
                CINT = matrix('cint'),
               
                id = "AGYSUB",
               
                time = "yr")


# Run the model

fit = ctStanFit(datalong = dff,
                ctstanmodel = model,
                chains = chains, # Specified in input selection
                iterations = iterations) # Specified in input selection





