library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)
library(httr)
library(tidycensus)
library(tidyverse)

source('utils/read-data.r')
source('utils/process-covariates.r')

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}

if(is.null(cmdoptions$options$full)) {
  FULL = Sys.getenv("FULL") == "TRUE"
} else {
  FULL = cmdoptions$options$full
}

if(DEBUG && FULL) {
  stop("Setting both debug and full run modes at once is invalid")
}

if(length(cmdoptions$args) == 0) {
  StanModel = 'base'
} else {
  StanModel = cmdoptions$args[1]
}

print(sprintf("Running %s",StanModel))
if(DEBUG) {
  print("Running in DEBUG mode")
} else if (FULL) {
  print("Running in FULL mode")
}

# Read which countires to use. Change this to states
countries <- read.csv('data/regions.csv', stringsAsFactors = FALSE)
# Read deaths data for regions
# however you want to get this in, this is  date, cases, deaths, country data
d <- read_obs_data(countries)
# Read ifr. This is very simplistic, assuming the crude IFR

D_data<-GET("https://covidtracking.com/api/states/daily.csv") %>% content 

state_pops<-get_acs(geography = "state", variables = c(pop="B01003_001"))
#v17 <- load_variables(2018, "acs5", cache = TRUE)

#View(v17)
state_key<-cbind(state.abb, state.name) %>% data.frame

# x<-c(m_5="B01001_003", m_9="B01001_004",m_14 = "B01001_005", 
#      m_17 = "B01001_006",m_19="B01001_007",m_20="B01001_008", 
#      m_21="B01001_009",m_24 = "B01001_010", m_29 = "B01001_011", 
#      m_34="B01001_012", m_39="B01001_013", m_44="B01001_014",
#      m_49="B01001_015", m_54="B01001_016", m_59="B01001_017",  
#      m_61="B01001_018", m_66="B01001_020", m_69="B01001_021", 
#      m_74="B01001_022",m_79="B01001_023",  m_84="B01001_024",  
#      m_85="B01001_025",f_5="B01001_027", f_9="B01001_028",f_14 = "B01001_029", 
#      f_17 = "B01001_030",f_19="B01001_031",f_20="B01001_032", 
#      f_21="B01001_033",f_24 = "B01001_034", f_29 = "B01001_035", 
#      f_34="B01001_036", f_39="B01001_037", f_44="B01001_038", 
#      f_49="B01001_039", f_54="B01001_040", f_59="B01001_041",  
#      f_61="B01001_042",f_64="B01001_043", f_66="B01001_044",
#      f_69="B01001_045", f_74="B01001_046", f_79="B01001_047",  
#      f_84="B01001_048",  f_85="B01001_049")

# #this gets the age covariates
# D_ages<-get_acs(geography = "state", variables = x)%>%
#   select(NAME, variable, estimate) %>%
#   pivot_wider( values_from = estimate, names_from = variable) %>% 
#   mutate(`0-9`=(m_5+m_9+f_5+f_9)/1000,
#          `10-19`=(m_14+m_17+m_19+f_14+f_17+f_19)/1000,
#          `20-29`=(m_20+m_21+m_24+m_21+m_24+m_29+f_20+f_21+f_24+f_29)/1000,
#          `30-39`=(m_34+m_39+f_34+f_39)/1000,
#          `40-49`=(m_44+m_49+f_44+f_49)/1000,
#          `50-59`=(m_54+m_59+f_59)/1000,
#          `60-69`=(m_61+m_69+f_61+f_64++f_66+f_69)/1000,
#          `70-79` = (m_74+m_79+f_74+f_79)/1000,
#          `80+` = (m_84+m_85+f_84+f_85)/1000
#          )%>% 
#   select(NAME, `0-9`,`10-19`,`20-29`,`30-39`,`40-49`,`50-59`,`60-69`,`70-79`,
#          `80+`)

#write_csv(D_ages, path = "~/Desktop/covid19model/data/ages.csv")

covid_data<-D_data %>% left_join(state_key, by = c("state"="state.abb")) %>%
  left_join(state_pops, by = c("state.name"="NAME")) 

covid_data_f<-covid_data%>%
  mutate(date =str_c(str_sub(date, 1,4),"-",str_sub(date, 5,6),"-", 
                     str_sub(date, 7,8)) %>% ymd, 
         pop_thou=estimate/1000, 
         pos_t=positive/pop_thou,  
         neg_t=negative/pop_thou,
         pend_t=pending/pop_thou, 
         death_t=death/pop_thou, 
         tot_t=total/pop_thou,
         ifr = death/positive,
         pop_thou=pop_thou*1000) %>% 
  select(-state.name, -GEOID, -variable, -estimate, -moe) %>% 
  filter(state!="DC") %>% replace(is.na(.), 0) %>% 
  mutate(state = as_factor(state)) %>%  
  mutate(d_rate=1000*death/pop_thou)%>% 
  filter(state%in%c("GU","AS","MP", "PR","VI")==FALSE) %>% 
  select(date, state, positive, death, ifr, total, pop_thou)

d <- covid_data_f %>% select(DateRep = date, Cases = positive, Deaths=death,
                             Country = state) %>% 
  left_join(tibble(state.name, state.abb), by = c("Country"="state.abb")) %>% 
  select(Country = state.name, DateRep, Cases, Deaths)

ifr.by.country <- covid_data_f %>%select(country=state, popt = pop_thou,ifr) %>%
  distinct %>% group_by(country) %>% summarise(ifr=mean(ifr), popt=popt[1]) %>% 
  left_join(tibble(state.name, state.abb), by = c("country"="state.abb")) %>% 
  select(-country, country = state.name)

# Read interventions
#interventions <- read_interventions(countries)

N2 <- 110 # increase if you need more forecast

countries<-covid_data_f %>% select(Regions = state) %>% distinct %>% 
  left_join(tibble(state.name, state.abb), by = c("Regions"="state.abb")) %>% 
  select(Regions = state.name)

npis<-read_csv("~/Desktop/covid19model/data/complete_npis_raw_policies.csv")

npis_f<-npis %>% 
  mutate(start_date = parse_date_time(start_date, orders = "m!*/d!/Y!")) %>% 
  group_by(state, npi) %>% 
  summarise(start_date = min(start_date, na.rm = TRUE)) %>% 
  mutate(npi2 =
           case_when(
             npi=="school_closure"~"schools_universities",
             npi%in%c("gathering_size_10_0", 
                    "gathering_size_100_to_26",
                    "gathering_size_100_to_26",
                    "gathering_size_25_to_11",
                    "gathering_size_500_to_101",
                    "religious_gatherings_banned",
                    "closing_of_public_venues")~"public_events",
             npi%in%c("social_distancing",
                      "non-essential_services_closure")~
               "social_distancing_encouraged",
             npi=="shelter_in_place"~"lockdown"
                      ) %>% fct_drop()
         )%>% group_by(state,npi2) %>% 
  summarise(start_date = min(start_date, na.rm = TRUE))

int_wide<-npis_f %>% na.omit %>% 
  pivot_wider( names_from = npi2, values_from = start_date) 

interventions<-tibble(
  state=int_wide$state,
  lockdown=int_wide$lockdown %>% unlist %>% as_datetime,
       public_events=int_wide$public_events %>% unlist%>% as_datetime,
       schools_universities=int_wide$schools_universities %>% unlist%>% 
         as_datetime,
       social_distancing_encouraged=int_wide$social_distancing_encouraged %>% 
         unlist%>% as_datetime,
  self_isolating_if_ill=ymd("2020-05-05")) 

interventions<-interventions%>% select(Country = state, schools_universities,
                        self_isolating_if_ill,public_events,lockdown,
                        social_distancing_encouraged) %>% 
  filter(Country%in%c("District of Columbia", "Guam","Puerto Rico")==FALSE)

processed_data <- process_covariates(countries = countries, 
                                     interventions = interventions, 
                                     d = d , ifr.by.country = ifr.by.country, 
                                     N2 = N2)
stan_data = processed_data$stan_data
dates = processed_data$dates
deaths_by_country = processed_data$deaths_by_country
reported_cases = processed_data$reported_cases

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan'))

if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=40,warmup=20,chains=2)
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=1800,warmup=1000,chains=5,thin=1,
                 control = list(adapt_delta = 0.95, max_treedepth = 15))
} else { 
  fit = sampling(m,data=stan_data,iter=1000,warmup=500,chains=4,thin=1,
                 control = list(adapt_delta = 0.95, max_treedepth = 10))
}   

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

countries <- countries$Regions
save.image(paste0('results/',StanModel,'-',JOBID,'.Rdata'))
save(fit,prediction,dates,reported_cases,deaths_by_country,countries,
     estimated.deaths,estimated.deaths.cf,out,
     file=paste0('results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

## Ensure that output directories exist
dir.create("results/", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/", showWarnings = FALSE, recursive = TRUE)
dir.create("web/data", showWarnings = FALSE, recursive = TRUE)

library(bayesplot)
filename <- paste0(StanModel,'-',JOBID)

print("Generating covariate size effects plot")
covariate_size_effects_error <- system(paste0("Rscript covariate-size-effects.r ", filename,'-stanfit.Rdata'),intern=FALSE)
if(covariate_size_effects_error != 0){
  stop(sprintf("Error while plotting covariate size effects! Code: %d", covariate_size_effects_error))
}

mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
ggsave(sprintf("results/%s-mu.png",filename),g,width=4,height=6)
tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
Rt_adj = do.call(cbind,tmp)
colnames(Rt_adj) = countries
g = (mcmc_intervals(Rt_adj,prob = .9))
ggsave(sprintf("results/%s-final-rt.png",filename),g,width=4,height=6)

print("Generate 3-panel plots")
plot_3_panel_error <- system(paste0("Rscript plot-3-panel.r ", filename,'-stanfit.Rdata'),intern=FALSE)
if(plot_3_panel_error != 0){
  stop(sprintf("Generation of 3-panel plots failed! Code: %d", plot_3_panel_error))
}

print("Generate forecast plot")
plot_forecast_error <- system(paste0("Rscript plot-forecast.r ",filename,'-stanfit.Rdata'),intern=FALSE)
if(plot_forecast_error != 0) {
  stop(sprintf("Generation of forecast plot failed! Code: %d", plot_forecast_error))
}

print("Make forecast table")
make_table_error <- system(paste0("Rscript make-table.r results/",filename,'-stanfit.Rdata'),intern=FALSE)
if(make_table_error != 0){
  stop(sprintf("Generation of alpha covar table failed! Code: %d", make_table_error))
}


verify_result_error <- system(paste0("Rscript web-verify-output.r ", filename,'.Rdata'),intern=FALSE)
if(verify_result_error != 0){
  stop(sprintf("Verification of web output failed! Code: %d", verify_result_error))
}
