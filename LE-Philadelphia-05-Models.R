
# Load Libraries ----------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(DemoTools)
library(splines)
library(spdep)
library(future)
library(furrr)
library(nimble)
library(classInt)

# Run Nimble Models -------------------------------------------------------

# Create variable to house the directory path to create ease during import process
dir_name <- "//files.drexel.edu\\encrypted\\SOPH\\UHC\\SchnakeMahl_PAVitalStat\\Data\\"
path_onedrive <- "C:/Users/ga437/OneDrive - Drexel University/Documents/UHC-LE-Philadelphia/Data - Cleansed/"

# Load data
load(file = paste0(path_onedrive, "phila_sf.Rdata"))
load(file = paste0(path_onedrive,'sex_years.Rdata'))
load(file=paste0(path_onedrive,'tract_ids.RData'))
load(file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata"))

# Age groups for model
age_limit<-85
ages <- seq(from = 0, to = age_limit, by = 1)

# Start parallel processing 
plan(multisession, .cleanup = T, workers = 4); gc()

# Start time of nimble models
nimble_start <- Sys.time()

# Run nimble models - one on each core
nimble_results_seer <- future_map(c(1,2,3,4), function(sex_years_var, .options=furrr_options(seed=TRUE)){
    
    set.seed(1234)
  
    deaths <- phila_death_rates_age_tract %>% filter(sex_years == sex_years_var) %>%
      ungroup() %>% select(GEOID, age, deaths, tract) %>%  spread(age, deaths) %>% 
      arrange(tract) %>% select(-tract)
    
    pop <- phila_death_rates_age_tract %>% filter(sex_years == sex_years_var) %>% 
      ungroup() %>% select(GEOID, age, pop, tract) %>% spread(age, pop) %>% 
      arrange(tract) %>% select(-tract)
    
    Y <- deaths %>% select(-c(GEOID)) %>% data.matrix()
    
    n <- pop %>% select(-c(GEOID)) %>% data.matrix()
    
    lambda_star <-  loess(log_rate ~ age, data=filter(phila_death_rates_age, sex_years == sex_years_var), degree = 2, span = 0.25)$fitted
    
    # Number of age groups 
    N_ages <- length(ages)
    
    #All tracts in Philly
    phila_tracts <- unique(phila_sf$GEOID)
    
    # Number of census tracts in Philly
    N_tracts <- length(phila_tracts)
    
    # Spline knot locations
    knot_loc<-c(0, 1, 10, 20, 40, 70)
    
    # B-spline basis matrix
    coef   = bs(as.numeric(ages), knots=c(0,1,10,20,40,70), degree=1)
    
    # Number of spline knots
    N_knots<-length(knot_loc)+1
    
    #Adjacency 
    phila_adj <- poly2nb(phila_sf, queen = T)
    NumCells = length(phila_adj)
    num = sapply(phila_adj,length)
    adj = unlist(phila_adj)
    sumNumNeigh = length(unlist(phila_adj))
    
    #Initial MCMC values
    inits <- function() {
      list(tau=as.integer(rep(100, times=N_knots)),
           tauns=as.integer(rep(100, times=N_knots)),
           betaij=array(0, dim=c( N_knots,N_tracts)),
           nonspat=array(0, dim=c(N_tracts, N_knots)),
           beta0 = rep(0,N_knots))
    }
    
    # Parameters to save
    parameters= c("lambda","tau","tauns","beta0", "betaij","nonspat")
    
    #Nimble data and constants
    nimble_data <- list(Y = as.matrix(Y))
    nimble_constants <- list(n = as.matrix(n),
                                       lambda_star=exp(lambda_star),
                                       N_knots=N_knots,
                                       coef=coef,
                                       N_tracts=N_tracts,
                                       N_ages=N_ages,
                                       sumNumNeigh=sumNumNeigh, num=num, adj=adj)
    
    # Nimble model
    nimble_model <- nimbleCode({
      for(i in 1:N_tracts){ 
        for(k in 1:N_ages){
          Y[i,k] ~ dpois(theta[i,k])
          theta[i,k] <- n[i,k] * lambda[i,k]  
          lambda[i,k] <- lambda_star[k]*exp(inprod(betas[i,], coef[k,])) 
        }
        
        for (j in 1:N_knots){
          betas[i,j] <- beta0[j] + nonspat[i,j] + betaij[j,i]
          nonspat[i,j] ~ dnorm(0,inv_tauns[j]) # BUGS/Nimble uses precision
        }
      } 
      
      for(j in 1:N_knots){
        betaij[j,1:N_tracts] ~ dcar_normal(adj[], weights[], num[], inv_tau[j], zero_mean = 1) # BUGS/Nimble uses precision
        beta0[j] ~ dnorm(0, 0.001)
        inv_tau[j] ~ dgamma(1, 0.14) 
        tau[j] <- 1/inv_tau[j]
        inv_tauns[j] ~ dgamma(1, 0.01) 
        tauns[j] <- 1/inv_tauns[j]
      }
      
      for(h in 1:sumNumNeigh) {
        weights[h] <- 1
      }
    })
    
    # Run model
    nimble_output <- nimbleMCMC(
      code = nimble_model,
      dimensions = list(betas=c(N_tracts, N_knots), weights=c(sumNumNeigh), coef=c(N_ages, N_knots)),
      data = nimble_data,
      constants = nimble_constants,
      inits = inits,
      monitors = parameters, 
      niter = 20000,
      nburnin = 10000,
      thin = 10)

    list(output=nimble_output)
    
}, .progress=T)

# End time of nimble models
nimble_end <- Sys.time()

# Print time to run nimble models
cat("Nimble models took ", difftime(nimble_end, nimble_start), ' hours to run. \n')

# Nimble models took  2.264729  hours to run.

# End parallel processing
plan(sequential)

# Save results
save(nimble_results_seer, file=paste0(path_onedrive, "nimble_results_seer_20250117.Rdata"))


# Lambdas ----------------------------------------------------------------

# Load nimble results and other data
load(file=paste0(path_onedrive, "nimble_results_seer_20250117.Rdata"))
load(file = paste0(path_onedrive,'sex_years.Rdata'))
load(file = paste0(dir_name, "Philly LE\\", "phila_death_rates.Rdata"))

# Extract lambdas by iteration
lambdas_nimble <- do.call(rbind, lapply(seq_along(nimble_results_seer), function(i) {
  # Get the output from the nimble results
  x <- nimble_results_seer[[i]]
  # lambda columns
  cols <- grep("^lambda", colnames(x$output))
  # lambda matrix
  lambdas_mat <- x$output[,cols]
  return(cbind(iteration = 1:nrow(lambdas_mat), sex_years = rep(i, nrow(lambdas_mat)),lambdas_mat))
  })) %>%
  as_tibble() %>%
  pivot_longer(cols = starts_with("lambda"), 
                       names_to = "varid", 
                       values_to = "rate") %>%
  mutate(tract = as.numeric(sub(".*?\\[(\\d+).*", "\\1", varid)),
         age = as.numeric(sub(".*?,\\s*(\\d+).*", "\\1", varid))-1) %>% 
  select(-varid) %>%
  inner_join(tract_ids) %>%
  inner_join(sex_years) %>%
  arrange(sex_years, GEOID, age, iteration)

# Free unused memory and save results
gc(); save(lambdas_nimble, file = paste0(path_onedrive,'lambdas_nimble_20250117.Rdata'))

# Median log lambdas (thetas) and their 95% credible intervals
thetas_med_ci <- lambdas_nimble %>%
  mutate(log_lambda = log(rate)) %>%
  group_by(GEOID, sex_years, sex_years_lab, age) %>%
  summarise(med = median(log_lambda), lci = quantile(log_lambda, probs = 0.025), uci = quantile(log_lambda, probs = 0.975)) %>%
  inner_join(phila_death_rates_age %>% select(c("sex_years", "sex_years_lab", "age", "log_rate", "theta_star")), 
             by = c("sex_years", "sex_years_lab", "age")) %>%
  inner_join(phila_death_rates_age_tract %>% select(c("GEOID", "sex_years", "sex_years_lab", "age", "pop", "deaths")),
             by = c("GEOID", "sex_years", "sex_years_lab", "age")) %>%
  mutate(offset = med - theta_star,
         offset_lci = lci - theta_star,
         offset_uci = uci - theta_star,
         crude = case_when(
           deaths == 0 ~ NA_real_,
           pop == 0 ~ NA_real_,
           TRUE ~ log(deaths/pop)),
         knot = case_when(
           age %in% c(0, 1, 10, 20, 40, 70, 85) ~ age,
           TRUE ~ NA_real_),
         rug = case_when(
           deaths == 0 ~ age,
           TRUE ~ NA_real_)) %>%
  ungroup() %>%
  mutate(fisher=cut(round(exp(med)*100000), breaks=classIntervals(round(exp(med)*100000),n=6,style="fisher")$brks,include.lowest=T, dig.lab = 10))

# Free unused memory and save results
gc(); save(thetas_med_ci, file = paste0(path_onedrive,'thetas_med_ci_20250117.Rdata'))

# Model Convergence -------------------------------------------------------

load(file=paste0(path_onedrive, "nimble_results_seer_20250117.Rdata"))
load(file = paste0(path_onedrive,'lambdas_nimble_20250117.Rdata'))

lambda_convergence <- lambdas_nimble %>%
  filter(age %in% c(0, 5, 10, 20, 30, 45, 70, 75, 85)) %>% # Don't need to look at all ages -- just take sample of knots and betwee-knots
  group_by(GEOID, age) %>%
  group_map(~{
    ggplot(.x, aes(x=iteration, y=rate))+
      geom_line()+
      theme_bw()+
      facet_wrap(~sex_years_lab)+
      ggtitle(paste0("Tract ", .y$GEOID, ", Age ", .y$age))
  })

pdf(file = "./Results/lambda_convergence_20250729.pdf")
lambda_convergence
dev.off()

lambda_density <- lambdas_nimble %>%
  filter(age %in% c(0, 20, 40, 70)) %>%
  group_by(GEOID, age) %>%
  group_map(~{
    ggplot(.x)+
      geom_density(aes(x=rate))+
      theme_bw()+
      facet_wrap(~sex_years_lab)+
      ggtitle(paste0("Tract ", .y$GEOID, ", Age ", .y$age))
  })

# Life Expectancy ---------------------------------------------------------

# Load lambdas
load(file = paste0(path_onedrive,'lambdas_nimble_20250117.Rdata')); gc()

# Start time of lifetable calcs
le_start <- Sys.time()

# Run lifetables
le_nimble  <- lambdas_nimble %>%
  group_by(GEOID, sex_years, iteration) %>% 
  group_map(~{
    ages<-as.numeric(.x$age)
    rates<-as.numeric(.x$rate)
    lt <- lt_single_mx(nMx=rates, Age= ages) %>% 
      mutate(GEOID = .y$GEOID, sex_years = .y$sex_years, iteration = .y$iteration) %>%
      select(c("Age", "ex", "GEOID", "sex_years", "iteration"))
  })

# End time of lifetable calcs
le_end <- Sys.time()

# Print time to run lifetables
cat("Nimble models took ", difftime(le_end, le_start), ' hours to run. \n')
# Time difference of 13.26059 hours

# Free unused memory and save results
gc(); save(le_nimble,file=paste0(path_onedrive, "le_nimble_20250117.Rdata"))

# Dataframe of LE by iteration
le_iter_nimble <- tibble(do.call(rbind,le_nimble)) %>%
  rename(le = ex) %>%
  inner_join(sex_years) %>%
  select(GEOID, iteration, Age, sex, years, le) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter((Age==0)|(Age==25)|(Age==65))

# Free unused memory and save results
gc(); save(le_iter_nimble,file=paste0(path_onedrive, "le_iter_nimble_20250117.Rdata"))

# Point estimates and 95% CIs for LE
le_df_nimble <- le_iter_nimble %>%
  arrange(GEOID, Age, sex, years, iteration) %>%
  group_by(GEOID, Age, sex, years) %>%
  summarise(lci = quantile(le, probs = c(.025), na.rm = TRUE),
            med = quantile(le, probs = c(.5), na.rm = TRUE),
            uci = quantile(le, probs = c(.975), na.rm = TRUE))

# Free unused memory and save results
gc(); save(le_df_nimble,file=paste0(path_onedrive, "le_df_nimble_20250117.Rdata"))
