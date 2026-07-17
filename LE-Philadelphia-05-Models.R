# Load Libraries & Data ---------------------------------------------------


rm(list = ls())
library(here)
library(tidyverse)
library(DemoTools)
library(splines)
library(spdep)
library(future)
library(furrr)
library(nimble)
library(data.table)
library(matrixStats)
library(coda)

# Create variable to house directory paths
results <- here("Results")
working_results <- here(results, "Working Results")
dir_name <- here("//files.drexel.edu/encrypted/SOPH/UHC/SchnakeMahl_PAVitalStat/Data")
path_onedrive <- here("Data - Cleansed")

# Date for file saving/loading
date <- "20260716"

# Load data created in 01-Crosswalks
load(file = here(path_onedrive, "phila_sf.Rdata"))
load(file = here(path_onedrive, "sex_years.Rdata"))
load(file = here(path_onedrive, "tract_ids.RData"))

# Load data created in 04-Mortality-Rates
load(file = here(dir_name, "Philly LE", paste0("phila_death_rates_", date, ".Rdata")))

# Age groups for model
age_limit <- 85
ages <- seq(from = 0, to = age_limit, by = 1)

# Load helper functions
source(here("Helpers", "nimble_helpers.R"))
source(here("Helpers", "extract_params.R"))
source(here("Helpers", "convergence_helpers.R"))
source(here("Helpers", "lt_helpers.R"))


# Run Nimble Models -------------------------------------------------------


# Adjacency
phila_adj <- poly2nb(phila_sf, queen = TRUE)
adj_vec <- unlist(phila_adj)

# Basis matrix
knot_loc <- c(0, 1, 10, 20, 40, 70)
coef <- bs(as.numeric(ages), knots = knot_loc, degree = 1)

nimble_config <- list(
  dims = list(
    N_ages = nrow(coef),
    N_knots = ncol(coef),
    N_tracts = length(phila_adj)
  ),
  coef = coef,
  adj = list(
    num = sapply(phila_adj, length),
    adj = adj_vec,
    sumNumNeigh = length(adj_vec)
  ),
  mcmc = list(
    niter = 50000,
    nburnin = 25000,
    thin = 10
  )
)

nimble_results <- run_nimble_parallel(
  death_rates_tract = phila_death_rates$death_rates_age_tract,
  death_rates_age   = phila_death_rates$death_rates_age,
  config            = nimble_config,
  label             = "Main",
  save_path         = here(path_onedrive, paste0("nimble_results_", date, ".Rdata")),
  obj_name          = "nimble_results"
)
# Main nimble models took 4.04 hours to run.

nimble_results_no_pop_adjust <- run_nimble_parallel(
  death_rates_tract = phila_death_rates_no_pop_adjust$death_rates_age_tract,
  death_rates_age   = phila_death_rates_no_pop_adjust$death_rates_age,
  config            = nimble_config,
  label             = "No population adjustments",
  save_path         = here(path_onedrive, paste0("nimble_results_no_pop_adjust_", date, ".Rdata")),
  obj_name          = "nimble_results_no_pop_adjust"
)
# No population adjustments nimble models took 3.69 hours to run.


# Lambdas ----------------------------------------------------------------


load(file = here(path_onedrive, paste0("nimble_results_", date, ".Rdata")))
lambdas <- extract_param(
  nimble_results = nimble_results,
  param          = "lambda",
  index_names    = c("tract", "age"),
  tract_ids      = tract_ids,
  sex_years_df   = sex_years,
  save_path      = here(path_onedrive, paste0("lambdas_", date, ".Rdata")),
  obj_name       = "lambdas"
)

load(file = here(path_onedrive, paste0("nimble_results_no_pop_adjust_", date, ".Rdata")))
lambdas_no_pop_adjust <- extract_param(
  nimble_results = nimble_results_no_pop_adjust,
  param          = "lambda",
  index_names    = c("tract", "age"),
  tract_ids      = tract_ids,
  sex_years_df   = sex_years,
  save_path      = here(path_onedrive, paste0("lambdas_no_pop_adjust_", date, ".Rdata")),
  obj_name       = "lambdas_no_pop_adjust"
)

# Median log lambdas (thetas) and their 95% credible intervals
iter_mat <- as.matrix(select(lambdas, starts_with("iter_")))

log_mat <- log(iter_mat)

thetas_med_ci <- lambdas %>%
  mutate(
    med = rowMedians(log_mat),
    lci = rowQuantiles(log_mat, probs = 0.025),
    uci = rowQuantiles(log_mat, probs = 0.975)
  ) %>%
  select(-starts_with("iter_"))

save(
  thetas_med_ci,
  file = here(path_onedrive, paste0("thetas_med_ci_", date, ".Rdata"))
)


# Model Convergence -------------------------------------------------------


# Load nimble results and lambdas
load(file = here(path_onedrive, paste0("nimble_results_", date, ".Rdata")))

# Extract intercepts (beta0s)
beta0 <- extract_param(
  nimble_results,
  param = "beta0",
  index_names = "knot",
  sex_years_df = sex_years
)

# beta0 traceplots
pdf(
  here(working_results, "beta0_traceplots.pdf"),
  width = 10,
  height = 8
)

make_traceplots(
  data       = beta0,
  group_vars = c("knot"),
  value_lab  = expression(beta[0])
) %>%
  walk(print)

dev.off()

# beta0 ESS
beta0_ess <- compute_ess(beta0)

# Extract spatial random effects
betaij <- extract_param(
  nimble_results,
  param = "betaij",
  index_names = c("knot", "tract"),
  tract_ids = tract_ids,
  sex_years_df = sex_years
)

set.seed(123)
sample_tracts <- sample(unique(betaij$tract), 10)

# Spatial random effects traceplots
pdf(
  here(working_results, "betaij_traceplots.pdf"),
  width = 10,
  height = 8
)

make_traceplots(
  betaij %>% 
    filter(tract %in% sample_tracts),
  group_vars = c("GEOID", "knot"),
  value_lab = expression(beta[spat])
) %>%
  walk(print)

dev.off()

# Spatial random effects ESS
betaij_ess <- compute_ess(betaij)

# Extract nonspatial random effects
nonspat <- extract_param(
  nimble_results,
  param = "nonspat",
  index_names = c("tract", "knot"),
  tract_ids = tract_ids,
  sex_years_df = sex_years
)

# Nonspatial random effects traceplots
pdf(
  here(working_results, "nonspat_traceplots.pdf"),
  width = 10,
  height = 8
)

make_traceplots(
  nonspat %>%
    filter(tract %in% sample_tracts),
  group_vars = c("GEOID", "knot"),
  value_lab = expression(beta[nonspat])
) %>%
  walk(print)

dev.off()

# Nonspatial random effects ESS
nonspat_ess <- compute_ess(nonspat)

# Extract spatial precision parameters
tau <- extract_param(
  nimble_results,
  param = "tau",
  index_names = "knot",
  sex_years_df = sex_years
)

# Spatial precision traceplots
pdf(
  here(working_results, "tau_traceplots.pdf"),
  width = 10,
  height = 8
)

make_traceplots(
  tau,
  group_vars = c("knot"),
  value_lab = expression(tau)
) %>%
  walk(print)

dev.off()

# Spatial precision ESS
tau_ess <- compute_ess(tau)

# Extract nonspatial precision parameters
tauns <- extract_param(
  nimble_results,
  param = "tauns",
  index_names = "knot",
  sex_years_df = sex_years
)

# Nonspatial precision traceplots
pdf(
  here(working_results, "tauns_traceplots.pdf"),
  width = 10,
  height = 8
)

make_traceplots(
  tauns,
  group_vars = c("knot"),
  value_lab = expression(tau[ns])
) %>%
  walk(print)

dev.off()

# Nonspatial precision ESS
tauns_ess <- compute_ess(tauns)


# Life Expectancy ---------------------------------------------------------


load(file = here(path_onedrive, paste0("lambdas_", date, ".Rdata")))
le_nimble <- run_lt_parallel(
  lambdas,
  label = "Main",
  save_path = here(path_onedrive, paste0("le_nimble_", date, ".Rdata")),
  obj_name = "le_nimble"
)
# Main lifetable calculations took 5.63 hours to run.

load(file = here(path_onedrive, paste0("lambdas_no_pop_adjust_", date, ".Rdata")))
le_nimble_no_pop_adjust <- run_lt_parallel(
  lambdas_no_pop_adjust,
  label = "No pop adjust",
  save_path = here(path_onedrive, paste0("le_nimble_no_pop_adjust_", date, ".Rdata")),
  obj_name = "le_nimble_no_pop_adjust"
)
# No pop adjust lifetable calculations took 5.38 hours to run.

le_iter_df <- build_le_iter(
  le_list = le_nimble,
  sex_years_df = sex_years,
  save_path = here(path_onedrive, paste0("le_iter_df_", date, ".Rdata")),
  obj_name = "le_iter_df"
)

le_iter_df_no_pop_adjust <- build_le_iter(
  le_list = le_nimble_no_pop_adjust,
  sex_years_df = sex_years,
  save_path = here(path_onedrive, paste0("le_iter_df_no_pop_adjust_", date, ".Rdata")),
  obj_name = "le_iter_df_no_pop_adjust"
)

le_df <- summarise_le_ci(
  df = le_iter_df,
  save_path = here(path_onedrive, paste0("le_df_", date, ".Rdata")),
  obj_name = "le_df"
)

le_df_no_pop_adjust <- summarise_le_ci(
  df = le_iter_df_no_pop_adjust,
  save_path = here(path_onedrive, paste0("le_df_no_pop_adjust_", date, ".Rdata")),
  obj_name = "le_df_no_pop_adjust"
)
