# script to implement IPCC 3-pool soil carbon model

# load req'd packages
library(tidyverse)

##################
# implement top-level function for C stock change 
SOC_stock_change <- function(Active_y, Slow_y, Passive_y){
  SOC_y <- Active_y + Slow_y + Passive_y
  A_CMineral <- SOC_y - lag(SOC_y, default = NA)
  A_CMineral <- ifelse(is.na(A_CMineral), 0, A_CMineral)
  return(A_CMineral)
}

##################
# read in model parameters
# source: https://www.ipcc-nggip.iges.or.jp/public/2019rf/index.html, Vol. 4 .zip folder
# assign parameters into new environment [pm]
Dat_param <- read_csv("parameter-data/model-parameters.csv", col_types = "ccnnnnc")
pm <- new.env()
for(i in 1:nrow(Dat_param)){
  assign(Dat_param$Parameter[i], Dat_param$BestEstimate[i], envir = pm)
}
rm(i)

##################
# calculate f4 (variable depending on sand fraction)
f4 <- function(sand){
  f4 <- 1 - pm$f5 - (pm$f4par1 + pm$f4par2 * sand)
  return(f4)
}

##################
# calculate f2 (variable depending on tillage)
f2 <- function(tillage){
  x <- match(tillage, c("unknown", "full", "reduced", "zero"))
  y <- c(pm$f2, pm$f2_ft, pm$f2_rt, pm$f2_nt)[x]
  return(y)
}

###################
# calculation of intermediate values
beta <- function(C_input, LC, NC){
  beta <- C_input * (pm$sp1 - pm$sp2 * LC / NC)
  return(beta)
}

alpha <- function(C_input, LC, NC, sand, tillage){
  beta <- beta(C_input = C_input, LC = LC, NC = NC)
  x <- beta * pm$f1
  y <- ((C_input * (1 - LC) - beta) * f2(tillage = tillage))
  z <- ((C_input * LC) * pm$f3 * (pm$f7 + pm$f8 * pm$f6))
  d <- 1 - (f4(sand = sand) * pm$f7) - (pm$f5 * pm$f8) - (f4(sand = sand) * pm$f6 * pm$f8)
  alpha <- (x + y + z) / d
  return(alpha)
}

##################
# soil water factor
wfac <- function(precip, PET){
  mappet_i <- pmin(1.25, precip/PET)
  W_i <- pm$wfacpar1 + pm$wfacpar2 * mappet_i + pm$wfacpar3 * mappet_i^2
  wfac <- 1.5 * (1 / 12 * sum(W_i))
  return(wfac)
}

##################
# temperature factor
tfac <- function(temp){
  prelim <- (pm$tmax - temp) / (pm$tmax - pm$topt)
  T_i <- prelim^pm$ta * exp(0.076 * (1 - prelim^pm$tb))
  tfac <- 1 / 12 * sum(T_i)
  return(tfac)
}

##################
# tillage factor
tillfac <- function(tillage){
  x <- match(tillage, c("unknown", "full", "reduced", "zero"))
  y <- c(pm$tillfac_ft, pm$tillfac_ft, pm$tillfac_rt, pm$tillfac_nt)[x]
  return(y)
}

##################
# algorithm to calculate active pool
k_a <- function(tfac, wfac, tillfac, sand){
  k_a <- pm$kfaca * tfac * wfac * (pm$k3par1 + (pm$k3par2 * sand)) * tillfac
  return(k_a)
}

active_y_ss <- function(k_a, alpha){
  active_y_ss <- alpha / k_a
  return(active_y_ss)
}

active_y <- function(k_a, active_y_ss){
  for(i in 1:length(active_y_ss)){
    if(i == 1) active_y <- active_y_ss[i]
    if(i > 1) active_y <- c(active_y, active_y[i - 1] + (active_y_ss[i] - active_y[i - 1]) * min(1, k_a[i]))
  }
  return(active_y)
}

###################
# algorithm to calculate slow pool
k_s <- function(tfac, wfac, tillfac){
  k_s <- pm$kfacs * tfac * wfac * tillfac
  return(k_s)
}

slow_y_ss <- function(C_input, LC, active_y_ss, k_s, k_a, sand){
  slow_y_ss <- ((C_input * LC * pm$f3) + (active_y_ss * k_a * f4(sand = sand))) / k_s
  return(slow_y_ss)
}

slow_y <- function(k_s, slow_y_ss){
  for(i in 1:length(slow_y_ss)){
    if(i == 1) slow_y <- slow_y_ss[i]
    if(i > 1) slow_y <- c(slow_y, slow_y[i - 1] + (slow_y_ss[i] - slow_y[i - 1]) * min(1, k_s[i]))
  }
  return(slow_y)
}

###################
# algorithm to calculate passive pool
k_p <- function(tfac, wfac){
  k_p <- pm$kfacp * tfac * wfac
  return(k_p)
}

passive_y_ss <- function(active_y_ss, slow_y_ss, k_a, k_s, k_p){
  passive_y_ss <- ((active_y_ss * k_a * pm$f5) + (slow_y_ss * k_s * pm$f6)) / k_p
  return(passive_y_ss)
}

passive_y <- function(k_p, passive_y_ss){
  for(i in 1:length(passive_y_ss)){
    if(i == 1) passive_y <- passive_y_ss[i]
    if(i > 1) passive_y <- c(passive_y, passive_y[i - 1] + (passive_y_ss[i] - passive_y[i - 1]) * min(1, k_p[i]))
  }
  return(passive_y)
}

###################
# function to calculate C inputs from crop residues (tonnes C per hectare)
C_in_residues <- function(yield, crop_type, frac_renew, frac_remove){
  lookup1 <- read_csv("parameter-data/below-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnn")
  lookup2 <- read_csv("parameter-data/above-ground-residue-coefficients.csv", na = c("", "NA"), col_types = "cnnnnn")
  
  RS <- lookup1 %>% filter(Crop == crop_type) %>% pull(RS)
  DRY <- lookup1 %>% filter(Crop == crop_type) %>% pull(DRY)
  Slope <- lookup2 %>% filter(Crop == crop_type) %>% pull(Slope)
  Intercept <- lookup2 %>% filter(Crop == crop_type) %>% pull(Intercept)
  
  yield_dry <- yield * DRY
  agdm <- yield_dry * Slope + Intercept
  bgr <- yield_dry * agdm * RS * frac_renew # note this line is different to IPCC (2019) -- presumed error in calculations (addition of +1 term to agdm, which makes no sense)
  agr <- agdm * frac_renew * (1 - frac_remove)
  
  C_in_residues <- agr * 0.42 + bgr * 0.42 # 42% C assumption
  return(C_in_residues)
}

###################
# function to calculate C inputs from manure (tonnes C per hectare)
C_in_manure <- function(man_nrate, man_type){
  lookup1 <- read_csv("parameter-data/manure-coefficients.csv", na = c("", "NA"), col_type = "cnnn")
  
  CN <- lookup1 %>% filter(Livestock_type == man_type) %>% pull(CN_ratio)
  C_in_manure <- CN * man_nrate * 10^-3 # manure C in tonnes ha-1
  return(C_in_manure)
}

###################
# functions to calculate crop N and Lignin fractions from crop and manure parameter data
N_frac <- function(crop_type, manure_type, C_res, C_man){
  lookup1 <- read_csv("parameter-data/crop-N-and-lignin-fractions.csv", na = c("", "NA"), col_type = "cnn") %>%
    filter(Crop == crop_type) %>%
    mutate(C_frac = 0.42)
  lookup2 <- read_csv("parameter-data/manure-coefficients.csv", na = c("", "NA"), col_type = "cnnn") %>%
    filter(Livestock_type == manure_type) %>%
    mutate(C_frac = N_frac * CN_ratio)
  
  tot_res <- C_res / lookup1$C_frac
  tot_man <- C_man / lookup2$C_frac
  
  N_res <- tot_res * lookup1$N_frac
  N_man <- tot_man * lookup2$N_frac
  
  return((N_res + N_man) / (tot_res + tot_man))
}

lignin_frac <- function(crop_type, manure_type, C_res, C_man){
  lookup1 <- read_csv("parameter-data/crop-N-and-lignin-fractions.csv", na = c("", "NA"), col_type = "cnn") %>%
    filter(Crop == crop_type) %>%
    mutate(C_frac = 0.42)
  lookup2 <- read_csv("parameter-data/manure-coefficients.csv", na = c("", "NA"), col_type = "cnnn") %>%
    filter(Livestock_type == manure_type) %>%
    mutate(C_frac = N_frac * CN_ratio)
  
  tot_res <- C_res / lookup1$C_frac
  tot_man <- C_man / lookup2$C_frac
  
  lignin_res <- tot_res * lookup1$Lignin_frac
  lignin_man <- tot_man * lookup2$Lignin_frac
  
  return((lignin_res + lignin_man) / (tot_res + tot_man))
}

###################
# function to modify (tibble) dataframe to include a first row calculated from run-in period
run_in <- function(df, years){
  df %>%
    arrange(year) %>% # make absolutely sure it's in chronological order
    slice(1:years) %>%
    summarise_all(.funs = ifelse(is.numeric(.), mean, median)) %>%
    mutate(year = NA) %>%
    bind_rows(df) %>%
    return()
}

###################
# fire off the whole darn shooting match
run_model <- function(df){
  df %>%
    mutate(alpha = alpha(C_input = C_tot,
                         LC = lignin_frac,
                         NC = N_frac,
                         sand = sand_frac,
                         tillage = till_type),
           beta = beta(C_input = C_tot,
                       LC = lignin_frac,
                       NC = N_frac),
           
           # active pool
           k_a = k_a(tfac = tfac,
                     wfac = wfac,
                     tillfac = tillfac(tillage = till_type),
                     sand = sand_frac),
           active_y_ss = active_y_ss(k_a = k_a,
                                     alpha = alpha),
           active_y = active_y(k_a = k_a, active_y_ss = active_y_ss),
           
           # slow pool
           k_s = k_s(tfac = tfac,
                     wfac = wfac,
                     tillfac = tillfac(till_type)),
           slow_y_ss = slow_y_ss(C_input = C_tot,
                                 LC = lignin_frac,
                                 active_y_ss = active_y_ss,
                                 k_s = k_s,
                                 k_a = k_a,
                                 sand = sand_frac),
           slow_y = slow_y(k_s = k_s,
                           slow_y_ss = slow_y_ss),
           
           # passive pool
           k_p = k_p(tfac = tfac,
                     wfac = tfac),
           passive_y_ss = passive_y_ss(active_y_ss = active_y_ss,
                                       slow_y_ss = slow_y_ss,
                                       k_a = k_a,
                                       k_s = k_s,
                                       k_p = k_p),
           passive_y = passive_y(k_p = k_p,
                                 passive_y_ss = passive_y_ss),
           
           # roundup
           total_y = active_y + slow_y + passive_y,
           stock_change = SOC_stock_change(active_y, slow_y, passive_y))
}

##########################
# build and run model for bush estate farm
build_model <- function(clim_data, crop_data, manure_data){
  
  # read in main climate data
  # we'll use this as the basis for the model simulation
  Dat_nest <- read_rds(clim_data)
  
  # yield data for bush estate's barley
  Dat_crop <- read_csv(crop_data)
  
  # manure application for bush estate's barley
  Dat_manure <- read_csv(manure_data)
  
  # sand percentage for soil at bush estate
  sand_frac <- 0.47006 # sampled from sand % raster — no point reading in every time
  
  #####################################################
  # starting with monthly climate variables, condensing to annual modification factors (tfac and wfac)
  #####################################################
  Dat_nest <- Dat_nest %>%
    mutate(data_full = data_full %>%
             map(function(df){
               df %>%
                 group_by(year) %>%
                 summarise(wfac = wfac(precip = precip_mm, PET = pet_mm),
                           tfac = tfac(temp = temp_centigrade)) %>%
                 ungroup()
             }))
  
  #####################################################
  # calculate crop-specific variables in the crop dataset
  #####################################################
  
  # C inputs from crop residues and manure
  Dat_crop <- Dat_crop %>%
    mutate(C_res = pmap_dbl(list(yield_tha, crop_type, frac_renew, frac_remove),
                            C_in_residues)) %>%
    left_join(Dat_manure, by = "year") %>%
    mutate(C_man = pmap_dbl(list(man_nrate, man_type),
                            C_in_manure),
           N_frac = pmap_dbl(list(crop_type, man_type, C_res, C_man),
                             N_frac),
           lignin_frac = pmap_dbl(list(crop_type, man_type, C_res, C_man),
                                  lignin_frac),
           C_tot = C_res + C_man)
  rm(Dat_manure)
  
  #####################################################
  # join crop-specific variables to main model data
  #####################################################
  # also adding in sand fraction data here since it's an odd one out
  Dat_nest <- Dat_nest %>%
    mutate(data_full = data_full %>%
             map(function(df){
               df %>%
                 mutate(sand_frac = sand_frac) %>%
                 left_join(Dat_crop %>%
                             select(year, N_frac, lignin_frac, C_tot), by = "year")
             }))
  
  #####################################################
  # run in model for 20 year period and add tillage type
  # can't run in with tillage already added, since it can't be averaged
  #####################################################
  runin_years <- 20
  
  Dat_nest <- Dat_nest %>%
    mutate(data_runin = data_full %>%
             map2(runin_years, run_in) %>%
             map(function(df){
               df %>%
                 left_join(Dat_crop %>% select(year, till_type), by = "year") %>%
                 mutate(till_type = ifelse(is.na(till_type), "full", till_type))
             }))
  
  #####################################################
  # run model
  #####################################################
  Dat_nest <- Dat_nest %>%
    mutate(scenario_baseline = data_runin %>%
             map(run_model))
  return(Dat_nest)
}

###################
# timeseries plot function
ts_plot <- function(df_bl, df_mod){
  baseline <- select(df_bl, "scenario_baseline")[[1, 2]] %>% filter(year == 2018) %>% pull(total_y)
  #ylims <- df %>%
  #  ungroup() %>%
  #  unnest(cols = c(col)) %>%
  #  filter(year >= 2018) %>%
  #  pull(total_y) %>%
  #  quantile(c(0.01, 0.99), na.rm = T) %>%
  #  as.numeric()
  
  df <- if(is.null(df_mod)){
    df_bl %>%
      mutate(scenario = "Baseline scenario")
  } else {
    bind_rows(`Baseline scenario` = df_bl, `Modified scenario` = df_mod, .id = "scenario")
  }
  
  df %>%
    unnest(cols = c("scenario_baseline")) %>%
    group_by(sample) %>%
    mutate(year = ifelse(is.na(year), min(year, na.rm = T) - 1, year)) %>%
    ggplot(aes(x = year, y = total_y, colour = scenario)) +
    geom_line(aes(group = interaction(sample, scenario)), alpha = 0.05) +
    geom_smooth(size = 0.5, method = "loess", se = F, span = 0.3) +
    geom_hline(yintercept = baseline, size = 0.5, colour = "black", lty = 2) +
    labs(x = "Year",
         y = expression("Soil organic carbon (tonnes ha"^{-1}*")"),
         colour = "",
         title = "") +
    scale_colour_manual(values = c("darkred", "darkgreen")) +
    #ylim(ylims) +
    #facet_wrap(~scenario, nrow = 1) +
    theme_classic()
}

stockchange_plot <- function(df_bl, df_mod){
  df <- if(is.null(df_mod)){
    df_bl %>%
      mutate(scenario = "Baseline scenario")
  } else {
    bind_rows(`Baseline scenario` = df_bl, `Modified scenario` = df_mod, .id = "scenario")
  }
  
  ylims <- df %>%
    unnest(cols = c("scenario_baseline")) %>%
    ungroup() %>%
    filter(year >= 2019) %>%
    pull(stock_change) %>%
    quantile(c(0.05, 0.95), na.rm = T) %>%
    as.numeric()
    
  df %>%
    unnest(cols = c("scenario_baseline")) %>%
    group_by(sample) %>%
    filter(!is.na(year)) %>%
    mutate(decade = (floor(year / 10) * 10) %>% as.ordered()) %>%
    ggplot() +
    geom_hline(yintercept = 0, size = 0.5, colour = "black", lty = 2) +
    geom_boxplot(aes(x = decade, y = stock_change, fill = scenario), outlier.shape = NA, alpha = 0.5) +
    ylim(ylims) +
    labs(fill = "",
         x = "Decade",
         y = expression("C stock change (tonnes ha"^{-1}*" year"^{-1}*")"),
         title = "") +
    scale_fill_manual(values = c("darkred", "darkgreen")) +
    theme_classic()
}

stockchange_table <- function(df_bl, df_mod){
  
  dur <- 2070 - 2019
  
  df <- if(is.null(df_mod)){
    tibble(`Mean C stock change by 2070` = 0,
           `Mean CO2 sequestration by 2070` = 0,
           `Mean annual C stock change, 2020-2070` = 0,
           `Mean annual CO2 sequestration, 2020-2070` = 0)
  } else {
    bind_rows(`Baseline scenario` = df_bl, `Modified scenario` = df_mod, .id = "scenario") %>%
      unnest(cols = c("scenario_baseline")) %>%
      group_by(sample) %>%
      filter(year == 2070) %>%
      group_by(scenario) %>%
      summarise(C_stocks = mean(total_y)) %>%
      spread(key = scenario, value = C_stocks) %>%
      mutate(C_stock_diff = `Modified scenario` - `Baseline scenario`) %>%
      summarise(`Mean C stock change by 2070` = mean(C_stock_diff),
                `Mean CO2 sequestration by 2070` = `Mean C stock change by 2070` * 44/12,
                `Mean annual C stock change, 2020-2070` = `Mean C stock change by 2070` / dur,
                `Mean annual CO2 sequestration, 2020-2070` = `Mean annual C stock change, 2020-2070` * 44/12)
  }
  return(df)
}

build_output <- function(df_bl, df_mod){
  
  if(is.null(df_mod)){
    return(NULL) 
  } else {
    df <- bind_rows(`Baseline scenario` = df_bl, `Modified scenario` = df_mod, .id = "scenario") %>%
      unnest(cols = c("scenario_baseline")) %>%
      group_by(sample) %>%
      mutate(year = ifelse(is.na(year), min(year, na.rm = T) - 1, year)) %>%
      group_by(scenario, year) %>%
      summarise(`Active C pool, tonnes / ha` = mean(active_y),
                `Slow C pool, tonnes / ha` = mean(slow_y),
                `Passive C pool, tonnes / ha` = mean(passive_y),
                `Total C stocks, tonnes / ha` = mean(total_y),
                `C stock change, tonnes / ha / year` = mean(stock_change))
    return(df)
  }
    
}

detach("package:tidyverse", unload = T)
