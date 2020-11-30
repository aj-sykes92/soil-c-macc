# script to implement IPCC 3-pool soil carbon model

# load req'd packages
library(tidyverse)
library(dplyr)

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
# calculate f2 (variable depending on tillage) ## stabilization efficiencies for str decay comp entering the active pool if til ==??
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
C_in_residues <- function(yield, crop_type, frac_renew, frac_remove, crop_agr_coeffs, crop_bgr_coeffs) {

  RS <- crop_bgr_coeffs %>% filter(Crop == crop_type) %>% pull(RS)
  DRY <- crop_bgr_coeffs %>% filter(Crop == crop_type) %>% pull(DRY)
  Slope <- crop_agr_coeffs %>% filter(Crop == crop_type) %>% pull(Slope)
  Intercept <- crop_agr_coeffs %>% filter(Crop == crop_type) %>% pull(Intercept)
  
  yield_dry <- yield * DRY
  agdm <- yield_dry * Slope + Intercept
  bgr <- (yield_dry + agdm) * RS * frac_renew # note this line is from IPCC (2019) Eq 11.6 --- presumed error in equivalent eq. 5.0H
  agr <- agdm * frac_renew * (1 - frac_remove)
  
  C_in_residues <- agr * 0.42 + bgr * 0.42 # 42% C assumption
  
  return(C_in_residues)
}

###################
# function to calculate C inputs from manure (tonnes C per hectare)
C_in_manure <- function(man_nrate, man_type, man_params) {
  CN <- man_params %>% filter(Livestock_type == man_type) %>% pull(CN_ratio)
  C_in_manure <- CN * man_nrate * 10^-3 # manure C in tonnes ha-1 
  return(C_in_manure)
}

###################
# functions to calculate crop N and Lignin fractions from crop and manure parameter data
N_frac <- function(crop_type, man_type, C_res, C_man, cc_dm_tha, cc_N_res, crop_params, man_params) { 
  
  lookup1 <- crop_params %>%
    filter(Crop == crop_type) %>%
    mutate(C_frac = 0.42)
  
  lookup2 <- man_params %>%
    filter(Livestock_type == man_type) %>%
    mutate(C_frac = N_frac * CN_ratio)
  
  tot_res <- C_res / lookup1$C_frac
  tot_man <- ifelse(C_man == 0, 0, C_man / lookup2$C_frac) # ifelse to prevent divide by zero error
  
  N_res <- tot_res * lookup1$N_frac
  N_man <- tot_man * lookup2$N_frac
  
  return((N_res + N_man + cc_N_res) / (tot_res + tot_man + cc_dm_tha))
}

lignin_frac <- function(crop_type, manure_type, C_res, C_man, cc_dm_tha, cc_lignin_res, crop_params, man_params) {
  lookup1 <- crop_params %>%
    filter(Crop == crop_type) %>%
    mutate(C_frac = 0.42)
  lookup2 <- man_params %>%
    filter(Livestock_type == manure_type) %>%
    mutate(C_frac = N_frac * CN_ratio)
  
  tot_res <- C_res / lookup1$C_frac
  tot_man <- ifelse(C_man == 0, 0, C_man / lookup2$C_frac) # ifelse to prevent divide by zero error
  
  lignin_res <- tot_res * lookup1$Lignin_frac 
  lignin_man <- tot_man * lookup2$Lignin_frac
  
  return((lignin_res + lignin_man + cc_lignin_res) / (tot_res + tot_man + cc_dm_tha))
}

###################
# cover crop functions

  # has cover crop in cell/year?
has_cc <- function(data, cc_probs) {
  data %>%
    left_join(cc_probs, by = "till_type") %>%
    mutate(cc_threshold = runif(n = nrow(data), min = frac_cc_min, max = frac_cc_max),
           has_cc = runif(n = nrow(data)) <= cc_threshold) %>%
    select(-frac_cc_min, -frac_cc_max, -cc_threshold)
}

    # get cover crop data
get_cover_crop_data <- function(data, cc_params, cc_bgr_coeffs, combi_min = 2, combi_max = 4) {
  
  # group size and combination
  combi_n <- sample(combi_min:combi_max, 1)
  combi_groups <- sample(1:max(cc_params$Group), combi_n, replace = F)
  
  combi_mix <- cc_params %>%
    filter(Group %in% combi_groups) %>%
    group_by(Group) %>%
    sample_n(1) %>%
    ungroup()
  
  # mix ratios
  ratio <- runif(n = combi_n)
  ratio = ratio / sum(ratio)
  
  # Cover crop frac remove by cover crop termination method
  sample <- runif(1)
  cc_end <- ifelse(sample >= 0.8, 0.666, 0.00) # Storr et al 2019. Approx 80% used herbicides from cc termination, while 20% used grazing/cultivation
  
  
  # output
  combi_mix <- combi_mix %>%
    summarise(has_cc = TRUE,
              cc_yield_tha = sum(Mean_yield_t_ha * ratio),
              cc_CN_ratio = sum(CN_ratio * ratio),
              cc_N_frac = sum(N_frac * ratio),
              cc_lignin_frac = sum(Lignin_frac * ratio),
              cc_RS = sum(RS * ratio),
              cc_DRY = sum(DRY * ratio)) %>%
    mutate(cc_agr_bio = cc_yield_tha * cc_DRY,
           cc_bgr_bio = cc_agr_bio * cc_RS,
           cc_dm_tha = cc_agr_bio*(1 - cc_end) + cc_bgr_bio,
           cc_N_res = cc_dm_tha * cc_N_frac,
           cc_C_res = cc_N_res * cc_CN_ratio,
           cc_lignin_res = cc_dm_tha * cc_lignin_frac) %>%
    select(has_cc, cc_dm_tha:cc_lignin_res)
  
  # zero option for has_cc == F
  combi_mix <- bind_rows(combi_mix,
                         tibble(has_cc = FALSE,
                                cc_dm_tha = 0,
                                cc_N_res = 0,
                                cc_C_res = 0,
                                cc_lignin_res = 0)
  )
  
  # add to Dat_nest
  data <- data %>%
    left_join(combi_mix, by = "has_cc")
 
    return(data)
}

###################
# function to modify (tibble) dataframe to include a first row calculated from run-in period
run_in <- function(df, years){
  df %>%
    arrange(year) %>% # make absolutely sure it's in chronological order
    slice(1:years) %>%
    summarise_all(~ifelse(is.numeric(.), mean(., na.rm = T), DescTools::Mode(.))) %>%
    mutate(origin = "run_in",
           year = NA) %>%
    bind_rows(df) %>%
    return()
}

###################
# run everything in order
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
                     tillfac = tillfac(tillage = till_type)),
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
                     wfac = wfac),
           passive_y_ss = passive_y_ss(active_y_ss = active_y_ss,
                                       slow_y_ss = slow_y_ss,
                                       k_a = k_a,
                                       k_s = k_s,
                                       k_p = k_p),
           passive_y = passive_y(k_p = k_p,
                                 passive_y_ss = passive_y_ss),
           
           # roundup
           total_y = active_y + slow_y + passive_y,
           stock_change = SOC_stock_change(active_y, slow_y, passive_y)) %>%
    select(year, yield_tha, C_tot, alpha:stock_change)
}

##########################
# model build functions
##########################

# residue C
build_residue_C <- function(Dat_nest, crop_agr_coeffs, crop_bgr_coeffs) {
  Dat_nest %>%
    mutate(
      data = map2(.data$data, .data$crop_type, function(data, crop_type) {
        data %>%
          mutate(C_res = C_in_residues(.data$yield_tha, crop_type,
                                       .data$frac_renew, .data$frac_remove,
                                       crop_agr_coeffs, crop_bgr_coeffs))
      })
    )
}

# cover crop data
build_cover_crops <- function(Dat_nest, cc_probs, cc_params) {
  Dat_nest %>%
    mutate(data = data %>%
             map(~has_cc(.x, cc_probs)) %>%
             map(~get_cover_crop_data(.x, cc_params))
    )
}

# manure C
build_manure_C <- function(Dat_nest, man_params) {
  Dat_nest %>%
    mutate(
      data = map2(.data$data, .data$man_type, function(data, man_type) {
        data %>%
          mutate(C_man = C_in_manure(.data$man_nrate, man_type, man_params))
      })
    )
}

# N/C/lignin fractions
build_fractions <- function(Dat_nest, crop_params, man_params) {
  Dat_nest %>%
    mutate(
      data = pmap(list(.data$data, .data$crop_type, .data$man_type), function(data, crop_type, man_type) {
        data %>%
          mutate(N_frac = N_frac(crop_type, man_type, .data$C_res, .data$C_man, .data$cc_dm_tha, .data$cc_lignin_res,
                                 crop_params, man_params),
                 lignin_frac = lignin_frac(crop_type, man_type, .data$C_res, .data$C_man, .data$cc_dm_tha, .data$cc_lignin_res,
                                           crop_params, man_params),
                 C_tot = .data$C_res + .data$C_man + .data$cc_C_res
          )
      })
    )
}

###################
# model timeseries plot function
ts_plot <- function(df_bl, df_mod = NULL, baseline_year = 2020){
  
  # baseline soil carbon
  #baseline <- df_bl$scenario_baseline[[1]] %>% filter(year == baseline_year) %>% pull(total_y)
  
  # bind up dfs if 2nd is present
  df <- if(is.null(df_mod)){
    df_bl %>%
      mutate(scenario = "Baseline scenario")
  } else {
    bind_rows(`Baseline scenario` = df_bl, `Modified scenario` = df_mod, .id = "scenario")
  }
  
  # plot
  df %>%
    mutate(scenario_baseline = scenario_baseline %>%
             map(~.x %>% mutate(total_y_rel = total_y / total_y[1]))) %>%
    unnest(cols = c("scenario_baseline")) %>%
    drop_na() %>%
    ggplot(aes(x = year, y = total_y_rel, colour = scenario)) +
    geom_line(aes(group = interaction(sample, scenario, x, y)), alpha = 0.05) +
    geom_smooth(size = 0.5, method = "loess", se = F, span = 0.3) +
    #geom_hline(yintercept = 1, size = 0.5, colour = "black", lty = 2) +
    labs(x = "Year",
         #y = expression("Soil organic carbon (tonnes ha"^{-1}*")"),
         y = "Soil organic carbon (fractional relative to baseline)",
         colour = "",
         title = "") +
    scale_colour_manual(values = c("darkred", "darkgreen")) +
    theme_classic()
}
