# script to implement different scenarios to the IPCC 3-pool soil carbon baseline model

library(tidyverse)


# Modifications to ipcc-c-model-functions

source("ipcc-c-model-functions.R")

##################################################################

# Scenario 1: 10% of conventional tillage becomes reduced tillage

#################################################################


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

##################

# I would like to modift this function, so there is a convertion from conventional to reduced tillage around 10%.
#Then the parameters affected by the type of tillage will change. Also how can I use a different env() to stored values and functions apart from the baseline model?


run_model <- function(df){
  
 
  till_type <- case_when(
    sample == 0.4 ~ "full",
    sample == 0.5 ~ "reduced",
    sample == 0.1 ~ "zero"
  )
    
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


##################################################################

# Scenario 2: Biennial convetional tillage. This aims to applied conv tillage every two years
#################################################################