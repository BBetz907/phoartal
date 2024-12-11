# AUTHOR:   B. Betz | USAID
# PURPOSE:  explore POART shared by Clar from CDC
# REF ID:   22eb92ad 
# LICENSE:  MIT
# DATE:     2024-12-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(stringr)
  library(janitor)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)


# IMPORT ------------------------------------------------------------------
filepath <- return_latest(folderpath = "Data/", pattern = "^20[0-9]{2}\\_Cascade")

  # read_psd
  # 
  # # Grab metadata
  #   metadata <- get_metadata(filepath) 
  # 
  ref_id <- "22eb92ad"

# MUNGE -------------------------------------------------------------------

  ## define lag function
  percent_change <- function(df){
    df <- df %>%
      #setup
      arrange(region, indicator, year) %>% 
      group_by(region, indicator) %>% 
      #calculate
      mutate(
        indicator = str_replace_all(indicator, "\\.", " "),
        indicator = recode(indicator,
                           "first95" = "1st 95",
                           "second95" = "2nd 95",
                           "third95" = "3rd 95",
                           "VL Tested" = "PLHIV Virally Tested",
                           "VL Suppressed" = "PLHIV Virally Suppressed"
                           ),
        percent_change = 
               round(
                 (value - lag(value)) / lag(value)*100 , 1
               )) %>% 
      #remove NA for first year
      ungroup()
  }
  
  
  ## identify indicators for slope
  slope_indicators <- c("diagnosed PLHIV", "PLHIV on ART", "PLHIV Virally Suppressed")
  bar_indicators <- c("PLHIV to be diagnosed", "PLHIV to be enrolled on ART", "PLHIV not virally suppressed")
  
  # munge
    dfreg <- read_xlsx(path = filepath, sheet = "Sheet2", range = "A1:M16") %>% 
    pivot_longer(cols=3:last_col(), names_to = "indicator") %>% 
    rename(region = NAME_REG) %>% 
    mutate(source = "shared file") %>%   
    janitor::clean_names() %>% 
      percent_change() %>% #apply local function
    glimpse()
  
