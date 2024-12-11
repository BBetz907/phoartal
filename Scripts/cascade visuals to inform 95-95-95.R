# AUTHOR:   B. Betz | USAID
# PURPOSE:  enhance analytics for POART
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

# GLOBAL VARIABLES --------------------------------------------------------
    
  # Create folder structure
    # glamr::si_setup()
  # SI specific paths/functions  
    # load_secrets()


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
  

  dfnat <- data.frame(year=c(2022,2023,2024), 
                      region="national",
                       source = c("dummy", "HARP","HARP"),
                       first95=c(61,61,61),second95=c(60,64,67),third95=c(27,40,43),
                       `estimated PLHIV` = c(158400, 189000,215400),
                       `diagnosed PLHIV` = c(99611,115680,131335),
                       `PLHIV on ART` = c(65197,74258,88544),
                       `VL Tested` = c(13265,33727,39003),
                       `VL Suppressed` = c(12811,29571,34252)) %>% glimpse() %>% 
    mutate(`PLHIV to be diagnosed` =  estimated.PLHIV - diagnosed.PLHIV,
           `PLHIV to be enrolled on ART` = diagnosed.PLHIV - PLHIV.on.ART, 
           `PLHIV not virally suppressed` = PLHIV.on.ART - VL.Suppressed) %>% 
    pivot_longer(cols=4:last_col(), names_to = "indicator") %>%
    percent_change() %>% #apply local function
    glimpse()
    
    dfnat %>% count(indicator)
    # export for validation
    dfnat2022 <- dfnat %>% filter(year==2022)
  
    write_csv(dfnat2022, "Dataout/nat2022.csv")
    
    write_csv(dfnat, "Dataout/nat.csv")
    
# VISUALIZE slope-------------------------------------------------------------------
slope <-  dfnat %>% 
      filter(indicator %in% slope_indicators) %>% 
      filter(!is.na(percent_change)) %>%
      mutate(yeartype = if_else(year==max(year), "present", "other")) %>% 
      ggplot(aes(x=year, y=percent_change, group = indicator)) +
        geom_line(size=1.5, color = "#808080") + 
      
        geom_point(aes(
                      # shape = yeartype, 
                       color=yeartype
                       ), size=4) +
      # Labels for percent change
      geom_text(
        aes(color=yeartype,
          label = paste0("+", round(percent_change, 0), "%")),
        hjust = -0.04, 
        vjust=-1.3,
        size = 4
      ) +
      scale_shape_manual(
        values = c("present" = 1, "other" = 16) # 1 = empty circle, 16 = filled circle
      ) +
      scale_color_manual(
        values = c("present" = grey80k, "other" = "#808080"),
        name = NULL) +
          facet_grid(cols = vars(indicator)) +
      scale_y_continuous(limits = c(0,70)) +
      scale_x_continuous(limits=c(2022.5, 2024.5))+
        # scale_x_continuous(
        # breaks = unique(dfnat$year), # Ensure all years are shown
        # labels = scales::number_format(accuracy = 1) # Remove decimals
      # ) +
          
  glitr::si_style_nolines()  +
      
      
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(hjust = 0),
        strip.placement = "switch",
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        
  ) +
      labs(
        title = "Despite increased coverage between <span style='color:#939598;'>2022-23</span> and <span style='color:#58595b;'>2023-24</span>",)
    
    # VISUALIZE bar-------------------------------------------------------------------
    
bar <-    dfnat %>% 
      filter(indicator %in% bar_indicators) %>% 
      mutate(indicator=factor(indicator, levels=bar_indicators)) %>% 
      mutate(focus=case_when(indicator=="PLHIV to be diagnosed" & year>=max(year)-1 ~ "focus",
                             indicator=="PLHIV not virally suppressed" & year==max(year)~"focus",
                             .default = "background")) %>% 
      ggplot(aes(x=year, y=value, group = indicator)) +
      geom_col(aes(fill=focus)) + 
      
      facet_grid(cols = vars(indicator)) +
      
      scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
      scale_color_manual(
        values = c("focus" = burnt_sienna, "background" = grey30k),
        name = NULL) +
      scale_fill_manual(
        values = c("focus" = burnt_sienna, "background" = grey30k),
        name = NULL) +      
      glitr::si_style_ygrid()  +
      theme(
            axis.title = element_blank(),
            legend.position = "none",
            strip.text = element_text(hjust = 0),
            title = element_markdown()) +
      labs(
        title = "<span style='color:#e07653;'>Cascade gaps</span> stalled progress towards the 95-95-95 goals",
      ) 
# EXPORT -------------------------------------------------------------------
    
output <-  slope / bar   + plot_layout(ncol = 1, heights = c(1,1.5)) 
    
    
ggsave(plot = output, filename = "Images/cascade_contrast.png", width = 8, height = 3.75)    
