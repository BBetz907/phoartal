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

## define function -----
  ### lag -------------
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
      
      value_last = case_when(year == max(year) ~  paste0(as.character(round(value/1000,0)),"K")),
      
      
      percent_change = 
        round(
          (value - lag(value)) / lag(value)*100 , 0
        ),
     
      percent_change_lab = case_when(percent_change > 0 ~ paste0("+", as.character(percent_change),"%"),
                                     percent_change < 0 ~ paste0(as.character(percent_change),"%"),
                                     .default = ""),
      
      percent_change_lablast = if_else(year==max(year), percent_change_lab, ""),

      ) %>% 
    #remove NA for first year
    ungroup()
}


## identify indicators for ordering ---
slope_indicators <- c("diagnosed PLHIV", "PLHIV on ART", "PLHIV Virally Suppressed")
bar_indicators <- c("PLHIV to be diagnosed", "PLHIV to be enrolled on ART", "PLHIV not virally suppressed")
indicator_group_order <- c("Estimated PLHIV", "Among diagnosed PLHIV", "Among PLHIV on ART")

# clean
# dfreg <- read_xlsx(path = filepath, sheet = "Sheet2", range = "A1:M16") %>% 
#   pivot_longer(cols=3:last_col(), names_to = "indicator") %>% 
#   rename(region = NAME_REG) %>% 
#   mutate(source = "shared file") %>%   
#   janitor::clean_names() %>% 
#   percent_change() %>% #apply local function
#   glimpse()


dfnat <- data.frame(year=c(2022,2023,2024), 
                    region="national",
                    source = "HARP",
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



## refine for diverging bar chart -------------------------------------------

dfnatdiverge <- dfnat %>% 
                  mutate(
                    # code indicator into 3x95 denominators
                         indicator_group =   case_when(
                                     str_detect(str_to_lower(indicator), "diag") ~ "Estimated PLHIV",
                                     str_detect(str_to_lower(indicator), "art") ~ "Among diagnosed PLHIV",
                                     str_detect(str_to_lower(indicator), "virally sup") ~ "Among PLHIV on ART",
                                     ),
                                 
                         indicator_group=factor(indicator_group, levels=indicator_group_order), #recode as a factor to sort facets
                                 
                         pos_neg = case_when(str_detect(indicator, "not|to\\sbe") ~ "neg",
                                                     .default = "pos"),
                         value_diverge = if_else(pos_neg == "neg", -value, value),
                                                               ) 

dfnatdiverge %>% count(indicator_group)


# visualize diverging bar ------

diverge <- dfnatdiverge %>% filter(!is.na(indicator_group)) %>% 
  mutate(
         focus=case_when(indicator=="PLHIV to be diagnosed" & year>=max(year)-1 ~ "focus",
                         # indicator=="PLHIV not virally suppressed" & year==max(year)~"focus",
                         .default = pos_neg)
         
  ) %>% 
  ggplot(aes(x = year, y = value_diverge, fill=focus)) +
  geom_col() +
  facet_grid(cols = vars(indicator_group)) +
  
  scale_y_continuous(
    labels = function(x) label_number(scale = 1e-3, suffix = "k")(abs(x)),  # Apply label_number to abs(x)
    # labels = label_number(scale = 1e-3, suffix = "k"),
    # labels = function(x) abs(x)  # Convert axis labels to absolute values
    
    limits = c(-225000,250000),
    breaks = c(-100000, -50000, 0, 50000, 100000,150000)
  ) +
  # scale_color_manual(
  #   values = c("focus" = burnt_sienna, "neg" = grey30k, "pos" = scooter),
  #   name = NULL) +
  scale_fill_manual(
    values = c("focus" = burnt_sienna_light, "neg" = grey30k, "pos" = genoa),
    name = NULL) +
  
  glitr::si_style_ygrid()  +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    panel.spacing = unit(4, "lines"),  # Increase space between panels
    strip.text = element_text(hjust = 0, face = "bold", colour = grey70k), #make labels bold
    title = element_markdown()) +
  
  labs(
    title = "<span style='color:#287c6f;'>Improved coverage across cascades</span> <span style='color:#ffcaa2;'>undermined by increases in new infections</span> from 2022 to 2024<br>",
    # subtitle = "from 2022 to 2024"
  ) 

diverge

ggsave(plot = diverge, filename = "Images/cascade_diverge.png", width = 9, height = 3.75)    

# visualize stacked bar ------
 
dfnatdiverge %>% glimpse()

stacked <- dfnatdiverge %>% filter(!is.na(indicator_group)) %>% 
  ggplot(aes(x = year,fill=pos_neg)) +
  geom_col(aes(y=value)) + #choose column chart

  #label all bars inside
  geom_text(aes(y=value-5000,
                label = paste0(as.character(round(value/1000,0)),"K"), colour = pos_neg),
            position = position_stack(vjust = 0.95),
            size =3.5,
            # fontface = "bold"
  ) +
  
  #label last bars in bold for emphasis
  geom_text(aes(y=value-5000,
                label = value_last, colour = pos_neg),
            position = position_stack(vjust = 0.95),
            size =3.5,
            fontface = "bold"
  ) +
# 
#   # label %change in last bar
#   geom_text(aes(y = value -15000,
#                 label = percent_change_lablast, colour = pos_neg,
#                ),
#             size = 3,
#             position = position_stack(vjust = 0.95),
#   ) +

  facet_grid(cols = vars(indicator_group)) + # see chart for each 95
  
  scale_y_continuous(
    labels = function(x) label_number(scale = 1e-3, suffix = "k")(abs(x)),  # Apply label_number to abs(x)
    limits = c(0,300000), #manually set limits for Y
    breaks = seq(0,200000, 50000) #manualy set breaks between labels
  ) +
  
  #assign color to bars
  scale_fill_manual(
    values = c("focus" = burnt_sienna_light, "neg" = grey30k, "pos" = genoa),
    name = NULL) +
  
  #assign color to text under bars
  scale_color_manual(
    values = c(pos = grey10k, neg = grey80k)
  ) +
  
  glitr::si_style_ygrid()  +
  theme(
    axis.title.x = element_blank(), #remove title for year
    axis.title.y = element_text(hjust = 0.22), # center title on thousands
    legend.position = "none", #hide legend, use PPT labels
    panel.spacing = unit(4, "lines"),  # Increase space between panels
    strip.text = element_text(
      hjust = 0.5,
                              face = "bold", colour = grey70k), #make labels bold
    title = element_blank() #element_markdown(), #set title format as markdown to color titles
    ) +
  
  labs(
    # title = "<span style='color:#287c6f;'>Coverage improved across cascades</span> <span style='color:#BCBEC0;'>but gaps remain</span><br>",
    # subtitle = "from 2022 to 2024",
    y = "thousands (k)", # label Y-axis
    
  ) 

stacked

ggsave(plot = stacked, filename = "Images/cascade_stacked.png", width = 9, height = 3.75)    

