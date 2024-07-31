###################################
# START INSTALL AND LOAD PACKAGES #
###################################

## packages to be installed from cran
from.cran <- c("gggenes", "here", "HMDHFDplus", "plyr", 
               "RColorBrewer", "tidyverse", "xlsx")

## check if installed, else install
for(i in c(from.cran)){
  
  if(i %in% from.cran){if(system.file(package = i) == ""){install.packages(i)}}

}

## load packages
library(tidyverse)

#################################
# END INSTALL AND LOAD PACKAGES #
#################################

# ----

##################
# START SET PATH #
##################

here::i_am("scripts/00_analysis.R")

################
# END SET PATH #
################

# ----

##########################
# START GENERATE FOLDERS #
##########################

if(!dir.exists(here::here("output"))){dir.create(here::here("output"))}
if(!dir.exists(here::here("output", "figures"))){dir.create(here::here("output", "figures"))}
if(!dir.exists(here::here("output", "tables"))){dir.create(here::here("output", "tables"))}

########################
# END GENERATE FOLDERS #
########################

# ----

####################
# START USER INPUT #
####################

## set color palettes
palette.1 <- RColorBrewer::brewer.pal(n = 12, "Paired")[c(2, 6)]
palette.2 <- RColorBrewer::brewer.pal(n = 11, "RdBu")[c(2, 4, 8, 10)]

##################
# END USER INPUT #
##################

# ----

######################################
# START EUROPEAN STANDARD POPULATION #
######################################

## manual input
EU.std <-
  rbind(
  c(0, 1000),
  c(1, 4000),
  c(5, 5500),
  c(10, 5500),
  c(15, 5500),
  c(20, 6000),
  c(25, 6000),
  c(30, 6500),
  c(35, 7000),
  c(40, 7000),
  c(45, 7000),
  c(50, 7000),
  c(55, 6500),
  c(60, 6000),
  c(65, 5500),
  c(70, 5000),
  c(75, 4000),
  c(80, 2500),
  c(85, 1500),
  c(90, 1000)
  ) %>% data.frame()

## assign variable names
names(EU.std) <- c("Age", "Pop")

## adjust age standard
EU.std <- 
  EU.std %>% 
  filter(Age >= 25) %>% 
  mutate(C = Pop / sum(Pop)) %>% 
  select(-Pop)

####################################
# END EUROPEAN STANDARD POPULATION #
####################################

# ----

###########################
# START HMD COUNTRY CODES #
###########################

## manual input
HMD.code <-
  rbind(
    c("Australia", "AUS"),  
    c("Austria", "AUT"), 
    c("Belarus", "BLR"), 
    c("Belgium",  "BEL"),
    c("Bulgaria", "BGR"),
    c("Canada", "CAN"), 
    c("Chile", "CHL"),
    c("Croatia", "HRV"),
    c("Czechia", "CZE"),
    c("Denmark", "DNK"),
    c("Estonia", "EST"),
    c("Finland", "FIN"),
    c("France",  "FRATNP"), 
    c("Germany", "DEUTNP"), 
    c("Greece", "GRC"),
    c("Hong Kong", "HKG"),
    c("Hungary", "HUN"),
    c("Iceland", "ISL"),
    c("Ireland", "IRL"),
    c("Israel", "ISR"),
    c("Italy", "ITA"),
    c("Japan", "JPN"), 
    c("Latvia", "LVA"),
    c("Lithuania", "LTU"),
    c("Luxembourg", "LUX"),
    c("Netherlands", "NLD"),
    c("New Zealand", "NZL_NP"),
    c("Norway", "NOR"),
    c("Poland", "POL"),
    c("Portugal", "PRT"),
    c("Republic of Korea", "KOR"),
    c("Russia", "RUS"),
    c("Slovakia", "SVK"),
    c("Slovenia", "SVN"),
    c("Spain", "ESP"),
    c("Sweden", "SWE"),
    c("Switzerland", "CHE"),
    c("Taiwan", "TWN"),
    c("United Kingdom", "GBR_NP"),
    c("USA", "USA"),
    c("Ukraine", "UKR")
    ) %>% data.frame()

## assign variable names
names(HMD.code) <- c("Country", "Code")

#########################
# END HMD COUNTRY CODES #
#########################

# ----

#######################
# START LOAD HMD DATA #
#######################

## load HMD exposures
Nx.5 <-
  map(HMD.code$Code, 
         function(x){
           HMDHFDplus::readHMD(unz(description = here::here("data", "exposures_d20240601.zip"), 
                                   filename = paste0("Exposures_5x1/", x, ".Exposures_5x1.txt"))) %>%
           select(-c(OpenInterval)) %>% 
           mutate(Country = HMD.code$Country[which(HMD.code$Code == x)],
                  Code = x) %>% 
           pivot_longer(cols = c("Female", "Male", "Total"),
                        names_to = "Sex",
                        values_to = "Exposures")
           })

## load HMD death counts
Dx.5 <-
  map(HMD.code$Code, 
         function(x){
           HMDHFDplus::readHMD(unz(description = here::here("data", "deaths_d20240601.zip"), 
                                   filename = paste0("Deaths_5x1/", x, ".Deaths_5x1.txt"))) %>%
           select(-c(OpenInterval)) %>% 
           mutate(Country = HMD.code$Country[which(HMD.code$Code == x)],
                  Code = x) %>% 
           pivot_longer(cols = c("Female", "Male", "Total"),
                        names_to = "Sex",
                        values_to = "Deaths")
           })

## load HMD life expectancy at birth
HMD.e0 <-
  map(HMD.code$Code, 
      function(x){
        HMDHFDplus::readHMD(unz(description = here::here("data", "e0_per_d20240601.zip"), 
                                filename = paste0("E0per/", x, ".E0per.txt"))) %>%
          mutate(Country = HMD.code$Country[which(HMD.code$Code == x)],
                 Code = x) %>% 
          pivot_longer(cols = c("Female", "Male", "Total"),
                       names_to = "Sex",
                       values_to = "e0")
      })

#####################
# END LOAD HMD DATA #
#####################

# ----

############################################
# START SELECT COUNTRIES W/ AVAILABLE DATA #
############################################

## combine HMD exposures and death counts 
HMD.full <-
  bind_rows(Nx.5) %>% 
  left_join(bind_rows(Dx.5)) %>% ## merge deaths and exposures
  mutate(Age = ifelse(Age >= 90 , 90, Age)) %>% ## aggregate deaths and exposures above age 90
  summarize(Deaths = sum(Deaths),
            Exposures = sum(Exposures),
            .by = c(Year, Country, Code, Sex, Age)) %>% 
  mutate(mx = Deaths / Exposures) ## calculate death rates

## restrict to countries with available data in observation period
HMD.available <-
  HMD.full %>% 
  filter(Year %in% 2000:2019) %>% 
  mutate(Start = min(Year),
         End = max(Year), .by = c(Country, Sex)) %>% ## find minimum and maximum year in 2000-2019 period
  filter(End == 2019, ## criterion 1: available data in 2019
         Start < 2009) %>% ## criterion 2: at least one observation before 2009
  filter(Year == Start | Year %in% c(2009, 2010, 2019)) %>% ## select only years of interest
  mutate(
         Missing = (mx == 0 | is.na(mx) | is.infinite(mx)) & Age >= 25, ## criterion 3: no zero deaths or denominators at age 25+
         Missing = sum(Missing), ## sum missings for each country
         .by = c(Country)
         ) %>% 
  mutate(Pop = sum(Exposures), .by = c(Country, Year)) %>% ## calculate total country population by year
  mutate(Pop = max(Pop), .by = Country) %>% ## use maximum population in observation period
  filter(Missing == 0 & Pop > 5000000) ## criterion 4: more than 5 million inhabitants

## create data set with selected countries
HMD.select <-
  HMD.full %>% 
  filter(Country %in% unique(HMD.available$Country), 
         Country != "Chile") ## exclude Latin American countries

##########################################
# END SELECT COUNTRIES W/ AVAILABLE DATA #
##########################################

# ----

##################################
# START TABLE 1: LIFE EXPECTANCY #
##################################

## data frame: life expectancy at birth
life.expectancy <-
  bind_rows(HMD.e0) %>% 
  filter(Country %in% unique(HMD.select$Country), Year %in% 2000:2019) %>% 
  mutate(Start = min(Year), 
         End = max(Year), .by = c(Country, Sex)) %>% 
  filter(Year == Start | Year == 2010 | Year == 2019)  %>% 
  mutate(Year = case_when(Year == 2019 ~ "Life Expectancy (End)", 
                          Year == 2010 ~ "Life Expectancy (2010)", 
                          TRUE ~ "Life Expectancy (Start)")) %>% 
  pivot_wider(id_cols = c("Country", "Code", "Sex", "Start", "End"),
              names_from = "Year",
              values_from = "e0")

## abbreviate labels
life.expectancy.tbl <-
  life.expectancy %>% 
  mutate(across(starts_with("Life"), ~ sprintf("%4.1f", .)))
  
################################
# END TABLE 1: LIFE EXPECTANCY #
################################

# ----

##################
# START FIGURE 1 #
##################

## vector of countries ordered by level of male life expectancy in first year
labels <-
  life.expectancy %>% 
  filter(Sex == "Male") %>% 
  arrange(`Life Expectancy (Start)`) %>% 
  pull(Country)

life.expectancy.2 <-
  life.expectancy %>% 
  filter(Sex != "Total") %>% 
  mutate(Country = factor(Country, levels = labels), ## reorder country variable
         Coord = as.numeric(factor(Country, levels = labels))) ## turn country variable into numeric variable

## minimum life expectancy, rounded for axis label
life.expectancy.min <-
  plyr::round_any(
    min(life.expectancy.2$`Life Expectancy (Start)`,
        life.expectancy.2$`Life Expectancy (End)`),
    f = floor, accuracy = 2.5
  )

## maximum life expectancy, rounded for axis label
life.expectancy.max <-
  plyr::round_any(
    max(life.expectancy.2$`Life Expectancy (Start)`,
        life.expectancy.2$`Life Expectancy (End)`),
    f = ceiling, accuracy = 2.5
  )

## plot
fig.1 <-
  ggplot(life.expectancy.2) +
  gggenes::geom_gene_arrow(aes(y = Coord, 
                               xmin = `Life Expectancy (Start)`, 
                               xmax = `Life Expectancy (End)`, 
                               color = Sex,
                               alpha = Sex), 
                           arrow_body_height = unit(1, "mm"), 
                           arrowhead_height = unit(2.5, "mm"),
                           arrowhead_width = unit(2, "mm"),
                           fill = "white",
                           size = 0.5,
                           show.legend = FALSE) +
  theme_bw() + 
  xlab("Life expectancy at birth (years)") +
  scale_y_reverse(breaks = 1:length(unique(life.expectancy.2$Country)),
                  labels = levels(life.expectancy.2$Country)) + ## add country labels
  scale_x_continuous(breaks = seq(life.expectancy.min, life.expectancy.max, 2.5)) +
  scale_color_manual(values = c("Male" = palette.1[1], 
                                "Female" = palette.1[2])) + 
  scale_alpha_manual(values = c("Male" = 0.8, "Female" = 1)) + ## make male arrows translucent
  theme(aspect.ratio = 1,
        panel.border = element_rect(linewidth = 0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.1, color = "gray85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 8,
                                    vjust = -1),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 6),
        axis.ticks.x = element_line(linewidth = 0.1),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(2, "pt"),
        text = element_text(family = "serif")) +
  coord_cartesian(xlim = c(life.expectancy.min, life.expectancy.max))

## save as .svg
ggsave(fig.1,
       width = 348 * 0.35,
       height = 348 * 0.35,
       units = "mm",
       dpi = 300,
       filename = paste0(here::here("output", "figures"), "/Dowd_Figure_1.svg"))

################
# END FIGURE 1 #
################

# ----

#######################
# START TABLE 1: ROMI #
#######################

## select countries
select.ROMI <-
  HMD.select %>% 
  filter(Year %in% 2000:2019) %>% 
  mutate(Start = min(Year), .by = c(Country, Sex)) %>% 
  filter(Year == Start | Year == 2009 | Year == 2010 | Year == 2019) %>% 
  filter(Age >= 25) %>% 
  select(Year, Country, Code, Sex, Age, mx) %>% 
  mutate(n = c(0, diff(Year)), .by = c(Country, Code, Sex, Age)) ## calculate length of interval

## calculate total ROMI
ROMI.std <- 
  select.ROMI %>% 
  left_join(EU.std) %>% 
  summarize(mx = sum(mx * C), .by = c(Year, Country, Code, Sex, n)) %>% ## calculate age-standardized mortality rate
  mutate(ROMI = - log(mx / lag(mx)) / n, .by = c(Country, Code, Sex)) %>% ## calculate ROMI
  filter(Year %in% c(2009, 2019)) %>% 
  select(Country, Code, Sex, Period = Year, ROMI) 

## reshape and calculate difference
ROMI.table <-
  ROMI.std %>% 
  pivot_wider(names_from = c("Sex", "Period"),
              values_from = "ROMI") %>% 
  mutate(Diff_Female = Female_2019 - Female_2009,
         Diff_Male = Male_2019 - Male_2009,
         Diff_Total = Total_2019 - Total_2009) %>%
  mutate(across(-c(Country, Code), ~ 1 + length(unique(ROMI.std$Country)) - row_number(.), .names = "{.col}_Rank")) %>% 
  mutate(across(-c(Country, Code, ends_with("_Rank")), ~
                  ifelse(. > 0, sprintf("%+5.3f", .), 
                         ifelse(. < 0, sprintf("%6.3f", .), 
                                sprintf("%5.3f", .))))) %>% 
  relocate(Country, Code, 
           starts_with("Female") & !ends_with("Rank"), Diff_Female,
           starts_with("Female") & ends_with("Rank"),           
           starts_with("Male") & !ends_with("Rank"), Diff_Male,
           starts_with("Male") & ends_with("Rank"),           
           starts_with("Total") & !ends_with("Rank"), Diff_Total,
           starts_with("Total") & ends_with("Rank"))

#####################
# END TABLE 1: ROMI #
#####################

# ----

######################################
# START TABLE 1: COMBINE E0 AND ROMI #
######################################

## set up table (main)
table.1 <-
  life.expectancy.tbl %>% 
  pivot_wider(names_from = "Sex",
              values_from = starts_with("Life Expectancy")) %>% 
  left_join(ROMI.table) %>% 
  arrange(as.numeric(Total_2019_Rank)) %>% 
  select(Country, 
         Total_2019, Total_2019_Rank,
         Total_2009, Total_2009_Rank,
         Diff_Total, Diff_Total_Rank,
         `Life Expectancy (Start)_Total`, `Life Expectancy (2010)_Total`, `Life Expectancy (End)_Total`)

## set up table (supplementary)
table.1.supp <-
  life.expectancy.tbl %>% 
  pivot_wider(names_from = "Sex",
              values_from = starts_with("Life Expectancy")) %>% 
  left_join(ROMI.table) %>% 
  arrange(as.numeric(Male_2019_Rank)) %>% 
  select(Country, 
         Male_2019, Male_2019_Rank,
         Male_2009, Male_2009_Rank,
         Diff_Male, Diff_Male_Rank,
         Female_2019, Female_2019_Rank,
         Female_2009, Female_2009_Rank,
         Diff_Female, Diff_Female_Rank)
         
## add columns names as separate row
table.1.out <-
  rbind(names(table.1) %>% matrix() %>% t(),
        as.matrix( 
          table.1
        ))

table.1.supp.out <-
  rbind(names(table.1.supp) %>% matrix() %>% t(),
        as.matrix( 
          table.1.supp
        ))

## write to excel
xlsx::write.xlsx(table.1.out, 
                 file = paste0(here::here("output", "tables"), "/Dowd_Table_1.xlsx"),
                 row.names = FALSE, 
                 col.names = FALSE)

xlsx::write.xlsx(table.1.supp.out, 
                 file = paste0(here::here("output", "tables"), "/Dowd_Table_1_Supplement.xlsx"),
                 row.names = FALSE, 
                 col.names = FALSE)

########################################
#  START TABLE 1: COMBINE E0 AND ROMI  #
########################################

# ----

##################
# START FIGURE 2 #
##################

## calculate age-specific ROMI  
HMD.ROMI <-
  select.ROMI %>% 
  mutate(ROMI = - log(mx / lag(mx)) / n, .by = c(Country, Code, Sex, Age)) %>% 
  filter(Year %in% c(2009, 2019)) %>% 
  select(Country, Code, Sex, Age, Period = Year, ROMI) %>% 
  mutate(Country = case_when(Country == "Republic of Korea" ~ "Rep. of Korea",
                             TRUE ~ Country))

## add last row again for geom_step (see below)
aux.ROMI <-
  HMD.ROMI %>% 
  add_row(
    
    HMD.ROMI %>% 
      filter(Age == max(Age)) %>% 
      mutate(Age = Age + 5)
    
  )

## maximum ROMI
ROMI.max <- plyr::round_any(max(HMD.ROMI$ROMI), f = ceiling, accuracy = 0.01)

## split available countries into groups
group <- round(length(unique(HMD.ROMI$Country)) / 2)

CNTRY <-
  list(unique(HMD.ROMI$Country)[1:group],
       unique(HMD.ROMI$Country)[(group + 1):length(unique(HMD.ROMI$Country))])

## plot
fig.2 <-
  map2(rep(CNTRY, 3), rep(c("Male", "Female", "Total"), each = 2), function(x, y){
  
  ggplot(data = HMD.ROMI %>% filter(Country %in% x, Sex == y), 
         aes(x = Age, y = ROMI)) +
  geom_bar(data = . %>% filter(Period == 2009), 
           aes(fill = ROMI), 
           stat = "identity", show.legend = TRUE) + ## plot 2000-2009 as bars
  geom_step(data = aux.ROMI %>% 
              filter(Country %in% x, Sex == y, Period == 2019) %>% 
              mutate(ROMI = ifelse(ROMI > 0, 0, ROMI)), ## plot negative values for 2010-2019 as steps
            aes(x = Age - 2.5), 
            stat = "identity", 
            color = palette.2[1], 
            linetype = "solid", 
            linewidth = 0.1) +
  geom_step(data = aux.ROMI %>% 
              filter(Country %in% x, Sex == y, Period == 2019) %>% 
              mutate(ROMI = ifelse(ROMI < 0, 0, ROMI)), ## plot positive values for 2010-2019 as steps
            aes(x = Age - 2.5), 
            stat = "identity", 
            color = palette.2[4], 
            linetype = "solid", 
            linewidth = 0.1) +
  geom_step(data = data.frame(x = c(25, 50, 90), y = c(0, 0, 0)), 
            aes(x = x, y = y, color = y), inherit.aes = FALSE, linewidth = 0.1) + ## plot zero data set to include correct legend
  geom_hline(yintercept = 0, color = "black", linewidth = 0.1) + ## add black vertical line at 0
  labs(x = "Age", y = "Rate of mortality improvement") +
  scale_x_continuous(breaks = c(25, 50, 75, 90),
                     minor_breaks = seq(25, 90, 5)) +
  scale_y_continuous(limits = c(-ROMI.max / 2, ROMI.max),
                     breaks = c(-ROMI.max / 2, 0, ROMI.max / 2, ROMI.max),
                     labels = function(x) ifelse(x > 0, sprintf("%+4.2f", x), 
                                                 ifelse(x < 0, sprintf("%5.2f", x), 
                                                        0))) + ## labels will have positive or negative sign
  binned_scale(name = paste0("2010", "\U2013", "2019"), ## add binned legend for steps
               aesthetics = "color",
               palette = function(x) c(palette.2[1], palette.2[4]),
               breaks = c(-100, 0, 100),
               limits = c(-100, 100),
               show.limits = FALSE, 
               guide = "colorsteps",
               labels = c("", paste0("\U2013", paste(rep("\U2009", 7), collapse = ""), "+"), "")
  ) +
  binned_scale(name = paste0("2000", "\U2013", "2009"), ## add binned legend for bars
               aesthetics = "fill",
               palette = function(x) c(palette.2[2], palette.2[3]),
               breaks = c(-100, 0, 100),
               limits = c(-100, 100),
               show.limits = FALSE, 
               guide = "colorsteps",
               labels = c("", paste0("\U2013", paste(rep("\U2009", 7), collapse = ""), "+"), "")
  ) +
  guides(color = guide_bins(order = 2, override.aes = list(fill = "transparent"), position = "inside"), ## adjust legends for bars and lines
         fill = guide_bins(order = 1, override.aes = list(color = "transparent"), position = "inside"),
         y = guide_axis(minor.ticks = TRUE)) +
  coord_flip(ylim = c(-ROMI.max / 2, ROMI.max)) + ## clip axis
  facet_wrap(~ Country, ncol = 6) + ## set column numbers
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 0.1),
        panel.spacing.y = unit(2, "pt"),
        strip.background = element_rect(fill = "white", linewidth = 0.1),
        strip.text = element_text(size = 6),
        legend.position.inside = c(0.8375, 0.075), 
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.spacing.x = unit(2, "pt"),
        legend.title.position = "top",
        legend.axis.line = element_blank(),
        legend.key.height = unit(6, "pt"),
        legend.key.width = unit(12, "pt"),
        legend.title = element_text(size = 6, hjust = 0.5),
        legend.text = element_text(size = 6, hjust = 0.5, margin = unit(c(0, 0, 1, 0), "pt")),
        axis.title.x = element_text(size = 8, vjust = -1),
        axis.title.y = element_text(size = 8, vjust = 2),
        axis.text.x = element_text(size = 6, angle = 315, hjust = 0),
        axis.text.y = element_text(size = 6),
        axis.ticks = element_line(linewidth = 0.1),
        axis.ticks.length = unit(2, "pt"),
        text = element_text(family = "serif"),
        plot.margin = unit(c(0, 8, 0, 8), "pt"))

})

## save as .svg
map(1:6, function(x){

sex <- ifelse(x %in% 1:2, "male", ifelse(x %in% 3:4, "female", "total"))  
letter <- ifelse(x %in% c(1, 3, 5), "a", "b")  

## save as .svg
ggsave(fig.2[[x]],
       width = 348 * 0.35,
       height = 240 * 0.35,
       units = "mm",
       dpi = 300,
       filename = paste0(here::here("output", "figures"), "/Dowd_Figure_2-", sex, "-", letter, ".svg"))

})

################
# END FIGURE 2 #
################

# ----
