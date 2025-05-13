
# setup----

# install.packages("pacman") 
library(pacman)
pacman::p_load(shiny, cdlTools, usmap, dplyr, ggplot2, stringr, tidyr, DT, shinythemes, tidycensus, geofacet,
               shinyWidgets, maps, cowplot, rsconnect, mapproj, ggpubr, shinyjs, shinyBS, BH, terra,
               plotly, usdata)

# setwd("~/OneDrive - The Pennsylvania State University/SoDA RA/deployment")

# load state data
raw_state_data = read.csv("./final_state_data.csv")

# load county data
raw_county_data = read.csv("./final_county_data.csv")

# change names of certain variables to make them easier to work with in shiny app
data_list = list(raw_state_data, raw_county_data)
for(i in 1:2){
  colnames(data_list[[i]]) = gsub("_in_", "_", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("labor_force", "lf", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("above_poverty_line", "apl", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("yes_high_school_diploma", "hsd", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("_yes_", "_", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("_within_", "_", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("household_income", "hi", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("literate", "lit", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("percent", "perc", colnames(data_list[[i]]))
  colnames(data_list[[i]]) = gsub("ratio_perc", "ratio", colnames(data_list[[i]]))
}

# assigning new re-labeled datasets to new objects and rounding some of the variables
raw_state_data = data_list[[1]]
raw_state_data$st_race_ratio_employed = round(raw_state_data$st_race_ratio_employed, 3)
raw_state_data$st_perc_white_employed = round(raw_state_data$st_perc_white_employed, 3)
raw_state_data$st_perc_nonwhite_employed = round(raw_state_data$st_perc_nonwhite_employed, 3)

raw_county_data = data_list[[2]]
raw_county_data$cnty_race_ratio_employed = round(raw_county_data$cnty_race_ratio_employed, 3)
raw_county_data$cnty_perc_white_employed = round(raw_county_data$cnty_perc_white_employed, 3)
raw_county_data$cnty_perc_nonwhite_employed = round(raw_county_data$cnty_perc_nonwhite_employed, 3)


# create version of data the is suitable for manipulations necessary for the app  
# join state and county
# need to get rid of county's state_name column
shiny_data = 
  full_join(raw_state_data, raw_county_data[names(raw_county_data) != "state_name"], 
            by = c("YEAR", "STATEFIP"))
names(shiny_data)

# remove cases that have NAs for state_name
shiny_data = shiny_data[!is.na(shiny_data$state_name),]

# create state labels
shiny_data$STATEFIP = ifelse(nchar(shiny_data$STATEFIP) == 1, 
                             paste0("0",shiny_data$STATEFIP), shiny_data$STATEFIP)
shiny_data$state_label = paste0(shiny_data$state_name, " (", "FIPS ", shiny_data$STATEFIP,")")

# create county labels
shiny_data$county_label = paste0(shiny_data$COUNTY_NAME, " (", shiny_data$COUNTY,")")

shiny_data$COUNTY = paste0(usdata::state2abbr(shiny_data$state_name), ": ", shiny_data$COUNTY)

# get longitude and latitude for counties (so that county heatmaps can be plotted in later step)
mapping_counties = ggplot2::map_data("county")
# subregion uses the same naming scheme that was used for the COUNTY_NAME in raw data
mapping_counties$COUNTY_NAME = mapping_counties$subregion

# for inter-state trends tab
state_fips = fips_codes

#info about variables 
variable_info = list("sex comparison variables" = 
                       list("employed" = 
                              list(description = "The ratio of the percentage of males employed to the percentage of females employed.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EMPSTAT, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_employed",
                                   available_levels = "State, County"),
                            "in labor force" =
                              list(description = "The ratio of the percentage of males in labor force to the percentage of females in labor force.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "LABFORCE, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_lf",
                                   available_levels = "State, County"),
                            "above poverty line" =
                              list(description = "The ratio of the percentage of males above the poverty line to the percentage of females in above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OFFPOV, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_apl",
                                   available_levels = "State, County"),
                            "attained high school diploma" = 
                              list(description = "The ratio of the percentage of males who attained a high school diploma to the percentage of females who attained a high school diploma.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_hsd",
                                   available_levels = "State, County"),
                            "attained bachelors" = 
                              list(description = "The ratio of the percentage of males who attained a bachelor’s degree to the percentage of females who attained a bachelor’s degree.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_bachelors",
                                   available_levels = "State, County"),
                            "literate" = 
                              list(description = "The ratio of the percentage of males who are literate to the percentage of females who are literate.",
                                   source = "IPUMS USA 1% Samples",
                                   values_from_source = "LIT, SEX, STATEFIP, COUNTY, YEAR, PERWT",
                                   variable_name = "sex_ratio_lit",
                                   available_levels = "State, County"),
                            "in white collar work" = 
                              list(description = "The ratio of the percentage of males who are in white-collar professions to the percentage of females who are in white-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occw",
                                   available_levels = "State, County"),
                            "in pink collar work" = 
                              list(description = "The ratio of the percentage of males who are in pink-collar professions to the percentage of females who are in pink-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occp",
                                   available_levels = "State, County"),
                            "in blue collar work" = 
                              list(description = "The ratio of the percentage of males who are in blue-collar professions to the percentage of females who are in blue-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occb",
                                   available_levels = "State, County")),
                     "race comparison variables" = 
                       list("employed" = 
                              list(description = "The ratio of the percentage of whites who are employed to the percentage of non-whites who are employed.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EMPSTAT, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_employed",
                                   available_levels = "State, County"),
                            "in labor force" =
                              list(description = "The ratio of the percentage of whites in the labor force to the percentage of non-whites in the labor force.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "LABFORCE, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_lf",
                                   available_levels = "State, County"),
                            "above poverty line" =
                              list(description = "The ratio of the percentage of whites above the poverty line to the percentage of non-whites above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OFFPOV, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_apl",
                                   available_levels = "State, County"),
                            "children above poverty line" =
                              list(description = "The ratio of the percentage of male chidlren above the poverty line to the percentage of female children in above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "SPMNCHILD, SPMPOV, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_childapl",
                                   available_levels = "State, County"),
                            "attained high school diploma" = 
                              list(description = "The ratio of the percentage of whites with at least high school diploma to the percentage of non-whites with at least high school diploma.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_hsd",
                                   available_levels = "State, County"),
                            "literate" = 
                              list(description = "The ratio of the percentage of whites who are literate to the percentage of non-whites who are literate.",
                                   source = "IPUMS USA 1% Samples",
                                   values_from_source = "LIT, RACE, STATEICP, COUNTYICP, YEAR, PERWT",
                                   variable_name = "race_ratio_lit",
                                   available_levels = "State, County"),
                            "household income" = 
                              list(description = "The ratio of the median household income of the whites to the median household income of the non-whites.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "HHINCOME, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_hi",
                                   available_levels = "State, County"),
                            "own homes"= 
                              list(description = "The ratio of the percentage of whites who own their homes to the percentage of non-whites who own their homes.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OWNERSHP, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_home",
                                   available_levels = "State, County"),
                            "voted" = 
                              list(description = "The ratio of the percent of whites who voted in a given year's election to the ratio of
                                   non-whites who did not vote.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "VOTED, RACE, STATEFIP, YEAR, VOSUPPWT",
                                   variable_name = "race_ratio_vote",
                                   available_levels = "State"),
                            "in white collar work" = 
                              list(description = "The ratio of the percentage of whites who are in white-collar professions to the percentage of non-whites who are in white-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occw",
                                   available_levels = "State, County"),
                            "in pink collar work" = 
                              list(description = "The ratio of the percentage of whites who are in pink-collar professions to the percentage of non-whites who are in pink-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occp",
                                   available_levels = "State, County"),
                            "in blue collar work" = 
                              list(description = "The ratio of the percentage of whites who are in blue-collar professions to the percentage of non-whites who are in blue-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occb",
                                   available_levels = "State, County"),
                            "life expectancy at birth" = 
                              list(description = "Life expectancy at birth, calculated using period life table [a(0)=.3, a(1)=.4, a(x)=.5] and period-specific mortality rates [M(x) converted to q(x) using Chiang’s formula].",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_e0",
                                   available_levels = "State"),
                            "life expectancy at age 1" = 
                              list(description = "Life expectancy at age 1, calculated using period life table [a(0)=.3, a(1)=.4, a(x)=.5] and period-specific mortality rates [M(x) converted to q(x) using Chiang’s formula].",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_e1",
                                   available_levels = "State"),
                            "life expectancy at birth (males)" = 
                              list(description = "Life expectancy of males at birth",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_males_e0",
                                   available_levels = "State"),
                            "life expectancy at birth (females)" = 
                              list(description = "Life expectancy of females at birth",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_females_e0",
                                   available_levels = "State"),
                            "life expectancy at age 1 (males)" = 
                              list(description = "Life expectancy of males at age 1",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_males_e1",
                                   available_levels = "State"),
                            "life expectancy at age 1 (females)" = 
                              list(description = "Life expectancy of females at age 1",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_females_e1",
                                   available_levels = "State")))


# create placeholder county variables for variables that don't exist at county level
state_variables = names(shiny_data)[substr(names(shiny_data),1,3)=="st_"]
missing_county_variables = gsub("st_", "cnty_",state_variables[!(gsub("st_","cnty_", state_variables) %in% names(shiny_data))])

for(missing_county_variable in missing_county_variables){
  shiny_data[missing_county_variable] = NA
}

# create a key to help for plotting with informative labels
variable_key_df = data.frame(variable = unlist(lapply(variable_info[["sex comparison variables"]], function(x) x$variable_name[[1]])))
variable_key_df$variable_names = row.names(variable_key_df)

variable_key_df_race = data.frame(variable = unlist(lapply(variable_info[["race comparison variables"]], function(x) x$variable_name[[1]])))
variable_key_df_race$variable_names = row.names(variable_key_df_race)

variable_key_df = rbind(variable_key_df, variable_key_df_race)

# create vector of variables that are available for county and sex 
sex_county_variables = c()
for(i in 1:length(variable_info["sex comparison variables"][["sex comparison variables"]])){
  
  variable = variable_info["sex comparison variables"][["sex comparison variables"]][[i]]
  
  if(grepl("County",
           variable[["available_levels"]])){
    
    sex_county_variables = c(sex_county_variables, 
                             names(variable_info["sex comparison variables"][["sex comparison variables"]])[i])
  }
}

# create vector of variables that are available for county and race 
race_county_variables = c()
for(i in 1:length(variable_info["race comparison variables"][["race comparison variables"]])){
  
  variable = variable_info["race comparison variables"][["race comparison variables"]][[i]]
  
  if(grepl("County",
           variable[["available_levels"]])){
    
    race_county_variables = c(race_county_variables, 
                             names(variable_info["race comparison variables"][["race comparison variables"]])[i])
  }
}


# user interface ----
ui = fluidPage(
  theme = shinytheme("lumen"),
  useShinyjs(),
  tags$head(
    
    # styling the psu logo that pops up in web tab
    tags$link(rel = "icon", type = "image/jpg", sizes = "32x36", href = "4_psu_bust.jpg"),
    
    # styling pickers
    tags$style(
      ".my-picker button.btn.dropdown-toggle.btn-default {
        text-transform: lowercase;
        font-family: Arial; 
        font-weight: normal; 
        font-size: 13px; 
        border: none;
      }"),
    tags$style(
      "button.actions-btn.bs-deselect-all.btn.btn-default {
        text-transform: capitalize;
        font-family: Arial; 
        font-weight: normal; 
        font-size: 13px; 
        border: none;
      }"),
    tags$style(
      "button.actions-btn.bs-select-all.btn.btn-default {
        text-transform: capitalize;
        font-family: Arial; 
        font-weight: normal; 
        font-size: 13px; 
        border: none;
      }")
    ),
  
  # styling the colors at the top of the webpage
  fluidRow(
    column(
      style = "
      background: 
      linear-gradient(to left, #052485, white),
      linear-gradient(to top, #052485, white),
      linear-gradient(to bottom, #052485, white)
      ; background-blend-mode: screen;",
      width = 12,
      titlePanel(h1("The Contextual Factors Project"), 
                 windowTitle = "The Contextual Factors Project")
    )
  ),

  tabsetPanel(
    ## intro page
    tabPanel(
      h4("About"),
      tags$div(
        style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: center; align-items: center; height: 70vh;",
        HTML(paste0('<div style="text-align: center; font-size: 17px; 
        font-family: Arial; max-width: 90%; padding: 20px; word-wrap: break-word;
        ">
          Welcome to the data dashboard for the Contextual Factors Project.<br> 
          This project is focused on communicating sex differences (Male vs. Female) and racial differences (White vs. Non-white) along a variety of outcomes and how these differences have changed over time.<br><br>
          Please use the tabs at the top of the page to explore the data.
          To get additional detail about the dataset, you may find it helpful to download and reference
          <a href="contextual_factors_codebook.pdf" download="contextual_factors_codebook.pdf">this codebook</a>.<br><br>
          The creation of this website was supported by the National Institutes on Aging (R01AG078518) and 
          the Center for Social Data Analytics at Penn State. The content and presentation of the data
          is solely the responsibility of the authors and does not necessarily represent the 
          official views of the National Institutes of Health or Penn State.<br><br>
          <i>This website is currently optimized for usage on laptop/desktop computers. If you notice
          any issues while using the dashboard or working with the data, please send an email to lzl65@psu.edu.</i></div>')
             ))

    ),
    ## (1) Data info-----
    tabPanel(h4("Data info & availability"),
             fluidRow(
               column(2, radioButtons("comparison1", label = h4("Comparison"), 
                                      c("Sex (male/female)", "Race (white/non-white)"), selected = "Race (white/non-white)")),
               column(3, tags$div(pickerInput("variable1", label = h4("Variables available for comparison"), 
                                     names(variable_info[["race comparison variables"]]), 
                                     multiple = FALSE),
                                     class = "my-picker")),
               column(1, radioButtons("level1", label = h4("Level"), 
                                      c("State", "County"), selected = "State")),
               column(6, uiOutput("info_text1")))
             ,
             fluidRow(
               style = "height: 88vh; min-height:700px",
               column(width = 12, style = "height: 88%", 
                      plotOutput("availability_plot", height = "88%")
                      ,
                      column(width = 12,
                             p(),
                             HTML('<div style="text-align: center; font-size: 17px;font-family: Arial;">Colored boxes denote data is available. However, county level data may be sparse despite being indicated as available. </div>'),
                             tags$div(style = "margin-top: 40px;"))
               ))
    )
    ,
    ## (2) Custom trends and statistics----
    tabPanel(h4("Custom trends and statistics"),
             
             # sidebar
             sidebarPanel(
               width = 3,
               tags$div(pickerInput("sex_variable2", label = h4("Sex (male/female) comparison variables"), 
                              names(variable_info[["sex comparison variables"]]),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE)),
                        id = "sex_variable2", class = "my-picker"),
               bsTooltip("sex_variable2", "Select variable to examine the ratio of males to females. Ratio that are -1 signify that the percentage of females (the denominator of the ratio) is 0", "top"),
               tags$div(pickerInput("race_variable2", label = h4("Race (white/non-white) comparison variables"), names(variable_info[["race comparison variables"]]),
                              selected = "attained high school diploma", multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
                        id = "race_variable2", class = "my-picker"),
               tags$div(pickerInput("state2", label = h4("States"), sort(unique(shiny_data$state_label)),
                              selected = "Pennsylvania (FIPS 42)", multiple = TRUE, 
                           options = pickerOptions(actionsBox = TRUE)), class = "my-picker"),
               tags$div(pickerInput("county2", label = h4("Counties", 
                                                          h5(HTML("Click <a href='https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt'>here</a> more info about location codes"))),
                              sort(unique(shiny_data$COUNTY[!grepl("NA", shiny_data$COUNTY)])),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE, liveSearch=T)), class = "my-picker"),
               materialSwitch(inputId = "reference2", label = h5("Switch frame of reference for figures"), value=FALSE),
               sliderInput("year2", h4("Years"), min = 1850, max = max(shiny_data$YEAR), 
                          value = c(1850, max(shiny_data$YEAR)), sep="", ticks = FALSE, step=1),
               bsTooltip("race_variable2", "Select variable to examine the ratio of whites to non-whites. Ratio that are -1 signify that the percentage of non-whites (the denominator of the ratio) is 0. While most variables are measured in units of percentage, life expectancy variables are measured in units of years and median income in $USD.", "top")
               ),
             
             # main plot display
             fluidRow(style = "height: 100vh; min-height:630px;",
                      column(width = 8, style = "height: 92%", 
                             plotlyOutput("main_plot", height = "92%")))
             
                      
    )
    ,
    ## (3) Heatmaps-----
    tabPanel(
      h4("Heatmaps"),
      tabsetPanel(
        tabPanel("Sex ratios - State level", 
                 fluidRow(
                   column(4, tags$div(
                     pickerInput(
                       "heat_sex_state_v",
                       label = h4("Variables available for comparison"),
                       choices = names(variable_info[["sex comparison variables"]])
                     )
                     , class = "my-picker"
                   )),
                   column(
                     4,
                     sliderInput(
                       "heat_sex_state_year",
                       h4("Year"),
                       min = 1850,
                       max = max(shiny_data$YEAR),
                       value = max(shiny_data$YEAR),
                       step = 12,
                       animate = TRUE,
                       sep = "",
                       ticks = FALSE
                     ),
                     bsTooltip(
                       "heat_sex_state_year",
                       "Select year to view heatmaps (displayed in 12 year increments).",
                       "bottom"
                     )
                   ),
                   column(
                     4,
                     HTML(
                       "Ratios with extreme values were winsorized using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a> to facilitate interpretation of general trends.
                            The true values of cases that were fixed to upper/lower limits can be explored in the table at the bottom of the page. Ratio that are -1 signify that the the denominator of the ratio (either the percentage of females or non-whites) is 0."
                     )
                   )
                 ), 
                 # end of first row
                 
                 # output heatmap
                 fluidRow(column(
                   width = 12, plotOutput("heat_sex_state_plot", height = "70vh", width = "100%")
                 )),
                 fluidRow(column(width = 12, uiOutput("heat_sex_state_plot_outliers")))
               ),
        tabPanel("Race ratios - State level", 
                 fluidRow(
                   column(4, tags$div(
                     pickerInput(
                       "heat_race_state_v",
                       label = h4("Variables available for comparison"),
                       choices = names(variable_info[["race comparison variables"]])
                     )
                     , class = "my-picker"
                   )),
                   
                   column(
                     4,
                     sliderInput(
                       "heat_race_state_year",
                       h4("Year"),
                       min = 1850,
                       max = max(shiny_data$YEAR),
                       value = max(shiny_data$YEAR),
                       step = 12,
                       animate = TRUE,
                       sep = "",
                       ticks = FALSE
                     ),
                     bsTooltip(
                       "heat_race_state_year",
                       "Select year to view heatmaps (displayed in 12 year increments).",
                       "bottom"
                     )
                   ),
                   column(
                     4,
                     HTML(
                       "Ratios with extreme values were winsorized using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a> to facilitate interpretation of general trends.
                            The true values of cases that were fixed to upper/lower limits can be explored in the table at the bottom of the page. Ratio that are -1 signify that the the denominator of the ratio (either the percentage of females or non-whites) is 0."
                     )
                   )
                 ), 
                 # end of first row
                 
                 # output heatmap
                 fluidRow(column(
                   width = 12,
                   plotOutput("heat_race_state_plot", height = "70vh", width = "100%"))), 
                 fluidRow(column(width = 12, uiOutput("heat_race_state_plot_outliers")))
        ),
      
        tabPanel("Sex ratios - County level",
                 fluidRow(
                   column(4, tags$div(
                     pickerInput(
                       "heat_sex_county_v",
                       label = h4("Variables available for comparison"),
                       choices = sex_county_variables
                     )
                     , class = "my-picker"
                   )),
                   column(3, tags$div(
                     pickerInput(
                       inputId = "heat_sex_county_state",
                       label = h4("State"),
                       choices = sort(unique(shiny_data$state_label)),
                       selected = "Pennsylvania (FIPS 42)"
                     ),
                     class = "my-picker"
                   )),
                   column(
                     3,
                     sliderInput(
                       "heat_sex_county_year",
                       h4("Year"),
                       min = 1850,
                       max = max(shiny_data$YEAR),
                       value = max(shiny_data$YEAR),
                       step = 12,
                       animate = TRUE,
                       sep = "",
                       ticks = FALSE
                     ),
                     bsTooltip(
                       "heat_sex_county_year",
                       "Select year to view heatmaps (displayed in 12 year increments).",
                       "bottom"
                     )
                   ),
                   column(
                     2,
                     HTML(
                       "Ratios with extreme values were winsorized using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a> to facilitate interpretation of general trends.
                            The true values of cases that were fixed to upper/lower limits can be explored in the table at the bottom of the page. Ratio that are -1 signify that the the denominator of the ratio (either the percentage of females or non-whites) is 0."
                     )
                   )
                 ), 
                 # end of first row

                 # output heatmap
                 fluidRow(column(
                   width = 12,
                   plotOutput("heat_sex_county_plot", height = "70vh", width = "100%"))), 
                 fluidRow(column(
                   width = 12,
                   uiOutput("heat_sex_county_plot_outliers")))
        ), 
        tabPanel("Race ratios - County level",
                 fluidRow(
                   column(4, tags$div(
                     pickerInput(
                       "heat_race_county_v",
                       label = h4("Variables available for comparison"),
                       choices = race_county_variables
                     )
                     , class = "my-picker"
                   )),
                   column(3, tags$div(
                     pickerInput(
                       inputId = "heat_race_county_state",
                       label = h4("State"),
                       choices = sort(unique(shiny_data$state_label)),
                       selected = "Pennsylvania (FIPS 42)"
                     ),
                     class = "my-picker"
                   )),
                   column(
                     3,
                     sliderInput(
                       "heat_race_county_year",
                       h4("Year"),
                       min = 1850,
                       max = max(shiny_data$YEAR),
                       value = max(shiny_data$YEAR),
                       step = 12,
                       animate = TRUE,
                       sep = "",
                       ticks = FALSE
                     ),
                     bsTooltip(
                       "heat_race_county_year",
                       "Select year to view heatmaps (displayed in 12 year increments).",
                       "bottom"
                     )
                   ),
                   column(
                     2,
                     HTML(
                       "Ratios with extreme values were winsorized using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a> to facilitate interpretation of general trends.
                            The true values of cases that were fixed to upper/lower limits can be explored in the table at the bottom of the page. Ratio that are -1 signify that the the denominator of the ratio (either the percentage of females or non-whites) is 0."
                     )
                   )
                 ), 
                 # end of first row
                 
                 # output heatmap
                 fluidRow(column(
                   width = 12,
                   plotOutput("heat_race_county_plot", height = "70vh", width = "100%"))), 
                 fluidRow(column(
                   width = 12,
                   uiOutput("heat_race_county_plot_outliers")))) 
      )), 
    
    ## (4) Inter-state trends ------
    tabPanel(h4("Inter-state trends"),
             fluidRow(
               column(3, 
                      tags$div(pickerInput("sex_variables_ist", 
                                           label = h4("Sex (male/female) comparison variables"),
                                           names(variable_info[["sex comparison variables"]]),
                                           selected = NULL, multiple = TRUE, 
                                           options = pickerOptions(actionsBox = TRUE)),
                               class = "my-picker")),
               bsTooltip("sex_variables_ist", "Select variable to examine the ratio of males to females.", 
                         "top"),
               column(3, 
                      tags$div(pickerInput("race_variables_ist", 
                                           label = h4("Race (white/non-white) comparison variables"),
                                           names(variable_info[["race comparison variables"]]),
                                           selected = "attained high school diploma", multiple = TRUE,
                                           options = pickerOptions(actionsBox = TRUE)),
                               class = "my-picker")),
               bsTooltip("race_variables_ist", "Select variable to examine the ratio of whites to non-whites", 
                         "top"),
               column(5, p(), p(),
                 HTML("Ratios with extreme values were winsorized using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a> to facilitate interpretation of general trends.
                            The true values of cases that were fixed to upper/lower limits can be explored in the table at the bottom of the page. Ratio that are -1 signify that the the denominator of the ratio (either the percentage of females or non-whites) is 0."),
               )), # end of first row

             fluidRow(
               column(width = 12, uiOutput("interstate_trend_image"))
               ,column(12,  uiOutput("ist_data_tables"))
               )
             )
    ,
    ## (5) download -----
    tabPanel(h4("Download data"),
             sidebarPanel(
               downloadButton("download_entire_data", "Download full dataset"),br(),br(),
               p(style = "font-size: 18px; color: rgba(0, 0, 0, 0.77); font-style: italic",
                 "Use filters below to create a custom subset of the data"),
               tags$div(pickerInput("sex_variable5", label = h4("Sex (male/female) comparison variables"), names(variable_info[["sex comparison variables"]]),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE)),class = "my-picker"),
               bsTooltip("sex_variable5", "Select variable to examine the ratio of males to females.", "top"),
               tags$div(pickerInput("race_variable5", label = h4("Race (white/non-white) comparison variables"), names(variable_info[["race comparison variables"]]),
                              selected = "attained high school diploma", multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),class = "my-picker"),
               bsTooltip("race_variable5", "Select variable to examine the ratio of whites to non-whites", "top"),
               tags$div(pickerInput("States_table", label = h4("States"), sort(unique(shiny_data$state_name)),
                           selected = "Pennsylvania", multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),class = "my-picker"),
               radioButtons("County_table", label = h4("Level"), choices = c("State", "County"), selected = "State"),
               sliderInput("Year_table", h4("Years"), min = 1850, max = max(shiny_data$YEAR), 
                           value = c(1850, max(shiny_data$YEAR)), sep="", ticks = FALSE,
                           step=1),
               br(),
               downloadButton("download_custom_data", "Download custom dataset"),
             )
             ,
             mainPanel(DT::dataTableOutput("data_table"))
    )
  )
)

# outputs -----
server <- function(input, output) {
  # (1) data info tab----
  ## reactives -----
  
  # change the dropdown options to reflect variables available for a specific comparison
  observe({
    if(input$comparison1 == "Sex (male/female)"){
      comparison1 = names(variable_info[["sex comparison variables"]])
    } else {
      comparison1 = names(variable_info[["race comparison variables"]])
    } 
    
    updatePickerInput("variable1",
                      choices = comparison1,
                      select = "attained high school diploma",
                      session = getDefaultReactiveDomain())
  })
  
  # create a reactive object that saves the comparison of interest that can be used for
  # 1) the plot and 2) the text
  
  comparison1 = reactive({
    if(input$comparison1 == "Sex (male/female)"){
      comparison1 = "sex comparison variables"
    } else {
      comparison1 = "race comparison variables"
    } 
    comparison1
  })
  
  ## plot-----
  # create logic for output of the plot which shows data availability by year 
  output$availability_plot <- renderPlot({
  
    # create key to differentiate state and count-level plots with
    level_of_analysis_key = 
      list("State" = "st_", "County" = "cnty_", 
           "State_color" = "blue3", 
           "County_color" = "darkorange1",
           "State_data" = raw_state_data,
           "County_data" = raw_county_data)
    
    # manipulate data based on user inputs 
    variable_based_on_comparison = 
      paste0(level_of_analysis_key[[input$level1]], 
             c(variable_info[[comparison1()]][[input$variable1]][["variable_name"]]))
    
    # variable_based_on_comparison = "st_sex_ratio_employed"
    if(variable_based_on_comparison %in% 
       names(level_of_analysis_key[[paste0(input$level1, "_data")]]) == FALSE){
      
      ggplot2::ggplot(data.frame(text = str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs.", width = 60)), 
                      aes(x = .5, y = .5, label = text)) + 
        geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + 
        theme_void()
      
    } else {
      
      for_availability = level_of_analysis_key[[paste0(input$level1, "_data")]][c("YEAR", "state_name", variable_based_on_comparison)]
      
      for_availability = for_availability[!is.na(for_availability[variable_based_on_comparison]),]
      for_availability = for_availability[!is.na(for_availability$state_name),]
      
      for_availability[variable_based_on_comparison] = 
        ifelse(!is.na(for_availability[variable_based_on_comparison]),
               1,NA)
      
      for_availability = dplyr::distinct(for_availability)
      
      for_availability$state_name = factor(for_availability$state_name, 
                                           levels = rev(sort(unique(for_availability$state_name))))
      
      ggpubr::ggballoonplot(for_availability, x = "YEAR", y = "state_name", size = 4.3, 
                            fill = level_of_analysis_key[[paste0(input$level1, "_color")]], 
                            color = level_of_analysis_key[[paste0(input$level1, "_color")]],shape=22) +
        scale_x_continuous(
          breaks = c(min(for_availability$YEAR):max(for_availability$YEAR)),
          labels = c(min(for_availability$YEAR):max(for_availability$YEAR)),
          position = "top",
          guide = guide_axis(check.overlap=TRUE)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1, size = 12),
              axis.text.y = element_text(size = 12),
              panel.grid.minor = element_blank()) 
        
      
    }
  })
  ## variable info -----
  # printing information about each variable
  output$info_text1 = renderUI({
    
    variable_description = c(paste0(variable_info[[comparison1()]][[input$variable1]][["description"]]))
    variable_source_info = paste0("Data source: ", 
                                  variable_info[[comparison1()]][[input$variable1]][["source"]])
    source_variables = paste0("Variables used from source: ",
                              variable_info[[comparison1()]][[input$variable1]][["values_from_source"]])
    available_levels = paste0("Available levels: ",
                              variable_info[[comparison1()]][[input$variable1]][["available_levels"]])
    output_text = c(variable_description, variable_source_info, source_variables, available_levels, " ")
    HTML(paste("<div style='font-size: 14px;'",">",output_text, collapse = "<p><p>"))
    
  })
  
  # (2) Trends and statistics tab -----   
  ## reactives ----
  
  # this is updating the year sliders to reflect what is available in the user specified dataset 
  observe({
    
    sex_variables = c()
    for(selection in input$sex_variable2){
      sex_variables = c(sex_variables,
                        variable_info[["sex comparison variables"]][[selection]][["variable_name"]])
    }
    
    race_variables = c()
    for(selection in input$race_variable2){
      race_variables = c(race_variables,
                         variable_info[["race comparison variables"]][[selection]][["variable_name"]])
    }
    
    if(!is.null(c(sex_variables, race_variables))){
      finding_min_max_years = shiny_data[c("YEAR", paste0("st_", c(sex_variables,race_variables)))]
      finding_min_max_years= finding_min_max_years[!(rowSums(is.na(finding_min_max_years[-1]))==ncol(finding_min_max_years)-1),]
      
      min_year = min(finding_min_max_years$YEAR,na.rm=TRUE)
      max_year = max(finding_min_max_years$YEAR,na.rm=TRUE)
      updateSliderInput("year2",
                        min = min_year,
                        max = max_year,
                        value = c(min_year,
                                  max_year),
                        session = getDefaultReactiveDomain())
    } else {
      updateSliderInput("year2",
                        min = 1850,
                        max = max(shiny_data$YEAR),
                        value = c(1850,
                                  max(shiny_data$YEAR)),
                        session = getDefaultReactiveDomain())
    }
  })
  
  ## main trend plot -----
  
  output$main_plot <- renderPlotly({
    
    sex_variables = c()
    for(selection in input$sex_variable2){
      sex_variables = c(sex_variables,
                        variable_info[["sex comparison variables"]][[selection]][["variable_name"]])
    }
    
    race_variables = c()
    for(selection in input$race_variable2){
      race_variables = c(race_variables,
                         variable_info[["race comparison variables"]][[selection]][["variable_name"]])
    }
    
    if(is.null(c(sex_variables, race_variables)) | 
       is.null(input$state2) & is.null(input$county2)){
      main_plot = ggplot2::ggplot(data.frame(text = 
                                   str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs. 
                                            Keep in mind that you need at least one variable 
                                            *and* one location.", width = 60)),
                      aes(x = .5, y = .5, label = text)) +
        geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + theme_void()
      
    } else {
      # this is for grabbing the right variables based on level of interest
      if(!is.null(input$state2)){
        reactive_year_frame_state = 
          shiny_data[c("YEAR", "state_label", paste0("st_", c(sex_variables,race_variables)))]
        
        ratio_indices = c()
        for(v_name in colnames(reactive_year_frame_state)[-c(1,2)]){
          ratio_indices = c(ratio_indices, which(colnames(shiny_data) %in% v_name))
        }
        
        for(ratio in ratio_indices){
          
          new_column_name = gsub("_ratio","",paste0("perc_",names(shiny_data)[ratio]))
          
          reactive_year_frame_state[new_column_name] = 
            paste0(shiny_data[[ratio-2]], "/", shiny_data[[ratio-1]])
          
        }
        
        reactive_year_frame_state = reactive_year_frame_state %>%
          dplyr::rename(., Location = state_label) %>%
          rename_with(~stringr::str_replace(., "^st_|", ""), matches("^st_")) %>%
          rename_with(~stringr::str_replace(., "^perc_st", "perc"), matches("^perc_st")) %>%
          dplyr::filter(Location %in% input$state2 & YEAR>=input$year2[1] & YEAR<=input$year2[2])
        
        
      } else(reactive_year_frame_state = NULL)
      if(!is.null(input$county2)){
        reactive_year_frame_county = shiny_data[c("YEAR", "COUNTY",paste0("cnty_", c(sex_variables,race_variables)))]
        
        ratio_indices = c()
        for(v_name in colnames(reactive_year_frame_county)[-c(1,2)]){
          ratio_indices = c(ratio_indices, which(colnames(shiny_data) %in% v_name))
        }
        
        for(ratio in ratio_indices){
          
          new_column_name = gsub("_ratio","",paste0("perc_",names(shiny_data)[ratio]))
          reactive_year_frame_county[new_column_name] = 
            paste0(shiny_data[[ratio-2]], "/", shiny_data[[ratio-1]])
          
        }

        reactive_year_frame_county = reactive_year_frame_county %>%
          dplyr::rename(., Location = COUNTY) %>%
          rename_with(~stringr::str_replace(., "^cnty_|", ""), matches("^cnty_")) %>%
          rename_with(~stringr::str_replace(., "^perc_cnty", "perc"), matches("^perc_cnty")) %>%
          dplyr::filter(Location %in% input$county2 & YEAR>=input$year2[1] & YEAR<=input$year2[2])
      } else(reactive_year_frame_county = NULL)
      
      # get rid of state and county indicators in variables
      reactive_year_frame = rbind(reactive_year_frame_state, reactive_year_frame_county)

      # get rid of duplicate rows
      reactive_year_frame = reactive_year_frame[!duplicated(reactive_year_frame),]
      
      # reshape the data to be ready for plotting
      reactive_year_ratio_frame_long = reactive_year_frame[!grepl("perc_", colnames(reactive_year_frame))] %>%
        tidyr::pivot_longer(cols = names(reactive_year_frame)[grepl("ratio", names(reactive_year_frame))],
                            names_to = "variable",
                            values_to = "ratio_value") 
      
      reactive_year_perc_frame_long = reactive_year_frame[!grepl("ratio_", colnames(reactive_year_frame))] %>%
        tidyr::pivot_longer(cols = names(reactive_year_frame)[grepl("perc_", names(reactive_year_frame))],
                            names_to = "variable",
                            values_to = "perc_value") 
      
      
      
      reactive_year_ratio_frame_long$perc_columns = reactive_year_perc_frame_long$perc_value
    
      reactive_year_frame_long = reactive_year_ratio_frame_long
 
      # get rid of rows with NA values
      reactive_year_frame_long = reactive_year_frame_long[!is.na(reactive_year_frame_long$ratio_value),]
      
      # input blank rows
      for(location in c(input$state2, input$county2)){
        for(selected_variable in c(sex_variables, race_variables)){
          blank_row = data.frame(YEAR = NA, 
                                 Location = location,
                                 variable = selected_variable,
                                 ratio_value = NA,
                                 perc_columns = NA)
          reactive_year_frame_long = rbind(reactive_year_frame_long, blank_row)
        }
      }
      
      reactive_year_frame_long$comparison = 
        ifelse(grepl("^sex_", reactive_year_frame_long$variable), 
               "sex ratio-",
               ifelse(grepl("^race_", reactive_year_frame_long$variable), 
                      "race ratio-", "unclear comparison"))
      
      reactive_year_frame_long = reactive_year_frame_long %>% 
        dplyr::left_join(variable_key_df, by = "variable")
     
      reactive_year_frame_long$variable = paste0(reactive_year_frame_long$comparison,
                                                 reactive_year_frame_long$variable_names)
      
      if(sum(!is.na(reactive_year_frame_long$ratio_value)) == 0){
        main_plot = ggplot2::ggplot(data.frame(text = 
                                     str_wrap("Sorry, we do not have data for this combination of criteria. 
                                            You will need to change one or more inputs. 
                                            Keep in mind that you need at least one variable 
                                            ('Sex' and/or 'Race comparison') and one location ('States' and/or 'Counties').", width = 60)),
                        aes(x = .5, y = .5, label = text)) +
          geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + 
          theme_void()
        
      } else {
        
        if(input$reference2 == TRUE){
          grouping = "Location"
          facet = "variable"} else {
            grouping = "variable"
            facet = "Location"
          }
        
        # last adjustments to dataframe to make labels look better
        reactive_year_frame_long = reactive_year_frame_long %>% 
          dplyr::rename("Year" = "YEAR",
                        "Ratio" = "ratio_value",
                        "Ratio Makeup" = "perc_columns")
        
        
         if(sum(reactive_year_frame_long$Ratio < 0, na.rm=T) >= 1){

           main_plot = ggplot2::ggplot(reactive_year_frame_long,
                                       aes(x = Year, y = Ratio, label = `Ratio Makeup`)) +
             geom_line(aes(fill = get(grouping),color = get(grouping)), size = 1) +
             geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
             geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
             labs(x = "Year", y = "Ratio") +
             scale_color_viridis_d(option = "C", name = "") +
             theme_minimal() +
             facet_wrap(~ get(facet), ncol = 2) +
             scale_x_continuous(
               breaks = ~ unique(round(axisTicks(., log = FALSE), 0)),
               guide = guide_axis(check.overlap = TRUE)) +
             theme(
               panel.spacing = unit(2, "lines"),
               strip.background = element_rect(fill = "white", color = NA))

         } else {
          
          main_plot = ggplot2::ggplot(reactive_year_frame_long, 
                                      aes(x = Year, y = Ratio, label = `Ratio Makeup`)) +
            geom_line(aes(fill = get(grouping),color = get(grouping)), size = 1) +
            geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
            labs(x = "Year", y = "Ratio") +
            scale_color_viridis_d(option = "C", name = "") +
            theme_minimal() +
            facet_wrap(~ get(facet), ncol = 2) +
            scale_x_continuous(
              breaks = ~ unique(round(axisTicks(., log = FALSE), 0)),
              guide = guide_axis(check.overlap = TRUE)) +
            theme(
              panel.spacing = unit(2, "lines"),
              strip.background = element_rect(fill = "white", color = NA)) 
        }
      }
    }
    plotly::ggplotly(main_plot, tooltip = c("Year", "Ratio", "Ratio Makeup")) %>%
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = -.15,
                      scroll = TRUE,
                      yanchor = "top",
                      borderwidth = .5)) 

  })
  
  # (3) heatmaps----
  ## (3.1) sex-state -----
  ### reactives -------
  
  # update slider to be in accordance with variable min and max at state level
 
  observeEvent(input$heat_sex_state_v, {
    variable =
      paste0("st_", variable_info[["sex comparison variables"]][[input$heat_sex_state_v]][["variable_name"]])
    finding_min_max_years = shiny_data[c("YEAR", variable)]
    finding_min_max_years =
      finding_min_max_years[!(is.na(finding_min_max_years[variable])), ]
    min_year = min(finding_min_max_years$YEAR, na.rm = TRUE)
    max_year = max(finding_min_max_years$YEAR, na.rm = TRUE)
    
    updateSliderInput(
      "heat_sex_state_year",
      min = min_year,
      max = max_year - 11,
      session = getDefaultReactiveDomain()
    )
    
    
  })
  
  ### heatmaps -----

  output$heat_sex_state_plot = renderPlot({
    # grab variable by its name in the df
    variable =
      paste0("st_", c(variable_info[["sex comparison variables"]][[input$heat_sex_state_v]][["variable_name"]]))
    
    state_shiny_data =
      unique(shiny_data[!is.na(shiny_data[variable]), c("YEAR", "state_label", variable)])
    
    no_negatives_df = state_shiny_data[!(state_shiny_data[[variable]] <= 0),]
    no_negatives_df = no_negatives_df[!is.na(no_negatives_df[[variable]]),]
    no_negatives_df$logged_variable = log(no_negatives_df[[variable]])
    
    # setting outlier thresholds
    IQR = summary(no_negatives_df$logged_variable)[5] - 
      summary(no_negatives_df$logged_variable)[2]
    rng = c(summary(no_negatives_df$logged_variable)[2]-(1.5*IQR),
            summary(no_negatives_df$logged_variable)[5]+(1.5*IQR))
    print(rng)
    rng = exp(rng)
    print(rng)
    print("#####")
    
    cases_outside_of_range = 
      state_shiny_data[(state_shiny_data[[variable]] < rng[1] |
                          state_shiny_data[[variable]] > rng[2] |
                          state_shiny_data[[variable]] <= 0),]%>%
      dplyr::rename("States" = "state_label", "Year" = "YEAR") # rename for display 
    
    #rename for display
    colnames(cases_outside_of_range)[ncol(cases_outside_of_range)] =
      paste0("sex ratio - ", input$heat_sex_state_v)

    # render the outlier table
    output$heat_sex_state_plot_outliers =
      renderUI(DT::renderDataTable(datatable(
        data = cases_outside_of_range,
        options = list(
          searching = TRUE,
          lengthMenu = list(c(5, 10, 20), c(5, 10, 20)), pageLength = 6),
          rownames = FALSE
      )))
    
  
    user_requested_map_df =
      state_shiny_data[state_shiny_data$YEAR %in%
                         c(input$heat_sex_state_year:(input$heat_sex_state_year +
                                                        11)), ]
    
    # fixing certain values to upper or lower limits (windsorize)
    user_requested_map_df[[variable]] =
      ifelse(
        user_requested_map_df[[variable]] < rng[1],
        rng[1],
        ifelse(user_requested_map_df[[variable]] > rng[2], rng[2], user_requested_map_df[[variable]])
      )
    
    # rename
    user_requested_map_df = user_requested_map_df %>%
      dplyr::rename("Year" = "YEAR", "state" = "state_label")
    
    colnames(user_requested_map_df)[3] =
      paste0("sex ratio - ", input$heat_sex_state_v)
    
    user_requested_map_df$state = substr(user_requested_map_df$state,
                                         1,
                                         nchar(user_requested_map_df$state) -
                                           10)
    
    
    # complete the data frame so missing states are denoted as gray within the same plot
    user_requested_map_df_complete_list = list()
    for (year_facet in c(input$heat_sex_state_year:(input$heat_sex_state_year +
                                                    11))) {
      user_requested_map_df_year_facet =
        user_requested_map_df[user_requested_map_df$Year == year_facet, ] %>%
        tidyr::complete(state = unique(shiny_data$state_name), Year = year_facet)
      
      user_requested_map_df_complete_list = c(user_requested_map_df_complete_list,
                                              list(user_requested_map_df_year_facet))
    }
    
    user_requested_map_df_complete_df =
      do.call(rbind, user_requested_map_df_complete_list)
    
    if (sum(is.na(user_requested_map_df_complete_df[[3]])) ==
        nrow(user_requested_map_df_complete_df)) {
      
    } else {
      # generating plot
      # create legend anchors. this is odd because equivalent ratios are not symmetrical around zero
      anchor = ifelse(abs(log(rng[2])) > abs(log(rng[1])), rng[2], rng[1])
      
      if(anchor > 1){
        low_anchor = 1/anchor
        high_anchor = anchor
      } else if(anchor < 1){
        low_anchor = anchor
        high_anchor = 1/anchor
      }
      
      user_requested_map_df_complete_df[[3]] = log(user_requested_map_df_complete_df[[3]])
      usmap::plot_usmap(
        data = user_requested_map_df_complete_df,
        values = names(user_requested_map_df_complete_df)[3],
        labels = FALSE
      ) +
        scale_fill_gradient2(
          low = "darkorange1",
          mid = "white",
          high = "midnightblue",
          midpoint = 0,
          breaks = c(round(log(low_anchor), 3), 0, round(log(high_anchor), 3)),
          labels = c(round(low_anchor, 3), 1, round(high_anchor, 3)),
          limits = c(round(log(low_anchor), 3)-0.00001, round(log(high_anchor), 3)+.000001),
          name = ""
        ) +
        facet_wrap(~ Year) +
        ggtitle(names(user_requested_map_df_complete_df)[3]) +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(1.6, "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "white", color = NA)
        )
    }
    
  })
  
  ## (3.2) race-state -----
  ### reactives -------
  
  # update slider to be in accordance with variable min and max at state level
  
  observeEvent(input$heat_race_state_v, {
    variable =
      paste0("st_", variable_info[["race comparison variables"]][[input$heat_race_state_v]][["variable_name"]])
    finding_min_max_years = shiny_data[c("YEAR", variable)]
    finding_min_max_years =
      finding_min_max_years[!(is.na(finding_min_max_years[variable])), ]
    min_year = min(finding_min_max_years$YEAR, na.rm = TRUE)
    max_year = max(finding_min_max_years$YEAR, na.rm = TRUE)
    
    updateSliderInput(
      "heat_race_state_year",
      min = min_year,
      max = max_year - 11,
      session = getDefaultReactiveDomain()
    )
    
    
  })
  
  ### heatmaps -----
  
  output$heat_race_state_plot = renderPlot({
    # grab variable by its name in the df
    variable =
      paste0("st_", c(variable_info[["race comparison variables"]][[input$heat_race_state_v]][["variable_name"]]))
    
    state_shiny_data =
      unique(shiny_data[!is.na(shiny_data[variable]), c("YEAR", "state_label", variable)])
    
    no_negatives_df = state_shiny_data[!(state_shiny_data[[variable]] <= 0),]
    no_negatives_df = no_negatives_df[!is.na(no_negatives_df[[variable]]),]
    no_negatives_df$logged_variable = log(no_negatives_df[[variable]])
    
    # setting outlier thresholds
    IQR = summary(no_negatives_df$logged_variable)[5] - 
      summary(no_negatives_df$logged_variable)[2]
    rng = c(summary(no_negatives_df$logged_variable)[2]-(1.5*IQR),
            summary(no_negatives_df$logged_variable)[5]+(1.5*IQR))
    rng = exp(rng)
    
    cases_outside_of_range = 
      state_shiny_data[(state_shiny_data[[variable]] < rng[1] |
                               state_shiny_data[[variable]] > rng[2] |
                               state_shiny_data[[variable]] <= 0),]%>%
      dplyr::rename("States" = "state_label", "Year" = "YEAR") # rename for display 
    
    #rename for display
    colnames(cases_outside_of_range)[ncol(cases_outside_of_range)] =
      paste0("race ratio - ", input$heat_race_state_v)
  
    # render the outlier table
    output$heat_race_state_plot_outliers =
      renderUI(DT::renderDataTable(datatable(
        data = cases_outside_of_range,
        options = list(
          searching = TRUE,
          lengthMenu = list(c(5, 10, 20), c(5, 10, 20)), pageLength = 6),
        rownames = FALSE
      )))
    
    user_requested_map_df =
      state_shiny_data[state_shiny_data$YEAR %in%
                         c(input$heat_race_state_year:(input$heat_race_state_year +
                                                        11)), ]
    
    # fixing certain values to upper or lower limits (windsorize)
    user_requested_map_df[[variable]] =
      ifelse(
        user_requested_map_df[[variable]] < rng[1],
        rng[1],
        ifelse(user_requested_map_df[[variable]] > rng[2], rng[2], user_requested_map_df[[variable]])
      )
    
    # rename
    user_requested_map_df = user_requested_map_df %>%
      dplyr::rename("Year" = "YEAR", "state" = "state_label")
    
    colnames(user_requested_map_df)[3] =
      paste0("race ratio - ", input$heat_race_state_v)
    
    user_requested_map_df$state = substr(user_requested_map_df$state,
                                         1,
                                         nchar(user_requested_map_df$state) -
                                           10)

    # complete the data frame so missing states are denoted as gray within the same plot
    user_requested_map_df_complete_list = list()
    for (year_facet in c(input$heat_race_state_year:(input$heat_race_state_year +
                                                    11))) {
      user_requested_map_df_year_facet =
        user_requested_map_df[user_requested_map_df$Year == year_facet, ] %>%
        tidyr::complete(state = unique(shiny_data$state_name), Year = year_facet)
      
      user_requested_map_df_complete_list = c(user_requested_map_df_complete_list,
                                              list(user_requested_map_df_year_facet))
    }
    
    user_requested_map_df_complete_df =
      do.call(rbind, user_requested_map_df_complete_list)
    
    if (sum(is.na(user_requested_map_df_complete_df[[3]])) ==
        nrow(user_requested_map_df_complete_df)) {
      
    } else {
      # generating plot
      # create legend anchors. this is odd because equivalent ratios are not symmetrical around zero
      anchor = ifelse(abs(log(rng[2])) > abs(log(rng[1])), rng[2], rng[1])
      
      if(anchor > 1){
        low_anchor = 1/anchor
        high_anchor = anchor
      } else if(anchor < 1){
        low_anchor = anchor
        high_anchor = 1/anchor
      }
      
      
      user_requested_map_df_complete_df[[3]] = log(user_requested_map_df_complete_df[[3]])
      usmap::plot_usmap(
        data = user_requested_map_df_complete_df,
        values = names(user_requested_map_df_complete_df)[3],
        labels = FALSE
      ) +
        scale_fill_gradient2(
          low = "darkorange1",
          mid = "white",
          high = "midnightblue",
          midpoint = 0,
          breaks = c(round(log(low_anchor), 3), 0, round(log(high_anchor), 3)),
          labels = c(round(low_anchor, 3), 1, round(high_anchor, 3)),
          limits = c(round(log(low_anchor), 3)-0.00001, round(log(high_anchor), 3)+.000001),
          name = ""
        ) +
        facet_wrap(~ Year) +
        ggtitle(names(user_requested_map_df_complete_df)[3]) +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(1.6, "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "white", color = NA)
        )
    }
    
  })
  
  ## (3.3) sex-county -----
  ### reactives ----

  observeEvent(c(input$heat_sex_county_v, input$heat_sex_county_state), {
    
    variable =
      paste0("cnty_", variable_info[["sex comparison variables"]][[input$heat_sex_county_v]][["variable_name"]])
    finding_min_max_years = shiny_data[c("YEAR", "state_label", variable)]
    
    finding_min_max_years =
      finding_min_max_years[!(is.na(finding_min_max_years[variable])), ]
    finding_min_max_years = 
      finding_min_max_years[finding_min_max_years$state_label == input$heat_sex_county_state,]
    
    min_year = min(finding_min_max_years$YEAR, na.rm = TRUE)
    max_year = max(finding_min_max_years$YEAR, na.rm = TRUE)
    
    updateSliderInput(
      "heat_sex_county_year",
      min = min_year,
      max = max_year - 11,
      session = getDefaultReactiveDomain()
    )
  })
  
  ### heatmaps ----
  output$heat_sex_county_plot = renderPlot({
    
    variable =
      paste0("cnty_", variable_info[["sex comparison variables"]][[input$heat_sex_county_v]][["variable_name"]])
    
    input_state = substr(input$heat_sex_county_state, 1, nchar(input$heat_sex_county_state)-10)
    
    user_requested_map_df = shiny_data %>% dplyr::filter(state_name=={{input_state}} &
                                                           !is.na(get(variable))) %>%
      dplyr::select(c(!!variable, COUNTY, county_label, state_name, STATEFIP, YEAR, COUNTY_NAME))
    
    if(nrow(user_requested_map_df)==0){
      
      ggplot2::ggplot(data.frame(text = str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs.", width = 60)), 
                      aes(x = .5, y = .5, label = text)) + 
        geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + 
        theme_void()
      
      
      
    } else {
      
      no_negatives_df = user_requested_map_df[!(user_requested_map_df[[variable]] <= 0),]
      no_negatives_df = no_negatives_df[!is.na(no_negatives_df[[variable]]),]
      no_negatives_df$logged_variable = log(no_negatives_df[[variable]])
      
      IQR = summary(no_negatives_df$logged_variable)[5] - 
        summary(no_negatives_df$logged_variable)[2]
      rng = c(summary(no_negatives_df$logged_variable)[2]-(1.5*IQR),
              summary(no_negatives_df$logged_variable)[5]+(1.5*IQR))
      rng = exp(rng)
      
      cases_outside_of_range = 
        user_requested_map_df[(user_requested_map_df[[variable]] < rng[1] |
                                 user_requested_map_df[[variable]] > rng[2] |
                                 user_requested_map_df[[variable]] <= 0),]
      
      outlier_table = cases_outside_of_range %>%
        dplyr::select(c(YEAR, state_name, county_label, !!variable))
      colnames(outlier_table)[4] = paste0("sex ratio - ", input$heat_sex_county_v)
      colnames(outlier_table)[3] = "Counties"
      colnames(outlier_table)[1] = "Year"

      
      output$heat_sex_county_plot_outliers = 
        renderUI(DT::renderDataTable(
          datatable(
            data = outlier_table,
            options = list(
              searching = TRUE,
              lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
              pageLength = 6
            ),
            rownames = FALSE)))
    
    # fixing certain values to upper or lower limits (windsorizing)
    user_requested_map_df[[variable]] = 
      ifelse(user_requested_map_df[[variable]] < rng[1],
             rng[1], ifelse(user_requested_map_df[[variable]] > rng[2],
                            rng[2], user_requested_map_df[[variable]]))

    mapping_counties_requested = mapping_counties %>% dplyr::filter(region == tolower(input_state))
    
    user_requested_map_df = user_requested_map_df %>% 
      dplyr::filter(YEAR %in% c(input$heat_sex_county_year:(input$heat_sex_county_year+11)))

    # complete the data frame so missing states are denoted by gray within the same plot
    merged_for_plot_complete_list = list()
    for(year_facet in c(input$heat_sex_county_year:(input$heat_sex_county_year+11))){
      # year_facet = 2012
      
      user_requested_map_df_year_facet =
        user_requested_map_df[user_requested_map_df$YEAR == year_facet, ] %>%
        tidyr::complete(COUNTY_NAME = unique(mapping_counties_requested$COUNTY_NAME),
                        YEAR = year_facet)

      merged_for_plot_complete_list = c(merged_for_plot_complete_list,
                                        list(user_requested_map_df_year_facet))

    }

    merged_for_plot = do.call(rbind, merged_for_plot_complete_list)

    # adding in longitude and latitude data
    merged_for_plot = dplyr::full_join(merged_for_plot,
                                       mapping_counties_requested[c("long", "lat", "group", "COUNTY_NAME")],
                                       by=c("COUNTY_NAME"))
    # create legend anchors. this is odd because equivalent ratios are not symmetrical around zero
    anchor = ifelse(abs(log(rng[2])) > abs(log(rng[1])), rng[2], rng[1])
    
    if(anchor > 1){
      low_anchor = 1/anchor
      high_anchor = anchor
    } else if(anchor < 1){
      low_anchor = anchor
      high_anchor = 1/anchor
    }
    
    merged_for_plot$logged_variable = log(merged_for_plot[[variable]])
    ggplot2::ggplot(data = merged_for_plot, aes(x = long, y = lat, group = group,
                                                fill = logged_variable)) +
      geom_polygon(color = "black") +
      coord_map() +
      facet_wrap(~YEAR) +
      theme_void() +
      labs(fill= stringr::str_wrap(paste0("sex ratio - ", input$heat_sex_county_v),15)) +
      scale_fill_gradient2(
        low = "darkorange1",
        mid = "white",
        high = "midnightblue",
        midpoint = 0,
        breaks = c(round(log(low_anchor), 3), 0, round(log(high_anchor), 3)),
        labels = c(round(low_anchor, 3), 1, round(high_anchor, 3)),
        limits = c(round(log(low_anchor), 3)-0.00001, round(log(high_anchor), 3)+.000001),
        name = ""
      ) +
      ggtitle(paste0("sex ratio - ", input$heat_sex_county_v)) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "white", color = NA)
      )
    
    }
    
  })
  
  ## (3.4) race-county -----
  ### reactives ----
  
  observeEvent(c(input$heat_race_county_v, input$heat_race_county_state), {
    
    variable =
      paste0("cnty_", variable_info[["race comparison variables"]][[input$heat_race_county_v]][["variable_name"]])
    finding_min_max_years = shiny_data[c("YEAR", "state_label", variable)]
    
    finding_min_max_years =
      finding_min_max_years[!(is.na(finding_min_max_years[variable])), ]
    finding_min_max_years = 
      finding_min_max_years[finding_min_max_years$state_label == input$heat_race_county_state,]
    
    min_year = min(finding_min_max_years$YEAR, na.rm = TRUE)
    max_year = max(finding_min_max_years$YEAR, na.rm = TRUE)
    
    updateSliderInput(
      "heat_race_county_year",
      min = min_year,
      max = max_year - 11,
      session = getDefaultReactiveDomain()
    )
  })
  
  ### heatmaps ----
  output$heat_race_county_plot = renderPlot({
    
    variable =
      paste0("cnty_", variable_info[["race comparison variables"]][[input$heat_race_county_v]][["variable_name"]])
    
    input_state = substr(input$heat_race_county_state, 1, nchar(input$heat_race_county_state)-10)
    
    user_requested_map_df = shiny_data %>% dplyr::filter(state_name=={{input_state}} &
                                                           !is.na(get(variable))) %>%
      dplyr::select(c(!!variable, COUNTY, county_label, state_name, STATEFIP, YEAR, COUNTY_NAME))
    
    if(nrow(user_requested_map_df)==0){
      
      ggplot2::ggplot(data.frame(text = str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs.", width = 60)), 
                      aes(x = .5, y = .5, label = text)) + 
        geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + 
        theme_void()
      
    } else {

      no_negatives_df = user_requested_map_df[!(user_requested_map_df[[variable]] <= 0),]
      no_negatives_df = no_negatives_df[!is.na(no_negatives_df[[variable]]),]
      no_negatives_df$logged_variable = log(no_negatives_df[[variable]])
      
      IQR = summary(no_negatives_df$logged_variable)[5] - 
        summary(no_negatives_df$logged_variable)[2]
      rng = c(summary(no_negatives_df$logged_variable)[2]-(1.5*IQR),
              summary(no_negatives_df$logged_variable)[5]+(1.5*IQR))
      rng = exp(rng)
      
      cases_outside_of_range = 
        user_requested_map_df[(user_requested_map_df[[variable]] < rng[1] |
                                 user_requested_map_df[[variable]] > rng[2] |
                                 user_requested_map_df[[variable]] <= 0),]
      
      outlier_table = cases_outside_of_range %>%
        dplyr::select(c(YEAR, state_name, county_label, !!variable))
      colnames(outlier_table)[4] = paste0("race ratio - ", input$heat_race_county_v)
      colnames(outlier_table)[3] = "Counties"
      colnames(outlier_table)[1] = "Year"
      
      output$heat_race_county_plot_outliers = 
        renderUI(DT::renderDataTable(
          datatable(
            data = outlier_table,
            options = list(
              searching = TRUE,
              lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
              pageLength = 6
            ),
            rownames = FALSE)))
      
      # fixing certain values to upper or lower limits (windsorizing)
      user_requested_map_df[[variable]] = 
        ifelse(user_requested_map_df[[variable]] < rng[1],
               rng[1], ifelse(user_requested_map_df[[variable]] > rng[2],
                              rng[2], user_requested_map_df[[variable]]))
      
      mapping_counties_requested = mapping_counties %>% dplyr::filter(region == tolower(input_state))
      
      user_requested_map_df = user_requested_map_df %>% 
        dplyr::filter(YEAR %in% c(input$heat_race_county_year:(input$heat_race_county_year+11)))
      
      # complete the data frame so missing states are denoted by gray within the same plot
      merged_for_plot_complete_list = list()
      for(year_facet in c(input$heat_race_county_year:(input$heat_race_county_year+11))){
        # year_facet = 2012
        
        user_requested_map_df_year_facet =
          user_requested_map_df[user_requested_map_df$YEAR == year_facet, ] %>%
          tidyr::complete(COUNTY_NAME = unique(mapping_counties_requested$COUNTY_NAME),
                          YEAR = year_facet)
        
        merged_for_plot_complete_list = c(merged_for_plot_complete_list,
                                          list(user_requested_map_df_year_facet))
        
      }
      
      merged_for_plot = do.call(rbind, merged_for_plot_complete_list)
      
      # adding in longitude and latitude data
      merged_for_plot = dplyr::full_join(merged_for_plot,
                                         mapping_counties_requested[c("long", "lat", "group", "COUNTY_NAME")],
                                         by=c("COUNTY_NAME"))
      
      
      # create legend anchors. this is odd because equivalent ratios are not symmetrical around zero
      if(rng[1] > 0){
        anchor = ifelse(abs(log(rng[2])) > abs(log(rng[1])), rng[2], rng[1])
      } else {
        anchor = ifelse(abs(log(rng[2])) > abs(log(rng[1])), rng[2], rng[1])
      }
  
      if(anchor > 1){
        low_anchor = 1/anchor
        high_anchor = anchor
      } else if(anchor < 1){
        low_anchor = anchor
        high_anchor = 1/anchor
      }
      
      merged_for_plot$logged_variable = log(merged_for_plot[[variable]])
      ggplot2::ggplot(data = merged_for_plot, aes(x = long, y = lat, group = group,
                                                  fill = logged_variable)) +
        geom_polygon(color = "black") +
        coord_map() +
        facet_wrap(~YEAR) +
        theme_void() +
        labs(fill= stringr::str_wrap(paste0("race ratio - ", input$heat_race_county_v),15)) +
        scale_fill_gradient2(
          low = "darkorange1",
          mid = "white",
          high = "midnightblue",
          midpoint = 0,
          breaks = c(round(log(low_anchor), 3), 0, round(log(high_anchor), 3)),
          labels = c(round(low_anchor, 3), 1, round(high_anchor, 3)),
          limits = c(round(log(low_anchor), 3)-0.00001, round(log(high_anchor), 3)+.000001),
          name = ""
        ) +
        ggtitle(paste0("race ratio - ", input$heat_race_county_v)) +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(1.6, "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          strip.text = element_text(size = 14),
          strip.background = element_rect(fill = "white", color = NA)
        )
      
    }
    
  })
  
  # (4) interstate trends-----
  
  ## find image -----
  # Send a pre-rendered image of interstate trends, and don't delete the image after sending it
  
  output$interstate_trend_image = renderUI({

    # set up contingency if there are no selections
     if(is.null(c(input$sex_variables_ist, input$race_variables_ist))){

       renderPlot({ggplot2::ggplot(
       data.frame(text = str_wrap("You must select at least one variable to explore.", width = 60)), 
       aes(x = .5, y = .5, label = text)) + 
           geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + 
           theme_void()
         })

     } else {

       sex_variables_ist = c()
       for(selection in input$sex_variables_ist){
         # selection = "attained high school diploma"
         sex_variables_ist = c(sex_variables_ist,
                               paste0("Sex_(male-female)_",
                                      gsub(" ", "_", selection), "_Yes.png"))
       }
       race_variables_ist = c()
       for(selection in input$race_variables_ist){
         race_variables_ist = c(race_variables_ist,
                                paste0("Race_(white-non-white)_",
                                       gsub(" ", "_", selection), "_Yes.png"))
       }

       if(length(c(sex_variables_ist, race_variables_ist)) > 0) {

         display_image = function(image) {

           n_images = length(c(sex_variables_ist, race_variables_ist))
           if(n_images == 1){
             width_value = 100
           } else if(n_images >= 2){
             width_value = 50
           }

           HTML(paste0('<img src = ', image, ' style="margin-top: 5px; width:',
                       width_value,'%; height:auto;"><br>'))
         }

         images_files = lapply(c(sex_variables_ist, race_variables_ist), display_image)

         div(style = "display: flex; flex-direction: row; flex-wrap: wrap;", images_files)

       }

     }

  })
  
  ## outliers table for interstate trends -----
  
  # set up contingency if there are no selections
  
  output$ist_data_tables = 
    renderUI({
      
      if(is.null(c(input$sex_variables_ist, input$race_variables_ist))){
        
        renderPlot({ggplot2::ggplot() + theme_void()})
        
       } else {

         # ist_data_outlier_list =
           # reactive({
             sex_variables_IST = c()
             for(selection in input$sex_variables_ist){
               sex_variables_IST =
                 c(sex_variables_IST,
                   variable_info[["sex comparison variables"]][[selection]][["variable_name"]])
             }

             race_variables_IST = c()
             for(selection in input$race_variables_ist){
               race_variables_IST =
                 c(race_variables_IST,
                   variable_info[["race comparison variables"]][[selection]][["variable_name"]])
               }

             v = paste0("st_", c(sex_variables_IST, race_variables_IST))

             ist_data_outlier_list =list()
             for(ist_variable in v){

                ist_variable_data = shiny_data[c("YEAR", "state_name", ist_variable)]
                # change column names within dataframe
                colnames(ist_variable_data)[1] = "Year"
                colnames(ist_variable_data)[2] = "State"
                ist_variable_data = ist_variable_data[!duplicated(ist_variable_data[c("Year","State")]),]
                ist_variable_data = ist_variable_data[!is.na(ist_variable_data[ist_variable]),]
                IQR = summary(ist_variable_data[[ist_variable]])[5] - summary(ist_variable_data[[ist_variable]])[2]
                rng = c(summary(ist_variable_data[[ist_variable]])[2]-(1.5*IQR),
                        summary(ist_variable_data[[ist_variable]])[5]+(1.5*IQR))
                data_outside_of_range = ist_variable_data[(ist_variable_data[[ist_variable]] < rng[1] |
                                                             ist_variable_data[[ist_variable]] > rng[2]),]
                ist_variable_comparison = ifelse(grepl("_sex_", ist_variable), "sex ratio - ",
                                                 ifelse(grepl("_race_", ist_variable), "race ratio - ",
                                                        "ISSUE DETECTED"))
                ist_variable_dropdown_variable =
                  variable_key_df[variable_key_df$variable == gsub("st_", "", ist_variable),"variable_names"]
                colnames(data_outside_of_range)[3] = paste0(ist_variable_comparison, ist_variable_dropdown_variable)
                ist_data_outlier_list = c(ist_data_outlier_list, setNames(list(data_outside_of_range), ist_variable))

               }
               # ist_data_outlier_list
             # })

           observe({
             lapply(seq_along(ist_data_outlier_list), function(i) {
               output[[paste0("data_table_", i)]] = renderDataTable({
                 datatable(data = ist_data_outlier_list[[i]],
                           options = list(
                             searching = TRUE,
                             lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
                             pageLength = 6),
                           rownames = FALSE)
               })
             })
           })

           ist_data_tables =
             lapply(seq_along(ist_data_outlier_list), function(i) {
               dataTableOutput(outputId = paste0("data_table_", i))
             })

           # ist_data_tables= renderPlot({hist(runif(100,4,6), main="not null")})
           # ist_data_tables
           }

      })
        
        
  # (5) download----
  
  custom_download_df = reactive({
    
    sex_variables = c()
    for(selection in input$sex_variable5){
      sex_variables = c(sex_variables,
                        variable_info[["sex comparison variables"]][[selection]][["variable_name"]])
    }
    race_variables = c()
    for(selection in input$race_variable5){
      race_variables = c(race_variables,
                         variable_info[["race comparison variables"]][[selection]][["variable_name"]])
    }
    
    if(input$County_table == "State"){
      input_variable_w_prefix = paste0("st_", c(sex_variables, race_variables))
      info_variables = c("YEAR", "STATEFIP", "state_name")
      custom_level_data = raw_state_data
    } else {
      input_variable_w_prefix = paste0("cnty_", c(sex_variables, race_variables))
      info_variables = c("YEAR", "STATEFIP", "state_name", "COUNTY", "COUNTY_NAME")
      custom_level_data = raw_county_data
    }
    
    indices_of_ratio_variables = which(names(custom_level_data) %in% input_variable_w_prefix)
    ratios_and_percentage_indices = c()
    for(index in indices_of_ratio_variables){
      ratios_and_percentage_indices = c(ratios_and_percentage_indices,
                                        index-2, index-1, index)
    }
    
    info_indices = match(info_variables, names(custom_level_data))
    
    custom_download_df = custom_level_data[(custom_level_data$YEAR >= input$Year_table[1] & custom_level_data$YEAR <= input$Year_table[2]) &
                                             custom_level_data$state_name %in% input$States_table,
                                    c(info_indices, ratios_and_percentage_indices)]
    
    # if(input$County_table == "No"){
    #   custom_download_df = custom_download_df[!duplicated(custom_download_df),]
    # } else (custom_download_df = custom_download_df)
    
    return(custom_download_df)
  })
  
  output$data_table = DT::renderDataTable(
    datatable(data = custom_download_df(),
              options = list(
                searching = TRUE,
                scrollX = TRUE,
                lengthMenu = list(c(15, 30, 45), # declare values
                                  c(15, 30, 45) # declare titles
                ), pageLength = 15),rownames = FALSE))
  
  output$download_custom_data <- downloadHandler(
    filename = "custom_dataset.csv",
    content = function(file) {
      write.csv(custom_download_df(),
                file, row.names = FALSE)
    }
  )
  
  output$download_entire_data <- downloadHandler(
    filename = "full_dataset.csv",
    content = function(file) {
      write.csv(shiny_data,
                file, row.names = FALSE)
    }
  )
  
      
}

# Run ----
shinyApp(ui = ui, server = server)
