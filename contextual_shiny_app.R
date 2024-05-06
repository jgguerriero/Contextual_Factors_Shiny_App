
# setup----

# install.packages("pacman") 
library(pacman)
pacman::p_load(shiny, cdlTools, usmap, dplyr, ggplot2, stringr, tidyr, DT, shinythemes, tidycensus, geofacet,
               shinyWidgets, maps, cowplot, rsconnect, mapproj, ggpubr, shinyjs, shinyBS, BH, terra)

# setwd("~/OneDrive - The Pennsylvania State University/SoDA RA/deployment")
# load state data
raw_state_data = read.csv("./final_state_data.csv")

# load county data
raw_county_data = read.csv("./final_county_data.csv")

data_list = list(raw_state_data, raw_county_data)
#testing how name changes might affect things
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

raw_state_data = data_list[[1]]
raw_county_data = data_list[[2]]

# create version of data the is suitable for manipulations necessary for the app  
# join state and county
# need to get rid of county's state_name column
shiny_data = full_join(raw_state_data, raw_county_data[names(raw_county_data) != "state_name"], by = c("YEAR", "STATEFIP"))
names(shiny_data)
# remove cases that have NAs for state_name
shiny_data = shiny_data[!is.na(shiny_data$state_name),]

# create state labels
shiny_data$STATEFIP = ifelse(nchar(shiny_data$STATEFIP) == 1, 
                             paste0("0",shiny_data$STATEFIP), shiny_data$STATEFIP)
shiny_data$state_label = paste0(shiny_data$state_name, " (", "FIPS ", shiny_data$STATEFIP,")")

# create county labels
shiny_data$county_label = paste0(shiny_data$COUNTY_NAME, " (", shiny_data$COUNTY,")")


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
                                   variable_name = "sex_ratio_employed"),
                            "in labor force" =
                              list(description = "The ratio of the percentage of males in labor force to the percentage of females in labor force.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "LABFORCE, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_lf"),
                            "above poverty line" =
                              list(description = "The ratio of the percentage of males above the poverty line to the percentage of females in above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OFFPOV, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_apl"),
                            "attained high school diploma" = 
                              list(description = "The ratio of the percentage of males who attained a high school diploma to the percentage of females who attained a high school diploma.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_hsd"),
                            "attained bachelors" = 
                              list(description = "The ratio of the percentage of males who attained a bachelor’s degree to the percentage of females who attained a bachelor’s degree.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_bachelors"),
                            "literate" = 
                              list(description = "The ratio of the percentage of males who are literate to the percentage of females who are literate.",
                                   source = "IPUMS USA 1% Samples",
                                   values_from_source = "LIT, SEX, STATEFIP, COUNTY, YEAR, PERWT",
                                   variable_name = "sex_ratio_lit"),
                            "in white collar work" = 
                              list(description = "The ratio of the percentage of males who are in white-collar professions to the percentage of females who are in white-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occw"),
                            "in pink collar work" = 
                              list(description = "The ratio of the percentage of males who are in pink-collar professions to the percentage of females who are in pink-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occp"),
                            "in blue collar work" = 
                              list(description = "The ratio of the percentage of males who are in blue-collar professions to the percentage of females who are in blue-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, SEX, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "sex_ratio_occb")),
                     "race comparison variables" = 
                       list("employed" = 
                              list(description = "The ratio of the percentage of whites who are employed to the percentage of non-whites who are employed.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EMPSTAT, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_employed"),
                            "in labor force" =
                              list(description = "The ratio of the percentage of whites in the labor force to the percentage of non-whites in the labor force.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "LABFORCE, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_lf"),
                            "above poverty line" =
                              list(description = "The ratio of the percentage of whites above the poverty line to the percentage of non-whites above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OFFPOV, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_apl"),
                            "children above poverty line" =
                              list(description = "The ratio of the percentage of male chidlren above the poverty line to the percentage of female children in above the poverty line.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "SPMNCHILD, SPMPOV, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_childapl"),
                            "attained high school diploma" = 
                              list(description = "The ratio of the percentage of whites with at least high school diploma to the percentage of non-whites with at least high school diploma.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "EDUC, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_hsd"),
                            "literate" = 
                              list(description = "The ratio of the percentage of whites who are literate to the percentage of non-whites who are literate.",
                                   source = "IPUMS USA 1% Samples",
                                   values_from_source = "LIT, RACE, STATEICP, COUNTYICP, YEAR, PERWT",
                                   variable_name = "race_ratio_lit"),
                            "household income" = 
                              list(description = "The ratio of the median household income of the whites to the median household income of the non-whites.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "HHINCOME, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_hi"),
                            "own homes"= 
                              list(description = "The ratio of the percentage of whites who own their homes to the percentage of non-whites who own their homes.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OWNERSHP, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_home"),
                            "voted" = 
                              list(description = "The ratio of the percent of whites who voted in a given year's election to the ratio of
                                   non-whites who did not vote.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "VOTED, RACE, STATEFIP, YEAR, VOSUPPWT",
                                   variable_name = "race_ratio_vote"),
                            "in white collar work" = 
                              list(description = "The ratio of the percentage of whites who are in white-collar professions to the percentage of non-whites who are in white-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occw"),
                            "in pink collar work" = 
                              list(description = "The ratio of the percentage of whites who are in pink-collar professions to the percentage of non-whites who are in pink-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occp"),
                            "in blue collar work" = 
                              list(description = "The ratio of the percentage of whites who are in blue-collar professions to the percentage of non-whites who are in blue-collar professions.",
                                   source = "IPUMS CPS Annual Social and Economic Supplement Samples",
                                   values_from_source = "OCC1950, RACE, STATEFIP, COUNTY, YEAR, ASECWT",
                                   variable_name = "race_ratio_occb"),
                            "life expectancy at birth" = 
                              list(description = "Life expectancy at birth, calculated using period life table [a(0)=.3, a(1)=.4, a(x)=.5] and period-specific mortality rates [M(x) converted to q(x) using Chiang’s formula].",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_e0"),
                            "life expectancy at age 1" = 
                              list(description = "Life expectancy at age 1, calculated using period life table [a(0)=.3, a(1)=.4, a(x)=.5] and period-specific mortality rates [M(x) converted to q(x) using Chiang’s formula].",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_e1"),
                            "life expectancy at birth (males)" = 
                              list(description = "Life expectancy of males at birth",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_males_e0"),
                            "life expectancy at birth (females)" = 
                              list(description = "Life expectancy of females at birth",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_females_e0"),
                            "life expectancy at age 1 (males)" = 
                              list(description = "Life expectancy of males at age 1",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_males_e1"),
                            "life expectancy at age 1 (females)" = 
                              list(description = "Life expectancy of females at age 1",
                                   source = "CDC National Vital Statistics Systems and National Center for Health Statistics",
                                   values_from_source = "Age- and race-specific death rates",
                                   variable_name = "race_ratio_females_e1")))


# create placeholder county variables for variables that dont exist at county level
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

# user interface ----
ui = fluidPage(
  theme = shinytheme("lumen"),
  useShinyjs(),
  tags$head(
    # 770 before 
    tags$link(rel = "icon", type = "image/jpg", sizes = "32x36", href = "4_psu_bust.jpg"),
    # next piece of code is to get rid of logos if window is too small
    tags$script(HTML('
      $(window).resize(function() {
        if ($(window).width() < 1000) { 
          $(".conditional-logos").hide();
        } else {
          $(".conditional-logos").show();
        }
      })
    ')),
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
      }"),
    tags$style(
      HTML("
      .nav.nav-tabs > li {
        width: calc(100% / 6); /* Divide the width equally among the tabs */
        text-align: center; 
      ")
    )
)
  ,

  fluidRow(
    column(
      style = "background: linear-gradient(to left, #052485, white), 
      linear-gradient(to bottom, #052485, white); background-blend-mode: screen;",
      width = 12,
      titlePanel(h1("The Contextual Factors Project"), 
                 windowTitle = "The Contextual Factors Project")
    )
    # ,
    # column(
    #   width = 8,
    #   style = "background: linear-gradient(to left, #062FB1, white), linear-gradient(to bottom, #062FB1, white); background-blend-mode: screen;",
    #   HTML('<span style="font-size: 50px;">&nbsp;</span>')  
    # )
    # ,
    # column(
    #   width = 6,
    #   div(
    #     class = "logos",
    #     align = "right", 
    #     style = "padding-top: 20px;",
    #     a(href="https://la.psu.edu/", img(src="PSU_LBS_RGB_2C.png", style = "max-width:20%; height:auto", class="conditional-logos")),
    #     a(href="https://soda.la.psu.edu/", img(src="2_soda_logo.jpg", style = "max-width:20%; height:auto", class="conditional-logos")),
    #     a(href="https://www.nih.gov/", img(src="3_NIH_logo.jpg", style = "max-width:20%; height:auto", class="conditional-logos"))
    #   )
    # )
  ),

  tabsetPanel(
    ## intro page
    tabPanel(
      "About",
      tags$div(
        style = "display: flex; flex-direction: row; flex-wrap: wrap; justify-content: center; align-items: center; height: 70vh;",
        HTML(paste0('<div style="text-align: center; font-size: 23px; 
        font-family: Arial; max-width: 80%; padding: 20px; word-wrap: break-word;
        ">
          Welcome to the data dashboard for the Contextual Factors Project!<br>
          Please use the tabs at the top of the page to explore the data.<br><br>
          To get additional detail about the dataset, you may find it helpful to download and reference
          <a href="contextual_factors_codebook.pdf" download="contextual_factors_codebook.pdf">this codebook</a>.<br><br>
          The creation of this website was supported by the National Institutes on Aging (R01AG078518) and 
          the Center for Social Data Analytics at Penn State. The content and presentation of the data
          is solely the responsibility of the authors and does not necessarily represent the 
          official views of the National Institutes of Health or Penn State.<br><br>
          <i>This website is currently optimized for usage on laptop/desktop computers. If you notice
          any issues while using the dashboard or working with the data, please send an email to jgg5264@psu.edu.</i></div>')
             ))

    ),
    ## (1) Data info-----
    tabPanel("Data info & availability",
             fluidRow(
               column(2, radioButtons("comparison1", label = h4("Comparison"), 
                                      c("Sex (%male/%female)", "Race (%white/%non-white)"), selected = "Race (%white/%non-white)")),
               column(3, tags$div(pickerInput("variable1", label = h4("Variables available for comparison"), 
                                     names(variable_info[["race comparison variables"]]), 
                                     multiple = FALSE),
                                     class = "my-picker")),
               column(1, radioButtons("level1", label = h4("Level"), 
                                      c("State", "County"), selected = "State")),
               column(5, uiOutput("info_text1"), tags$style(type="text/css", "#info_text1 { margin-top: 15px; }"))
             )
             ,
             fluidRow(
               column(width = 12, 
                      plotOutput("availability_plot", height="650px")
                      
               ,
                      column(width = 12,
                             p(),
                             HTML('<div style="text-align: center; font-size: 17px;font-family: Arial;">Colored boxes denote data is available. However, county level data may be sparse despite being indicated as available. </div>'),
                             tags$div(style = "margin-top: 40px;"))
               ))
    )
    ,
    ## (2) Trends and statistics-----
    tabPanel("Trends and statistics",
             sidebarPanel(
               tags$div(pickerInput("sex_variable2", label = h4("Sex (%male/%female) comparison variables"), 
                              names(variable_info[["sex comparison variables"]]),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE)),
                        id = "sex_variable2", class = "my-picker"),
               bsTooltip("sex_variable2", "Select variable to examine the ratio of males to females.", "top"),
               tags$div(pickerInput("race_variable2", label = h4("Race (%white/%non-white) comparison variables"), names(variable_info[["race comparison variables"]]),
                              selected = "attained high school diploma", multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
                        id = "race_variable2", class = "my-picker"),
               bsTooltip("race_variable2", "Select variable to examine the ratio of whites to non-whites.", "top"),
               tags$div(pickerInput("state2", label = h4("States"), sort(unique(shiny_data$state_label)),
                              selected = "Pennsylvania (FIPS 42)", multiple = TRUE, 
                           options = pickerOptions(actionsBox = TRUE)), class = "my-picker"),
               tags$div(pickerInput("county2", label = h4("Counties", 
                                                          h5(HTML("Click <a href='https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt'>here</a> more info about location codes"))),
                              sort(unique(shiny_data$COUNTY[!grepl("NA", shiny_data$COUNTY)])),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE)), class = "my-picker"),
               materialSwitch(inputId = "reference2", label = h5("Switch frame of reference for figures"), value=FALSE),
              sliderInput("year2", h4("Years"), min = 1850, max = max(shiny_data$YEAR), 
                          value = c(1850, max(shiny_data$YEAR)), sep="", ticks = FALSE, step=1),
              tags$div(pickerInput("year_spotlight2", h4("Spotlight year"), c(1850:max(shiny_data$YEAR)), 
                                   selected = 2022),
                       id = "year_spotlight2", class = "my-picker"),
             bsTooltip("year_spotlight2", "Select a single year to examine values that makes up sex and/or race ratios. While most variables are measured in units of percentage, life expectancy variables are measured in units of years and median income in $USD. All values range between 0 and 100, with $USD being in thousands (K) to fit on 0-100 scale.", "bottom")),
             fluidRow(column(width = 7, plotOutput("main_plot", height = "600px"),#450
                             column(12, plotOutput("non_life_exp_no_labels", height="400px"))) #350
             )
    )
    ,
    ## (3) Heatmaps-----
    tabPanel("Heatmaps",
             fluidRow(
               column(2, radioButtons("Comparison_heat", label = h4("Comparison"), 
                                      choices = c("Sex (%male/%female)", "Race (%white/%non-white)"), 
                                      selected = "Race (%white/%non-white)")),
               column(2, tags$div(pickerInput("Variable_map_2", label = h4("Variables available for comparison"), 
                                     names(variable_info[["race comparison variables"]]), 
                                     multiple = FALSE), 
                                  class = "my-picker")),
               column(1, radioButtons("Level_heat", label = h4("Level"), choices = c("State", "County"), 
                                      selected = "State")),
               conditionalPanel(
                 condition = "input.Level_heat == 'County'",
                 column(2, tags$div(pickerInput(inputId = "state_for_county_selection_dropdown", label = h4("State"),
                                       choices = sort(unique(shiny_data$state_label)),
                                       selected = "Pennsylvania (FIPS 42)"),
                                    class = "my-picker"))),
               column(2, sliderInput("Year_2", h4("Year"), min = 1850, max = max(shiny_data$YEAR), 
                                     value = max(shiny_data$YEAR), step = 12, animate = FALSE, sep="",
                                     ticks = FALSE),
                      bsTooltip("Year_2", "Select year to view heatmaps (displayed in 12 year increments).", "bottom"),),
               column(2, radioButtons("heat_contrast", h4("Increase color contrast"), 
                                      choices = c("No", "Yes"), selected = "Yes"))),
             conditionalPanel(
               condition = "input.heat_contrast == 'No'",
               column(width = 12, plotOutput("heatmap_no_contrast", height="675px"))),
             conditionalPanel(
               condition = "input.heat_contrast == 'Yes'",
               column(width = 8, plotOutput("heatmap_yes_contrast", height="600px")),
               column(3, HTML("Ratios with extreme values were identified using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a>. 
               These values were fixed to the upper or lower thresholds of the fences to create additional color contrast.
               The year-location combinations with ratios that have been fixed and their true values can be explored in the table below."),
                      p(),
                      DT::dataTableOutput("outlier_table"))),
    )
    ,
    ## (4) interstate trends ------
    tabPanel("Inter-state trends",
             fluidRow(
               column(2, tags$div(pickerInput("sex_variables_ist", label = h4("Sex (%male/%female) comparison variables"), 
                                              names(variable_info[["sex comparison variables"]]),
                                        selected = NULL, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),class = "my-picker")),
               bsTooltip("sex_variables_ist", "Select variable to examine the ratio of males to females.", "top"),
               column(2, tags$div(pickerInput("race_variables_ist", label = h4("Race (%white/%non-white) comparison variables"), 
                                              names(variable_info[["race comparison variables"]]),
                                        selected = "attained high school diploma", multiple = TRUE,
                                        options = pickerOptions(actionsBox = TRUE)),class = "my-picker")),
               bsTooltip("race_variables_ist", "Select variable to examine the ratio of whites to non-whites", "top"),
               column(2, radioButtons("map_3_outliers", label = h4("Remove extreme values"), c("No", "Yes"), selected = "Yes")),
               conditionalPanel(
                 condition = "input.map_3_outliers == 'Yes'",
                 column(5, p(), p(), 
                 HTML("Ratios with extreme values were identified using <a href='https://aakinshin.net/posts/tukey-outlier-probability/'>Tukey's Fences</a>. 
               These values were fixed to the upper or lower thresholds of the fences to facilitate interpretation of general trends.
               The year-location combinations with ratios that have been fixed and their true values can be explored in the table below."),
               )))
             ,
             fluidRow(
               column(width = 12, uiOutput("interstate_trend_image"))
               ,
               conditionalPanel(condition = "input.map_3_outliers == 'Yes'",
                                column(12, # column spec isnt doing anything
                                       # div(DT::dataTableOutput("map3_data_table"),
                                       uiOutput("ist_data_tables")
                                # ,id = "map3_data_table")
                                ))
             )
             #,
    )
    ,
    ## (5) download -----
    tabPanel("Download",
             sidebarPanel(
               downloadButton("download_entire_data", "Download full dataset"),br(),br(),
               p(style = "font-size: 18px; color: rgba(0, 0, 0, 0.77); font-style: italic",
                 "Use filters below to create a custom subset of the data"),
               tags$div(pickerInput("sex_variable5", label = h4("Sex (%male/%female) comparison variables"), names(variable_info[["sex comparison variables"]]),
                              selected = NULL, multiple = TRUE, 
                              options = pickerOptions(actionsBox = TRUE)),class = "my-picker"),
               bsTooltip("sex_variable5", "Select variable to examine the ratio of males to females.", "top"),
               tags$div(pickerInput("race_variable5", label = h4("Race (%white/%non-white) comparison variables"), names(variable_info[["race comparison variables"]]),
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
  ##reactives -----
  # change the dropdown options to reflect variables available for a specific comparison
  observe({
    if(input$comparison1 == "Sex (%male/%female)"){
      comparison1 = names(variable_info[["sex comparison variables"]])
    } else {
      comparison1 = names(variable_info[["race comparison variables"]])
    } 
    updatePickerInput("variable1",
                      choices = comparison1,
                      select = "attained high school diploma",
                      session = getDefaultReactiveDomain())
    
  })
  
  
  # create a reactive object that saves the comparison of interest that can be used for 1) the plot and 2) the text
  comparison1 = reactive({
    if(input$comparison1 == "Sex (%male/%female)"){
      comparison1 = "sex comparison variables"
    } else {
      comparison1 = "race comparison variables"
    } 
    comparison1
  })
  
  
  ##plot-----
  # create logic for output of the plot which shows data availability by year 
  output$availability_plot <- renderPlot({
  
    
    # create key to differentiate state and count-level plots with
    level_of_analysis_key = list("State" = "st_", "County" = "cnty_", 
                              "State_color" = "blue3", "County_color" = "darkorange1",
                              "State_data" = raw_state_data,
                              "County_data" = raw_county_data)
    
    # manipulate data based on user inputs 
    variable_based_on_comparison = paste0(level_of_analysis_key[[input$level1]], 
                                          c(variable_info[[comparison1()]][[input$variable1]][["variable_name"]]))
    
    # variable_based_on_comparison = "st_sex_ratio_employed"
    if(variable_based_on_comparison %in% 
       names(level_of_analysis_key[[paste0(input$level1, "_data")]]) == FALSE){
      
      ggplot2::ggplot(data.frame(text = 
                                   str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs.", width = 60)),
                      aes(x = .5, y = .5, label = text)) +
        geom_text(size = 6, hjust = .5, vjust=-3, color = "red") + theme_void()
      
    } else {
      
      for_availability = level_of_analysis_key[[paste0(input$level1, "_data")]][c("YEAR", "state_name", variable_based_on_comparison)]
      
      
      for_availability = for_availability[!is.na(for_availability[variable_based_on_comparison]),]
      for_availability = for_availability[!is.na(for_availability$state_name),]
      
      for_availability[variable_based_on_comparison] = ifelse(!is.na(for_availability[variable_based_on_comparison]),
                                                              1,NA)
      
      for_availability = dplyr::distinct(for_availability)
      
      for_availability$state_name = factor(for_availability$state_name, 
                                           levels = rev(sort(unique(for_availability$state_name))))
      
      
      
      ggpubr::ggballoonplot(for_availability, x = "YEAR", y = "state_name", size = 4.3, 
                            fill = level_of_analysis_key[[paste0(input$level1, "_color")]], 
                            color = level_of_analysis_key[[paste0(input$level1, "_color")]],shape=22) +
        scale_x_continuous(breaks = c(min(for_availability$YEAR):max(for_availability$YEAR)),
                           labels = c(min(for_availability$YEAR):max(for_availability$YEAR)),
                           position = "top") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
              axis.text.y = element_text(size = 12),
              plot.title = element_text(hjust = 0.5))
      
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
    output_text = c(variable_description, variable_source_info, source_variables)
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
  
  output$main_plot <- renderPlot({
    
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
        reactive_year_frame_state = shiny_data[c("YEAR", "state_label", paste0("st_", c(sex_variables,race_variables)))]
        reactive_year_frame_state = reactive_year_frame_state %>%
          dplyr::rename(., Location = state_label) %>%
          rename_with(~stringr::str_replace(., "^st_", ""), starts_with("st_")) %>%
          dplyr::filter(Location %in% input$state2 & YEAR>=input$year2[1] & YEAR<=input$year2[2])
      } else(reactive_year_frame_state = NULL)
      if(!is.null(input$county2)){
        reactive_year_frame_county = shiny_data[c("YEAR", "COUNTY",paste0("cnty_", c(sex_variables,race_variables)))]
        reactive_year_frame_county = reactive_year_frame_county %>%
          dplyr::rename(., Location = COUNTY) %>%
          rename_with(~stringr::str_replace(., "^cnty_", ""), starts_with("cnty_")) %>%
          dplyr::filter(Location %in% input$county2 & YEAR>=input$year2[1] & YEAR<=input$year2[2])
      } else(reactive_year_frame_county = NULL)
      
      reactive_year_frame = rbind(reactive_year_frame_state, reactive_year_frame_county)
      # get rid of duplicate rows
      reactive_year_frame = reactive_year_frame[!duplicated(reactive_year_frame),]
      
      # reshape the data to be ready for plotting
      reactive_year_frame_long = reactive_year_frame %>%
        tidyr::pivot_longer(cols = names(reactive_year_frame)[-c(1:2)],
                            names_to = "variable",
                            values_to = "variable_value")
      
      # get rid of rows with NA values
      reactive_year_frame_long = reactive_year_frame_long[!is.na(reactive_year_frame_long$variable_value),]
      
      # input blank rows
      for(location in c(input$state2, input$county2)){
        for(selected_variable in c(sex_variables, race_variables)){
          blank_row = data.frame(YEAR = NA, Location = location,
                                 variable = selected_variable,
                                 variable_value = NA)
          reactive_year_frame_long = rbind(reactive_year_frame_long, blank_row)
        }
      }
      
      reactive_year_frame_long$comparison = ifelse(grepl("^sex_", reactive_year_frame_long$variable), "sex ratio - ",
                                                   ifelse(grepl("^race_", reactive_year_frame_long$variable), "race ratio - ", "unclear comparison"))
      
      reactive_year_frame_long = reactive_year_frame_long %>% 
        dplyr::left_join(variable_key_df, by = "variable")
     
      reactive_year_frame_long$variable = paste0(reactive_year_frame_long$comparison,
                                                 reactive_year_frame_long$variable_names)
      
      if(sum(!is.na(reactive_year_frame_long$variable_value)) == 0){
        main_plot = ggplot2::ggplot(data.frame(text = 
                                     str_wrap("Sorry, we do not have data for this combination of criteria. 
                                            You will need to change one or more inputs. 
                                            Keep in mind that you need at least one variable 
                                            ('Sex' and/or 'Race comparison') and one location ('States' and/or 'Counties').", width = 60)),
                        aes(x = .5, y = .5, label = text)) +
          geom_text(size = 6, hjust = .5, vjust=.5, color = "red") + theme_void()
        
      } else {
        
        if(input$reference2 == TRUE){
          grouping = "Location"
          facet = "variable"} else {
            grouping = "variable"
            facet = "Location"
          }
        
        main_plot = ggplot2::ggplot(reactive_year_frame_long, aes(x = YEAR, y = variable_value)) +
          geom_line(aes(color = get(grouping)), size = 1.5) +
          geom_point(aes(fill = get(grouping), color = get(grouping)), shape = 21, size = 2) +
          geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
          labs(x = "Year", y = "Ratio") +
          scale_color_viridis_d(option = "C", name = grouping) +
          scale_fill_viridis_d(option = "C", name = grouping) +  
          theme_linedraw() +
          facet_wrap(~ get(facet)) +
          theme(
            panel.spacing = unit(2, "lines"),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 20),
            strip.text = element_text(size = 11, color = "black"),
            strip.background = element_rect(fill = "white", color = NA))
      }
    }
    main_plot
  })
  
  ## barchart ----
  
  user_implied_data_frame_sub_plot_long = reactive({
    
    if(is.null(c(input$sex_variable2, input$race_variable2)) | 
       is.null(input$state2) & is.null(input$county2)){
      user_implied_data_frame_sub_plot_long = NULL
      
    } else {
      
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
      
        if(!is.null(input$state2)){
          input_variable= paste0("st_", c(sex_variables,race_variables))
          variable_indices = sapply(input_variable, function(variable) {which(names(shiny_data) %in% variable)})
          percent_columns = shiny_data[,c(which(names(shiny_data)=="YEAR"), which(names(shiny_data)=="state_label"),
                                          variable_indices-2, variable_indices-1)]
          user_implied_data_frame_state = percent_columns %>%
            dplyr::filter(YEAR==input$year_spotlight2 &
                            state_label %in% input$state2) %>%
            dplyr::rename(., Location = state_label) %>%
            rename_with(~stringr::str_replace(., "^st_", ""), starts_with("st_"))
        } else(user_implied_data_frame_state=NULL)
        
        if(!is.null(input$county2)){
          input_variable= paste0("cnty_", c(sex_variables,race_variables))
          variable_indices = sapply(input_variable, function(variable) {which(names(shiny_data) %in% variable)})
          percent_columns = shiny_data[,c(which(names(shiny_data)=="YEAR"), which(names(shiny_data)=="COUNTY"),
                                          variable_indices-2,variable_indices-1)]
          user_implied_data_frame_county = percent_columns %>%
            filter(YEAR==input$year_spotlight2 &
                     COUNTY %in% input$county2) %>%
            dplyr::rename(., Location = COUNTY) %>%
            rename_with(~stringr::str_replace(., "^cnty_", ""), starts_with("cnty_"))
        } else(user_implied_data_frame_county=NULL)
        
        user_implied_data_frame_sub_plot = rbind(user_implied_data_frame_state,user_implied_data_frame_county)
        user_implied_data_frame_sub_plot_long = user_implied_data_frame_sub_plot %>%
          pivot_longer(cols = names(user_implied_data_frame_sub_plot)[-c(1:2)],
                       names_to = "initial_pivot",
                       values_to = "variable_value")
        
        for(location in c(input$state2, input$county2)){
          for(variables in gsub("st_|cnty_", "", names(shiny_data[,c(variable_indices-2, variable_indices-1)]))){
            new_row = data.frame(YEAR = input$year_spotlight2, Location = location,
                                 initial_pivot = variables,
                                 variable_value = NA)
            user_implied_data_frame_sub_plot_long = rbind(user_implied_data_frame_sub_plot_long,
                                                          new_row)
          }
        }
        
        user_implied_data_frame_sub_plot_long$initial_pivot = gsub("perc_", "", user_implied_data_frame_sub_plot_long$initial_pivot)
        user_implied_data_frame_sub_plot_long$comparison = user_implied_data_frame_sub_plot_long$initial_pivot
        
        
        user_implied_data_frame_sub_plot_long = user_implied_data_frame_sub_plot_long %>%
          dplyr::mutate(comparison = dplyr::case_when(stringr::str_detect(initial_pivot, "^male_") ~ "Male",
                                                      stringr::str_detect(initial_pivot, "^female_") ~ "Female",
                                                      stringr::str_detect(initial_pivot, "^white_") ~ "White",
                                                      stringr::str_detect(initial_pivot, "^nonwhite_") ~ "Non-White",
                                                      stringr::str_detect(initial_pivot, "^whitemale_") ~ "White Male",
                                                      stringr::str_detect(initial_pivot, "^nonwhitemale_") ~ "Non-White Male",
                                                      stringr::str_detect(initial_pivot, "^whitefemale_") ~ "White Female",
                                                      stringr::str_detect(initial_pivot, "^nonwhitefemale_") ~ "Non-White Female",
                                                      stringr::str_detect(initial_pivot, "^median_white_") ~ "White",
                                                      stringr::str_detect(initial_pivot, "^median_nonwhite_") ~ "Non-White",
                                                      TRUE ~ initial_pivot))
        
        # get rid of duplicates
        user_implied_data_frame_sub_plot_long = user_implied_data_frame_sub_plot_long[!duplicated(user_implied_data_frame_sub_plot_long),]
        
        user_implied_data_frame_sub_plot_long = user_implied_data_frame_sub_plot_long %>%
          dplyr::mutate(new_pivot = dplyr::case_when(stringr::str_detect(initial_pivot, "^male_|^female_") ~ 
                                                       stringr::str_replace(initial_pivot, "^male_|^female_", "sex_ratio_"),
                                                     stringr::str_detect(initial_pivot, "^white_|^nonwhite_") ~ 
                                                       stringr::str_replace(initial_pivot, "^white_|^nonwhite_", "race_ratio_"),
                                                     stringr::str_detect(initial_pivot, "^whitemale_|^nonwhitemale_") ~ 
                                                       stringr::str_replace(initial_pivot, "^whitemale_|^nonwhitemale_", "race_ratio_males_"),
                                                     stringr::str_detect(initial_pivot, "^whitefemale_|^nonwhitefemale_") ~ 
                                                       stringr::str_replace(initial_pivot, "^whitefemale_|^nonwhitefemale_", "race_ratio_females_"),
                                                     stringr::str_detect(initial_pivot, "^median_nonwhite_|^median_white_") ~ 
                                                       stringr::str_replace(initial_pivot, "^median_nonwhite_|^median_white_", "race_ratio_"),
                                                      TRUE ~ initial_pivot))
        
        # change variable name to match the main plot
        user_implied_data_frame_sub_plot_long$race_or_sex =
          ifelse(user_implied_data_frame_sub_plot_long$comparison == "Female" | 
                   user_implied_data_frame_sub_plot_long$comparison == "Male",
                 "sex ratio - ", "race ratio - ")
  
        user_implied_data_frame_sub_plot_long = user_implied_data_frame_sub_plot_long %>% 
          dplyr::left_join(variable_key_df, by = c("new_pivot"="variable"))
        
        user_implied_data_frame_sub_plot_long$variable_names = 
          paste0(user_implied_data_frame_sub_plot_long$race_or_sex, user_implied_data_frame_sub_plot_long$variable_names)
        
        user_implied_data_frame_sub_plot_long
    }
  })
  
  
  # this is for all variable -- originally it was for only non-life expectancy variables (hence the outdated naming convention)
  
  non_life_exp_no_labels = reactive({
    
    if(is.null(c(input$sex_variable2, input$race_variable2)) | 
       is.null(input$state2) & is.null(input$county2)){
        non_life_exp_no_labels = ggplot2::ggplot() + theme_void()
      
      } else {
        
        non_life_exp_sub_plot_data = user_implied_data_frame_sub_plot_long()
        
        if(sum(!is.na(non_life_exp_sub_plot_data$variable_value))==0){
          non_life_exp_no_labels = ggplot2::ggplot() + theme_void()
          
          } else {
            
            non_life_exp_sub_plot_data$variable_value = ifelse(!grepl("life expectancy|household income", non_life_exp_sub_plot_data$variable_names), 
                                                               non_life_exp_sub_plot_data$variable_value*100, non_life_exp_sub_plot_data$variable_value)
            non_life_exp_sub_plot_data$variable_value_label = ifelse(!grepl("life expectancy", non_life_exp_sub_plot_data$variable_names), 
                                                                     paste0(round(non_life_exp_sub_plot_data$variable_value,2), "%"), 
                                                                     paste0(round(non_life_exp_sub_plot_data$variable_value,2), " years"))
            
            
            non_life_exp_sub_plot_data$variable_value = ifelse(grepl("household income", non_life_exp_sub_plot_data$variable_names), 
                                                               non_life_exp_sub_plot_data$variable_value*.0001, non_life_exp_sub_plot_data$variable_value)
            
            non_life_exp_sub_plot_data$variable_value_label = ifelse(grepl("household income", non_life_exp_sub_plot_data$variable_names), 
                                                                     paste0("$",non_life_exp_sub_plot_data$variable_value, "K"), 
                                                                     non_life_exp_sub_plot_data$variable_value_label)
            
            non_life_exp_sub_plot_data$variable = non_life_exp_sub_plot_data$variable_names
            
            if(input$reference2 == TRUE){
              fill = "Location"
              facet = "variable"} else {
                fill = "variable"
                facet = "Location"}
            
            non_life_exp_no_labels = ggplot2::ggplot(non_life_exp_sub_plot_data, aes(x = comparison, y = variable_value, fill = get(fill))) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_fill_viridis_d(option = "C", name = fill) +
              theme_linedraw() +
              # ylab(stringr::str_wrap("Percent/Years for life expectancy variables/USD for income variable)", width = 30)) + 
              ylab("") + 
              xlab("Comparison") +
              ggtitle(paste0("Spotlight year: ", input$year_spotlight2)) +
              facet_wrap(~ get(facet)) +
              geom_vline(xintercept = 1.5, color = "black", linewidth = .33) +
              geom_vline(xintercept = 2.5, color = "black", linewidth = .33) +
              geom_vline(xintercept = 3.5, color = "black", linewidth = .33) +
              geom_vline(xintercept = 4.5, color = "black", linewidth = .33) +
              geom_vline(xintercept = 5.5, color = "black", linewidth = .33) +
              theme(
                panel.spacing = unit(2, "lines"),
                legend.position="bottom",
                axis.text.x = element_text(size = 15, angle = 90), axis.text.y = element_text(size = 15),
                axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 18),
                title = element_text(size = 15),
                strip.text = element_text(
                  size = 11, 
                  color="black"),
                strip.background = element_rect(fill = "white", color = NA))+
              scale_y_continuous(limits = c(0,110), breaks = c(20,40,60,80,100)) +
              guides(fill = guide_legend(ncol = 2, byrow = TRUE))
              if((length(c(input$sex_variable2,input$race_variable2)) + length(input$state2) + length(input$county2))<6){
                non_life_exp_no_labels = non_life_exp_no_labels +
                  geom_text(aes(label=variable_value_label), position=position_dodge(width=0.9), vjust=-0.25
                            , size = 5) 
              } 
          }
      }
    non_life_exp_no_labels
  })
  
  output$non_life_exp_no_labels = renderPlot({non_life_exp_no_labels()})
  
  # (3) heatmaps----
  
  ## reactives -------
  # change the dropdown options to reflect variables available for a specific comparison
  observe({
    if(input$Comparison_heat == "Sex (%male/%female)"){
      Comparison_heat = names(variable_info[["sex comparison variables"]])
    } else {
      Comparison_heat = names(variable_info[["race comparison variables"]])
    }
    updatePickerInput("Variable_map_2",
                      choices = Comparison_heat,
                      selected = "attained high school diploma",
                      session = getDefaultReactiveDomain())
    
  })
  
  # update slider to be in accordance with variable min and max 
  observe({
    if(input$Comparison_heat == "Sex (%male/%female)"){
      heat_comparison = "sex comparison variables"
    } else {
      heat_comparison = "race comparison variables"
    }
    variable_based_on_comparison = paste0("st_",
                                          c(variable_info[[heat_comparison]][[input$Variable_map_2]][["variable_name"]]))
    finding_min_max_years = shiny_data[c("YEAR", variable_based_on_comparison)]
    finding_min_max_years= finding_min_max_years[!(is.na(finding_min_max_years[variable_based_on_comparison])),]
    min_year = min(finding_min_max_years$YEAR,na.rm=TRUE)
    max_year = max(finding_min_max_years$YEAR,na.rm=TRUE)
    
    updateSliderInput("Year_2",
                      min = min_year,
                      max = max_year-11,
                      session = getDefaultReactiveDomain())
  })
  
  ## heatmaps -----
  heatmap <- reactive({
    # need to figure out how to repel labels 
    
    if(input$Comparison_heat == "Sex (%male/%female)"){
      heat_comparison = "sex comparison variables"
    } else {
      heat_comparison = "race comparison variables"
    }
    
    variable_based_on_comparison = paste0("st_",
                                          c(variable_info[[heat_comparison]][[input$Variable_map_2]][["variable_name"]]))
    
    # variable_based_on_comparison = "st_sex_ratio_employed"
    
    if(input$Level_heat == "State"){
      input_variable_map_2 = variable_based_on_comparison
      state_shiny_data = unique(shiny_data[!is.na(shiny_data[input_variable_map_2]),
                                           c("YEAR", "state_label", input_variable_map_2)])
      
      
      if(input$heat_contrast == "Yes"){
        
        IQR = summary(state_shiny_data[[input_variable_map_2]])[5] - summary(state_shiny_data[[input_variable_map_2]])[2]
        rng = c(summary(state_shiny_data[[input_variable_map_2]])[2]-(1.5*IQR),
                summary(state_shiny_data[[input_variable_map_2]])[5]+(1.5*IQR))
        
        cases_outside_of_range = state_shiny_data[(
          state_shiny_data[[input_variable_map_2]] < rng[1] |
            state_shiny_data[[input_variable_map_2]] > rng[2]),] %>%
          dplyr::rename("Location" = "state_label") 
        
        colnames(cases_outside_of_range)[3] = paste0(stringr::str_extract(heat_comparison, "\\w+"), " ratio - ", input$Variable_map_2)
        colnames(cases_outside_of_range)[2] = "States"
        colnames(cases_outside_of_range)[1] = "Year"
        
        output$outlier_table = DT::renderDataTable(
          datatable(data = cases_outside_of_range,
                    options = list(
                      searching = TRUE,
                      lengthMenu = list(c(5, 10, 20), # declare values
                                        c(5, 10, 20) # declare titles
                      ), pageLength = 6),rownames = FALSE))
        
        user_requested_map_df = state_shiny_data[state_shiny_data$YEAR %in% c(input$Year_2:(input$Year_2+11)),]
        
        # fixing certain values to upper or lower limits
        user_requested_map_df[[input_variable_map_2]] = ifelse(user_requested_map_df[[input_variable_map_2]] < rng[1],
                                                               rng[1], ifelse(user_requested_map_df[[input_variable_map_2]] > rng[2],
                                                                              rng[2], user_requested_map_df[[input_variable_map_2]]))
      } else {
        
        # range to use based on ALL years
        rng = range(state_shiny_data[[input_variable_map_2]], na.rm=TRUE)
        
        # data to display
        user_requested_map_df = state_shiny_data[state_shiny_data$YEAR %in% c(input$Year_2:(input$Year_2+11)),]
        
      }
    
      
      input_variable_map_3 = gsub("st_", "", input_variable_map_2)
      
      
      comparison_input_variable_map_3 = ifelse(grepl("^sex_", input_variable_map_3), "sex ratio - ",
                                                   ifelse(grepl("^race_", input_variable_map_3), 
                                                          "race ratio - ", "unclear comparison"))
      
      input_variable_map_3 = variable_key_df[variable_key_df$variable == input_variable_map_3, "variable_names"]
      
      
      full_variable_name = paste0(comparison_input_variable_map_3, input_variable_map_3)
      
      colnames(user_requested_map_df)[which(colnames(user_requested_map_df) == input_variable_map_2)] = 
        full_variable_name
      
      user_requested_map_df$state = substr(user_requested_map_df$state_label, 1, 
                                           nchar(user_requested_map_df$state_label)-10)
      
      # complete the data frame so missing states are denoted by gray within the same plot 
      user_requested_map_df_complete_list = list()
      for(year_facet in c(input$Year_2:(input$Year_2+11))){
        # year_facet = 1975
        
        user_requested_map_df_year_facet = 
          user_requested_map_df[user_requested_map_df$YEAR == year_facet, ] %>%
          tidyr::complete(state = unique(shiny_data$state_name),
                          YEAR = year_facet) 
        
        user_requested_map_df_complete_list = c(user_requested_map_df_complete_list,
                                                   list(user_requested_map_df_year_facet))
        
      }
      
      user_requested_map_df_complete_df = do.call(rbind, user_requested_map_df_complete_list)
      
      usmap::plot_usmap(data = user_requested_map_df_complete_df, values = full_variable_name, labels = FALSE) +
        scale_fill_gradient2(low="darkorange1", mid="white", high="midnightblue",
                             midpoint=1,
                             breaks=seq(round(rng[1],3),round(rng[2],3),round((rng[2]- rng[1])/9,2)), #breaks in the scale bar
                             limits=c(rng[1],rng[2]))+
        facet_wrap(~ YEAR) +
        labs(fill = full_variable_name) +
        theme(legend.position = "right",
              legend.key.height = unit(2, "cm"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.margin = margin(t = 6, b = 3, unit = "cm"),
              strip.text = element_text(size = 14),
              strip.background = element_rect(fill = "white", color = NA))
      
    } else if(input$Level_heat == "County"){

      input_variable_map_2 = gsub("st_", "cnty_",variable_based_on_comparison)
      
      input_state = paste0(input$state_for_county_selection_dropdown)
      
      input_state = substr(input_state, 1, nchar(input_state)-10)
      
      user_requested_map_df = shiny_data %>% dplyr::filter(state_name=={{input_state}} &
                                                             !is.na(get(input_variable_map_2))) %>%
        dplyr::select(c(!!input_variable_map_2, COUNTY, county_label, state_name, STATEFIP, YEAR, COUNTY_NAME))
      
      
      if(input$heat_contrast == "Yes"){
        IQR = summary(user_requested_map_df[[input_variable_map_2]])[5] - summary(user_requested_map_df[[input_variable_map_2]])[2]
        rng = c(summary(user_requested_map_df[[input_variable_map_2]])[2]-(1.5*IQR),
                summary(user_requested_map_df[[input_variable_map_2]])[5]+(1.5*IQR))
        
        cases_outside_of_range = user_requested_map_df[(user_requested_map_df[[input_variable_map_2]] < rng[1] |
                                                          user_requested_map_df[[input_variable_map_2]] > rng[2]),]
        
        outlier_table = cases_outside_of_range %>% 
          dplyr::select(c(YEAR, county_label, !!input_variable_map_2))
        colnames(outlier_table)[3] = paste0(stringr::str_extract(heat_comparison, "\\w+"), " ratio - ", input$Variable_map_2)
        colnames(outlier_table)[2] = "Counties"
        colnames(outlier_table)[1] = "Year"
        
        output$outlier_table = DT::renderDataTable(
          datatable(data = outlier_table,
                    options = list(
                      searching = TRUE,
                      lengthMenu = list(c(5, 10, 20), # declare values
                                        c(5, 10, 20) # declare titles
                      ), pageLength = 6),rownames = FALSE))
        
        # fixing certain values to upper or lower limits
        user_requested_map_df[[input_variable_map_2]] = ifelse(user_requested_map_df[[input_variable_map_2]] < rng[1],
                                                               rng[1], ifelse(user_requested_map_df[[input_variable_map_2]] > rng[2],
                                                                              rng[2], user_requested_map_df[[input_variable_map_2]]))
      } else {
        rng = range(user_requested_map_df[[input_variable_map_2]],na.rm=TRUE)
      }
      
      mapping_counties_requested = mapping_counties %>% dplyr::filter(region == tolower(input_state))
      
      user_requested_map_df = user_requested_map_df %>% dplyr::filter(YEAR %in% c(input$Year_2:(input$Year_2+11)))
      
      # complete the data frame so missing states are denoted by gray within the same plot 
      merged_for_plot_complete_list = list()
      for(year_facet in c(input$Year_2:(input$Year_2+11))){
        # year_facet = 2012
        
        # there needs to be something around here 
        
        user_requested_map_df_year_facet = 
          user_requested_map_df[user_requested_map_df$YEAR == year_facet, ] %>%
          tidyr::complete(COUNTY_NAME = unique(mapping_counties_requested$COUNTY_NAME),# here change to county stuff
                          YEAR = year_facet) 
        
        merged_for_plot_complete_list = c(merged_for_plot_complete_list,
                                                list(user_requested_map_df_year_facet))
        
      }
      
      merged_for_plot = do.call(rbind, merged_for_plot_complete_list)
      
      # adjusting names for variables
      input_variable_map_3 = gsub("cnty_", "", input_variable_map_2)
      
      comparison_input_variable_map_3 = ifelse(grepl("^sex_", input_variable_map_3), "sex ratio - ",
                                               ifelse(grepl("^race_", input_variable_map_3), 
                                                      "race ratio - ", "unclear comparison"))
      
      input_variable_map_3 = variable_key_df[variable_key_df$variable == input_variable_map_3, "variable_names"]
      
      full_variable_name = paste0(comparison_input_variable_map_3, input_variable_map_3)
      
      colnames(merged_for_plot)[which(colnames(merged_for_plot) == input_variable_map_2)] = 
        full_variable_name
      
      # adding in longitude and latitude data 
      
      merged_for_plot = dplyr::full_join(merged_for_plot,
                                         mapping_counties_requested[c("long", "lat", "group", "COUNTY_NAME")],
                                         by=c("COUNTY_NAME"))

      if(nrow(merged_for_plot) == 0){
        
        ggplot2::ggplot(data.frame(text = 
                                     str_wrap("We do not have data for this combination of criteria. 
                                            You will need to change one or more inputs.", width = 60)),
                        aes(x = .5, y = .5, label = text)) +
          geom_text(size = 6, hjust = .5, vjust=-3, color = "red") + theme_void()
        
      } else {

        ggplot2::ggplot(data = merged_for_plot, aes(x = long, y = lat, group = group, 
                                                    fill = get(full_variable_name))) +
          geom_polygon(color = "black") +
          coord_map() +
          facet_wrap(~YEAR) +
          theme_void() +
          labs(fill= full_variable_name) +
          scale_fill_gradient2(low="darkorange1", mid="white", high="midnightblue", #colors in the scale
                               midpoint=1,
                               breaks=seq(round(rng[1],3),round(rng[2],3), round((rng[2]- rng[1])/9,2)), #breaks in the scale bar
                               limits=c(rng[1],rng[2]))+
          theme(legend.position = "right",
                legend.key.height = unit(3, "cm"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                legend.margin = margin(t = 6, b = 6, unit = "cm"),
                strip.text = element_text(size = 16),
                strip.background = element_rect(fill = "white", color = "white"))
      }
    }
  })
  
  output$heatmap_no_contrast = renderPlot({heatmap()})
  output$heatmap_yes_contrast = renderPlot({heatmap()})
  
  # (4) interstate trends-----
  
  ## find image -----
  # Send a pre-rendered image of interstate trends, and don't delete the image after sending it
  
  output$interstate_trend_image = renderUI({
    
    sex_variables_ist = c()
    for(selection in input$sex_variables_ist){
      sex_variables_ist = c(sex_variables_ist,
                            paste0("Sex_(male-female)_", 
                                   gsub(" ", "_", selection), "_", input$map_3_outliers,".png"))
    }
    race_variables_ist = c()
    for(selection in input$race_variables_ist){
      race_variables_ist = c(race_variables_ist,
                             paste0("Race_(white-non-white)_", 
                                    gsub(" ", "_", selection), "_", input$map_3_outliers,".png"))
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
      
      print(images_files)
      
      div(style = "display: flex; flex-direction: row; flex-wrap: wrap;", images_files)
    }
    
  })
  
  ## outliers for interstate trends -----
  
  ist_data_outlier_list =
    reactive({
            sex_variables_IST = c()
            for(selection in input$sex_variables_ist){
              sex_variables_IST = c(sex_variables_IST,
                                variable_info[["sex comparison variables"]][[selection]][["variable_name"]])
            }
            race_variables_IST = c()
            for(selection in input$race_variables_ist){
              race_variables_IST = c(race_variables_IST,
                                 variable_info[["race comparison variables"]][[selection]][["variable_name"]])
            }
            
            v = paste0("st_", c(sex_variables_IST, race_variables_IST))
            
            ist_data_outlier_list =list()
            for(ist_variable in v){
              # ist_variable = "st_sex_ratio_employed"
              
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
                                               ifelse(grepl("_race_", ist_variable), "race ratio - ", "ISSUE DETECTED"))
              ist_variable_dropdown_variable = 
                variable_key_df[variable_key_df$variable == gsub("st_", "", ist_variable),"variable_names"]
              colnames(data_outside_of_range)[3] = paste0(ist_variable_comparison, ist_variable_dropdown_variable)
              
              ist_data_outlier_list = c(ist_data_outlier_list, setNames(list(data_outside_of_range), ist_variable))
              
            }
            ist_data_outlier_list
          })
  
  # render outlier table based on reactive ist_data_outlier_list
  
  observe({
    lapply(seq_along(ist_data_outlier_list()), function(i) {
      output[[paste0("data_table_", i)]] = renderDataTable({
        datatable(data = ist_data_outlier_list()[[i]],
                  options = list(
                    searching = TRUE,
                    lengthMenu = list(c(5, 10, 20), c(5, 10, 20)),
                    pageLength = 6),
                  rownames = FALSE)
      })
    })
  })
  
  output$ist_data_tables <- renderUI({
    lapply(seq_along(ist_data_outlier_list()), function(i) {
      dataTableOutput(outputId = paste0("data_table_", i))
    })
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
