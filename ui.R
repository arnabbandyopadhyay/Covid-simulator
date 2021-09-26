#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => R-SHINY USER INTERFACE
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________
# # FYI: To run SOCRATES via Github (latest version!)
library('shiny')
library('shinycssloaders')
library('shinyWidgets')
# runGitHub('socrates_rshiny','lwillem')

# # load all functions and packages
# this is done automatically when running the App
source('R/socrates_main.R')
# Define UI for social contact application
shinyUI(pageWithSidebar(
  
  
  # Application title
  # headerPanel(ui_title,
  #             windowTitle = paste0('SOCRATES (',version_id,')')),
  
  headerPanel(ui_title,
              windowTitle = 'SECIR UI'),
  
  # Sidebar with controls
  sidebarPanel(width=4,
               shinyjs::useShinyjs(),
               
 
    # if(bool_is_comix_ui){
    #   uiOutput("socrates_website_data")
    # },
    # 
    # if(bool_is_comix_ui){
    #   hr()
    # },


    selectInput(inputId = "country", 
                label = "Country",
                choices = opt_country,
                selectize = ),
    checkboxInput('custom_city','Custom City',value = FALSE),
    conditionalPanel(
      condition = 'input.custom_city',
      helpText(h5('Provide population of the city',style='color:#000000;')),
      numericInput('custom_city_0_18','0-18 Population',value=0),
      numericInput('custom_city_18_40','18-40 Population',value=0),
      numericInput('custom_city_40_60','40-60 Population',value=0),
      numericInput('custom_city_60+','60+ Population',value=0)
    ),
    
    # # waves (dynamic, only if wave info is present)
    uiOutput(outputId = 'dynamicWaveInput'),
    
    helpText(h3('Age breaks',style='color:#000000;')),
    helpText(h4('0-18,18-40,40-60,60+',style='color:#000000;')),
    
    # textInput(inputId="age_breaks_text",
    #           label="Age breaks (comma delimited)",
    #           value=opt_age_breaks),
   
    #by default 'all contacts' to prevent warnings/errors, can be extended in "server" script. 
    selectInput("daytype", "Type of day",
                opt_day_type[1]),
    
    conditionalPanel(condition = bool_selectInput_duration,
    selectInput("duration", "Contact duration",
                opt_duration)
    ),
    
    selectInput("touch", "Contact intensity",
                opt_touch),
    
    selectInput("gender", "Gender",
                opt_gender),
    
    tabsetPanel(type = "tabs", id="distancing_transmission",
                tabPanel("General",
                         checkboxInput("bool_reciprocal", "Reciprocity",value = TRUE),
                         checkboxInput("bool_weigh_age", "Weigh by age",value = TRUE),
                         checkboxInput("bool_weigh_week", "Weigh by week/weekend",value = TRUE),

                         # SPC (optional)
                         conditionalPanel(
                           condition = 'output.panelStatus',
                           checkboxInput("bool_spc", "Include supplemental professional contacts (SPC)",value = TRUE)),
                         
                         # HH members (optional)
                         conditionalPanel(
                           condition = 'output.panelStatusHome',
                           checkboxInput("bool_hhmember_selection", "Set contacts at 'Home' with non-household members as 'leisure contacts'",value = FALSE)),
                         
                         checkboxInput("bool_location", "Include all locations",value = TRUE),
                         conditionalPanel(
                           condition = "input.bool_location == false",
                           checkboxGroupInput('cnt_location',
                                              label = 'Included locations',
                                              choices = opt_location,
                                              selected = opt_location))
                        ),
                tabPanel("Model parameters",
                         
                         airDatepickerInput('date_range','Period',range = TRUE, value=c(Sys.Date(), Sys.Date()+180),view = "months"),
                         checkboxInput('seasonality','Seasonality',value = FALSE),
                         conditionalPanel(condition='input.seasonality == true',
                                          sliderInput('seasonality_amplitude','Amplitude', min=0, max=0.9, value=0)),
                         # sliderInput('sim_time','Simulation time (in months)',min=2,max=12,value=6),
                         sliderInput('R0','Reproduction Number (R0)',min=0.5,max=3,value=2),
                         sliderInput('age_2_vac_agreed','Agreed for Vaccination (in %) of 18-40',min=0,max=90,value=70),
                         sliderInput('age_3_vac_agreed','Agreed for Vaccination (in %) of 40-60',min=0,max=90,value=70),
                         sliderInput('age_4_vac_agreed','Agreed for Vaccination (in %) of 60+',min=0,max=90,value=70),
                         # uiOutput('age_2_vaccinated'),
                         # uiOutput('age_3_vaccinated'),
                         # uiOutput('age_4_vaccinated')),
                         checkboxInput("preexisting_memory", h5(HTML("<b>Pre-existing immune memory/ <br/> vaccinated population</b>"))),
                         conditionalPanel(condition='input.preexisting_memory==true',
                                          # sliderInput('age_2_pre_vaccinated','18-40 already vaccinated',min=0,max=90,value=0),
                                          # sliderInput('age_3_pre_vaccinated','40-60 already vaccinated',min=0,max=90,value=0),
                                          # sliderInput('age_4_pre_vaccinated','60+ already vaccinated',min=0,max=90,value=0))),
                                          uiOutput('age_2_vaccinated'),
                                          uiOutput('age_3_vaccinated'),
                                          uiOutput('age_4_vaccinated'))),

                tabPanel("Vaccination",
                         helpText(h3('First Vaccination',style='color:#000000;')),
                         helpText(h4('No vaccination of Age group 0-18',style='color:#000000;')),
                         sliderInput('age_2','Age group 18-40',min=0,max=500000,value=0),
                         sliderInput('age_3','Age group 40-60',min=0,max=500000,value=0),
                         sliderInput('age_4','Age group 60+',min=0,max=500000,value=0),
                         helpText(h3('Second Vaccination',style='color:#000000;')),
                         sliderInput('gap','Interval between vaccine first and second dose',min=30,max=120,value=30),
                         helpText(h4('No vaccination of Age group 0-18',style='color:#000000;')),
                         sliderInput('f_age_2','Age group 18-40',min=0,max=500000,value=0),
                         sliderInput('f_age_3','Age group 40-60',min=0,max=500000,value=0),
                         sliderInput('f_age_4','Age group 60+',min=0,max=500000,value=0),
                         ), 
                
                tabPanel("Options", 
                         checkboxInput("bool_age_range", "Age range: sample at random",value = TRUE),
                         checkboxInput("bool_age_missing", "Missing contact age: remove participant",value = FALSE),
                         checkboxInput("bool_matrix_limit", "Specify the color scale of the social contact matrix?",value = FALSE),
                         conditionalPanel(condition = "input.bool_matrix_limit == true",
                                          numericInput(inputId="ui_scale_max",
                                                       label = "Color scale upper limit (≥1)",
                                                       value = NA,
                                                       min   = 1))
                ),
                tabPanel("Measures", checkboxInput("bool_physical_distancing","Include physical distancing"),
                         checkboxInput("bool_apply_measures","Apply Measures"),
                                       conditionalPanel(
                                         condition = "input.bool_physical_distancing == true && input.bool_apply_measures == true",
                                         # sliderInput("measures_start","Apply measures after (in months)",min=0,max=12,value=1),
                                         uiOutput('home_measures_start'),
                                         uiOutput('work_measures_start'),
                                         uiOutput('school_measures_start'),
                                         uiOutput('transport_measures_start'),
                                         uiOutput('leisure_measures_start'),
                                         uiOutput('other_measures_start'),
                                         sliderInput("cnt_reduction_home","Reduce 'home' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_work","Reduce 'work' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_school","Reduce 'school' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_transport","Reduce 'transport' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_leisure","Reduce 'leisure' contacts (%)",min=0,max=100,value=0),
                                         sliderInput("cnt_reduction_otherplace","Reduce 'otherplace' contacts (%)",min=0,max=100,value=0))
                                         
                                         
                         )
                # tabPanel("Transmission",checkboxInput("bool_transmission_param", "Age-specific transmission",value = FALSE),
                #                         conditionalPanel(
                #                           condition = "input.bool_transmission_param == true",
                #                           uiOutput("sliders_susceptibility"),
                #                           uiOutput("sliders_infectiousness"))
                #         )
                ),
    actionButton('reset','Reset',icon('refresh'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton('simulate','Simulate',icon('refresh'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
   
    
    hr(),
    helpText('DOWNLOAD'),
    downloadButton('download_matrix',"Download matrix (.csv)", style = "width:99%;"),
    downloadButton('download_all',"Download results (.RData)",style = "width:99%;"),
    downloadButton('download_ode_data',"Download simulation results (.csv)", style = "width:99%;"),

    # # add version and link to project website
    # headerPanel(""),
    # uiOutput("project_website"),
    # if(!bool_is_comix_ui){
    #   uiOutput("socrates_website_comix")
    # },
    # helpText('SOCRATES',version_id)

  ),
  
  

  mainPanel(
    
    # allways show matrix with contact rates
    fluidRow(splitLayout(cellWidths = c("40%", "30%","30%"),withSpinner(plotOutput('plot_cnt_matrix',width = "450px", height = "300px")),
                         withSpinner(plotOutput('tot_inf',width = "350px", height = "350px")),
                         withSpinner(rbokehOutput('tot_dead',width = "350px", height = "350px")))),
    
    # previous second panel
    # fluidRow(
    #   box(
    #     title = "", 
    #     width = 10,
    #     background = "red",
    #     actionButton("logscale", "log"),
    #     withSpinner(plotOutput('age_inf',width = "1150px", height = "250px"))
    #   )),
    #                                                                                           
    
    fluidRow(splitLayout(cellWidths = c("100%"),withSpinner(plotlyOutput('age_inf_plotly',width = "1200px", height = "500px")))),
    
    # fluidRow(splitLayout(cellWidths = c("90%", "10%"),withSpinner(plotOutput('age_inf',width = "1150px", height = "250px")),
    #                      actionButton("logscale", "log"))),
             # conditionalPanel(
             #   condition = "!input.logscale % 2 == 0",plotOutput('age_inf',width = "1150px", height = "250px"),
             #   condition = "input.logscale % 2 == 0",plotOutput('age_inf_log',width = "1150px", height = "250px"))),
             #   
             # 


 
    # fluidRow(checkboxInput("logscale", "logscale",value = TRUE),plotOutput('age_inf',width = "1150px", height = "250px")),
    
                
    
    fluidRow(withSpinner(plotlyOutput('age_dead',width = "1200px", height = "500px"))),
    
    
    
    
   
    
    
    
    # use tabs
    tabsetPanel(type = "tabs",
                id='tabs_results',
                tabPanel("All results", 
                         #verbatimTextOutput("social_contact_analysis"),
                         verbatimTextOutput("social_contact_analysis_only_matrix"),
                         #helpText('Please note that contacts whose ages are given as a range but not exactly will have their age set to the mid-point of the range.')
                         #helpText('Please note that contacts whose ages are not given exactly will have by default their age sampled at random from the given range. If you want to use the midpoint, deselect "Age range: sample at random" [update 2020-10-05].')
                         #helpText('The SOCRATES platform has been updated since the publication in BMC Research Notes. See the "Updates" tab for more info.')
                ),
                tabPanel("Model",img(src='vacc_Schematic.png',height = 500, width = 1000)),
                # tabPanel("Matrix per capita", 
                #          helpText('This per capita matrix presents the daily contact rate for every individual of an age group with all other individuals in the population.'),
                #          plotOutput('plot_cnt_matrix_per_capita',width = "80%", height = "300px")),
                tabPanel("Contact rates", 
                         plotOutput('plot_mean_number_contacts',width = "80%", height = "300px"))
                # tabPanel("Participants", 
                #          helpText('Brief summary of participant data:'),
                #          dataTableOutput('table_participants')),
                # tabPanel("Weights",     
                #          uiOutput("project_website_weights"),
                #          dataTableOutput('table_weights')),
                # tabPanel("Data sets",
                #          uiOutput("project_website_data"),
                #          dataTableOutput("social_contact_data")),
                # tabPanel("About CoMix",
                #         includeMarkdown("doc/doc_comix.md")),
                # tabPanel("Updates",
                #          includeMarkdown("doc/doc_updates.md"))
        )
  )
))
