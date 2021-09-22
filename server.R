#___________________________________________________________________________
# This file is part of the SOcial Contact RATES (SOCRATES) modelling project
# 
# => MAIN SCRIPT FOR THE R-SHINY SERVER
#
#  Copyright 2020, SIMID, UNIVERSITY OF ANTWERP & HASSELT UNIVERSITY
#___________________________________________________________________________

# load all functions and packages
# this is done automatically when running the App

# Define server logic required to plot various output
shinyServer(function(input, output, session) {
  
  ## CoMix: hide some tabs
  if(bool_is_comix_ui){
    hideTab(inputId = "distancing_transmission", target = "Distancing")
    hideTab(inputId = "distancing_transmission", target = "Transmission")
  } else{
    hideTab(inputId = "tabs_results", target = "About CoMix")    
  }
  
  # already vaccinated

  output$age_2_vaccinated<-renderUI({
    sliderInput('age_2_pre_vaccinated','18-40 already vaccinated',min=0,max=input$age_2_vac_agreed,value=10)
  })

  output$age_3_vaccinated<-renderUI({
    sliderInput('age_3_pre_vaccinated','40-60 already vaccinated',min=0,max=input$age_3_vac_agreed,value=10)
  })

  output$age_4_vaccinated<-renderUI({
    sliderInput('age_4_pre_vaccinated','60+ already vaccinated',min=0,max=input$age_4_vac_agreed,value=10)
  })

  
  output$home_measures_start<-renderUI({
    sliderInput('home_measures_start','Apply Home measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  output$work_measures_start<-renderUI({
    sliderInput('work_measures_start','Apply Work measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  output$school_measures_start<-renderUI({
    sliderInput('school_measures_start','Apply School measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  output$transport_measures_start<-renderUI({
    sliderInput('transport_measures_start','Apply Transport measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  output$leisure_measures_start<-renderUI({
    sliderInput('leisure_measures_start','Apply leisure measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  output$other_measures_start<-renderUI({
    sliderInput('other_measures_start','Apply Other measures after (in Days)',min=0,max=as.numeric(input$date_range[2]-input$date_range[1]),value=0)
  })
  
 
  
  ## Wave input ----
  ## list to store reactive values
  values <- reactiveValues()
  
  # observeEvent(input$date_range[1], {
  #   if (is.null(input$date_range[2])){
  #     input$date_range[2]<-input$date_range[1]+180
  #   }
  #   # # If end date is earlier than start date, update the end date to be the same as the new start date
  #   # if (input$date_range[2] < input$date_range[1]) {
  #   #   end_date = input$date_range[1]
  #   # }
  #   updateDateRangeInput(session,"date_range", start=input$date_range[1], end=input$date_range[2], min=input$date_range[1] )
  # })

  # The dynamic input definition
  output$dynamicWaveInput <- renderUI({
    
    # This input exists if the `country` survey contains wave info
    if (opt_country_admin$has_waves[opt_country_admin$name == input$country]) {
      selectInput(inputId = 'wave_dynamic',
                  label   = 'Wave',
                  choices = opt_waves)
    } else {
      return(NULL)
    }
    
  })
  
  ## this bit fixes the issue
  ## force the dynamic dynamicWaveInput to reset if the country survey has no waves
  observe({
    if(opt_country_admin$has_waves[opt_country_admin$name == input$country]) {
      values$w_dynamic <- input$wave_dynamic
    } else {
      values$w_dynamic <- opt_waves[[1]]
    }
  })
  
  
    
  
  
  # # Setup ####
  # # create memory variable for the transmission param sliders
  # bool_update <- reactiveValues(age_breaks_text = '')
  
  # create bool to show the SPC checkbox
  output$panelStatus <- reactive({
    opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  # create bool to show the 'home member' checkbox
  output$panelStatusHome <- reactive({
    opt_country_admin$has_hhmember_cnt_data[opt_country_admin$name == as.character(input$country)]
  })
  outputOptions(output, "panelStatusHome", suspendWhenHidden = FALSE)

  # Update UI panel(s) ####
  observe({
  
    # if the SCP checkbox is not shown (nor used), set as "TRUE" 
    # MESSAGE ==>> "SCP are never excluded if the checkbox is not shown"
    show_spc_panel <- opt_country_admin$has_suppl_professional_cnt_data[opt_country_admin$name == as.character(input$country)]
    if(!show_spc_panel){
      updateCheckboxInput(session,"bool_spc", value = TRUE)
    }
    
    # if the HH-member checkbox is not shown (nor used), set as "FALSE"
    # MESSAGE ==>> "selection is never excluded if the checkbox is not shown"
    show_hhmember_panel <- opt_country_admin$has_hhmember_cnt_data[opt_country_admin$name == as.character(input$country)]
    print(show_hhmember_panel)
    if(!show_hhmember_panel){
      updateCheckboxInput(session,"bool_hhmember_selection", value = FALSE)
    }
    
    # Update whether the location-specific checkboxes are displayed
    if(input$bool_location)
      updateCheckboxGroupInput(session, "cnt_location", selected = opt_location)
    
    # if physical distancing slider are not displayed => set all to 0
    if(!input$bool_physical_distancing){
      updateSliderInput(session, "cnt_reduction_home", value = 0)
      updateSliderInput(session, "cnt_reduction_work", value = 0)
      updateSliderInput(session, "cnt_reduction_school", value = 0)
      updateSliderInput(session, "cnt_reduction_transport", value = 0)
      updateSliderInput(session, "cnt_reduction_leisure", value = 0)
      updateSliderInput(session, "cnt_reduction_otherplace", value = 0)
    }
      
    # Update 'daytype' input, by default 'all contacts' to prevent warnings/errors  
    # Options can be extended based on data availability
    flag_country <- input$country == opt_country_admin$name
    if(opt_country_admin$has_holiday_data[flag_country]){
      updateSelectInput(session,"daytype", choices = opt_day_type, selected = input$daytype)
    } else if(opt_country_admin$has_dayofweek_data[flag_country]) {
      if(input$daytype %in% opt_day_type[1:3]){
        updateSelectInput(session,"daytype", choices = opt_day_type[1:3], selected = input$daytype)
      } else{
        updateSelectInput(session,"daytype", choices = opt_day_type[1:3])
      }
    } else {
      updateSelectInput(session,"daytype", choices = opt_day_type[1])
    }
    
    # update contact duration options
    if(opt_country_admin$has_cnt_duration_data[flag_country]){
      updateSelectInput(session,"duration", choices = opt_duration, selected = input$duration)
    } else{
      updateSelectInput(session,"duration", choices = opt_duration[1], selected = opt_duration[1])
    }
    
    # update contact intensity options
    if(opt_country_admin$has_cnt_touch_data[flag_country]){
      updateSelectInput(session,"touch", choices = opt_touch, selected = input$touch)
    } else{
      updateSelectInput(session,"touch", choices = opt_touch[1], selected = opt_touch[1])
    }
    
    # update wave  options
    if(opt_country_admin$has_waves[flag_country]){
      updateSelectInput(session,"wave_dynamic", choices = opt_waves[1:(opt_country_admin$num_waves[flag_country]+1)], selected = input$wave_dynamic)
    } else {
      updateSelectInput(session,"wave_dynamic", choices = opt_waves[1], selected = opt_waves[1])
    }
    
     
      
      # get age groups
      age_groups <- parse_age_values(opt_age_breaks)
      num_age_groups <- length(age_groups)
      age_groups_label <- paste0('[',age_groups,'-',c(age_groups[-1],'+'),')')
      age_groups_label[num_age_groups] <- paste0(age_groups[num_age_groups],'+')
      
      # update sliders: susceptibility
      output$sliders_susceptibility <- renderUI({
        
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_susceptibility",i),
                      label = paste('Susceptibility:',age_groups_label[i]),
                      min = 0, max = 2, value = 1,step=0.1)
        })
      })
      
      # update sliders: infectiousness
      output$sliders_infectiousness <- renderUI({
        
        lapply(seq(age_groups), function(i) {
          sliderInput(inputId = paste0("s_infectiousness",i),
                      label = paste('infectiousness:',age_groups_label[i]),
                      min = 0, max = 2, value = 1,step=0.1)
        }) # end: lapply
      }) # end= renderUI
      
    
  }) # end: observe
 
  ## Update results ####
  
  observeEvent(input$simulate,{

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'In progress...',
                 detail = 'Please wait.',
                )
    
    # combine general options
    features_select <- c(ifelse(is.null(input$bool_reciprocal),FALSE,input$bool_reciprocal),
                        input$bool_weigh_age,
                        input$bool_weigh_week,
                        input$bool_age_range,
                        input$bool_age_missing,
                        input$bool_spc,
                        input$bool_hhmember_selection)

    # parse transmission parameters
    age_susceptibility_text    <- parse_input_list(input,'s_susceptibility')
    age_infectiousness_text    <- parse_input_list(input,'s_infectiousness')
    
    
    # combine contact reductions
    # TODO: use notation from opt_location (capitals etc.)
    cnt_reduction <- data.frame(Home       = input$cnt_reduction_home/100,
                                Work       = input$cnt_reduction_work/100,
                                School     = input$cnt_reduction_school/100,
                                Transport  = input$cnt_reduction_transport/100,
                                Leisure    = input$cnt_reduction_leisure/100,
                                Otherplace = input$cnt_reduction_otherplace/100)
    
    cnt_reduction_null <- data.frame(Home       = 0,
                                Work       = 0,
                                School     = 0,
                                Transport  = 0,
                                Leisure    = 0,
                                Otherplace = 0)
    

    # fix for wave
    if(is.null(values$w_dynamic)) values$w_dynamic <- opt_waves[[1]]
    
    # run social contact analysis
    out <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_location = input$cnt_location,
                                       cnt_matrix_features = opt_matrix_features[features_select],
                                       age_breaks_text     = opt_age_breaks,
                                       weight_threshold     = weight_threshold,
                                       bool_transmission_param = FALSE, # input$bool_transmission_param,
                                       age_susceptibility_text = age_susceptibility_text,
                                       age_infectiousness_text = age_infectiousness_text,
                                       cnt_reduction           = cnt_reduction,
                                       wave                    = values$w_dynamic)
    
    out_no_measures <- run_social_contact_analysis(country      = input$country,
                                       daytype      = input$daytype,
                                       touch        = input$touch,
                                       duration     = input$duration,
                                       gender       = input$gender,
                                       cnt_location = input$cnt_location,
                                       cnt_matrix_features = opt_matrix_features[features_select],
                                       age_breaks_text     = opt_age_breaks,
                                       weight_threshold     = weight_threshold,
                                       bool_transmission_param = FALSE, # input$bool_transmission_param,
                                       age_susceptibility_text = '1,1,1,1',
                                       age_infectiousness_text = '1,1,1,1',
                                       cnt_reduction           = cnt_reduction_null,
                                       wave                    = values$w_dynamic)
    
    
    
    
    # write.csv(out$matrix,'test.csv')
    # 
    # zcm11<-out$matrix[1,1]
    # zcm12<-out$matrix[1,2]
    # zcm13<-out$matrix[1,3]
    # zcm14<-out$matrix[1,4]
    # 
    # zcm21<-out$matrix[2,1]
    # zcm22<-out$matrix[2,2]
    # zcm23<-out$matrix[2,3]
    # zcm24<-out$matrix[2,4]
    # 
    # zcm31<-out$matrix[3,1]
    # zcm32<-out$matrix[3,2]
    # zcm33<-out$matrix[3,3]
    # zcm34<-out$matrix[3,4]
    # 
    # zcm41<-out$matrix[4,1]
    # zcm42<-out$matrix[4,2]
    # zcm43<-out$matrix[4,3]
    # zcm44<-out$matrix[4,4]
    
    
    # plot social contact matrix
    output$plot_cnt_matrix <- renderPlot({
      
      scale_max <- ifelse(input$bool_matrix_limit == TRUE ,input$ui_scale_max,NA)
      plot_cnt_matrix(out$matrix,scale_max=scale_max)
      
    })
    
    
    
    # my_fn_1 <- reactive({
    #   input$age_2_pre_vaccinated
    # })
    # 
    # my_fn_2 <- reactive({
    #   input$age_3_pre_vaccinated
    # })
    # 
    # my_fn_3 <- reactive({
    #   input$age_4_pre_vaccinated
    # })
    #          
    
  
    # vaccination
    
    vaccination<-c(input$age_2,input$age_3,input$age_4,input$f_age_2,input$f_age_3,input$f_age_4)
    gap<-input$gap
    
    agreed_vac_percent<-c(input$age_2_vac_agreed,input$age_3_vac_agreed,input$age_4_vac_agreed)/100
    
   
    
    ### contact reduction tracing
    if (!is.null(input$home_measures_start)){
      delay<-c(input$home_measures_start,
               input$work_measures_start,
               input$school_measures_start,
               input$transport_measures_start,
               input$leisure_measures_start,
               input$other_measures_start)
      
      measures<-data.frame(location=c('Home','Work','School','Transport','Leisure','Otherplace'),
                           value=c(input$cnt_reduction_home/100,
                                   input$cnt_reduction_work/100,
                                   input$cnt_reduction_school/100,
                                   input$cnt_reduction_transport/100,
                                   input$cnt_reduction_leisure/100,
                                   input$cnt_reduction_otherplace/100),
                           delay=delay)
      
      
      
      
      measures<-measures[order(delay),]
      measures<-measures[which(!measures$value==0),]
      seq1<-unique(measures$delay)
      
      soe<-sapply(1:length(seq1), function (x) measures[which(measures$delay==seq1[x]),1])
      
      print(soe)
      
      m_location<-sapply(1:length(soe), function(x) paste(soe[[x]], collapse ='-'))
      
      cnt_reduction_sequence<-NULL
      
      for (i in 1:length(soe)){
        cnt_reduction_dummy <- data.frame(Home       = 0,
                                          Work       = 0,
                                          School     = 0,
                                          Transport  = 0,
                                          Leisure    = 0,
                                          Otherplace = 0)
        for (j in 1:length(soe[[i]])){
          
          cnt_reduction_dummy[1,colnames(cnt_reduction_dummy)==soe[[i]][j]]<-measures[measures$location==soe[[i]][j],2]
          
        }
        cnt_reduction_sequence<-rbind(cnt_reduction_sequence,cnt_reduction_dummy)
        
        
        
        
      }
      print(cnt_reduction_sequence)
      cnt_reduction_sequence<-cumsum(cnt_reduction_sequence)
      cnt_reduction_sequence$Delay<-seq1
      cnt_reduction_sequence$Flag<-m_location
      
      print(cnt_reduction_sequence)
      
      measures_mat<-sapply(1:dim(cnt_reduction_sequence)[1], function (x) run_social_contact_analysis(country      = input$country,
                                                                                                      daytype      = input$daytype,
                                                                                                      touch        = input$touch,
                                                                                                      duration     = input$duration,
                                                                                                      gender       = input$gender,
                                                                                                      cnt_location = input$cnt_location,
                                                                                                      cnt_matrix_features = opt_matrix_features[features_select],
                                                                                                      age_breaks_text     = opt_age_breaks,
                                                                                                      weight_threshold     = weight_threshold,
                                                                                                      bool_transmission_param = FALSE, # input$bool_transmission_param,
                                                                                                      age_susceptibility_text = age_susceptibility_text,
                                                                                                      age_infectiousness_text = age_infectiousness_text,
                                                                                                      cnt_reduction           = cnt_reduction_sequence[x,c(1:6)],
                                                                                                      wave                    = values$w_dynamic)
      )
      
      print(measures_mat[1,])
      
      
    }
    
    
    
    # measures
    
    
    if (input$bool_physical_distancing == TRUE && input$bool_apply_measures == TRUE){
      cmat_before_measures=out_no_measures$matrix
      cmat_after_measures=measures_mat
      delay_measures=cnt_reduction_sequence
      # cmat_after_measures=out$matrix
      # delay_measures=input$measures_start
    }
    else {
      cmat_before_measures=out_no_measures$matrix
      cmat_after_measures=out_no_measures$matrix
      delay_measures=Inf
    }
    
    # ode data
    # init_cond<-init_cond_fn(input$country,values$a,values$b,values$c)
    # print(init_cond)
    

    
    age_ratio<-unlist(demography[input$country == rownames(demography),7:10],use.names = FALSE)
    print(age_ratio)
    
    
    if (input$preexisting_memory==TRUE){
      # imm<-c(input$age_2_pre_vaccinated/100,input$age_3_pre_vaccinated/100,input$age_4_pre_vaccinated/100)
      # init_cond<-init_cond_fn(input$country,imm[1],imm[2],imm[3])
      
      if (is.null(input$age_2_pre_vaccinated)){
        imm1 <- 0
      }
      else{imm1<-input$age_2_pre_vaccinated/100}
      
      if (is.null(input$age_3_pre_vaccinated)){
        imm2 <- 0
      }
      else{imm2<-input$age_3_pre_vaccinated/100}
      
      if (is.null(input$age_4_pre_vaccinated)){
        imm3 <- 0
      }
      else{imm3<-input$age_4_pre_vaccinated/100}
      
      imm<-c(imm1,imm2,imm3)
      
      
    }
    else{ 
      imm<-c(0,0,0)
      }
    
    
    print(imm)
    
    #   observe({
    #   imm<-c(input$age_2_pre_vaccinated/100,input$age_3_pre_vaccinated/100,input$age_4_pre_vaccinated/100)
    #   
    #   
    #   values$init_cond<-init_cond
    #   
    #   
    # })
    #   
    #   
    #   observe({
    #     print(values$init_cond)
    #   })
    #   
      
    if (input$custom_city){
      pop_custom<-c(input$custom_city_0_18,input$custom_city_18_40,input$custom_city_40_60,input$'custom_city_60+')
      init_cond<-init_cond_custom(pop_custom,imm[1],imm[2],imm[3])
      init_pop<-pop_custom
    }
    else {
      init_cond<-init_cond_fn(input$country,imm[1],imm[2],imm[3])
      init_pop<-unlist(demography[input$country == rownames(demography),2:5],use.names = FALSE)
    }
    
    if (input$date_range){
      sim_time<-as.numeric(input$date_range[2]-input$date_range[1])
    }
    
    
    # seasonality
    
    if (input$seasonality){
      dg<-as.Date('2020-01-01')
      d0<-input$date_range[1]
      
      sea_first<-abs(as.numeric(d0 -dg)) %% 365
      sea_sec<-1:sim_time  # (sea_first:(sea_first+sim_time))%%365
      seasonal_factor_t=1+input$seasonality_amplitude*sin((2*pi*(sea_sec+sea_first)/365) + pi/2)
      seasonal_factor_0=1+input$seasonality_amplitude*sin((2*pi*sea_first/365) + pi/2)
      seasonal_factor <- seasonal_factor_t/seasonal_factor_0
      plot(seq.Date(input$date_range[1],input$date_range[1]+length(seasonal_factor)-1,1),seasonal_factor)
      
    }
    else {
      seasonal_factor=rep(1,sim_time+1)
    }
    
    
    print(seasonal_factor)
    print(length(seasonal_factor))
    
    dy_data<-ode_dynamics(country=input$country,init_cond=init_cond,init_pop=init_pop,seasonality=seasonal_factor,
                          age_ratio=age_ratio,cnt_matrix_features=opt_matrix_features[features_select],time=sim_time,R0=input$R0,
                       cmat_before_measures = cmat_before_measures, cmat_after_measures = cmat_after_measures,
                       vaccination=vaccination,agreed_vac_percent=agreed_vac_percent,gap=gap, delay_measures=delay_measures)
   
      
   
      # print(dy_data())
      # print(c(values$a,values$b,values$c))
  
    
    # observe({
    #   init_cond_fn(input$country,input$age_2_pre_vaccinated/100,input$age_3_pre_vaccinated/100,input$age_4_pre_vaccinated/100)
    # })
    
    
    # dy_data<-ode_dynamics(country=input$country,init_cond=init_cond,age_ratio=age_ratio,cnt_matrix_features=opt_matrix_features[features_select],time=input$sim_time,R0=input$R0,
    #                       cmat_before_measures = cmat_before_measures, cmat_after_measures = cmat_after_measures,
    #                       vaccination=vaccination,agreed_vac_percent=agreed_vac_percent,gap=gap, delay_measures=delay_measures)

    
    
    output$tot_inf <- renderPlot({
      
      dy_data$Day<-seq.Date(as.Date(input$date_range[1])+1, as.Date(input$date_range[2]), "days")
      print(head(dy_data))
      plot_tot_inf(dy_data)
      
      
    })

    output$tot_dead <- renderRbokeh({
      plot_tot_dead(dy_data)
    })

    
    
    
    # plot ode
    
    # output$age_inf <- renderPlot({
    #   
    #   if (input$logscale %% 2 == 0){
    #     plot_age_inf(dy_data)
    #   }
    #   else{
    #     plot_age_inf_log(dy_data)
    #   }
    #   
    # })
    
    output$age_inf_plotly <- renderPlotly({
      
      sz=dim(dy_data)[1]
      dy_data$seasonality<-seasonal_factor[1:sz]
      
      plot_age_inf_plotly(dy_data)
      
      # # chart option buttons
      # scale_axis <- list(
      #   type = "buttons",
      #   direction = "right",
      #   xanchor = 'center',
      #   yanchor = "top",
      #   pad = list('r'= 0, 't'= 10, 'b' = 10),
      #   x = 0.5,
      #   y = 1.27,
      #   buttons = list(
      #     
      #     list(method = "restyle",
      #          args = list("type", "linear"),
      #          label = "Linear"),
      #     
      #     list(method = "restyle",
      #          args = list("type", "log"),
      #          label = "log")
      #     
      #   ))
      # 
      # # color option buttons  
      # plot_types <- list(
      #   type = "buttons",
      #   direction = "right",
      #   xanchor = 'center',
      #   yanchor = "top",
      #   pad = list('r'= 0, 't'= 10, 'b' = 10),
      #   x = 0.5,
      #   y = 1.,
      #   buttons = list(
      #     
      #     list(method = "restyle",
      #          args = list("colorscale", "Rainbow"),
      #          label = "Rainbow"),
      #     
      #     list(method = "restyle",
      #          args = list("colorscale", "Jet"),
      #          label = "Jet"),
      #     
      #     list(method = "restyle",
      #          args = list("colorscale", "Earth"),
      #          label = "Earth"),
      #     
      #     list(method = "restyle",
      #          args = list("colorscale", "Electric"),
      #          label = "Electric")
      #   ))
      
      
      
      
    })
    
    # output$age_inf_log <- renderPlot({
    #   plot_age_inf_log(dy_data)
    # })
    
    output$age_dead <- renderPlot({
      plot_age_dead(dy_data)
    })
    
    # output$tot_dead <- renderPlot({plot(ode_time,tot_dead)})
    
    # plot social contact matrix per capita
    output$plot_cnt_matrix_per_capita <- renderPlot({
      if('matrix_per_capita' %in% names(out)){
        plot_cnt_matrix(mij = out$matrix_per_capita, 'per capita')
      } else{
        plot(0,col=0,axes=F,xlab='',ylab='')
        text(1,0,"MISSING DATA ISSUE...\nUNABLE TO PLOT THE MATRIX")    
      }
    })
    
    # plot mean number of social contacts
    output$plot_mean_number_contacts <- renderPlot({
      plot_mean_number_contacts(mij = out$matrix)
    })
    
    # print results
    output$social_contact_analysis <- renderPrint({
      # exclude results with separate tab
      list_exclude <- c('weights','participants','participants.weights','meta_data')
      out[!names(out) %in% list_exclude]
    })
    
    # print results
    output$social_contact_analysis_only_matrix <- renderPrint({
      # exclude results with separate tab
      list_include <- c('matrix','demography','survey_period')
      out[names(out) %in% list_include]
    })
    
    # download matrix
    output$download_matrix <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_matrix.csv")
      },
      content = function(file) {
        
        cnt_matrix           <- unlist(out$matrix)
        colnames(cnt_matrix) <- paste0('contact_',colnames(cnt_matrix))
        cnt_matrix           <- cbind(age_group=row.names(out$matrix),cnt_matrix)
        
        write.table(cnt_matrix, file,sep=',',row.names=F)
      }
    )
    
    # download all
    output$download_all <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_social_contact_analysis.RData")
      },
      content = function(file) {
        download_contact_matrices(  country      = input$country,
                                    daytype      = input$daytype,
                                    touch        = input$touch,
                                    duration     = input$duration,
                                    gender       = input$gender,
                                    cnt_location = input$cnt_location,
                                    cnt_matrix_features = opt_matrix_features[features_select],
                                    age_breaks_text     = opt_age_breaks,
                                    weight_threshold     = weight_threshold,
                                    bool_transmission_param = input$bool_transmission_param,
                                    age_susceptibility_text = age_susceptibility_text,
                                    age_infectiousness_text = age_infectiousness_text,
                                    cnt_reduction           = cnt_reduction,
                                    wave                    = values$w_dynamic,
                                    filename                = file)
      }
    )
    
    # download simulation result
    output$download_ode_data <- downloadHandler(
      filename = function(file) {
        paste0(format(Sys.time(),'%Y%m%d%H%M%S'),"_simulation_result.csv")
      },
      content = function(file) {
        
        result           <- download_ode_data(dy_data)
        write.table(result, file,sep=',',row.names=F)
      }
    )
    
    # create url link
    output$project_website <- renderUI({
      tagList("More info:", url)
    })
    
    # create url link
    output$project_website_data <- renderUI({
      tagList("More info on the social contact data initiative 
              and links to the ZENODO repositories are provided at", url,". Info about the Supplementary Professional Contacts 
              (SPC) for the French dataset ",url_doc_spc)
    })
    # add social contact data info
    output$social_contact_data <- renderDataTable({
    data_table_description
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
    # create url link
    output$project_website_weights <- renderUI({
      tagList('Based on the selected options, we calculate participant weights to account for age and the number of observations during week (5/7) and weekend (2/7) days. 
                         The United Nation’s World Population Prospects are used as reference. Weights are constraint to a maximum of 3 to limit the influence of single participants.
                         ',url_doc_weights)
    })
    # add weights table
    output$table_weights <- renderDataTable({
      if(any(is.null(out$participants.weights))){
        data.table('No weights selected' = '')
      } else {
        out$participants.weights
      }
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
     ))
    
    # add weights table
    output$table_participants <- renderDataTable({
        out$participants
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '170px', targets = 0))
    ))
    
    print(names(input))
    
  }, ignoreNULL = FALSE)
  
  ######## end of observe event
  
  
  
  observeEvent(input$reset, {
    lists_input<-names(input)
    for (i in 1:length(names(input))){
      shinyjs::reset(lists_input[i])
    }
    
    
  })

  
  # create url link
  output$socrates_website <- renderUI({
    tagList(url_socrates)
  })
  
  # create url link
  output$socrates_website_data <- renderUI({
    tagList("The goal of the CoMix project is to measure social distancing during the COVID-19 pandemic. This tool is part of the", url_socrates)
  })
  
  # create url link
  output$socrates_website_comix <- renderUI({
    tagList("Also have look at", url_socrates_comix)
  })
  
})