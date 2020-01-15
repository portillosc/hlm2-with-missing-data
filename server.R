library('shiny')
library("foreign")

source("functions.R")

server <- function(input, output, session) {
  # set the size options for acceptable input
  options(shiny.maxRequestSize=30*1024^2)
  options(max.print=1000000)
  
  # initialize internal variables
  reactive_values_list <- reactiveValues(cur="", 
                                         model_equation_matrix=data.frame(L1=character(),
                                                                          Equation=character(),
                                                                          random_effect=character(),
                                                                          stringsAsFactors = FALSE),
                                         hlmm2_parameters=c(0,1,1,1,1),
                                         df=NULL,
                                         df2=NULL)
  
  # read in df1
  observe({
    req(input$file1)
    
    # switch the tab panel to the L1 data tab
    updateTabsetPanel(session, "inTabset",
                      selected = 'L1_data_tab'
    )
    
    tryCatch({
      reactive_values_list$df <- return_data(input$file1$datapath,
                                             input$input_file_format)
      output$error_text_1<-renderUI({
        return(NULL)
      })
    },
    error=function(e){
      reactive_values_list$df<-NULL
      output$error_text_1<-renderUI({
        HTML('<h4>Upload a file that matches the selected file format.</h4>')
      })
    })
  })
  # read in df2
  observe({
    req(input$file2)
    
    # switch the tab panel to the L1 data tab
    updateTabsetPanel(session, "inTabset",
                      selected = 'L2_data_tab'
    )
    
    tryCatch({
      reactive_values_list$df2 <- return_data(input$file2$datapath,
                                              input$input_file_format)
      output$error_text_2<-renderUI({
        return(NULL)
      })
    },
    error=function(e){
      reactive_values_list$df2<-NULL
      output$error_text_2<-renderUI({
        HTML('<h4>Upload a file that matches the selected file format.</h4>')
      })
    })
  })
  
  ###############################################################################################
  # Creating UI elements
  
  # Render the table for the L1 dataset
  output$L1_table_display <- renderTable({
    req(reactive_values_list$df)
    
    # return the first 1000 rows
    return(head(reactive_values_list$df,n=1000))
  })
  # Render the contents of the L2 table
  output$L2_table_display <- renderTable({
    req(reactive_values_list$df2)
    
    # return the first 1000 rows
    return(head(reactive_values_list$df2,n=1000))
  })
  # create the selection box for the L2id
  output$select_L2_id <- renderUI({
    req(reactive_values_list$df)
    req(reactive_values_list$df2)
    req(input$input_file_format)
    
    selectInput('L2_id_selection', 'Select L2 ID', c(""))
  })
  # create the title for the L1 model specification
  output$L1_title <- renderUI({
    req(input$L2_id_selection)
    tags$h3('L1 Model:')
  })
  # create the response variable selection input
  output$select_response <- renderUI({
    req(input$L2_id_selection)
    selectInput('response_selection', 'Select Outcome Variable', c(""))
  })
  # create the L1 covariates selection
  output$select_L1_covariates <- renderUI({
    req(input$response_selection)
    selectInput('L1_covariate_selection', 'Select L1 Covariates', c(""), multiple = TRUE)
  })
  # create the title for the L2 model specification
  output$L2_Title <- renderUI({
    req(input$response_selection)
    tags$h3('L2 Model:')
  })
  # create the L1 covariate radio buttons
  output$select_L1_covariates_radio_buttons <- renderUI({
    req(input$response_selection)
    radioButtons("L1_covariate_radio_buttons", HTML("Select &beta; Coefficient"), append(c("Intercept"), input$L1_covariate_selection))
  })
  # create the L1 covariate random interaction checkboxes
  output$L1_covariate_random_interaction_checkboxes <- renderUI({
    req(input$response_selection)
    L1_random_effects_checkbox("L1_covariate_checkboxes", "Random Effect:",
                               c("Intercept"),
                               selected=c("Intercept"), 
                               colors=rep('#F5F5F5',1))
  })
  # create the L2 covariates selection
  output$select_L2_covariates <- renderUI({
    req(input$response_selection)
    selectizeInput('L2_covariate_selection', 'Select L2 Covariates', c(""), multiple = TRUE)
  })
  # create the title for the multiple imputation section
  output$MI_section_title <- renderUI({
    req(input$response_selection)
    tags$h3('Multiple Imputation:')
  })
  # create the number of imputations input
  output$number_of_imputations <- renderUI({
    req(input$response_selection)
    numericInput('imputation_number','Select Number of Imputations (Optional)',
                 5,
                 min=1,
                 max=100)
  })
  # create the unique seed
  output$imputation_seed <- renderUI({
    req(input$response_selection)
    val <- paste0(sample(LETTERS,20,TRUE),collapse = '')
    textInput('unique_seed','Specify Seed Phrase (Optional)',value=val)
  })
  # create the action button to run the estimation
  output$run <- renderUI({
    req(input$response_selection)
    actionButton('estimate_model',"Estimate HLM2")
  })
  # create the L1 model equation
  output$summary <- renderUI({
    req(reactive_values_list$df)
    req(reactive_values_list$df2)
    req(input$response_selection)
    
    # print the L1 formula
    equation <- return_L1_equation(input$response_selection, input$L1_covariate_selection)
    HTML(equation)
  })
  
  # End creating UI elements
  ###############################################################################################
  
  ###############################################################################################
  # Update UI elements
  
  # update the L2 id selection
  observe({
      req(input$file1)
      req(input$file2)
      
      data <- reactive_values_list$df2
      updateSelectInput(session, "L2_id_selection", choices = names(data), selected = "")
    })
  # update the response variable selection
  observe({
    req(input$L2_id_selection)
    df <- reactive_values_list$df
    
    # remove the selected response variable from the list of L1 covariates
    fullNames <- names(df)
    fullNames <- fullNames[fullNames!=input$L2_id_selection]
    
    updateSelectInput(session, "response_selection",
                      choices = fullNames,
                      selected = "")
  })
  # update the L1 covariate selection
  observe({
    req(input$response_selection)
    
    df <- reactive_values_list$df
    
    # remove the selected response variable from the list of L1 covariates
    fullNames <- names(df)
    fullNames <- fullNames[ fullNames != input$response_selection]
    fullNames <- fullNames[ fullNames != input$L2_id_selection]
    
    updateSelectInput(session, "L1_covariate_selection",
                      choices = fullNames)
  })
  # update the L1 covariate random interaction check boxes
  observe({
    output$L1_covariate_random_interaction_checkboxes <- renderUI({
      req(input$response_selection)
      if(vector_is_empty(input$L1_covariate_checkboxes)){
        L1_random_effects_checkbox("L1_covariate_checkboxes", "Random Effect:",
                                   choices = c('Intercept',input$L1_covariate_selection),
                                   selected=c('Intercept'), 
                                   colors=rep('#F5F5F5',length(c('Intercept',input$L1_covariate_selection))))
      }
      else{
        currently_selected = input$L1_covariate_checkboxes
        currently_selected = currently_selected[currently_selected %in% input$L1_covariate_selection]
        if(!'Intercept' %in% currently_selected){
          currently_selected <- c('Intercept',currently_selected)
        }
        L1_random_effects_checkbox("L1_covariate_checkboxes", "Random Effect:",
                                   choices = c('Intercept',input$L1_covariate_selection),
                                   selected=currently_selected, 
                                   colors=rep('#F5F5F5',length(c('Intercept',input$L1_covariate_selection))))
      }
    })
  })
  # update the L2 covariate selection
  observeEvent({input$L1_covariate_radio_buttons
                input$response_selection
                input$L2_id_selection},{
    req(input$response_selection)
    req(input$L1_covariate_radio_buttons)
    
    print('made it')
    
    reactive_values_list$cur = input$L1_covariate_radio_buttons
    df <- reactive_values_list$df2
    
    if(!nrow(reactive_values_list$model_equation_matrix)==0){
      # assign the contents of the current L1's L2 covariates
      selected <- reactive_values_list$model_equation_matrix[which(reactive_values_list$model_equation_matrix$L1==reactive_values_list$cur), "Equation"]
      
      df <- reactive_values_list$df2
      
      # remove the selected response L2 id from the list of L1 covariates
      fullNames <- names(df)
      
      fullNames <- fullNames[ fullNames != input$L2_id_selection]
      
      if(nchar(selected) == 0){
        # run this if there's nothing selected for the current L1 covariate
        updateSelectizeInput(session, "L2_covariate_selection", choices = fullNames, selected = NULL)
      }
      else{
        corrected_string <- substr(selected,0,nchar(selected)-1)
        split_string <- strsplit(corrected_string, ",")
        
        # convert the list to vector
        split_string <- unlist(split_string)
        updateSelectizeInput(session, "L2_covariate_selection", choices = fullNames, selected = split_string)
      }
    }else{
      # assign the contents of the current L1's L2 covariates
      selected <- reactive_values_list$model_equation_matrix[which(reactive_values_list$model_equation_matrix$L1==reactive_values_list$cur), "Equation"]
      
      df <- reactive_values_list$df2
      
      # remove the selected response L2 id from the list of L1 covariates
      fullNames <- names(df)
      
      fullNames <- fullNames[ fullNames != input$L2_id_selection]
      
      if(nchar(selected) == 0){
        # run this if there's nothing selected for the current L1 covariate
        updateSelectizeInput(session, "L2_covariate_selection", choices = fullNames, selected = NULL)
      }
    }
  })
  # update the model equation matrix for random effects
  observeEvent(input$L1_covariate_checkboxes, {
    req(input$response_selection)
    if(vector_is_empty(input$L1_covariate_checkboxes)){
      reactive_values_list$model_equation_matrix$random_effect <- ""
    }
    else{
      for(i in 1:nrow(reactive_values_list$model_equation_matrix)){
        if(reactive_values_list$model_equation_matrix[i, "L1"] %in% input$L1_covariate_checkboxes){
          reactive_values_list$model_equation_matrix[i, "random_effect"] <- "true"
        }
        else{
          reactive_values_list$model_equation_matrix[i, "random_effect"] <- ""
        }
      }
    }
  },ignoreNULL=FALSE)
  # update the L2eq matrix
  observeEvent({input$L1_covariate_selection
    input$file2}, {
      req(reactive_values_list$df2)
      cov2str = ""
      
      if(!("Intercept" %in% reactive_values_list$model_equation_matrix$L1)){
        de <- data.frame(as.character("Intercept"), as.character(""), as.character("true"), stringsAsFactors = FALSE)
        names(de) <- c("L1","Equation","random_effect")
        reactive_values_list$model_equation_matrix = rbind(reactive_values_list$model_equation_matrix, de)
      }
      for(value in input$L1_covariate_selection){
        if(!(value %in% reactive_values_list$model_equation_matrix$L1)){
          de <- data.frame(as.character(value), "", as.character(""), stringsAsFactors = FALSE)
          names(de) <- c("L1","Equation","random_effect")
          reactive_values_list$model_equation_matrix = rbind(reactive_values_list$model_equation_matrix, de)
        }
      }
      
      # extract the intercept row
      intercept_row <- reactive_values_list$model_equation_matrix[1,]
      
      # filter out anything that doesnt belong
      reactive_values_list$model_equation_matrix <- reactive_values_list$model_equation_matrix[reactive_values_list$model_equation_matrix$L1 %in% as.character(input$L1_covariate_selection), ]
      
      # add the intercept row back in
      reactive_values_list$model_equation_matrix = rbind(intercept_row, reactive_values_list$model_equation_matrix)
    },ignoreNULL=FALSE)
  # update the "Equation" column in the L2eq matrix
  observeEvent({input$L2_covariate_selection
    input$file2}, {
      req(reactive_values_list$df2)
      if(!vector_is_empty(input$L2_covariate_selection)){
        cov2str = ""
        
        for(value in input$L2_covariate_selection){
          cov2str = paste0(cov2str, as.character(value), ",")
        }
        reactive_values_list$model_equation_matrix[reactive_values_list$model_equation_matrix$L1 == input$L1_covariate_radio_buttons, "Equation"] <- cov2str
      }
      else{
        reactive_values_list$model_equation_matrix[reactive_values_list$model_equation_matrix$L1 == input$L1_covariate_radio_buttons, "Equation"] <- ""
      }
      print(reactive_values_list$model_equation_matrix)
    })
  # reset the equation if the response variable is changed
  observeEvent(input$response_selection, {
    reactive_values_list$model_equation_matrix <- data.frame(L1=character(),
                                                             Equation=character(),
                                                             random_effect=character(),
                                                             stringsAsFactors = FALSE)
    de <- data.frame(as.character("Intercept"), as.character(""), as.character("true"), stringsAsFactors = FALSE)
    names(de) <- c("L1","Equation","random_effect")
    reactive_values_list$model_equation_matrix = rbind(reactive_values_list$model_equation_matrix, de)
    
    updateSelectInput(session, "L2_covariate_selection", choices = "")
  })
  
  # End updating UI elements
  ###############################################################################################
  
  ###############################################################################################
  # Make primary output for user interface
  
  # update the heirarchical equation
  output$heirarchical_equation <- eventReactive({reactive_values_list$model_equation_matrix
  }, {
    req(input$response_selection)
    return(return_hierarchical_equation(input$response_selection, 
                                        reactive_values_list$model_equation_matrix, 
                                        input$L1_covariate_selection))
  })
  
  # end making output for user interface
  ###############################################################################################
  
  
  ##### Handle downloading data tab three #####
  output$download_handler <- downloadHandler(filename <- function() {
    return('output_hlmm2.html')
  },content <- function(file) {
    file.copy('output.html',file)
  })
  
  ##### Handle downloading data tab four #####
  output$download_handler_2 <- downloadHandler(filename <- function() {
    return('output_mhlm2.html')
  },content <- function(file) {
    file.copy('output_2.html',file)
  })
  
  ###### Run the main portion of the program ########
  # 
  
  observeEvent(input$estimate_model, {
    withProgress(message = 'Estimating Model',value=0,{
      full_parts <- 5
      
      # initialize the L1 and L2 datasets
      df <- reactive_values_list$df
      df2 <- reactive_values_list$df2
      
      # get the null/complete data
      missing_complete_summary <- return_L1_L2_nulls(df,df2)
      
      L2_dat <- subset(df2, select=c(input$L2_id_selection))
      
      # compress the contents of the Equation column into a vector
      L2vec <- reactive_values_list$model_equation_matrix[['Equation']]
      L2vec_collapse <- paste0(L2vec,collapse="")
      L2vec_sub <- substr(L2vec_collapse,1,nchar(L2vec_collapse)-1)
      L2vec_split <- strsplit(L2vec_sub,",")
      L2_vec_unique <- unique(L2vec_split)
      
      # check to see if L2_vec has any values in order to make L2.dat
      if(!identical(L2_vec_unique[[1]], character(0))){
        complete_names <- names(missing_complete_summary$L2_completely_observed)
        missing_names <- names(missing_complete_summary$L2_missing)
        is_comp_L2 <- complete_names[complete_names %in% unlist(L2_vec_unique)]
        has_null_L2 <- missing_names[missing_names %in% unlist(L2_vec_unique)]
        output2 <- df2[c(has_null_L2,is_comp_L2)]
        L2_dat <- cbind(L2_dat,output2)
      }
      
      # make L1.dat
      complete_names <- names(missing_complete_summary$L1_completely_observed)
      missing_names <- names(missing_complete_summary$L1_missing)
      is_comp_L1 <- complete_names[complete_names %in% input$L1_covariate_selection]
      has_null_L1 <- missing_names[missing_names %in% input$L1_covariate_selection]
      L1_dat_names <- c(input$L2_id_selection, input$response_selection, has_null_L1, is_comp_L1)
      # this is the L1.dat file before any interactions are processed or random effects are added
      L1_dat <- df[L1_dat_names]
      
      # assign the equation matrix to a variable
      equation_matrix <- reactive_values_list$model_equation_matrix
      print(equation_matrix)
      
      # get any missing covariates involved in interactions
      missing_cov_interactions <- c() # skips the case where the only beta is the intercept
      
      if(nrow(equation_matrix)>1){
        missing_cov_interactions<-return_missing_interactions_vector(equation_matrix,missing_complete_summary$L1_missing,missing_complete_summary$L2_missing)
      }
      
      # calculate 5 parameters
      null_L1 <- missing_complete_summary$L1_missing
      null_L2 <- missing_complete_summary$L2_missing
      parameters <- return_5_parameters(equation_matrix,null_L1,null_L2,L1_dat_names,input$L1_covariate_selection)
      
      ##### handle 2 step imputation #####
      if(length(missing_cov_interactions)>0){
        # trim the missing data
        trimmed <- trim_missing_L1_L2_data(L1_dat,L2_dat,parameters)
        L1_dat <- trimmed$l1
        L2_dat <- trimmed$l2
        
        # save the response column and original order of columns in another variable
        r_name <- names(L1_dat)[2]
        r_column <- L1_dat[,c(2)]
        original_names <- names(L1_dat)
        
        incProgress(1/full_parts, detail = "Writing data out Values")
        
        # remove response column
        L1_dat<-L1_dat[,-c(2)]
        
        add_as_missing<-TRUE
        # adjust the parameters to reflect dropping the response variable
        parameters[3] <- parameters[3] - 1 
        
        # write the tables out to files
        write.table(L1_dat,file = "L1out.dat",sep=" ", col.names = F, row.names = F)
        write.table(L2_dat,file="L2out.dat",sep=" ", col.names = F, row.names = F)
        write.table(L1_dat,file = "L1outoriginal.dat",sep=" ", col.names = F, row.names = F)
        write.table(L2_dat,file="L2outoriginal.dat",sep=" ", col.names = F, row.names = F)
        prep_L1_L2_files()
        
        incProgress(1/full_parts, detail = "Estimating MHLM2")
        
        print(parameters)
        
        # do this once
        # estimate mhlm
        #system("gcc -O hlmm2v4.c matlib2.c -lm -w -o hlmm2v4")
        system(paste("./hlmm2v4", parameters[1], parameters[2], parameters[3], parameters[4], parameters[5]), intern = TRUE)
        
        # process the output of the initial mhlm
        
        
        incProgress(1/full_parts, detail = "Imputing missing values for two step imputation")
        
        # compile the imputation program
        #system("gcc hlmm2micontext1.c matlib3.c com.c linpack.c randlib.c -lm -w -o hlmm2micontext1")  
        
        
        processed_imputation <- process_second_imputation_step(L1_dat,
                                                               L2_dat,
                                                               input$imputation_number,
                                                               parameters,
                                                               missing_cov_interactions,
                                                               original_names,
                                                               r_column,
                                                               add_as_missing,
                                                               equation_matrix,
                                                               input$unique_seed,
                                                               input$L1_covariate_selection)
        
        updated_parameters <- processed_imputation$p
        L1_names <- processed_imputation$l1name
        L2_names <- processed_imputation$l2name
        num_of_extra_cols <- processed_imputation$extra_cols
        
        # recalculate the 5 parameters
        updated_parameters[2] <- updated_parameters[2] + updated_parameters[1]
        updated_parameters[4] <- updated_parameters[4] + updated_parameters[1]
        updated_parameters[1] <- 0
        if(updated_parameters[3] > 1){
          updated_parameters[4] <- updated_parameters[4] + updated_parameters[3] - 1
          updated_parameters[3] <- 1
        }
        # make the theta list
        theta_list <- return_theta_list(input$imputation_number,
                                        num_of_extra_cols,
                                        updated_parameters,
                                        L1_names[1:(length(L1_names)-num_of_extra_cols)],
                                        L2_names,
                                        equation_matrix,
                                        input$response_selection)
        theta_test <<- theta_list
        
        calculated_variables <- return_calculated_outputs_non_fully_observed(theta_list,equation_matrix)
        
        cv <<- calculated_variables
        
        calculated_variables <- paste0(
          '<style>table{border-spacing: 25px;}</style><div style="font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;">',
          calculated_variables,
          '</div>'
        )
        
        cv2 <<- calculated_variables
        
        output$comb_output_1 <- renderUI({
          HTML(calculated_variables)
        })
        output$hlm2_title <- renderText({
          return("Combined HLM2 Estimates")
        })
        output$L1_var_mat_txt <- renderText({
          return("Missing values imputed by a two-stage iterative imputation approach")
        })
        
        # clear the rest of the output
        # send the output to the ui
        output$L1_var_mat <- renderTable({
          return(NULL)
        })
        output$L2_var_mat_txt <- renderText({
          return(NULL)
        })
        output$L2_var_mat <- renderTable({
          return(NULL)
        })
        output$alpha_vector_txt <- renderText({
          return(NULL)
        })
        output$alpha_vector <- renderTable({
          return(NULL)
        })
        output$low_tri_txt <- renderText({
          return(NULL)
        })
        output$low_tri <- renderTable({
          return(NULL)
        })
        output$fixed_mat_txt <- renderText({
          return(NULL)
        })
        output$fixed_mat <- renderTable({
          return(NULL)
        })
        output$imp_model_header <- renderTable({
          return(NULL)
        })
        
        ##### M2I write out tab three #####
        
        
        updateTabsetPanel(session, "inTabset",
                          selected = 'est'
        )
        
      }
      else{
        incProgress(4/full_parts, detail = "Estimating MHLM2")
        missing_complete_summary <- return_L1_L2_nulls(L1_dat,L2_dat)
        null_data <- missing_complete_summary$L1_missing
        null_data2 <- missing_complete_summary$L2_missing
        
        response <- complete_interactions(equation_matrix,L1_dat,L2_dat,null_data,null_data2,parameters)
        parameters <- response$p
        num_of_extra_cols <- 0
        if(length(missing_cov_interactions)>0){
          if(!(parameters[1]==0 & parameters[3]==1))
            num_of_extra_cols <- response$n
        }
        response <- random_interactions(equation_matrix,response$d,L2_dat,null_data,null_data2,response$p)
        
        parameters <- response$p
        L1_dat <- response$d
        
        # write the tables out to files
        write.table(L1_dat,file = "L1out.dat",sep=" ", col.names = F, row.names = F)
        write.table(L2_dat,file="L2out.dat",sep=" ", col.names = F, row.names = F)
        prep_L1_L2_files()
        
        # estimate mhlm
        #system("gcc -O hlmm2v4.c matlib2.c -lm -w -o hlmm2v4")
        system(paste("./hlmm2v4", parameters[1], parameters[2], parameters[3], parameters[4], parameters[5]), intern = TRUE)
        
        info = file.info("L1cov.par")
        empty = rownames(info[info$size == 0, ])
        if(length(empty) != 0){
          output$L1_var_mat_txt <- renderText({
            return("Something went wrong")
          })
        }
        else{
          if(sum(is.na(L1_dat))==0 & sum(is.na(L2_dat))==0){
            print('made it')
            # get missing data again
            missing_complete_summary <- return_L1_L2_nulls(L1_dat,L2_dat)
            null_data <- missing_complete_summary$L1_missing
            null_data2 <- missing_complete_summary$L2_missing
            
            showNotification('All data completely observed. Skipping imputation step.')
            
            # read in data
            L1_covariate_data <- return_L1_covariate_table(L1_dat,names(null_data),input$response_selection)
            L2_covariate_data <- return_L2_covariate_table(names(null_data),equation_matrix,L1_dat,L2_dat,names(null_data2),input$response_selection)
            alpha_data <- return_alpha_table(equation_matrix,input$response_selection,null_data,null_data2,L1_dat,L2_dat)
            
            # handle fixed effects and l1/l2 variance covariance matrix
            valp_table <- read.table("Valp.par")
            colnames(valp_table)<-NULL
            valp_table_to_print <- valp_table
            vcov_table <- read.table("Vcov.par")
            colnames(vcov_table)<-NULL
            vcov_table_to_print <- vcov_table
            
            # send the output to the ui
            output$mhlm2_title <- renderText({
              return("Estimated Imputation Model")
            })
            output$L1_var_mat_txt <- renderText({
              return("L1 Variance Covariance Matrix")
            })
            output$L1_var_mat <- renderTable({
              return(L1_covariate_data$print)
            },rownames = TRUE,digits = 6)
            output$L2_var_mat_txt <- renderText({
              return("L2 Variance Covariance Matrix")
            })
            output$L2_var_mat <- renderTable({
              return(L2_covariate_data$print)
            },rownames = TRUE,digits = 6)
            output$alpha_vector_txt <- renderText({
              return("Fixed Effects")
            })
            output$alpha_vector <- renderTable({
              return(alpha_data$print)
            },digits = 6)
            output$low_tri_txt <- renderText({
              return("Variance Covariance Matrix of Lower Triangular L2 Followed by L1 Variance Covariance Matrix")
            })
            output$low_tri <- renderTable({
              return(vcov_table_to_print)
            },digits = 6)
            output$fixed_mat_txt <- renderText({
              return("Variance Covariance Matrix of Fixed Effects")
            })
            output$fixed_mat <- renderTable({
              return(valp_table_to_print)
            },digits = 6)
            
            # make the theta list
            theta <- return_theta_list_fully_observed(num_of_extra_cols,
                                                      parameters,
                                                      L1_dat,
                                                      L2_dat,
                                                      equation_matrix,
                                                      input$response_selection)
            
            theta_c_test <<- theta
            
            html_output <- return_calculated_outputs_fully_observed(theta,equation_matrix)
            
            output$comb_output_1 <- renderUI({
              HTML(
                paste0(
                  '<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\"><b>Random Effects:</b><br>',
                  html_output,
                  '</div>'
                  )
              )
            })
            output$hlm2_title <- renderText({
              return("HLM2 Estimates")
            })
            
            test_print_to_file <<- vcov_table_to_print
            
            ######FO write results for tab three out #####
            file.create('output.html')
            fileConn<-file("output.html")
            writeLines(
              paste(
                '<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">',
                '<h3>HLM2 Estimates</h3>',
                '<b>Random Effects:</b><br>',
                html_output,
                '</div>'
              ),
              fileConn
            )
            close(fileConn)
            
            ######FO write results for tab four out #####
            file.create('output_2.html')
            fileConn<-file("output_2.html")
            writeLines(
              paste(
                '<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">',
                '<h3>Estimated Imputation Model</h3>',
                '<h4>L2 Variance Covariance Matrix</h4>',
                convert_FO_to_HTML_table_to_print(L2_covariate_data$print),
                '<h4>L1 Variance Covariance Matrix</h4>',
                convert_FO_to_HTML_table_to_print(L1_covariate_data$print),
                '<h4>Fixed Effects</h4>',
                convert_FO_no_row_names_to_HTML_table(alpha_data$print),
                '<h4>Variance Covariance Matrix of Lower Triangular L2 Followed by L1 Variance Covariance Matrix</h4>',
                convert_FO_no_row_names_to_HTML_table(round(vcov_table_to_print,6)),
                '<h4>Variance Covariance Matrix of Fixed Effects</h4>',
                convert_FO_no_row_names_to_HTML_table(round(valp_table_to_print,6)),
                '</div>'
              ),
              fileConn
            )
            close(fileConn)            
            
            # select the tab with the output
            updateTabsetPanel(session, "inTabset",
                              selected = 'est'
            )
          }
          else{
            # get missing data again
            missing_complete_summary <- return_L1_L2_nulls(L1_dat,L2_dat)
            null_data <- missing_complete_summary$L1_missing
            null_data2 <- missing_complete_summary$L2_missing
            
            # read in data
            L1_covariate_data <- return_L1_covariate_table(L1_dat,names(null_data),input$response_selection)
            L2_covariate_data <- return_L2_covariate_table(names(null_data),equation_matrix,L1_dat,L2_dat,names(null_data2),input$response_selection)
            alpha_data <- return_alpha_table(equation_matrix,input$response_selection,null_data,null_data2,L1_dat,L2_dat)
            
            # handle fixed effects and l1/l2 variance covariance matrix
            valp_table <- read.table("Valp.par")
            colnames(valp_table)<-NULL
            valp_table_to_print <- valp_table
            vcov_table <- read.table("Vcov.par")
            colnames(vcov_table)<-NULL
            vcov_table_to_print <- vcov_table
            
            # send the output to the ui
            #########################################################################
            output$mhlm2_title <- renderText({
              return("Estimated Imputation Model")
            })
            output$L1_var_mat_txt <- renderText({
              return("L1 Variance Covariance Matrix")
            })
            output$L1_var_mat <- renderTable({
              return(L1_covariate_data$print)
            },rownames = TRUE,digits = 6)
            output$L2_var_mat_txt <- renderText({
              return("L2 Variance Covariance Matrix")
            })
            output$L2_var_mat <- renderTable({
              return(L2_covariate_data$print)
            },rownames = TRUE,digits = 6)
            output$alpha_vector_txt <- renderText({
              return("Fixed Effects")
            })
            output$alpha_vector <- renderTable({
              return(alpha_data$print)
            },digits = 6)
            output$low_tri_txt <- renderText({
              return("Variance Covariance Matrix of Lower Triangular L2 Followed by L1 Variance Covariance Matrix")
            })
            output$low_tri <- renderTable({
              return(vcov_table_to_print)
            },digits = 6)
            output$fixed_mat_txt <- renderText({
              return("Variance Covariance Matrix of Fixed Effects")
            })
            output$fixed_mat <- renderTable({
              return(valp_table_to_print)
            },digits = 6)
            #########################################################################
            
            # make the imputation header
            output$imp_model_header <- renderUI({
              HTML(return_imputation_header())
            })
            output$imp_model_header_2 <- renderUI({
              return(NULL)
            })
            
            incProgress(1/full_parts, detail = "Imputing missing data")
            
            # compile and run hlmm2mi
            trimmed_seed <- trim_seed_string(input$unique_seed)
            #system("gcc hlmm2micontext2.c matlib3.c com.c linpack.c randlib.c -lm -w -o hlmm2micontext2")
            system(paste("./hlmm2micontext2",
                         parameters[1],
                         parameters[2],
                         parameters[3],
                         parameters[4],
                         parameters[5],
                         input$imputation_number,
                         trimmed_seed), intern = TRUE)
            
            # recalculate the 5 parameters
            parameters[2] <- parameters[2] + parameters[1]
            parameters[4] <- parameters[4] + parameters[1]
            parameters[1] <- 0
            if(parameters[3] > 1){
              parameters[4] <- parameters[4] + parameters[3] - 1
              parameters[3] <- 1
            }
            # make the theta list
            theta_list <- return_theta_list(input$imputation_number,
                                            num_of_extra_cols,
                                            parameters,
                                            names(L1_dat)[1:(length(names(L1_dat))-num_of_extra_cols)],
                                            names(L2_dat),
                                            equation_matrix,
                                            input$response_selection)
            theta_test <<- theta_list
            
            calculated_variables <- return_calculated_outputs_non_fully_observed(theta_list,equation_matrix)
            
            calculated_variables_to_print <- paste0(
              '<style>table{border-spacing: 25px;}</style><div style="font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;">',
              '<h3>Combined HLM2 Estimates</h3>',
              calculated_variables,
              '</div>'
            )
            
            calculated_variables <- paste0(
              '<style>table{border-spacing: 25px;}</style><div style="font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;">',
              calculated_variables,
              '</div>'
            )
            
            ######MO write results for tab three out #####
            file.create('output.html')
            fileConn<-file("output.html")
            writeLines(
              paste(
                calculated_variables_to_print
              ),
              fileConn
            )
            close(fileConn)
            
            ######MO write results for tab four out #####
            file.create('output_2.html')
            fileConn<-file("output_2.html")
            writeLines(
              paste(
                '<div style=\"font-family: Times New Roman,Times,serif; font-size: 140%; display:inline;\">',
                '<h3>Estimated Imputation Model</h3>',
                '<h4>L2 Variance Covariance Matrix</h4>',
                convert_FO_to_HTML_table_to_print(L2_covariate_data$print),
                '<h4>L1 Variance Covariance Matrix</h4>',
                convert_FO_to_HTML_table_to_print(L1_covariate_data$print),
                '<h4>Fixed Effects</h4>',
                convert_FO_no_row_names_to_HTML_table(alpha_data$print),
                '<h4>Variance Covariance Matrix of Lower Triangular L2 Followed by L1 Variance Covariance Matrix</h4>',
                convert_FO_no_row_names_to_HTML_table(round(vcov_table_to_print,6)),
                '<h4>Variance Covariance Matrix of Fixed Effects</h4>',
                convert_FO_no_row_names_to_HTML_table(round(valp_table_to_print,6)),
                '</div>'
              ),
              fileConn
            )
            close(fileConn)              
            
            output$comb_output_1 <- renderUI({
              HTML(calculated_variables)
            })
            output$hlm2_title <- renderText({
              return("Combined HLM2 Estimates")
            })
            updateTabsetPanel(session, "inTabset",
                              selected = 'est'
            )
          }
        }
      }
    })
  })
  
  #
  ###############################################################################################
}