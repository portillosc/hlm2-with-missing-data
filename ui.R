ui <- fluidPage(
  br(),
  tags$h2("HLM2 with Missing Data", style = 'display:inline;'),
  
  HTML(paste0('<center style="font-family: Times New Roman,Times,serif; font-size: 125%; display:inline;">',
              '<i>',
              '<b>&nbsp;&nbsp; By <a href="http://www.people.vcu.edu/~yshin/"',
              '>Shin, Y.</a>, Raudenbush, S.W., Portillo, S.',
              '</b>',
              '</i>',
              '</center>')
  ),
  tags$br(),
  tags$br(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Checkbox for the file formats
      selectInput("input_file_format", "Select File Format",
                  c("spss", "stata", "whitespace delimited", "comma delimited")),
      
      # Upload L1 and L2 datasets
      fileInput("file1", "Select L1 Dataset",
                multiple = FALSE),
      fileInput("file2", "Select L2 Dataset",
                multiple = FALSE),
      
      # Multilevel model specification
      uiOutput('select_L2_id'),
      
      uiOutput('L1_title'),
      
      # added indents to these UI elements with a div tag
      div(style='padding-left: 20px',
          uiOutput('select_response')),
      div(style='padding-left: 20px',
          uiOutput('select_L1_covariates')),
      uiOutput('L2_Title'),
      
      # radio buttons and checkboxes for L1 covariates
      div(style='padding-left: 20px',
          fluidRow(
            column(5,
                   uiOutput('select_L1_covariates_radio_buttons')),
            column(6,
                   uiOutput('L1_covariate_random_interaction_checkboxes'))
          )
      ),
      
      # choose the L2 covariates that have interactions with L1 covariates
      div(style='padding-left: 20px',
          uiOutput('select_L2_covariates')),
      
      # multiple imputation specification
      uiOutput('MI_section_title'),
      div(style='padding-left: 20px',
          uiOutput('number_of_imputations')),
      div(style='padding-left: 20px',
          uiOutput('imputation_seed')),
      
      # action button to estimate the model
      uiOutput('run')
    ),
    
    # Header panel for the table outputs
    mainPanel(
      htmlOutput("summary"),
      htmlOutput("heirarchical_equation"),
      tabsetPanel(
        id="inTabset",
        
        # panel for L1 data
        tabPanel(
          "L1 Data",
          value = "L1_data_tab",
          uiOutput('error_text_1'),
          div(style='height:400px;width:screen.width; overflow: scroll;overflow-y: scroll;',
              tableOutput("L1_table_display"))
        ),
        
        # panel for L2 data
        tabPanel(
          "L2 Data",
          value = "L2_data_tab",
          uiOutput('error_text_2'),
          div(style='height:400px;width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("L2_table_display"))
        ),
        tabPanel(
          "Estimated HLM2",
          value = "est",
          downloadButton('download_handler'),
          tags$h3(textOutput("hlm2_title")),
          uiOutput("imp_model_header_2"),
          tags$br(),
          uiOutput("comb_output_1"),
          tableOutput("L2_var_mat_2"),
          uiOutput("comb_output_2"),
          tableOutput("upper_triangular_tau_matrix"),
          uiOutput("comb_output_3")
        ),
        tabPanel(
          "Estimated Imputation Model",
          downloadButton('download_handler_2'),
          tags$h3(textOutput("mhlm2_title")),
          uiOutput("imp_model_header"),
          tags$br(),
          tags$h4(textOutput("L2_var_mat_txt")),
          div(style='width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("L2_var_mat")),
          tags$h4(textOutput("L1_var_mat_txt")),
          div(style='width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("L1_var_mat")),
          tags$h4(textOutput("alpha_vector_txt")),
          div(style='width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("alpha_vector")),
          tags$h4(textOutput("low_tri_txt")),
          div(style='width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("low_tri")),
          tags$h4(textOutput("fixed_mat_txt")),
          div(style='width:screen.width;  overflow: scroll;overflow-y: scroll;',
              tableOutput("fixed_mat"))
        )
      )
    )
  ),
  HTML('<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>')
)