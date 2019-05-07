
#' @author 
#' Marta Karas <marta.karass@gmail.com>, <mkaras2@jhmi.edu> 
#' 
#' @description 
#' R code to generate Shiny application - simple tool for filtering study based 
#' on criteria defined via simple CSV file schema.
#' 
#' The filtering logic works as follows: 
#' for all study variables considered (defined via CSV input file),
#' the selected variable value in filter must  
#' have value "1" in corresponding CSV input file to pass the filter. 
#' 
#' * For examle, if a particular study allows ALL values of certain variable,
#'   I'd put "1" for all values of certain variable to CSV input file for that variable, etc. 


rm(list = ls())
library(shiny)
library(dplyr)
library(data.table)
library(reshape)
library(stringi)


## FIXED PARAMS 
## File path
DATA.PATH           <- "../data/data-foo.csv"
## Data file column names for meta information
STUDY.ID.VARNAME    <- "Study"
STUDY.URL.VARNAME   <- "Url"
STUDY.META.VARNAMES <- c(STUDY.URL.VARNAME, "Title", "Info", "Summary")


## AUXILIARY FUNCTIONS 
## Function to create link to be put in a study name column in the output table
createLink <- function(study.val, url.val) {
  url.val.clean <- stri_replace_all_charclass(url.val, "\\p{WHITE_SPACE}", "")
  if(nchar(url.val)==0){
    return(study.val)
  } else {
    return(sprintf(paste0('<a href="', URLdecode(url.val.clean),'" target="_blank">', study.val ,'</a>')))
  }
}


ui <- fluidPage(
  br(),
  titlePanel("Screening Site for XXX"),
  br(),
  sidebarLayout(
    sidebarPanel(
      uiOutput("filter_params")
    ),
    mainPanel(
      dataTableOutput("filtered_results_df")
    )
  )
)


server <- function(input, output) {
  
  ##############################################################
  ##   REACTIVES TO GENERATE DERIVATIVES OF DATA FILE INPUT   ##
  ##############################################################
  
  ## Read input CSV data 
  read.data <- reactive({ 
    as.data.frame(fread(DATA.PATH, stringsAsFactors = FALSE))
  })
  
  ## Subset input CSV data to keep study description (meta) info only
  get.study.description.df <- reactive({ 
      data <- read.data()
      data[, c(STUDY.ID.VARNAME, STUDY.META.VARNAMES)]
  })
  
  ## Subset input CSV data to information about
  ## - Study ID
  ## - Study variables,
  ## - Study variables values,
  ## in long format data frame.
  get.study.var.varvals.df_long <- reactive({ 
    data <- read.data()
    var.df <- data[, setdiff(names(data), c(STUDY.META.VARNAMES))]
    var.df %>%
      melt(id.vars = STUDY.ID.VARNAME) %>%
      separate(col = variable,
               into = c("variable", "variable_value"),
               sep = "=",
               remove = TRUE) %>%
      arrange_(c(STUDY.ID.VARNAME, "variable", "variable_value"))
  })

  ## Subset input CSV data to keep information about
  ## - Study variables,
  ## - Study variables values
  ## in a list format.
  get.var.varvalues.list <- reactive({ 
    var.varvalues.df <- 
      get.study.var.varvals.df_long() %>%
      select(variable, variable_value) %>% 
      distinct()
    var.varvalues.list <- list()
    for (variable.i in unique(var.varvalues.df$variable)){
      varvalues.vec.i <- 
        var.varvalues.df %>% 
        filter(variable == variable.i) %>%
        select(variable_value) %>% 
        unlist() %>% 
        as.vector()
      varvalues.vec.i <- sort(varvalues.vec.i)
      var.varvalues.list[[variable.i]] <- list(
        variable = variable.i, 
        variable_value = varvalues.vec.i)
    }
    var.varvalues.list
  })

  
  ##################################
  ##   RENDER UI BASED ON DATA    ##
  ##################################
  
  output$filter_params <- renderUI({
    
    data <- read.data()
    if (is.null(data)) return({
      c(renderText({"Unable to read data file. Contact <mkaras2@jhu.edu> for help." }))
    })
    
    ## Get list of variables and variable values 
    var.varvalues.list <- get.var.varvalues.list()
    
    ## Dynamically populate UI with selectInput objects 
    c(lapply(var.varvalues.list, function(var.i) {
      selectInput(inputId = var.i$variable,
                  label   = var.i$variable,
                  choices = var.i$variable_value)}),
      list(br(), tags$head(tags$style(HTML('#submit_varvals_filter{background-color:LightSkyBlue}'))),
      actionButton("submit_varvals_filter", "SUBMIT FILTER", width = "100%", icon = icon("hand-o-right", "fa-1x"))))
  })
  

  ####################################
  ##   AFTER SUBMIT BUTTON CLICK    ##
  ####################################

  ## COLLECT NEWLY SUBMITTTED FILTER OF VARIABLES VALUES AFTER SUBMIT BUTTON CLICK
  get.varvals.filter <- eventReactive(input$submit_varvals_filter, {
    
    if (is.null(data)) return(NULL)
    
    var.varvalues.list <- get.var.varvalues.list()
    if (is.null(var.varvalues.list) || length(var.varvalues.list) == 0) return(NULL)
    
    input_name.vec     <- sapply(var.varvalues.list, function(el) el$variable)
    input_value.vec    <- sapply(input_name.vec, function(input_name.i) input[[input_name.i]])
    
    new.varvals.filter.df <-  data.frame(
      variable = input_name.vec,
      variable_value = input_value.vec, 
      stringsAsFactors = FALSE)
    
    return(new.varvals.filter.df)
  })
  
  
  ## GENERATE FILTERED DATA TABLE VIEW 
  output$filtered_results_df <- renderDataTable({

    ## Get "fixed" (data input file-specific) objects 
    study.description.df  <- get.study.description.df()
    study.var.varvals.df_long           <- get.study.var.varvals.df_long()
    
    ## Get data from newly submitted filter of variables values
    new.varvals.filter.df <- get.varvals.filter()
    if (is.null(new.varvals.filter.df)) return(NULL)

    ## Filter study to keep only these which pass the filter
    out.df <-
      study.var.varvals.df_long %>%
      inner_join(new.varvals.filter.df %>% mutate(value_filter = 1), 
                 by = c("variable", "variable_value")) %>%
      group_by(Study) %>%
      summarize(do_agree = min(value - value_filter, na.rm = TRUE)) %>%
      filter(do_agree > -1) %>%
      inner_join(study.description.df, by = STUDY.ID.VARNAME) %>%
      select(-do_agree) %>%
      as.data.frame()
    if (nrow(out.df) == 0) {
      showNotification("No records match filtering criteria", duration = 3);
      return(NULL)
    }

    out.df$Study <- sapply(1:nrow(out.df), function(i){
      createLink(out.df[i, STUDY.ID.VARNAME], 
                 out.df[i, STUDY.URL.VARNAME]) 
    })
    out.df <- out.df[, c(setdiff(names(out.df), STUDY.URL.VARNAME))]
    out.df
  },
  
  ## Define output table look features 
  filter = 'top',
  rownames = TRUE,
  escape = FALSE)

}


shinyApp(ui = ui, server = server)
