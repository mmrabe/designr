#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#library(designr)

factorPanel <- function(id, random = F, grouping = NULL) {
  tags$div(
    id = paste0("factor", id),
    wellPanel(
      textInput(paste0("factor", id, "Name"), "Factor name", value=paste0("Factor_", id)),
      selectInput(paste0("factor", id, "Type"), "Levels", list(`Random factor` = c("random"), `Fixed factor` = c(`Boolean (TRUE/FALSE)`="fixed bool", `Integer (1, 2, ...)` = "fixed int", `Custom levels` = "fixed char")), selected = if(random) "random" else "fixed bool"),
      conditionalPanel(sprintf("input.%s == 'fixed char'", paste0("factor", id, "Type")), textInput(paste0("factor", id, "CharLevels"), NULL, value="a, b")),
      conditionalPanel(sprintf("input.%s == 'fixed int'", paste0("factor", id, "Type")), helpText("Enter the first value and the number of levels."), numericInput(paste0("factor", id, "IntOffset"), NULL, 1L, min=NA, max=NA, step=1L, width="45%"), numericInput(paste0("factor", id, "IntLevels"), NULL, 2L, min=1L, max=NA, step=1L, width="45%")),
      if(!is.null(grouping)) conditionalPanel(sprintf("input.%s == 'random'", paste0("factor", id, "Type")) ,selectizeInput(paste0("factor", id, "Groups"), "Grouping factors", multiple = T, choices = grouping)) else tags$div(),
      conditionalPanel(sprintf("input.%s != 'random'", paste0("factor", id, "Type")), checkboxInput(paste0("factor", id, "Blocked"), "Blocked"))
    )
  )
}

factorPanelAssignment <- function(id, grouping, ranfacs) {
  tags$div(
    id = paste0("factor", id),
    wellPanel(
      selectizeInput(paste0("factor", id, "RandomFactors"), "Random factors", multiple = T, choices = ranfacs),
      selectInput(paste0("factor", id, "Type"), NULL, c(Constraint = "assignment"), selected = "assignment"),
      selectizeInput(paste0("factor", id, "Groups"), "Grouping factors", multiple = T, choices = grouping)
    )
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("designr demo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       factorPanel("1"),
       tags$div(id = "factorsPlaceholder"),
       actionButton("add","Add"), actionButton("constrain","Constrain"), tags$span(id="removePlaceholder"),
       actionButton("go","Generate")
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       textOutput("error"),
       helpText("To install and load the package:"),
       tags$pre("install.packages(\"https://maxrabe.com/software/designr_latest.tar.gz\", repos = NULL)\nlibrary(designr)"),
       helpText("Function syntax of design:"),
       verbatimTextOutput("designFormula1", placeholder = T),
       helpText("Factor syntax of design (equivalent):"),
       verbatimTextOutput("designFormula2", placeholder = T),
       helpText("Preview of the design:"),
       tableOutput("designMatrix")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #if(!dir.exists(file.path(getwd(), "designr")))
  #  install.packages("https://maxrabe.com/software/designr_0.1.0.tar.gz", lib=getwd(), repos=NULL)
  
  for(file in list.files("designr", "\\.R$", full.names = T)) source(file, local = T)
  
  #library("designr", lib.loc = getwd())
  
  desout <- reactiveValues(df=data.frame(), ran=character(0), fix=character(0), err=NULL, formula=list(full = character(0), short = character(0)))
  
  n.factors <- function(set=NULL) {
    if(is.null(set)) {
      get("nfactors", envir = sys.frame(1))
    }else{
      assign("nfactors", set, envir = sys.frame(1))
      return(set)
    }
  }
  n.factors(1L)
  
  factor.names <- function() {
    vapply(seq_len(n.factors()), function(i) if(!is.null(input[[paste0("factor", i, "RandomFactors")]])) paste(input[[paste0("factor", i, "RandomFactors")]], collapse=":") else input[[paste0("factor", i, "Name")]], character(1))
  }
  
  observeEvent(input$add, {
    choices <- 1:n.factors()
    if(n.factors() == 1L) insertUI("#removePlaceholder", "afterBegin", actionButton("remove","Remove"))
    names(choices) <- factor.names()
    insertUI("#factorsPlaceholder", "beforeBegin", factorPanel(n.factors(n.factors()+1L), grouping = choices))
  })
  
  observeEvent(input$constrain, {
    ranfacs <- seq_along(desout$ran)
    names(ranfacs) <- desout$ran
    choices <- 1:n.factors()
    names(choices) <- factor.names()
    if(n.factors() == 1L) insertUI("#removePlaceholder", "afterBegin", actionButton("remove","Remove"))
    insertUI("#factorsPlaceholder", "beforeBegin", factorPanelAssignment(n.factors(n.factors()+1L), ranfacs = ranfacs, grouping = choices))
  })
  
  observeEvent(input$remove, {
    if(n.factors() <= 1L) return()
    if(n.factors() <= 2L) removeUI("#removePlaceholder *")
    removeUI(paste0("#factor", n.factors()))
    n.factors(n.factors()-1L)
  })
  
  observeEvent(factor.names(), {
    fnames <- factor.names()
    for(i in seq_len(n.factors()-1)+1) {
      choices <- seq_len(i-1)
      names(choices) <- fnames[seq_len(i-1)]
      updateSelectizeInput(session, paste0("factor", i, "Groups"), choices = choices, selected = input[[paste0("factor", i, "Groups")]])
      choices <- seq_along(desout$ran)
      names(choices) <- desout$ran
      updateSelectizeInput(session, paste0("factor", i, "RandomFactors"), choices = choices, selected = input[[paste0("factor", i, "RandomFactors")]])
    }
  })
  
  observeEvent(input$go, {
    
    desout$formulas$full <- character(0)
    desout$formulas$short <- character(0)
    
    if(n.factors() > 10) {
      design <- "This is just a demo. If you would like to see what you can do with more than 5 factors, please download the library and see on your own computer!"
    }else{
      design <- factor.design()
    }
    
    random.names <- character(0)
    
    for(i in seq_len(n.factors())) {
      
      factype <- strsplit(input[[paste0("factor",i,"Type")]], " ", fixed = T)[[1L]]
      
      full = character(0)
      short = character(0)
      
      groups <- character(0)
      if(!is.null(input[[paste0("factor",i,"Groups")]])) {
        groups <- factor.names()[as.integer(input[[paste0("factor",i,"Groups")]])]
      }
      if(length(groups)==1L) {
        full <- c(full, sprintf("groups = %s", shQuote(groups)))
      }else if(length(groups)>1L){
        full <- c(full, sprintf("groups = c(%s)", paste(shQuote(groups), collapse=", ")))
      }
      if(length(groups) > 0L) short <- c(short,  sprintf("(%s)", paste(groups, collapse=", ")))
      if(factype[1] == "assignment") {
        if(length(input[[paste0("factor",i,"RandomFactors")]]) < 2L) design <- "Constraint must apply to at least 2 random factors."
        ranfacs <- random.names[as.integer(input[[paste0("factor",i,"RandomFactors")]])]
        if(is(design, "factor.container")) design <- tryCatch(design + random.factor(name = ranfacs, groups = groups), error = function(e) as.character(e))
        full <- sprintf("random.factor(%s)", paste(c(sprintf("c(%s)", paste(shQuote(ranfacs), collapse=", ")), full), collapse=", "))     
        short <- sprintf("%s%s", paste(ranfacs, collapse=":"), short)
        random.names <- c(random.names, paste(ranfacs, collapse=":"))
      }else if(factype[1] == "random") {
        if(is(design, "factor.container")) design <- tryCatch(design + random.factor(name = input[[paste0("factor",i,"Name")]], groups = groups), error = function(e) as.character(e))
        full <- sprintf("random.factor(%s)", paste(c(shQuote(input[[paste0("factor",i,"Name")]]), full), collapse=", "))
        short <- paste0(c(input[[paste0("factor",i,"Name")]], short), collapse="")
        random.names <- c(random.names, input[[paste0("factor",i,"Name")]])
      }else if(factype[1] == "fixed") {
        if(factype[2] == "bool") {
          levels <- c(T, F)
          shlevels <- "T, F"
          lglevels <- "c(T, F)"
        }else if(factype[2] == "int") {
          levels <- seq(from=input[[paste0("factor",i,"IntOffset")]], length.out=input[[paste0("factor",i,"IntLevels")]])
          shlevels <- sprintf("%d:%d", input[[paste0("factor",i,"IntOffset")]], input[[paste0("factor",i,"IntOffset")]]-1L+input[[paste0("factor",i,"IntLevels")]])
          lglevels <- shlevels
        }else if(factype[2] == "char") {
          levels <- strsplit(input[[paste0("factor",i,"CharLevels")]], " *, *")[[1]]
          shlevels <- paste(levels, collapse=", ")
          lglevels <- sprintf("c(%s)", paste(shQuote(levels), collapse=", "))
        }
        blocked <- F
        if(input[[paste0("factor",i,"Blocked")]] && factype[1] == "fixed") {
          blocked <- T
          shlevels <- paste0(shlevels, ", blocked = T")
          lglevels <- paste0(lglevels, ", blocked = T")
        }
        short <- sprintf("%s[%s]", input[[paste0("factor",i,"Name")]], shlevels)
        full <- sprintf("fixed.factor(%s, levels = %s)", shQuote(input[[paste0("factor",i,"Name")]]), lglevels)
        if(is(design, "factor.container")) design <- tryCatch(design + fixed.factor(input[[paste0("factor",i,"Name")]], levels = levels, blocked = blocked, block.name = NULL), error=function(e) as.character(e))
      }
      
      desout$formulas$full <- c(desout$formulas$full, full)
      desout$formulas$short <- c(desout$formulas$short, short)
    }
    
    if(is.character(design)) {
      desout$err <- design
      desout$df <- data.frame()
      desout$ran <- character(0)
      desout$fix <- character(0)
    } else {
      desout$ran <- names(random.factors(design))
      desout$fix <- names(fixed.factors(design))
      desout$err <- NULL
      desout$df <- output.design(design, order_by = names(random.factors(design, include.interactions = F)))$codes
    }
    
  })
  
  output$designMatrix <- renderTable({
    desout$df
  })
  
  output$error <- renderText({
    desout$err
  })
  
  output$designFormula1 <- renderText({
    if(length(desout$formulas$full) == 0L) return(NULL)
    if(length(desout$formulas$full) == 1L) return(paste0("design <- factor.design() + ", desout$formulas$full))
    return(paste0("design <- ", paste(desout$formulas$full, collapse = " + ")))
  })
  output$designFormula2 <- renderText({
    if(length(desout$formulas$short) == 0L) return(NULL)
    return(paste0("design <- factor.design(~ ", paste(desout$formulas$short, collapse = " + "), ")"))
  })
  
  output
}

# Run the application 
shinyApp(ui = ui, server = server)

