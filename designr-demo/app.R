#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(designr)

factorPanel <- function(id, random = F, grouping = NULL) {
  tags$div(
    id = paste0("factor", id),
    wellPanel(
      textInput(paste0("factor", id, "Name"), "Factor name", value=paste0("Factor_", id)),
      selectInput(paste0("factor", id, "Type"), "Levels", list(`Random factor` = "random", `Fixed factor` = c(`Boolean (TRUE/FALSE)`="fixed bool", `Integer (1, 2, ...)` = "fixed int", `Custom levels` = "fixed char")), selected = if(random) "random" else "fixed bool"),
      conditionalPanel(sprintf("input.%s == 'fixed char'", paste0("factor", id, "Type")), helpText("Enter a comma-separated list of levels. You may enter the same value multiple times."), textInput(paste0("factor", id, "CharLevels"), NULL, value="a, b")),
      conditionalPanel(sprintf("input.%s == 'fixed int'", paste0("factor", id, "Type")), helpText("Enter the first value and the number of levels."), numericInput(paste0("factor", id, "IntOffset"), NULL, 1L, min=NA, max=NA, step=1L, width="45%"), numericInput(paste0("factor", id, "IntLevels"), NULL, 2L, min=1L, max=NA, step=1L, width="45%")),
      if(!is.null(grouping)) conditionalPanel(sprintf("input.%s == 'random'", paste0("factor", id, "Type")) ,selectizeInput(paste0("factor", id, "Groups"), "Grouping factors", multiple = T, choices = grouping)) else tags$div(),
      conditionalPanel(sprintf("input.%s != 'random'", paste0("factor", id, "Type")), checkboxInput(paste0("factor", id, "Blocked"), "Blocked"))
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
       actionButton("add","Add"), tags$span(id="removePlaceholder"),
       actionButton("go","Generate")
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       helpText("Long version of design:"),
       verbatimTextOutput("designFormula1"),
       helpText("Short version of design:"),
       verbatimTextOutput("designFormula2"),
       helpText("Preview of the design:"),
       tableOutput("designMatrix")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  desout <- reactiveValues(df=data.frame(), formula=list(full = character(0), short = character(0)))
  
  makeformula <- function(design) {
    f1 <- paste0('~', paste(lapply(design, function(f) {
      if(is.random.factor(f)) {
        ret <- paste(f@name, collapse=":")
        if(length(f@groups) > 0L) {
          ret <- sprintf("%s[%s]", ret, paste(f@groups, collapse=", "))
        }
        if(f@replications > 1L) ret <- sprintf("%s * %d", ret, f@replications)
        return(ret)
      }else if(is.fixed.factor(f)) {
        ret <- sprintf("%s(%s)", paste(f@name, collapse=":"), paste(f@levels[,f@name,drop=T], collapse=", "))
        if(length(f@groups) > 0L) {
          ret <- sprintf("%s[%s]", ret, paste(f@groups, collapse=", "))
        }
        if(f@replications > 1L) ret <- sprintf("%s * %d", ret, f@replications)
        return(ret)
      }else{
        return("...")
      }
    }), collapse=" + "),"")
    f2 <- paste0('', paste(lapply(design, function(f) {
      if(is.random.factor(f)) {
        ret <- character(0)
        if(length(f@groups) == 1L) {
          ret <- c(ret, sprintf("groups = %s", shQuote(f@groups)))
        }else if(length(f@groups) > 1L) {
          ret <- c(ret, sprintf("groups = c(%s)", paste(shQuote(f@groups), collapse=", ")))
        }
        for(n in names(f@extra)) {
          if(inherits(f@extra[[n]], c("logical","integer","numeric"))) ret <- c(ret, sprintf("%s = %s", n, f@extra[[n]]))
          else ret <- c(ret, sprintf("%s = %s", n, shQuote(f@extra[[n]])))
        }
        if(f@replications > 1L) ret <- c(ret, sprintf("replications = %dL", f@replications))
        if(length(f@name) == 1L) ret <- c(shQuote(f@name), ret)
        else ret <- c(sprintf("c(%s)", paste(shQuote(f@name), collapse=", ")), ret)
        return(sprintf("random.factor(%s)", paste(ret, collapse=", ")))
      }else if(is.fixed.factor(f)) {
        ret <- character(0)
        if(inherits(f@levels[,f@name], c("logical","integer","numeric"))) ret <- c(ret, sprintf("levels = c(%s)", paste(f@levels[,f@name], collapse=", ")))
        else ret <- c(ret, sprintf("levels = c(%s)", paste(shQuote(f@levels[,f@name]), collapse=",")))
        if(f@replications > 1L) ret <- c(ret, sprintf("replications = %dL", f@replications))
        if(length(f@groups) == 1L) {
          ret <- c(ret, sprintf("groups = %s", shQuote(f@groups)))
        }else if(length(f@groups) > 1L) {
          ret <- c(ret, sprintf("groups = c(%s)", paste(shQuote(f@groups), collapse=", ")))
        }
        for(n in names(f@extra)) {
          if(inherits(f@extra[[n]], c("logical","integer","numeric"))) ret <- c(ret, sprintf("levels = c(%s)", paste(shQuote(f@levels[,f@name]), collapse=", ")))
          else ret <- c(ret, sprintf("%s = %s", n, shQuote(f@extra[[n]])))
        }
        if(length(f@name) == 1L) ret <- c(shQuote(f@name), ret)
        else ret <- c(sprintf("c(%s)", paste(shQuote(f@name), collapse=", ")), ret)
        return(sprintf("fixed.factor(%s)", paste(ret, collapse=", ")))
      }else{
        return("...")
      }
    }), collapse=" + "))
    return(c(f1, f2))
  }
  
  
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
    vapply(seq_len(n.factors()), function(i) input[[paste0("factor", i, "Name")]], character(1))
  }
  
  observeEvent(input$add, {
    choices <- 1:n.factors()
    if(n.factors() == 1L) insertUI("#removePlaceholder", "afterBegin", actionButton("remove","Remove"))
    names(choices) <- factor.names()
    insertUI("#factorsPlaceholder", "beforeBegin", factorPanel(n.factors(n.factors()+1L), grouping = choices))
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
    }
  })
  
  observeEvent(input$go, {
    
    
    
    
    desout$formulas$full <- character(0)
    desout$formulas$short <- character(0)
    
    if(n.factors() > 5) {
      design <- "This is just a demo. If you would like to see what you can do with more than 5 factors, please download the library and see on your own computer!"
    }else{
      design <- factor.design()
    }
    
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
      short <- c(short, sprintf("(%s)", paste(groups, collapse=", ")))
      print(factype)
      if(factype[1] == "random") {
        if(is(design, "factor.container")) design <- design + random.factor(name = input[[paste0("factor",i,"Name")]], groups = groups)
        full <- sprintf("random.factor(%s)", paste(c(shQuote(input[[paste0("factor",i,"Name")]]), full), collapse=", "))
        short <- paste0(c(input[[paste0("factor",i,"Name")]], short), collapse="")
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
        if(is(design, "factor.container")) design <- design + fixed.factor(input[[paste0("factor",i,"Name")]], levels = levels, blocked = blocked, block.name = NULL)
      }
      
      desout$formulas$full <- c(desout$formulas$full, full)
      desout$formulas$short <- c(desout$formulas$short, short)
    }
    
    if(is.character(design)) desout$df <- design
    else desout$df <- output.design(design, order_by = names(random.factors(design, include.interactions = F)))$codes
    
  })
  
  output$designMatrix <- renderTable({
    if(is.data.frame(desout$df)) return(desout$df)
    else stop(desout$df)
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

