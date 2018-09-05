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

factorPanelsFromPreset <- function(...) {
  args <- list(...)
  fixfacs <- integer(0)
  ranfacs <- integer(0)
  for(i in seq_along(args)) {
    if(is.null(args[[i]]$name)) args[[i]]$name <- sprintf("Factor_%d", i)
    names(args[[i]]) <- paste0("val.", names(args[[i]]))
    args[[i]]$id <- i
    nf <- as.integer(i)
    names(nf) <- paste(args[[i]]$val.name, collapse=":")
    args[[i]]$grouping <- fixfacs
    args[[i]]$ranfacs <- ranfacs
    if(is.null(args[[i]]$val.type) || grepl("^fixed($| )", args[[i]]$val.type))
      fixfacs <- c(fixfacs, nf)
    else
      ranfacs <- c(ranfacs, nf)
  }
  lapply(args, function(arg) do.call(factorPanel, arg))
}

factorPanel <- function(id, grouping = integer(0), ranfacs = integer(0), val.name = paste0("Factor_", id), val.type = "fixed char", val.char.levels = "a, b", val.int.offset = 1L, val.int.levels = 2L, val.groups = NULL, val.blocked = F, val.replications = 1L) {
  tags$div(
    id = paste0("factor", id),
    wellPanel(
      selectInput(paste0("factor", id, "Type"), NULL, list(`Random factor` = c(`Random factor (unit)`="random", `Interaction (constraint)`="assignment"), `Fixed factor` = c(`Boolean (TRUE/FALSE)`="fixed bool", `Integer (1, 2, ...)` = "fixed int", `Custom levels` = "fixed char")), selected = val.type, selectize = F),
      conditionalPanel(sprintf("input.%s == 'assignment'", paste0("factor", id, "Type")), selectizeInput(paste0("factor", id, "RandomFactors"), NULL, multiple = T, choices = ranfacs, selected = ranfacs[1:2])),
      conditionalPanel(sprintf("input.%s != 'assignment'", paste0("factor", id, "Type")), textInput(paste0("factor", id, "Name"), NULL, value=val.name)),
      conditionalPanel(sprintf("input.%s == 'fixed char'", paste0("factor", id, "Type")), textInput(paste0("factor", id, "CharLevels"), NULL, value=val.char.levels)),
      conditionalPanel(sprintf("input.%s == 'fixed int'", paste0("factor", id, "Type")), helpText("Enter the first value and the number of levels."), tags$table(tags$tr(tags$td(numericInput(paste0("factor", id, "IntOffset"), NULL, val.int.offset, min=NA, max=NA, step=1L)), tags$td(numericInput(paste0("factor", id, "IntLevels"), NULL, val.int.levels, min=1L, max=NA, step=1L))))),
      if(!is.null(grouping)) conditionalPanel(sprintf("input.%1$s == 'random' || input.%1$s == 'assignment'", paste0("factor", id, "Type")) ,selectizeInput(paste0("factor", id, "Groups"), "Grouping factors", multiple = T, choices = grouping, selected = val.groups)) else tags$div(),
      conditionalPanel(sprintf("input.%1$s != 'random' && input.%1$s != 'assignment'", paste0("factor", id, "Type")), checkboxInput(paste0("factor", id, "Blocked"), "Blocked factor", value = val.blocked)),
      conditionalPanel(sprintf("input.%1$s == 'random'", paste0("factor", id, "Type")), checkboxInput(paste0("factor", id, "Replicate"), "Replicate levels", value = val.replications > 1L)),
      conditionalPanel(sprintf("input.%s == 'random' && input.%s", paste0("factor", id, "Type"), paste0("factor", id, "Replicate")), numericInput(paste0("factor", id, "Replications"), NULL, value = val.replications, min=1L, max=5L, step=1L))
    )
  )
}

examples <- list(
  # `Recognition study` = factorPanelsFromPreset(
  #   list(name="study_lop", type="fixed char", char.levels="deep, shallow"),
  #   list(name="test_payoff", type="fixed char", char.levels="low, high"),
  #   list(name="Subject", type="random", groups=c(1,2), replications = 2L),
  #   list(name="lop_deep_cr", type="fixed bool"),
  #   list(name="lop_shallow_cr", type="fixed bool"),
  #   list(name="Item", type="random", groups=c(4,5)),
  #   list(name="is_old", type="fixed bool"),
  #   list(name="study_pos", type="fixed int", int.offset=1L, int.levels=2L),
  #   list(name="test_pos", type="fixed int", int.offset=1L, int.levels=4L),
  #   list(name=c(3,6), type="assignment", groups=c(7,8,9))
  # ),
  `Rating` = factorPanelsFromPreset(
    list(name="instruction", type="fixed char", char.levels="easy, difficult"),
    list(name="Subject", type="random", groups=c(1), replications=5L),
    list(name="Item", type="random", groups=c(1), replications=10L)
  )
)

example.choices <- seq_along(examples)
names(example.choices) <- names(examples)
example.choices <- c("Select example" = 0L, example.choices)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("designr demo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       factorPanel(1L),
       tags$div(id = "factorsPlaceholder"),
       wellPanel(actionButton("add","Add"), actionButton("remove","Remove"),
       actionButton("go","Generate")),
       wellPanel(helpText("To load an example and overwrite the settings above, select an item from this menu:"),selectInput("loadExample", NULL, choices = example.choices, selectize = F))
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       helpText("To install and load the package:"),
       tags$pre("install.packages(\"https://maxrabe.com/software/designr_latest.tar.gz\", repos = NULL)\nlibrary(designr)"),
       helpText("Function syntax of design:"),
       verbatimTextOutput("designFormula1", placeholder = T),
       helpText("Factor syntax of design (equivalent):"),
       verbatimTextOutput("designFormula2", placeholder = T),
       helpText("Preview of the design:"),
       verbatimTextOutput("designOutputSyntax", placeholder = T),
       tags$div(textOutput("error"), style="font-weight:bold;color:red"),
       tableOutput("designMatrix"),
       helpText("Example analysis calls:"),
       verbatimTextOutput("analysisSyntax", placeholder = T)
     )
   )
)

max.rows <- 1000L
max.cols <- 20L

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #if(!dir.exists(file.path(getwd(), "designr")))
  #  install.packages("https://maxrabe.com/software/designr_0.1.0.tar.gz", lib=getwd(), repos=NULL)
  
  for(file in list.files("designr", "\\.R$", full.names = T)) source(file, local = T)
  
  #library("designr", lib.loc = getwd())
  
  desout <- reactiveValues(df=data.frame(), ran=character(0), fix=character(0), err=NULL, formula=list(full = character(0), short = character(0)), analysis = NULL)
  
  n.factors <- function(input) {
    i <- 0L
    while(!is.null(input[[paste0("factor", i+1L, "Type")]])) i <- i + 1L
    return(i)
  }
  
  factor.names <- function() {
    vapply(seq_len(n.factors(input)), function(i) if(input[[paste0("factor", i, "Type")]] == "assignment") {if(length(input[[paste0("factor", i, "RandomFactors")]])>0) paste(vapply(input[[paste0("factor", i, "RandomFactors")]], function(j) input[[paste0("factor", j, "Name")]], character(1)), collapse=":") else "?"} else if(nchar(input[[paste0("factor", i, "Name")]])>0) input[[paste0("factor", i, "Name")]] else "?", character(1))
  }
  
  observeEvent(input$loadExample, {
    example <- as.integer(input$loadExample)
    if(example > 0L && example <= length(examples)) {
      for(i in n.factors(input):1) {
        removeUI(paste0("#factor", i))
      }
      insertUI("#factorsPlaceholder", "beforeBegin", examples[[example]])
      updateSelectInput(session, "loadExample", selected = 0L)
    }
  })
  
  observeEvent(input$add, {
    choices <- 1:n.factors(input)
    names(choices) <- factor.names()[choices]
    is.fixed <- vapply(seq_along(choices), function(i) grepl("^fixed($| )", input[[paste0("factor", i, "Type")]]), logical(1))
    fchoices <- choices[is.fixed]
    rchoices <- choices[!is.fixed]
    insertUI("#factorsPlaceholder", "beforeBegin", factorPanel(n.factors(input)+1L, grouping = fchoices, ranfacs = rchoices))
  })
  
  
  observeEvent(input$remove, {
    if(n.factors(input) <= 1L) return()
    removeUI(paste0("#factor", n.factors(input)))
  })
  
  observeEvent(quote(factor.names()), event.quoted = T, {
    fnames <- factor.names()
    for(i in seq_len(n.factors(input)-1)+1) {
      choices <- 1:(i-1)
      names(choices) <- factor.names()[choices]
      is.fixed <- vapply(seq_along(choices), function(i) grepl("^fixed($| )", input[[paste0("factor", i, "Type")]]), logical(1))
      fchoices <- choices[is.fixed]
      rchoices <- choices[!is.fixed]
      updateSelectizeInput(session, paste0("factor", i, "RandomFactors"), choices = rchoices, selected = input[[paste0("factor", i, "RandomFactors")]])
      updateSelectizeInput(session, paste0("factor", i, "Groups"), choices = fchoices, selected = input[[paste0("factor", i, "Groups")]])
    }
  })
  
  observeEvent(input$go, {
    
    desout$formulas$full <- character(0)
    desout$formulas$short <- character(0)
      
    design <- factor.design()
    
    random.names <- character(0)
    
    for(i in seq_len(n.factors(input))) {
      
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
      fac2add <- NULL
      if(length(groups) > 0L) short <- c(short,  sprintf("(%s)", paste(groups, collapse=", ")))
      if(factype[1] == "assignment") {
        if(length(input[[paste0("factor",i,"RandomFactors")]]) < 2L) design <- "Constraint must apply to at least 2 random factors."
        ranfacs <- random.names[as.integer(input[[paste0("factor",i,"RandomFactors")]])]
        fac2add <- random.factor(name = ranfacs, groups = groups)
        full <- sprintf("random.factor(%s)", paste(c(sprintf("c(%s)", paste(shQuote(ranfacs), collapse=", ")), full), collapse=", "))     
        short <- sprintf("%s%s", paste(ranfacs, collapse=":"), short)
        random.names <- c(random.names, paste(ranfacs, collapse=":"))
      }else if(factype[1] == "random") {
        if(input[[paste0("factor",i,"Replicate")]]) {
          replications <- as.integer(input[[paste0("factor",i,"Replications")]])
          if(replications < 1L) replications <- 1L
        }else{
          replications <- 1L
        }
        if(replications>1L) {
          full <- c(full, sprintf("replications = %dL", replications))
          short <- c(short, sprintf(" * %d", replications))
        }
        fac2add <- random.factor(name = input[[paste0("factor",i,"Name")]], replications = replications, groups = groups)
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
        fac2add <- fixed.factor(input[[paste0("factor",i,"Name")]], levels = levels, blocked = blocked, block.name = NULL)
        random.names <- c(random.names, input[[paste0("factor",i,"Name")]])
      }
      
      if(!is.null(fac2add) && is(design, "factor.design")) {
        if(nrow(design@design)*nrow(fac2add@levels) > max.rows) {
          design <- sprintf("Your design is probably exceeding %d rows, which is why this app will not display a preview of the design matrix. You can, however, copy the code above and try out on your own computer.", max.rows)
        } else if(ncol(design@design)+length(setdiff(colnames(fac2add@levels), colnames(design@design))) > max.cols) {
          design <- sprintf("Your design is probably exceeding %d columns, which is why this app will not display a preview of the design matrix. You can, however, copy the code above and try out on your own computer.", max.cols)
        } else {
          design <- tryCatch(design + fac2add, error = function(e) as.character(e))
        }
      }

      
      desout$formulas$full <- c(desout$formulas$full, full)
      desout$formulas$short <- c(desout$formulas$short, short)
    }
    
    if(is.character(design)) {
      desout$err <- design
      desout$df <- data.frame()
      desout$ran <- character(0)
      desout$fix <- character(0)
      desout$analysis <- NULL
    } else {
      desout$ran <- names(random.factors(design))
      desout$fix <- names(fixed.factors(design))
      desout$err <- NULL
      output <- output.design(design, order_by = names(random.factors(design, include.interactions = F)))
      desout$df <- output$codes
      desout$analysis <- paste(sprintf("%s(%s)", c("stats::lm","lme4::lmer"), c(output$fixed.model.formula, output$mixed.model.formula)), collapse = "\n")
    }
    
  })
  
  output$designMatrix <- renderTable({
    desout$df
  })
  
  output$error <- renderText({
    desout$err
  })
  
  output$analysisSyntax <- renderText({
    desout$analysis
  })
  
  output$designFormula1 <- renderText({
    if(length(desout$formulas$full) == 0L) return(NULL)
    if(length(desout$formulas$full) == 1L) return(paste0("design <- factor.design() + ", desout$formulas$full))
    return(paste0("design <- ", paste(desout$formulas$full, collapse = " +\n     ")))
  })
  output$designFormula2 <- renderText({
    if(length(desout$formulas$short) == 0L) return(NULL)
    return(paste0("design <- factor.design(~ \n     ", paste(desout$formulas$short, collapse = " +\n     "), ")"))
  })
  output$designOutputSyntax <- renderText({
    if(length(desout$ran) == 0) return(paste0("output <- output.design(design)\nprint(output$codes)"))
    else if(length(desout$ran) == 1) return(paste0("output <- output.design(design, order_by=",shQuote(desout$ran),")\nprint(output$codes)  # also check $units[[",shQuote(desout$ran),"]] for group assignments of the random factor ;-)"))
    else return(paste0("output <- output.design(design, order_by=c(",paste(shQuote(desout$ran[!grepl(':', desout$ran, fixed=T)]), collapse=", "),"))\nprint(output$codes)  # also check $units for group assignments of the random factors ;-)"))
  })
  
  output
}

# Run the application 
shinyApp(ui = ui, server = server)

