#libs
library(shiny)
library(DT)
library(shinythemes)

options(scipen = 999)

#funs
## print percents
percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

## function that gets the number of possible orders based on cands, ranks, voting
## system. include noVote adds one for a blank race
## this is based on rules following behavior
possible_orders <- function(ncands, nranks,irv, includeNoVote=T) {
  if (irv) {
    return(sum(factorial(ncands)/
                 factorial(ncands-nranks:1)) + (includeNoVote*1))
  } else {
    return(sum(factorial(ncands)/
                 (factorial(nranks:1)*factorial(ncands-nranks:1)))+ (includeNoVote*1))
  }
}

## get the expected number of unique votes at maximum entropy dist
## option to get sample coverage included
expected_unique <- function(po, voters, coverage=F) {
  if (coverage) {
    return((1-(1-1/po)^voters))
  } else {
    return((1-(1-1/po)^voters)*voters)
  }
}

## function to convert frame provided to one with needed info
format_frame <- function(main) {
  main$po <- mapply(possible_orders, 
                    ncands =main$cands, 
                    nranks=main$choices, 
                    irv=main$isRanked)
  
  
  main$expected.coverage <- expected_unique(main$po, main$numvoters, T)
  main <- main[order(main$po, decreasing=T),]
  main$expected.unique <- trimws(format(main$po -  main$expected.coverage*main$po,digits = 0,big.mark = ",", big.interval = 3))
  
  main$expected.coverage <- percent(main$expected.coverage, digits=0)
  main$po <- trimws(format(main$po,digits = 0,big.mark = ",", big.interval = 3))
  
  names(main) <- c("Candidates", "Vote For", "Ranked Contest", "Num Voters", "Possible Orders", "E[Coverage]", "E[Unique Votes]")
  return(main)  
}


## calculates ballot wide coverage numbers across contests
ballot_wide <- function(main) {
  main$po <- mapply(possible_orders, 
                    ncands =main$cands, 
                    nranks=main$choices, 
                    irv=main$isRanked)
  po <- prod(main$po)
  res <- expected_unique(po, voters = main$numvoters[1],coverage = T)
  return(c(format(po, digits=0, big.mark = ",", big.interval = 3), percent(res, digits = 2)))
}



ui <- fluidPage(theme = shinytheme("readable"),
  
  # App title ----
  titlePanel("Ballot Complexity Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        # tab for individual row inputs
        tabPanel("Input",
                 br(),
                 "Enter in the details of your ballot and this tool will take that information and return info about its complexity.",
                 br(),
                 "The right side should add a row for a contest each time you click \"Add Contest.\" When you are finished, click \"Get Ballot Complexity\"",
                 br(),br(),

                 numericInput(inputId = "cands",
                              label = "Number of Candidates/Referenda:",
                              value = 2),
                 numericInput(inputId = "choices",
                              label = "Vote for",
                              value = 1),
                 checkboxInput(inputId = "isRanked",
                               label = "Is this contest ranked?",
                               value = FALSE),
                 numericInput(inputId = "voters",
                   label = "Expected number of voters",
                   value = 100),
                 actionButton("submit", "Add Contest", class = "btn-primary"),
                 br()),
        # tab for uploading entire file at once
        tabPanel("Upload",
                 br(),
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
       
        
      ),
 
      br(),
      br()
      
      
            ),

      actionButton("submit2", "Get Ballot Complexity", class = "btn-primary",width = "100%")
    ),

    mainPanel(
      h3("Ballot"),
      dataTableOutput("view"),
      verbatimTextOutput("show_result", placeholder = T)
     
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # initialize a data.frame to hold the data
  main <- data.frame("cands"=1, "choices"=1, "isRanked"=F, "numvoters"=F)
  main <- main[-1,]
  
  # bind new data when called
  add_to_table <- function(cands, choices, isRanked, numvoters) {
    main <- rbind(main, data.frame(cands, choices, isRanked, numvoters))
    main$numvoters <-numvoters
    main
  }
  
  # function to fix true false column if it does not match R's formatting
  fix_false <- function(i) {
    if (is.numeric(i)) {
      if (i <- 1) {
        ret <- TRUE
      } else {
        ret <- FALSE
      }
    } else {
      if (grepl("t", tolower(i)) | i=="1") {
        ret <- TRUE
      } else {
        ret <- FALSE
      }
    }
    ret
  }
  
  
  # event for submit button
  observeEvent(input$submit, {
    main <<- add_to_table(input$cands, input$choices, input$isRanked, input$voters)
    main$numvoters <<- input$voters
    output$view <- renderDataTable({
      main
    },options = list(dom = 't'))
  })
  
  # event for file upload
  observeEvent(input$file1, {
    dat <- read.csv(file = input$file1$datapath)
    req(dat)
    dat$isRanked <- sapply(dat$isRanked, fix_false)
    if (is.null(dat$numvoters)) {
      csvvoters <<- input$voters
    } else {
      csvvoters <- dat$numvoters
    }
    main <<- add_to_table(dat$cands, dat$choices, dat$isRanked, csvvoters)
    output$view <- renderDataTable({
      main
    },options = list(dom = 't'))
  })
  
  

  output$show_result <- renderPrint({
    x <- format_frame(main)
    print(paste("Possible orders for this ballot:",
      ballot_wide(main)[1]))
    
    print("")  
    print(paste("Expected coverage of possible orders(maximum entropy):",
                ballot_wide(main)[2]))
    print("")
    print("Contests with the lowest expected coverage:")
    
    str(x)
  }) |> bindEvent(input$submit2)
  
}

# Create Shiny app ----
shinyApp(ui, server)

