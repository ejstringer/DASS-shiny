

library(lubridate)
library(shiny)
library(ggplot2)
library(shinyjs)
library(rsconnect)



smtp <- emayili::server(
  host = Sys.getenv("GMAIL_SERVER"),
  port = 587,
  username = Sys.getenv("GMAIL_USERNAME"),
  password = Sys.getenv("GMAIL_PASSWORD")
)

ui <- fluidPage(
  hr(),
  useShinyjs(),
  
  textOutput('details'),
  
  textInput('course', "What is your course code? (eg. ENB001)"),
  radioButtons('location', "Are you taking this test in your computer lab or somewhere else?",
               c("lab", "elsewhere")),
  radioButtons('hand', "Are you left or right handed?",
               c('right', 'left')),
  actionButton('submitdetails', "confirm details"),
  
  hidden(actionButton('start','Start practicing!')),
  hidden(textOutput('timeleft')),
  hidden(textOutput('randomtime')),
  hidden(plotOutput('game')),
  hidden(textInput('guess', label = "your guess:")), 
  hidden(actionButton('submit', 'submit')),
  hidden(actionButton('nextseq', 'next sequence')),
  hidden(actionButton('nexttest', 'start Test')),
  hidden(downloadButton("downloadData", "Download")),
  hidden(actionButton('email', 'Submit Results')),
  hidden(textOutput('confirmation'))
  
)

server <- function(input, output, session) {
  
  
  # first entry ------------------------------------------------------------------  
  output$details <- renderText({
    print("Welcome! Let's collect some data, starting with some general information about you.")
  })
  
  observeEvent(input$submitdetails,{
    show('start')
    show('timeleft')
    hide('details')
    hide('submitdetails')
    hide('location')
    hide('hand')
    hide('course')
    
  })
  
  # start practic ----------------------------------------------------------------  
  # Initialize the practice time, test times, and length of sequence
  
  ###############__########################
  ############# EDIT ##################### 
  
  practiceTIME <- sample(30:120, 1) # practice time
  testTimes <- sort(c(5,10))        # how long you see sequence
  remember <- 12                    # length of sequence
  alphabet <- sample(c(top = 1, middle = 9, bottom = 17),1)
  
  ############# stop edits ############### 
  ################__######################
  
  timer <- reactiveVal(practiceTIME)
  timer2 <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  active2 <- reactiveVal(FALSE)
  test <- reactiveVal(0)
  
  
  values <- reactiveVal("black")
  
  mydf <- reactiveValues(df = data.frame(x = 1:remember, y = rep(1,remember),
                                         n = sample(c(1:9,LETTERS[alphabet:(alphabet+8)]),
                                                    remember , replace = TRUE)))
  
  not.in.use.df <- data.frame(x = rep(1:3,3), y = rep(1:3, each= 3), 
                              n = sample(c(1:9,LETTERS)[alphabet:(alphabet+8)], 9, replace = TRUE))
  
  
  
  test20 <- reactiveVal(0)
  test10 <- reactiveVal(0)
  score <- reactiveVal(0)
  firstTest <- sample(testTimes, 1)
  
  output$randomtime <- renderText({
    paste("exposure time left:", seconds_to_period(timer2()))
  })
  
  output$game <- renderPlot({
    
    
    plot(c(0, mydf$df$x, max(mydf$df$x)+1),
         c(0, mydf$df$y, max(mydf$df$y)+1),
         type='n',axes=FALSE,ann=FALSE,
         main = paste("exposure time left:", timer2()))
    
    text(mydf$df$x, mydf$df$y,
         labels = mydf$df$n,
         cex = 2, col = values())
    
    
  })
  
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("practice time left: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          hide('submit')
          hide('nextseq')
          hide('guess')
          hide('timeleft')
          hide('randomtime')
          hide('start')
          hide('game')
          timer2(firstTest)
          show('nexttest')
          active2(FALSE)
          mydf$df$n <- sample("",nrow(mydf$df), replace = TRUE)
          updateTextInput(session,"guess", value="")
          showModal(modalDialog(
            title = "Practice completed!",
            "Well done, lets start the test."
          ))
        }
      }
    })
  })
  
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active2())
      {
        timer2(timer2()-1)
        if(timer2()<1)
        {
          active2(FALSE)
          values("white")
          show('submit')
          show('guess')
        }
      }
    })
  })
  
  
  
  # observers for actionbuttons
  observeEvent(input$start, {
    active(TRUE)
    active2(TRUE)
    timer2(sample(testTimes, 1))
    show('game')
    show('randomtime')
    hide('start')
    
  })
  
  # observe submit
  observeEvent(input$submit,{
    myguess <- strsplit(toupper(input$guess), "")[[1]]
    n <- mydf$df$n
    correct <- n == myguess[1:length(n)]
    correct[is.na(correct)] <- FALSE
    
    ncorrect <- sum(correct)
    halfcorrect <- sum(n[!correct] %in% myguess[1:length(n)],na.rm = T)*0.75
    
    seqnc <- 1:ncorrect
    bonus <- seqnc*0.66
    score1 <- ncorrect
    
    if (test() == 1) {
      if(firstTest == testTimes[1]) test10(ncorrect)
      if(firstTest == testTimes[2]) test20(ncorrect)
      score(score() + score1)
      show('nexttest')
    }
    if(test() == 2){
      if(firstTest == testTimes[1]) test20(ncorrect)
      if(firstTest == testTimes[2]) test10(ncorrect)
      score(score() + score1)
      show('downloadData')
      show('email')
      hide('nexttest')
    } 
    
    
    values(ifelse(correct, "forestgreen", "red"))
    show('game')
    if(test() == 0) show('nextseq')
    hide('guess')
    hide('submit')
    updateTextInput(session,"guess", value="")
  })
  
  observeEvent(input$nextseq, {
    mydf$df$n <- sample(c(1:9,LETTERS[alphabet:(alphabet+8)]),
                        nrow(mydf$df), replace = TRUE)
    values("black")
    timer2(sample(testTimes, 1))
    show('game')
    active2(TRUE)
    hide('nextseq')
  })
  
  
  observeEvent(input$nexttest, {
    hide('nexttest')
    mydf$df$n <- sample(c(1:9,LETTERS[alphabet:(alphabet+8)]),
                        nrow(mydf$df), replace = TRUE)
    active2(TRUE)
    test(test() + 1)
    values('black')
    show('game')
    show('randomtime')
    
    if(test() == 2) timer2(ifelse(firstTest==testTimes[1],
                                  testTimes[2], testTimes[1]))
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("memorytest_results", ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(data.frame(date_of_test = date(Sys.time()),
                           time_of_test = format(as.POSIXct(Sys.time()), format = "%H:%M"),
                           course = input$course,
                           test_location = input$location,
                           handedness = input$hand,
                           alphabet = names(alphabet),
                           practice_time = practiceTIME,
                           first_test = ifelse(firstTest==testTimes[1],
                                               "short", "long"),
                           results_short = test10(),
                           results_long = test20(),
                           results_total = score(),
                           comments = ""), 
                file, row.names = FALSE)
    }
  )
  
  
  # ## Submission confirmation
  output$confirmation <- renderText({
    paste("Answers and results successfully submitted - cheers!")
  })
  # 
  # Submit answers 
  observeEvent(input$email,{
    
    studentResults <- data.frame(date_of_test = date(Sys.time()),
                                 time_of_test = format(as.POSIXct(Sys.time()), format = "%H:%M"),
                                 course = input$course,
                                 test_location = input$location,
                                 handedness = input$hand,
                                 alphabet = names(alphabet),
                                 practice_time = practiceTIME,
                                 first_test = ifelse(firstTest==testTimes[1],
                                                     "short", "long"),
                                 results_short = test10(),
                                 results_long = test20(),
                                 results_total = score(),
                                 comments = "")
    path_results <- tempfile(pattern = 'DASSmemory_results',  fileext = ".csv")
    write.csv(studentResults, path_results, row.names = FALSE)
    
    
    params <- 'lettsss goooooo! Have a good weekend :) -Em'
    
    
    email <- emayili::envelope(
      to = c("u3084083@uni.canberra.edu.au",
             "adrian.dusting@canberra.edu.au"), # feel free to put your own email address while troubleshooting! =)
      from = "11723.DASS@gmail.com",
      subject = "Memory_test_results",
      html=tagList(
        p(params)
      )
    )
    emailatt<- emayili::attachment(email, path_results)
    
    smtp(emailatt, verbose = TRUE)
    
    show('confirmation')
    
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
