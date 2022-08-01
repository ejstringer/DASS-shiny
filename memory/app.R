
library(lubridate)
library(shiny)
library(ggplot2)
library(shinyjs)
library(rsconnect)



ui <- fluidPage(
  hr(),
  useShinyjs(),
  
  textOutput('details'),
  selectInput('lab', 'Which lab are you in?', 
              c("Tuesday - 3:30",
                "Wednesday - 9:30",
                "Wednesday - 12:30",
                "Wednesday - 2:30",
                "Wednesday - 4:30",
                "Thursday - 9:30",
                "Thursday - 12:30",
                "Thursday - 3:30"
                )),
  textInput('course', "What is your course code? (eg. ENB001)"),
  radioButtons('location', "Are you taking this test in your computer lab or somewhere else?",
               c("lab", "elsewhere")),
  radioButtons('hand', "Are you left or right handed?",
               c('right', 'left')),
  radioButtons('good', "Do you think you have a good memory?",
              c("yes", "no")),
  actionButton('submitdetails', "confirm details"),
  hidden(actionButton('start','Start practicing!')),
  #numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
  #textOutput('randomtime'),
  hidden(textOutput('timeleft')),
  hidden(plotOutput('game')),
  hidden(textInput('guess', label = "your guess:")), 
  hidden(actionButton('submit', 'submit')),
  hidden(actionButton('nextseq', 'next sequence')),
  hidden(actionButton('nexttest', 'start Test')),
  hidden(downloadButton("downloadData", "Download"))
  
  
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
    hide('lab')
    hide('submitdetails')
    hide('location')
    hide('hand')
    hide('course')
    hide('good')
    
  })
  
# start practic ----------------------------------------------------------------  
  # Initialize the practice time, test times, and length of sequence

###############__########################
############# EDIT ##################### 
  
  practiceTIME <- sample(60:300, 1) # practice time
  testTimes <- sort(c(5,10))        # how long you see sequence
  remember <- 12                    # length of sequence
  
############# stop edits ############### 
################__######################
    
  timer <- reactiveVal(practiceTIME)
  timer2 <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  active2 <- reactiveVal(FALSE)
  test <- reactiveVal(0)
  
  values <- reactiveVal("black")
  
  mydf <- reactiveValues(df = data.frame(x = 1:remember, y = rep(1,remember),
                                         n = sample(c(1:9,LETTERS),
                                                   remember , replace = TRUE)))
  
  not.in.use.df <- data.frame(x = rep(1:3,3), y = rep(1:3, each= 3), 
                   n = sample(c(1:9,LETTERS), 9, replace = TRUE))
  
  
  
  test20 <- reactiveVal(0)
  test10 <- reactiveVal(0)
  score <- reactiveVal(0)
  firstTest <- sample(testTimes, 1)
  
  #guess <- reactiveVal()
  
  output$game <- renderPlot({
  
    
    df <- data.frame(x = 1:6, y = rep(1,6),
                     n = sample(c(1:9,LETTERS), 6, replace = TRUE))
    
    #plotcol <- ifelse(values(), "black", "white")
    ggplot(mydf$df, aes(x, y, label = n)) +
      geom_text(hjust=0,vjust=0, size = 10,
                colour = values())+
      ggtitle(timer2())+
      xlim(0,max(mydf$df$x)+1)+
      ylim(0,max(mydf$df$y)+1)+
      theme_void()
    
  })
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
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
    score1 <- sum(seqnc*bonus) + halfcorrect
    
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
    mydf$df$n <- sample(c(1:9,LETTERS),nrow(mydf$df), replace = TRUE)
    values("black")
    timer2(sample(testTimes, 1))
    show('game')
    active2(TRUE)
    hide('nextseq')
  })
  
  
  observeEvent(input$nexttest, {
    hide('nexttest')
    mydf$df$n <- sample(c(1:9,LETTERS),nrow(mydf$df), replace = TRUE)
    active2(TRUE)
    test(test() + 1)
    values('black')
    show('game')
    
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
                           computer_lab = input$lab,
                           test_location = input$location,
                           handedness = input$hand,
                           good_memory = input$good,
                           practice_time = practiceTIME,
                           test_times = paste(paste(testTimes,
                                                    collapse = " and "),
                                              "seconds"),
                           first_test = ifelse(firstTest==testTimes[1],
                                                "short", "long"),
                           results_short = test10(),
                           results_long = test20(),
                           score = score(),
                           comments = ""), 
                file, row.names = FALSE)
    }
  )
  
  
  
  
}

shinyApp(ui, server)


### test normality of scores
