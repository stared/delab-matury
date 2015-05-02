library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  data <- read.csv("../../dane/przetworzone/sumy_laureaty.csv")
  
  # histogram bez podziału na grupy
  ggHistMatury<-function(nazwa) {
    sum_wynik <- data[nazwa]
    procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
    colnames(procent_wynik) <- c("procent_wynik")
    krok <- 100 * 1/max(sum_wynik,  na.rm=T)
    p <- ggplot(procent_wynik, aes(x=procent_wynik, y = ..density.. * 100)) +
      geom_histogram(colour="white", binwidth=krok) +
      xlab("% punktów") +
      ylab("% uczniów") +
      ggtitle(nazwa) 
    return(p)
  }
  
  output$distPlot <- renderPlot({
    #x    <- faithful[, 2]  # Old Faithful Geyser data
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggHistMatury("j_polski_podstawowa")
  })
  
})
