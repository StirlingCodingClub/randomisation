#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  
  column(4,
         fluidRow(
           column(4, offset = 1,
                  actionButton("go", "Randomise"), 
                  
           ),
           column(1, offset = 0),
           column(4, offset = 1,
                  actionButton("reset", "Reset")
           ),
         ),
         DT::dataTableOutput("tt")
  ),
  
  column(8, 
         plotOutput("rplot")
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sp   <- c(rep("SO1", 10), rep("SO2", 14))
  eg   <- c(50, 52, 53, 54, 54, 49, 50, 50, 52, 51,
            44, 44, 46, 36, 51, 50, 36, 45, 51, 45, 49, 39, 51, 43);
  wp   <- as.data.frame(cbind(sp, eg));
  difs <- tapply(X = eg, INDEX = sp, FUN = mean);
  df1  <- difs[1] - difs[2];
  
  M  <- reactiveValues(data = df1)
  N  <- reactiveValues(data = 1:24)
  W  <- reactiveValues(data = as.data.frame(cbind(sp, eg)))
  P  <- reactiveValues(data = df1);
  
  observeEvent(input$go, {
    theord  <- sample(1:24, 24);
    Species <- sp[theord];
    Eggs    <- eg;
    N$data  <- theord;
    W$data  <- as.data.frame(cbind(Species, Eggs));
    difs    <- tapply(X = eg, INDEX = sp[theord], FUN = mean);
    dval    <- difs[1] - difs[2];
    M$data  <- c(M$data, dval);
    P$data  <- dval;
  })
  
  observeEvent(input$reset, {
    N$data  <- 1:24;
    M$data  <- df1;
    W$data  <- as.data.frame(cbind(sp, eg))
  })
  
  output$rplot <- renderPlot({
    breaks <- seq(from = -8, to = 8, by = 1)
    par(mar = c(5, 5, 1, 1));
    hist(M$data, main = "", xlab = "Random mean difference", cex.lab = 1.5, 
         cex.axis = 1.5, xlim = c(-8, 8), ylim = c(0, 20), breaks = breaks,
         col = "grey");
    arrows(x0 = df1, x1 = df1 , y0 = 6, y1 = 1.5, 
           length = 0.15, lwd = 4, col = "red")
    text(x = df1 , y = 6.5, labels = "Observed", cex = 1.5, col = "red")

    answer <- paste("E[SO1] - E[SO2] = ", round(P$data, digits = 3))
    text(x = 0, y = 19.5, labels = answer, cex = 2.5)
    hist(df1, main = "", xlab = "Random mean difference", cex.lab = 1.5, 
         cex.axis = 1.5, xlim = c(-8, 8), ylim = c(0, 10), breaks = breaks,
         add = TRUE, col = "red");
  }, height = 800)

  output$tt <- DT::renderDataTable({
    
    wp   <- data.frame(Species = W$data[,1], Eggs = W$data[,2])
    ddd  <- datatable(wp)
    formatStyle(ddd, "Species", 
                backgroundColor = styleEqual( c("SO1", "SO2"), 
                                              c("#E69F00", "#56B4E9")
                ),
                fontWeight = 'bold',
                pageLength = 50
    )
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)




