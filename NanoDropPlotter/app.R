#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(reshape)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Nanodrop Plotter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "fileLoad", label = "Load ndv file", accept = ".ndv", multiple = TRUE) # ADD MULTIPLE IN FUTURE
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         width = 10, plotOutput("NanoPlot"), downloadButton(outputId = "downloadplot", "Save image")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  nanoplotdata <- reactive({
    inFile <- input$fileLoad
    if (is.null(inFile)) {
      return(NULL)
    } else {
      numfiles = nrow(inFile)
    }
    
    for (i in 1:numfiles) {
      if ( exists(x = "nanodat") == FALSE) {
        nanodat <- read.delim(file = inFile[[i, 'datapath' ]], header = TRUE, skip = 4)
      } else {
        tmp <- read.delim(file = inFile[[i, 'datapath' ]], header = TRUE, skip = 4)
        nanodat <- bind_rows(nanodat,tmp)
      }
    }
    nanodat
  })
  
  output$NanoPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      if (is.null(nanoplotdata())) {
        return()
      }
     plotVals <- subset(nanoplotdata(), select = c(Sample.ID, X220:ncol(nanodat)))
      meltPlot <- melt(plotVals, id=(c("Sample.ID")))
      meltPlot$variable <- substring(meltPlot$variable, 2)
      ggplot(meltPlot) + 
        theme_bw() + 
        theme(panel.grid = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        geom_line(aes(x = as.numeric(variable), y = value, group = Sample.ID, colour = Sample.ID), size = 2) + 
        geom_vline(xintercept = c(230,260,280), colour = "Black", linetype = "dotted") +
        geom_hline(yintercept = 0, colour = "Black", linetype = "dotted") +
        scale_x_continuous(name = "Wavelength (nm)", breaks = c(230, 260, 280),labels = c("230", "260", "280"), limits = c(220, 350), expand = c(0, 0)) + 
        scale_y_continuous(name = "10mm Absorbance") + 
        labs(colour = "Samples") + 
        ggtitle(label = "Nanodrop Results") +
        NULL
   })
   # https://stackoverflow.com/questions/40666542/shiny-download-table-data-and-plot?rq=1
   output$downloadplot <- downloadHandler(
     filename <- function() {
       paste0('NanoPlot', 'jpeg', sep = ".")
     },
     content <- function(file) {
       tiff(filename = file,width = 1000, height = 800)
       
       plotVals <- subset(nanoplotdata(), select = c(Sample.ID, X220:ncol(nanodat)))
       meltPlot <- melt(plotVals, id=(c("Sample.ID")))
       meltPlot$variable <- substring(meltPlot$variable, 2)
       ImageSave <- ggplot(meltPlot) + 
         theme_bw() + 
         theme(panel.grid = element_blank()) + 
         theme(plot.title = element_text(hjust = 0.5)) + 
         geom_line(aes(x = as.numeric(variable), y = value, group = Sample.ID, colour = Sample.ID), size = 2) + 
         geom_vline(xintercept = c(230,260,280), colour = "Black", linetype = "dotted") +
         geom_hline(yintercept = 0, colour = "Black", linetype = "dotted") +
         scale_x_continuous(name = "Wavelength (nm)", breaks = c(230, 260, 280),labels = c("230", "260", "280"), limits = c(220, 350), expand = c(0, 0)) + 
         scale_y_continuous(name = "10mm Absorbance") + 
         labs(colour = "Samples") + 
         ggtitle(label = "Nanodrop Results") +
         NULL
       print(ImageSave)
       
       dev.off()
     },
     contentType = "image/png"
   )
       
}

# Run the application 
shinyApp(ui = ui, server = server)

