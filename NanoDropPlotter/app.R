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

# Define UI for application
ui <- fluidPage(
   remove(input, tmp, nanodat,meltPlot,plotVals),
   # Application title
   titlePanel("Nanodrop Plotter"),
   
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "fileLoad", label = "Load ndv file", accept = ".ndv", multiple = TRUE), # ADD MULTIPLE IN FUTURE
         downloadButton(outputId = "downloadplot", "Save image"),
         downloadButton(outputId = "downloadtable", "Save table"),
         width = 2,
         NULL
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         verticalLayout(
           plotOutput(outputId = "NanoPlot")
         ),
         verticalLayout(
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           tableOutput(outputId = "NanoTable")
         )
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

  plotInput <- function() {
    if (is.null(nanoplotdata())) {
      return()
    }
    nanodat <- nanoplotdata()
    plotVals <- subset(nanodat, select = c(Sample.ID, X220:ncol(nanodat)))
    meltPlot <- melt(plotVals, id=(c("Sample.ID")))
    meltPlot$variable <- substring(meltPlot$variable, 2)
    ggplot(meltPlot) + 
      theme_bw() + 
      theme(panel.grid = element_blank()) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(text = element_text(size = 20, colour = "Black")) +
      theme(axis.text = element_text(size = 15, colour = "Black")) +
      geom_line(aes(x = as.numeric(variable), y = value, group = Sample.ID, colour = Sample.ID), size = 2) + 
      geom_vline(xintercept = c(230,260,280), colour = "Black", linetype = "dotted") +
      geom_hline(yintercept = 0, colour = "Black", linetype = "dotted") +
      scale_x_continuous(name = "Wavelength (nm)", breaks = c(230, 260, 280),labels = c("230", "260", "280"), limits = c(220, 350), expand = c(0, 0)) + 
      scale_y_continuous(name = "10mm Absorbance") + 
      labs(colour = "Samples") + 
      ggtitle(label = "Nanodrop Results") +
      NULL
  }
  
    
  output$NanoPlot <- renderPlot(height = 600, {
    plotInput()
   })
  
  output$downloadplot <- downloadHandler(
     filename = function() { paste0('NanoPlot', '.tiff', sep = "") },
     content = function(file) {
       ggsave(filename = file, plot = plotInput(), device = "tiff", width = 210, height =  148, units = "mm", dpi = 320)
     }
  )
  
  Generate_nanoTable <- reactive({
    if (is.null(nanoplotdata())) {
      return()
    }
    (returnTable <- tbl_df(nanoplotdata()) %>%
      select("Sample.ID", "Date", "Time", "ng.ul", "Cursor.abs.", "A260", "A280", "X260.280", "X260.230") %>%
      rename(c("Sample.ID" = "Sample", "ng.ul" = "ng/uL", "Cursor.abs." = "A230", "X260.280" = "260/280", "X260.230" = "260/230"))
    )
    
  })
  
  
  output$NanoTable <- renderTable({
    Generate_nanoTable()
  })
  
  output$downloadtable <- downloadHandler(
    filename = function() { paste0('NanoTable', '.tsv', sep = "") },
    content = function(file) {
      write.table(x = Generate_nanoTable(), file = file, row.names = FALSE, sep = "\t")
    },
    contentType = "text/csv"
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

