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
library(gridExtra)
library(shiny)
library(DT)

# Define UI for application
ui <- fluidPage(
  rm(list = ls()),
  #remove(input, tmp, nanodat,meltPlot,plotVals),
   # Application title
   titlePanel("Nanodrop Plotter"),
   
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "fileLoad", label = "Load ndv file", accept = c(".ndv", ".tsv", ".csv"), multiple = TRUE),
         textInput(inputId = "PlotTitle", label = "Title:", value = "Nanodrop Plot"), 
         downloadButton(outputId = "downloadplot", "Save Image"),
         # downloadButton(outputId = "downloadtable", "Save table"),
         downloadButton(outputId = "exportPDF", "Save Page"),
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
           dataTableOutput(outputId = "NanoTable")
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
      updateTextInput(inputId = "PlotTitle", value = tools::file_path_sans_ext(input$fileLoad$name))
    }
    
    for (i in 1:numfiles) {
      if ( exists(x = "nanodat") == FALSE) {
        nanodat <- read.delim(file = inFile[[i, 'datapath' ]], header = TRUE, skip = 4)
      } else {
        tmp <- read.delim(file = inFile[[i, 'datapath' ]], header = TRUE, skip = 4)
        nanodat <- bind_rows(nanodat,tmp)
      }
    }
    nanodat <- cbind(id=(1:nrow(nanodat)), nanodat)
    nanodat
  })
  
  plotInput <- function(nanodat) {
    if (nrow(nanodat) == 0) {
      ggplot() +
        geom_blank() + 
        theme_bw() +
        theme(panel.grid = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(text = element_text(size = 20, colour = "Black")) +
        theme(axis.text = element_text(size = 15, colour = "Black")) +
        geom_vline(xintercept = c(230,260,280), colour = "Black", linetype = "dotted") +
        geom_hline(yintercept = 0, colour = "Black", linetype = "dotted") +
        scale_x_continuous(name = "Wavelength (nm)", breaks = c(230, 260, 280),labels = c("230", "260", "280"), limits = c(220, 350), expand = c(0, 0)) +
        scale_y_continuous(name = "10mm Absorbance") +
        labs(colour = "Samples") +
        ggtitle(label = input$PlotTitle) +
      return()
    }
    nanodat <- subset(nanodat, select = c(Sample.ID, X220:ncol(nanodat)))
    meltPlot <- melt(nanodat, id=(c("Sample.ID")))
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
      ggtitle(label = input$PlotTitle) +
      NULL
  }
  
    
  # output$NanoPlot <- renderPlot(height = 600, {
  #   plotInput()
  #  })
  
  observe({
    #req(input$NanoTable_rows_selected)
    if(is.null(input$NanoTable_rows_selected)) {
      plotdata <- nanoplotdata()
    } else {
      plotdata <- nanoplotdata()
      plotdata <- plotdata[input$NanoTable_rows_selected, ]
    }
    
    output$NanoPlot <- renderPlot(height = 600, {
      print("I've made it this far")
        plotInput(plotdata)
       })
  })
  
  output$downloadplot <- downloadHandler(
     filename = function() { paste0(input$PlotTitle, '.tiff', sep = "") },
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
      rename(c("Sample.ID" = "Sample", "ng.ul" = "ng/uL", "Cursor.abs." = "A230", "X260.280" = "260/280", "X260.230" = "260/230")) %>%
        datatable(extensions = 'Buttons',
                  selection = list(mode = 'multiple', target = 'row') ,
                  options = list(dom = "Blfrtip",
                                 buttons = list("copy",
                                                list(extend = "collection",
                                                     buttons = c("csv", "excel", "pdf"),
                                                     text = "Download")), # end of buttons customization
                                 # customize the length menu
                                 lengthMenu = list(c(10, 20, -1), # declare values
                                                   c("10", "20", "All") # declare titles
                                                    ), # end of lengthMenu customization
                                 pageLength = 12) # end of options
                  ) # end of datatables
      )
    })
  
  
  output$NanoTable <- renderDT({
    Generate_nanoTable()
  })
  
  # output$downloadtable <- downloadHandler(
  #   filename = function() { paste0(input$PlotTitle, '.tsv', sep = "") },
  #   content = function(file) {
  #     write.table(x = Generate_nanoTable(), file = file, row.names = FALSE, sep = "\t")
  #   },
  #   contentType = "text/csv"
  # )

  # output$exportPDF <- downloadHandler(
  #   filename = function() { paste0(input$PlotTitle, '.pdf', sep = "") },
  #   content = function(file) {
  #     pdf(file = file, onefile = TRUE, paper = "a4r", width = 10)
  #     grid.arrange(plotInput(), tableGrob(Generate_nanoTable()))
  #     dev.off()
  #   
  #   },
  #   contentType = "application/pdf"
  # )
  output$exportPDF <- downloadHandler(
    filename = function() { paste0(input$PlotTitle, '.pdf', sep = "") },
    content = function(file) {
      pdf(file = file, onefile = TRUE, paper = "a4", width = 8, height = 11)
      grid.arrange(plotInput(), tableGrob(Generate_nanoTable()))
      dev.off()

    },
    contentType = "application/pdf"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

