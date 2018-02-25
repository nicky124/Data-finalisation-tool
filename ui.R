
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)
#library(rhandsontable)
library(shinyBS)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data finalisation tool (ALPHA)"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(#textOutput("downloadlocation"),
      
      fileInput("data",
                "Load dataset (.csv files only for now)",
                multiple=TRUE,
                accept=c("text/csv")),
      uiOutput("replace"),
      uiOutput("notAcsv"),
      uiOutput("displayErrors"),
      uiOutput("displayChangelog")
    ),
    
    
    mainPanel(
      bsCollapse(id="collapseExample",
                 open="Welcome",
                 bsCollapsePanel("Welcome",
                                 h5("This tool has been produced to help you to correct and annotate your data."),
                                 h5("Upload a table and work through each panel on the right to produce a data pack (.zip file) that will help others to understand your dataset. 
                                    ")
                                 # tableOutput("charsumtab"),
                                 #      tableOutput("mergeTable"),
                                 
                                 #h5("Welcome to the data finalisation tool. This app is currently in development, but will allow you to perform the following functions:"),
                                 #tableOutput("Functions_available"),
                                 #textOutput("Potential_functions",inline=F)
                                 ),
                 bsCollapsePanel("View imported data",
                                 uiOutput("displaySummary"),
                                 #textOutput("VIDPanelInfo"),
                                 dataTableOutput("fullTable")),
                 bsCollapsePanel(textOutput("charPanelTitle"),
                                 uiOutput("charPanel0")),
                 bsCollapsePanel(textOutput("numPanelTitle"),
                                 uiOutput("numPanel0")),
                 bsCollapsePanel("Merge factor levels",
                                 helpText("This section lists the unique values in the selected column. Typing errors may cause problems in analysis - for example Female and female may be treated as different levels. White space may also produce similar errors which can be hard to spot. If a level is listed twice in the list below, you may have such an error. You can overwrite the selected values by typing a new value and clicking the Merge button."),
                                 uiOutput("varsToMerge"),
                                 uiOutput("valsToMerge"),
                                 uiOutput("levelMerge"),
                                 uiOutput("mergeButton"),
                                 textOutput("mergeError"),
                                 textOutput("DataMod")),
                 # rHandsontableOutput("TEST"),
                 bsCollapsePanel("Annotate dataset",
                                 h5("Annotating your dataset helps others to understand your data. As a bare minimum you should include the units of measurement, and describe any acronyms or codes that have been used. Descriptions are encouraged."),
                                 bsCollapse(bsCollapsePanel("Descriptions (for each column)",
                                                            uiOutput("annoDesc")),
                                            bsCollapsePanel("Units (for each column)",
                                                            uiOutput("annoUnit")),
                                            bsCollapsePanel("Codes and acronyms (for unique levels within character columns)",
                                                            uiOutput("annoFactor")))),
                 bsCollapsePanel("Export dataset",
                                 downloadButton("DataPack", label = "Export data and annotations"),
                                 HTML("
                                      <br>
                                      Click the link to download a data pack (.zip file) containing: <p><p>
                                      1) Your original, unmodified dataset (rawdata.csv)<br>
                                      2) A list of changes made to the dataset by this tool (changelog.csv) <br>
                                      3) A table of annotations (annotations.csv) and codes (codes.csv) <br>
                                      4) The modified dataset (modifieddata.csv)<p>
                                      We hope you've found this tool useful. To provide any feedback, or to report an error, please contact:"
                                 ),
                                 tags$a(href="mailto:nicola.mcpherson@cefas.co.uk?subject=Data Finalisation Tool", "Nicky McPherson")
                 ))
    )
  )
))
