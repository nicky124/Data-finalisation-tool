#options(shiny.trace=TRUE)
#DT::datatable(options = list(scrollX = TRUE))
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)
library(data.table)
library(ggplot2)
library(reshape2)
library(plyr)


shinyServer(function(session,input, output){
  
  # places to store the data, changelog and errors
  Data      <- reactiveValues(df=NULL, charData=NULL, charSumm=NULL, headers=NULL, raw=NULL, new=FALSE, GO=1)
  Changelog <- reactiveValues(merge=NULL, annotate=NULL, codes=NULL, trig=NULL)
  Errors    <- reactiveValues(Nameless=NULL, mergeMissing=NULL, notacsv=NULL)
  
  ##############################################################################
  ######################## Errors & messages ###################################
  ##############################################################################
  output$parseErrors <- renderText(Errors$Nameless)
  output$loadError   <- renderText(Errors$notacsv)
  output$mergeTable  <- renderTable(Changelog$merge)
  
  # display the current data table (including changes)
  output$fullTable <- renderDataTable(as.data.table(Data$df),options = list(scrollX = TRUE, pageLength = 5))
  
  # Show the summary table
  output$numSummary <- renderTable({subset(Data$numSummary, select=c("variable", input$numSumVars))})
  
  output$parseDataSummary <- renderText({  # A very simple data summary (rows & columns)
    paste("Your dataset has ", ncol(Data$df), " columns, of which ", ifelse(is.data.frame(Data$charData),ncol(Data$charData)-1,0), "contain non-numerical data")
  })
  
  ##############################################################################
  ####################### Read the data uploaded by the user ###################
  ##############################################################################
  observeEvent(input$data,{ # step 1, new file specified
    Data$new<-TRUE
  })
  
  # Only show the summary when data is not NULL
  output$displaySummary <- renderUI({
    ifelse(is.null(Data$df),
           tagList(
             h5("You'll need to upload some data first!")),
           tagList(
             h5( "Your data is in the table below - please check that it has uploaded correctly."),
             textOutput("parseDataSummary"))) 
  })
  
  
#  output$VIDPanelInfo <-renderText({ifelse(is.null(Data$df),"You'll need to upload some data first!", "Here's your data - browse to check that it has uploaded OK.")})
  
  output$replace <- renderUI({ # step 2, continue if there's no pre-existing dataset, otherwise, confirm
    if(is.null(Data$df) & Data$new==TRUE) {Data$GO<-Data$GO+1}
    if(!is.null(Data$df) & Data$new==TRUE) {
      tagList(actionButton("replace","Overwrite existing dataset?"),
              helpText("Your data has not been saved, consider using the export tool if you have annotated or altered your dataset."))}
  })
  observeEvent(input$replace,{ # step 2b, only proceed if user approves overwrite
    Data$GO<-Data$GO+1})
  
  observeEvent(Data$GO,{ # step 3 Proceed 
    Data$new           <-FALSE
    Data$charMelt      <-NULL
    Data$charData      <-NULL
    Data$numSummary    <-NULL
    Data$numData       <-NULL
    Data$numMelt       <-NULL
    Data$charSumm      <-NULL
    Changelog$annotate <-NULL
    Changelog$merge    <-NULL
    Changelog$codes    <-NULL
    
    Data$df <- tryCatch( # If read.csv fails, reset data to NULL (otherwise app crashes)
      read.csv(input$data$datapath, stringsAsFactors = F),
      simpleError=function(cond) NULL)
    
    if(is.null(Data$df))  {# if dataset remains NULL after call, return an error message
      if(!is.null(input$data)) Errors$notacsv <- "Sorry, invalid file"
      return()
    }
    
    # if you've made it this far, dataset has loaded successfully so remove error and load headers
    updateCollapse(session, "collapseExample", open = "View imported data", close = NULL, style = NULL)
    
    Errors$notacsv <- NULL
    Data$raw <- read.csv(input$data$datapath, stringsAsFactors = F)
    Data$headers <- read.csv(input$data$datapath, stringsAsFactors = F, header=F)[1,]
  })
  
  
  observeEvent(Data$df,{ # Things to recalculate when the dataset is changed: character and numeric subsets, summary data etc
    # indices of numeric columns
    nums <- which(sapply(Data$df[1,],is.numeric)>0)
    
    # handle exceptions (eg where "nums" is empty)
    if(length(nums)>0) DataChars <- Data$df[,-nums]
    if(length(nums)==0) DataChars <- Data$df
    
    if((nrow(DataChars)*ncol(DataChars))>0){ # Write character data only if there is some! 0x0 matrices are not NULL...
      DataChars <- cbind(DataChars,1:nrow(DataChars))
      Data$charData <- DataChars
      Data$charMelt <- melt(Data$charData, id.vars=names(Data$charData[length(names(Data$charData))]))
      Data$charSumm <-ddply(Data$charMelt,c("variable","value"),summarize,frequency=length(value))
    }
    
    if(length(nums)>0) {
      Data$numData <- cbind(Data$df[,nums],1:nrow(Data$df)) # add a column for indexing
      Data$numMelt <- melt(Data$numData, id.vars=names(Data$numData)[ncol(Data$numData)])
      xXx<- ddply(Data$numMelt,c("variable"), summarize, Min=min(value,na.rm=T), Median = median(value,na.rm=T), Mean=mean(value,na.rm=T), Max=max(value,na.rm=T), SD=sd(value,na.rm=T), NA_count=sum(is.na(value)), Missing_count=sum(value=="", na.rm=T))
      Data$numSummary<- xXx
    }
  })
  
  # Custom UI display - only shows when changes have been made
  output$displayChangelog <- renderUI({
    if(is.null(Changelog$merge)) return()
    tagList(
      h3("Changelog"),
      tableOutput("mergeTable")
    )
  })
  
  # Custom error display - only shows when one or more headers are missing NOT WORKING
  output$displayErrors <- renderUI({
    if(is.null(Errors$Nameless)) return()
    tagList(
      h3("Errors"),
      textOutput("parseErrors")
    ) 
  })
  
  # Custom error display - only shows when csv fails to load
  output$notAcsv <- renderUI({
    if(is.null(Errors$notacsv)) return()
    tagList(
      h4("Your dataset must be in .csv format, with headers in the top row only."),
      h5("If you're using Excel, the easiest way to create a .csv is to create a new worksheet then copy and paste your
         data table into this sheet, so that the top left corner of your dataset is in cell A1.
         Next, hit F12 (or file -> Save as) and choose Comma-separated values (.csv) as the file format. Only this
         table will be saved.
         "),
      
      h3("Data loading error"),
      textOutput("loadError"),
      h5("Make sure your dataset meets these criteria:"),
      h5("  1) One header row only"),
      h5("  2) .csv format (available in Excel -> Save as...)")
      ) 
  })
  
  # Only show the summary when data is not NULL
#  output$displaySummary <- renderUI({
#    if(is.null(Data$df)) return()
#    tagList(
#      h3("Data summary"),
#      textOutput("parseDataSummary")
#    ) 
#  })
  
  #####################################################################
  ######################## Annotations ################################
  #####################################################################
  
  output$annoDesc <- renderUI({
    colsInData <- as.integer(ncol(Data$df))
    tagList(helpText("You do not need to complete a description for every variable, only where the meaning is not clear from the title."),
            helpText("eg. Species codes are consistent with STARFISH (Cefas Fish Health Inspectorate)"),
            lapply(1:colsInData, function(i) {
              textInput(inputId=paste("annoDesc",i,sep=""),
                        names(Data$df)[i],
                        "")
            })
    )
  })
  
  
  
  output$annoFactor <- renderUI({
    
    if(is.null(Data$charMelt)) return(helpText("Only applicable for non-numeric variables"))
    variables=unique(paste(Data$charMelt[,"variable"]))
    
    tagList(
      helpText("You do not need to enter descriptions where the meaning is clear (for example times or dates), but you should provide explanations for any codes and/or abbreviations that appear in each column."),
      helpText("If the codes are also used in other datasets, it may be more appropriate to refer to a source in the variable description field (above)."),
      lapply(1:length(variables), function(i) {
        DM=subset(Data$charMelt,variable==variables[i])
        levels=unique(DM[,"value"])
        bsCollapsePanel(paste(variables[i]),
                        lapply(1:length(levels), function(j) {
                          textInput(inputId=paste("annoFactorLevel",i,j,sep="_"),levels[j],"")
                        }
                        ))}
      )
    )
  })
  
  observeEvent(input$collapseExample, {
    Changelog$trig <- rnorm(1)
  })
  
  
  observeEvent(Changelog$trig,{ # save the annotations when the panels change (ensures that the results are there for download)
    if(is.null(input$data)) return() # don't bother if there's no data
    
    outputOptions(output, "annoDesc", suspendWhenHidden = FALSE) # make sure the data-dependent input fields have been created
    outputOptions(output, "annoUnit", suspendWhenHidden = FALSE)
    outputOptions(output, "annoFactor", suspendWhenHidden = FALSE)
    
    colsInData <- as.integer(ncol(Data$df)) #grab the number of columns
    
    TESTER  = sapply(1:colsInData, function(j) {input[[paste("annoUnit",j,sep="")]]}) #grab the annotation fields
    TESTER2 = sapply(1:colsInData, function(j) {input[[paste("annoDesc",j,sep="")]]})
    
    annos=data.frame(Name = unlist(lapply(1:colsInData, function(i){names(Data$df)[i]}))) #create the basic annotation table
    
    if(!(sum(unlist(TESTER)=="")==colsInData)){#only add columns if one or more box has been filled in
      annos$Units  = unlist(lapply(1:colsInData, function(i){input[[paste("annoUnit",i,sep="")]]}))
    }
    if(!(sum(unlist(TESTER2)=="")==colsInData)){#only add columns if one or more box has been filled in
      annos$Description  = unlist(lapply(1:colsInData, function(i){input[[paste("annoDesc",i,sep="")]]}))
    }
    if(ncol(annos)>1){Changelog$annotate=annos}#only write the results if there are some annotations
    
    variables=unique(paste(Data$charMelt[,"variable"]))
    ANNOS=NULL
    
    if(!is.null(Data$charMelt)){# Don't do this if there's no character data!
      for(i in 1:length(variables)){
        DM=subset(Data$charMelt,variable==variables[i])
        levels=unique(DM[,"value"])
        codeAnnos=lapply(1:length(levels), function(j) {
          input[[paste("annoFactorLevel",i,j,sep="_")]]
        })
        #print(codeAnnos)
        if(sum(unlist(codeAnnos)!="")>0 & sum(!is.null(unlist(codeAnnos)))>0){ # skip this variable if there are no annotations
          neAnnos=which(codeAnnos!="") # which annotations are not empty?
          for (k in neAnnos){
            ANNOS=rbind(ANNOS,
                        c(variables[i],paste(levels[k], " = ", input[[paste("annoFactorLevel",i,k,sep="_")]], ";"))
            )
          }
        }
      }
      if(!is.null(ANNOS)) {ANNOS<- as.data.frame(ANNOS)
      if(ncol(ANNOS)>1) names(ANNOS)=c("Variable", "[Code] = [Description]")}
    }
    Changelog$codes<- ANNOS
    
  })
  
  
  output$annoUnit <- renderUI({
    colsInData <- as.integer(ncol(Data$df))
    tagList(helpText("For example metres, kilograms, minutes, timezone"),
            lapply(1:colsInData, function(i) {
              textInput(inputId=paste("annoUnit",i,sep=""),
                        names(Data$df)[i],
                        "")
            })
    )
    
  })
  
  #########################################################################
  ############################ Plotting ###################################
  #########################################################################
  
  output$plotChar <- renderPlot({
    pv<- ggplot(subset(Data$charMelt, variable%in%input$chars)) + 
      geom_bar(aes(x=as.factor(value), fill=variable),stat="Count")+ 
      coord_flip()+xlab("Value")+ylab("Frequency")+
      scale_fill_discrete(  h =input$HueC,guide=F)+theme_minimal(base_size=input$plotFontSizeC)
    
    if(input$charPlotMult==TRUE) pv<-pv+facet_wrap(~variable, scales="free")
    pv
  })
  
  output$plotVar <- renderPlot({
    pv<- ggplot(subset(Data$numMelt, variable%in%input$numPlot)) + 
      geom_histogram(aes(x=value, fill=variable)) + scale_fill_discrete( h =input$HueN,guide=F)+
      ylab("Value") + theme_minimal(base_size=input$plotFontSizeN) + xlab("Variable") +coord_flip() + ylab("Frequency") + xlab("Value")
    
    if(input$numPlotMult==TRUE) pv<-pv+facet_wrap(~variable, scales="free")
    pv
  })
  
  ###########################################################################
  ############################## MERGE ######################################
  ###########################################################################
  output$varsToMerge <- renderUI({ifelse(!is.null(Data$charMelt),
                                         tagList(selectInput("mergeVar", "Choose a variable", choices=names(Data$charData)[-ncol(Data$charData)])),
                                         tagList(h5("No Character data found")))
  })
  output$valsToMerge <- renderUI({
    TAB=table(Data$charData[,input$mergeVar])
    ifelse(!is.null(Data$charMelt),tagList(
      checkboxGroupInput("mergeVal", "Choose values to merge", choiceNames=paste(names(TAB), " (", TAB, " occurrences)",sep=""), choiceValues=names(TAB))),
      tagList(h5("")))
  })
  output$levelMerge <- renderUI({
    if(!is.null(input$mergeVal)) textInput("levelname","Replace levels with: ", value=input$mergeVal[1])
  })
  
  # Display error message below merge button if no levels are selected
  output$mergeError=renderText(Errors$mergeMissing)
  
  # Perform the merge/replacement
  observeEvent(input$mergeButton,{
    if(length(input$mergeVal)==0) Errors$mergeMissing = "Please select one or more levels to replace" # return error if no levels are selected
    if(length(input$mergeVal)>0) { # otherwise proceed with replacement
      newData=Data$df
      newData[which(newData[,input$mergeVar]%in%as.character(input$mergeVal)),input$mergeVar]=as.character(input$levelname)
      Data$df <- newData
      mergeDetails <- data.frame(# Record the changes made 
        Variable=input$mergeVar,
        Original=input$mergeVal,
        Replacement=input$levelname)      
      Changelog$merge = rbind(Changelog$merge,mergeDetails)
    }
  })
  
  output$mergeButton<-renderUI({ifelse(!is.null(Data$charMelt),
                                tagList(actionButton("mergeButton", "Merge selected levels")),
                                tagList(h5("")))
  })
  ##############################################################################
  ########################## Progress, notes etx ###############################
  ##############################################################################
  
  output$Potential_functions <- renderText({paste("Other options: ",
                                                  "Join csvs together (checking names and formats);",
                                                  "Highlight potential outliers in each column;",
                                                  "Option to email dataset direct to Data manager (only if MDR link is included)", sep="\n")
  })
  
  output$Functions_available <- renderTable({
    Functionality=c(
      "Import a .csv dataset",
      "Import datasets in other (text) formats",
      "Display customisable plot summaries of character and numeric data",
      "Display table summaries (range, min, max, mean)",
      "Allow users to view unique factor levels and merge them where appropriate",
      "Error Handling (dataset too large, column headers missing, no numerical data...",
      "Guide users through the annotation of their dataset (providing units, format, code descriptions etc",
      "Export the dataset (original and modified)",
      "Checksums for dimensions of input and output tables",
      "Export annotation table",
      "Export list of changes (if any) made to the original dataset",
      "Instruct users on what to do next (eg. metadata on the MDR, contact Data Manager)."  
    )
    
    Completion=c(100,
                 0,
                 100,
                 100,
                 100,
                 50,
                 75,
                 100,
                 0,
                 100,
                 100,
                 0)
    
    df=data.frame(Completion,Functionality)
    names(df)[1]="Completion (%)"
    return(df)
    
  })  
  
  #################################################################
  ##################### Download ##################################
  #################################################################
  
  output$DataPack <- downloadHandler(
    filename = 'DataPack.zip',
    content = function(filename) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      print (tempdir())
      
      if(!is.null(Changelog$merge)){
        filenamec<- "Changelog.csv"
        write.csv(Changelog$merge,filenamec)
        fs<-c(fs,filenamec)
      }
      if(!is.null(Data$raw)){
        filenamec <- "RawData.csv"
        write.csv(Data$raw,filenamec)
        fs<-c(fs,filenamec)
      }
      if(!is.null(Data$df)){
        filenamec <- "ModifiedData.csv"
        write.csv(Data$df,filenamec)
        fs<-c(fs,filenamec)
      }
      if(!is.null(Changelog$codes)){
        filenamec <- "Codes.csv"
        write.csv(Changelog$codes,filenamec)
        fs<-c(fs,filenamec)
      }
      
      if(!is.null(Changelog$annotate)){
        #if(!(nrow(Changelog$annotate)==sum(Changelog$annotate[,c("Description","Units")]==""))){
        filenamec <- "Annotations.csv"
        write.csv(Changelog$annotate,filenamec)
        fs<-c(fs,filenamec)
      }
      #}
      zip(zipfile=filename, files=fs)
    }
  )
  
  ########################################################################################################
  ####################### Panel shown depends on presence of character data ##############################
  ########################################################################################################
  
  output$charPanelTitle <- renderText({ifelse(is.null(Data$charData),"No character data found","Character data")})
  
  output$charPanel0 <- renderUI({if(is.null(Data$charData)){
    helpText("No character data found - you may not have uploaded data yet, or maybe it is all numeric data?")} else {
      theChoices=names(Data$charData)[-ncol(Data$charData)] #removes the id column from the list of choices
      tagList(h5("At least one column of your data has been interpreted as non-numeric. Including symbols or letters in columns that contain counts or measurements often causes problems during analysis, and should be avoided where possible. Notable exceptions include dates and times, but it's still important to format these consistently"),
              h5("A maximum of four variables are plotted by default, but more can be added by clicking below."),
        bsCollapse(bsCollapsePanel("Plot options",
                                         checkboxGroupInput("chars", "Select variables to plot", 
                                                            choiceNames=theChoices, 
                                                            choiceValues=theChoices, 
                                                            selected=theChoices[1:min(4,length(theChoices))],inline=TRUE),
                                         checkboxInput("charPlotMult","Split by variable?", value=TRUE),
                                         sliderInput("HueC","Colours",min=0,max=350,value=c(100,260)),
                                         sliderInput("plotFontSizeC","Font size",min=5,max=50,value=18)
      )),
      # rHandsontableOutput("sumChar"),
      plotOutput("plotChar"))
    }}
  )
  
  
  
  
  ########################################################################################################
  ####################### Panel shown depends on presence of numeric data ################################
  ########################################################################################################
  output$numPanelTitle <- renderText({ifelse(is.null(Data$numData),"No numeric data found","Numeric data")})
  
  output$numPanel0 <- renderUI({if(is.null(Data$numData)){
    helpText("No numeric data found - you may not have uploaded data yet, or maybe it is all character data?")} else {
      theChoices=names(Data$numData)[-ncol(Data$numData)]      
      tagList(#helpText("Any characters (other than NA,NaN,+/-Inf and NULL) 
        #will register as non-numeric data in R, and will cause the whole column to be categorised as text.
        #Avoid including units, symbols and qualifiers (cm, m, @10C etc) together  with your measurements and instead 
        #use a separate column or table for this information."),
              h5("A maximum of four variables are plotted by default, but more can be added by clicking below."),
              helpText("If columns are missing they may contain text, in which case they will be listed in the 'Character data' panel"),
              bsCollapse(bsCollapsePanel("View data summary",
                                         checkboxGroupInput("numSumVars","Choose metrics", choiceNames = unique(names(Data$numSummary)[-1]), choiceValues = unique(names(Data$numSummary)[-1]),selected=unique(names(Data$numSummary)[-1]),inline=T),
                                         tableOutput("numSummary"))
              ),
              bsCollapse(bsCollapsePanel("Plot options",
                                         checkboxGroupInput("numPlot", "Choose variables to plot", 
                                                            choiceNames=theChoices,
                                                            choiceValues=theChoices,
                                                            selected=theChoices[1:min(4,length(theChoices))],inline=TRUE),
                                         checkboxInput("numPlotMult","Split by variable?", value=TRUE),
                                         sliderInput("HueN","Colours",min=0,max=350,value=c(100,260)),
                                         sliderInput("plotFontSizeN","Font size",min=5,max=50,value=18)
              )
              ),
              plotOutput("plotVar")
      )}})
})


