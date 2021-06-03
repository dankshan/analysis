## Important: ##  


library(readr)  
library(svydiags) 
library(dplyr)
library(shiny)
library(DT)
library(survey)   
library(gtools)
library(ggplot2)
library(InformationValue)
library(cyphr)
library(openssl)
library(keyring)
#dir0 <- '/Users/criv292/DropboxPersonal/Dropbox (Uni of Auckland)/CRR-DropBox/Y19_Weighting/weighting/surveydata/'
#dir0a <- '/Users/criv292/DropboxPersonal/Dropbox (Uni of Auckland)/CRR-DropBox/Y19_Weighting/weighting/educationcounts/'
#SurveyPath <- "/Users/criv292/DropboxPersonal/Dropbox (Uni of Auckland)/CRR-DropBox/Y19_Weighting/data/youth2019.rds"
 

dirShinydata<-   'data/'

 
source('helper1_DataWrangling.R')

source('helper2_Module_Plot.R')
source('helper2_Module_Totals.R')
source('helper2_Module_Model.R')



ui <- navbarPage(   "YOUTH-19", # img(src='image1.png',align = "left",height='35px',width='100px'), 
                     tabPanel('FilteringWha',    sidebarLayout( 
                                sidebarPanel( 
                                  radioButtons("FilteringWha",    "Include Wharekura?" ,  choices = list("Exclude"   ,  'Include' ),  selected = 'Include') 
                             # selectizeInput(  ('idFiltering'), label = "Variable Filter", options= list(maxOptions = 2000),c('None', VarFilter) , selected = 'None' ),
                              #selectInput('idCategoriesFiltering', 'Select categories:',  '', '' , multiple = T  )
                                )  ,  
                              mainPanel(verbatimTextOutput('ididPrint' )
                              ))) , 
                   tabPanel('Plots', uiOutput("RenderUIPlot")),
                   tabPanel("Totals/Means/Proportions", uiOutput('RenderUITotals')),
                   tabPanel("Linear and Logistic Models", uiOutput('RenderUIModels'))  
                   )

server <- function(input, output, session ) { 
   
  
  VarCalRe  <-   reactive({ 
    if(input$FilteringWha=="Exclude"){  
       VarCal2 <-  VarCal
     }
    
    if(input$FilteringWha=="Include"){  
      VarCal2 <- VarCalWha
    } 
  VarCal2}
  ) 
  
  #observeEvent(input$idFiltering,{   
   # if( (input$idFiltering)!='None'){
    #  updateSelectInput(session = getDefaultReactiveDomain()
     #                   , inputId = "idCategoriesFiltering"
      #                  , choices = unique( (mydata[, input$idFiltering]))
      #                  , selected=unique( (mydata[, input$idFiltering]))[1]) } }
  #)   
  
  
  mydataRe   <- reactive( {
    if( input$FilteringWha=='Include') {
    mydata2 <-  mydata
    }
  
   if( input$FilteringWha=='Exclude') {
    mydata2  <- mydata[mydata$Wharekura==0, ]
   } 
   
  return(mydata2) }
  )
  
  
  NatDatRE   <-  reactive({ 
    if(  input$FilteringWha=='Include') {
      NatDat   <-  cbind(DataNational[,   c(VarCalRe(), 'TotalRoll')])
    }
  
    if( input$FilteringWha=='Exclude') {
      NatDat   <-   cbind(DataNational[ DataNational$WharekuraEligible ==0,   c(VarCalRe(), 'TotalRoll')])
    } 
     
          
    return(  NatDat  ) }
  )
   
 

 RegDatRE   <-  reactive({ 
    if( input$FilteringWha=='Include') {
      NatDat   <-  cbind(RegionalData[,  c(VarCalRe(), 'TotalRoll')])
    }
    if(  input$FilteringWha=='Exclude') {
      NatDat   <-   cbind(RegionalData[RegionalData$WharekuraEligible ==0,   c(VarCalRe(), 'TotalRoll')])
    } 
    
    return(  NatDat)}
  )
  

  
 N  <- reactive({   sum(NatDatRE()$TotalRoll)    })
 NReg <- reactive({    sum(RegDatRE()$TotalRoll)  })
  
  
 NatDatTotRE  <- reactive({ 
   aux <-   colSums( cbind(NatDatRE()  ))
   aux[names(aux)!='TotalRoll']
 
 })
  
  
 RegDatTotRE  <-  reactive({ 
   aux <-   colSums(cbind(RegDatRE() ))
   aux[names(aux)!='TotalRoll']
   
   
 })
  
  
 
  
  
  
  output$RenderUIPlot <- renderUI({  selectVarPlotUI("PlotVarInput", "Var to Plot", VarPlot= VarPlot ,    VarCal= VarCalRe()  ) })
  
  output$RenderUITotals <- renderUI({  selectVarTotalsUI("TotalsVarInput", "Totals ", VarTotals=VarTotals, VarTotalsCat = VarTotalsCat,  VarCal= VarCalRe() ) })
  
  output$RenderUIModels <- renderUI({  selectVarModelUI("ModelsVarInput", "Models ", VarModels =VarModels ,VarModelsCon=VarModelsCon, VarCal= VarCalRe() ) })
  
  
  callModule(selectVarPlot, "PlotVarInput"  ,   mydata=   mydataRe  , NationalTotalsCal =   NatDatTotRE , RegionalTotalsCal=   RegDatTotRE ,    AuxBinary= AuxBinary, AuxCharacter=AuxCharacter  , AuxNumeric  = AuxNumeric,  VarCal=  VarCalRe , N=N, NReg=NReg  )
  callModule(selectVarTotals, "TotalsVarInput"  ,   mydata=   mydataRe  , NationalTotalsCal =   NatDatTotRE , RegionalTotalsCal=   RegDatTotRE ,    AuxBinary= AuxBinary, AuxCharacter=AuxCharacter  , AuxNumeric  = AuxNumeric,  VarCal=  VarCalRe , N=N, NReg=NReg  )
  callModule(selectVarModel, "ModelsVarInput"  ,    mydata=   mydataRe  , NationalTotalsCal =   NatDatTotRE , RegionalTotalsCal=   RegDatTotRE ,    AuxBinary= AuxBinary, AuxCharacter=AuxCharacter  , AuxNumeric  = AuxNumeric,  VarCal=  VarCalRe , N=N, NReg=NReg   )
  
  
     
  
  
}
  shinyApp(ui, server) 
  