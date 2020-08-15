
##########
###### for Models

selectVarModelUI <-   function(id, label = "selectVarModels",   VarModels,  VarModelsCon , VarCal  ) {
ns               <-   NS(id)
  
sidebarLayout( 
  sidebarPanel(    tagList(h2('Model Fitting'),
                              br(), 
                             
                             textInput(ns('Password'), label = "Password",   value = PASS0),  
                           selectInput(ns('Family'), label = "Type of Model", c('Linear','Logistic') , selected = 'Linear', 
                                       multiple =FALSE, selectize = TRUE, width = NULL, size = NULL), 
                           selectInput(ns('estType'), 'Estimates: National or Regional?', c('National','Regional')  , selected = 'National' ),  
                          selectInput(ns('Weights'), label = "Sampling Weights", c('Original Weights','Calibrated Weights') , selected = 'Original Weights', 
                                       multiple =FALSE, selectize = TRUE, width = NULL, size = NULL),
                         selectInput(ns('YVar'), label = "Outcome Variable",  VarModelsCon, selected = VarModelsCon[1], 
                                         multiple =FALSE, selectize = TRUE, width = NULL, size = NULL), 
                             selectizeInput(ns('XVar'), label = "Explanatory Variables", options= list(maxOptions = 2000), choices=VarModels , selected = VarModels[1], multiple=TRUE  ), 
                           "Select the    reference level for each variable:",  
                          fluidRow(column(1,''), column(4,       selectInput( ns("XVarselect"), "VariableX", choices = VarModels[1] )   ) ,
                                        column(5, selectInput(  ns("referenceXVar"), "Reference Category", choices = df$Ref[df$Var==VarModels[1] ], selected  = (df$Ref[df$Var==VarModels[1] ])[1]  )) ), 
                             selectizeInput(ns('InterVar'), label = "Interactions",  options= list(maxOptions = 2000),choices= ''   , selected = NULL, multiple=TRUE  ),
                         br(),br(),
                         tags$hr(style="border-color: black;"),
                         "Optional:", br(),
                         selectInput(ns('calVarInput'), 'Variables for calibration', VarCal  , selected = VarCal , multiple =TRUE,
                                     selectize = TRUE, width = NULL, size = NULL)
                             
    ) 
    ) , 
    mainPanel( p('Note: National estimates assume that the  sample is informative of the national  population. Calibration
                 uses the main demographic factors  to adjust the sampling weights so that the sample is representative of the population of interest. 
                 An implicit assumption of calibration in the Youth19 survey is that outcomes/variables of interest  do not depend on regional level factors, 
                 but that they can be explained by other factors such as age, gender, etc. 
                 This extrapolation can be done for outcomes/variables where  there is compelling evidence (from experts) that the factors contributing to such 
                 outcomes are very similar in all the regions in NZ after controlling for the calibration variables. 
                 This assumptions should use scientific judgment and common sense to make inferences that go beyond the limitations of statistics.', style = "color:#BD0759;"   ),
               br() , h2("Results from the model:"),    
              div(   dataTableOutput(ns('PrintModelOut')), style = "font-size:70%")   ,br(), br(), br(), br(),
                  fluidRow( column(5,h4("Reference categories for each variable:") ,  div(dataTableOutput(   ns("SelectedRef") )    , style = "font-size:70%") ) , 
                            column(1,''),column(6, h4("Goodness of Fit") , 
                    div(dataTableOutput(   ns("list2")   ),  style = "font-size:70%") ) ),
              br(), br(),
              fluidRow( column(5,h4("Classification table:"), h6("*Only valid for the sample. These results do not extrapolate to the regional or national populations:") ,  div(verbatimTextOutput(ns("tableClas") )    , style = "font-size:50%") )   )
              ,   br(),br(),  
              
          br(), br(), hr(), 'Diagnostic Plots',  plotOutput(ns('plotDiag') )
              
               ) )
  
} 


selectVarModel <- function(input, output, session , mydata, NationalTotalsCal ,RegionalTotalsCal, AuxBinary , AuxCharacter , AuxNumeric,   VarCal, N, NReg   ) {

  
  
  observeEvent(input$Weights,{ 
    
    if(input$Weights=="Calibrated Weights" ){
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "calVarInput"
                        , choices = VarCal()
                        , selected= VarCal() ) }
    
  })  
  ## finish observeEvent 
  
  observeEvent(input$estType,{ 
    if(input$estType=="National" ){
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "Weights"
                        , choices = "Calibrated Weights"
                        , selected="Calibrated Weights") } 
    
    if(input$estType=="Regional" ){
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "Weights"
                        , choices = c("Original Weights","Calibrated Weights")
                        , selected="Calibrated Weights") } 
  }) 
  
  
  
  
   global <- reactiveValues(checked = checked)
  global2 <- reactiveValues(checked = checked)
  
  
  
  
  
  ### To choose the reference categories: 
  AAA <- reactive( { 
 
    if(input$Password==PASS){
    input$referenceXVar
      isolate({
        if(!is.null(input$referenceXVar)){
          possible <- as.character(df$Ref[df$Var %in% input$XVarselect])
          for(nr in 1:length(possible)){
            global2$checked[[possible[nr]]] <- (possible %in% input$referenceXVar)[nr]
          }
        }  
      }
      )
      AAA    <-  df[unlist(global2$checked),]
      aaa    <-  (AAA[,1] %in%input$XVar)
      AAA2   <-  AAA[aaa ,]  
      for(vi in 1:nrow(AAA2)){
      AAA2[vi,2]   <- gsub(paste(AAA2[vi,1], '_', sep='') ,''  , AAA2[vi,2] )  
      }
    AAA2
    
    }
    
    
    })
  
  
  mydataRe  <- reactive({ 
  
    AAA2copy  <-  AAA() 
    mydataCopy  <- mydata()
    AnyCat  <-   sum(is.na(AAA2copy$Var))==0
    
    if(AnyCat){
    for(vi in  AAA2copy$Var ){  
     aux  <-as.character( mydataCopy[, vi ])
   
      levelsi  <- unique(aux)
      aux2  <- (AAA2copy$Ref[AAA2copy$Var==vi])[1]
      levlesiOrdered  <- c(aux2, levelsi[!levelsi%in% aux2]  )
      mydataCopy[, vi ]<- factor(aux, levels = levlesiOrdered ) 
    }
    }
    
      
   
    mydataCopy
  
  })
 
  
 
  output$SelectedRef   <- renderDataTable( data.frame(AAA()), extensions = 'Buttons',  options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))
    
  )  ) 
  

  
  observe({
    input$referenceXVar
    isolate({
      if(!is.null(input$referenceXVar)){
        possible <- as.character(df$Ref[df$Var %in% input$XVarselect])
        for(nr in 1:length(possible)){
          global$checked[[possible[nr]]] <- (possible %in% input$referenceXVar)[nr]
        }
      }
    })
    
   
  })
  
  
  observe({
    allPossibleB <- df$Ref[df$Var %in% input$XVarselect]
    updateSelectInput(session, 
                             "referenceXVar",  
                             choices = allPossibleB,
                             selected = df$Ref[unlist(global$checked)] 
    )
  })
  
 
  
  
    observeEvent(input$XVar, {  
      aux0 <- VarModelsCat
      aux  <- aux0[aux0%in% input$XVar]
      
      updateSelectInput(session, "XVarselect",
                        choices = aux )}
     )
    
    
    
    
 
 
 
 
     
  
    
 ModelRE  <- reactive({  
      if(input$Password==PASS)
      {
        VarCali       <-    input$calVarInput
        
        if(input$estType=='National'){ 
          
          des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydataRe()   ))
          
          if(input$Weights==  'Calibrated Weights'){
            aaa           <-    NationalTotalsCal()[VarCali]     
            
            formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
            desCal        <-    (survey::calibrate(des1,   formcal  , c(N(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
          }
        }
        
        
        
        if(input$estType=='Regional'){ 
          des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydataRe()   ))
          
          if(input$Weights==  'Calibrated Weights'){
            aaa           <-    RegionalTotalsCal()[VarCali]     
            
            formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
            desCal        <-    (survey::calibrate(des1,   formcal  , c(NReg(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
          }
          
        }
        
        
        
        VarTotals    <-    input$TotalVarInput1
        
        Xvar   <-  input$XVar
        Yvar   <-  input$YVar
        
        wInp         <- input$Weights
        Family         <- input$Family
        
        
        
        if( is.null(input$InterVar)){
          aux1 <- paste( c(Xvar )  , collapse = '+')  
          formModel  <-   formula(paste("as.numeric(",Yvar,')' , '~',   aux1 )  ) 
        }
        
        if(!is.null(input$InterVar)){
          aux1 <- paste( c(Xvar, input$InterVar)  , collapse = '+')  
          formModel  <-   formula(paste( "as.numeric(",Yvar,')' , '~',   aux1 )  ) 
        }
        
        
        
        
        
        ### ?binary 
        
        ### SAmpling WEgihts 
        if( wInp=='Original Weights') { 
          if(Family =='Logistic'){
            fit1 <- survey::svyglm(formModel , design = des1, family='quasibinomial',na.action='na.omit')
         }
          
          if(Family =='Linear'){  
            fit1 <- survey::svyglm(formModel , design = des1,  na.action='na.omit')
         
          }}
        
        
        
        if( wInp=='Calibrated Weights') { 
          if(Family =='Logistic'){
            fit1 <- survey::svyglm(formModel , design = desCal, family='quasibinomial', na.action('na.omit'))
           
          }
          
          if(Family =='Linear'){  
           fit1 <- survey::svyglm(formModel , design = desCal , na.action('na.omit'))
           
            
          }
        }
      }
      
      
       fit1
      
    } )
    
  
  
  
  
 
  
  TABModel  <- reactive({  
   if(input$Password==PASS)
      { 
       fit1<- ModelRE()
       Family         <- input$Family
       
        if(Family =='Logistic'){
           saa <- summary(fit1)
          TAB1 <- round(saa$coefficients[,  ], 4)
          caa <-  confint(fit1)
          seOdds <-  round(TAB1[,2]* exp(TAB1[,1]),4)
          odds <- cbind( exp(TAB1[,1]) -  1.96*seOdds, exp(TAB1[,1]) + 1.96*seOdds)
          caa <- paste( '(', round(caa[,1],4), ',',round(caa[,2],4), ')')
          odds <- paste( '(', round(odds[,1],4), ',',round(odds[,2],4), ')')
          auxodds<- round(exp(TAB1[,1]),4)
          TAB <- data.frame(TAB1, caa,  auxodds ,seOdds ,odds)
          colnames(TAB)[3: 5] <- c('t-value','p-value', 'CI (95%)')
          colnames(TAB)[6: 8] <- c('OR', 'se(OR)' ,' OR- CI  (95%)')
          
         TAB[,5]<-  as.character(TAB[,5])
          TAB[,8]<-  as.character(TAB[,8])
          
        }
        
        if(Family =='Linear'){  
     
          saa  <-  summary(fit1 )
      
          TAB1 <-  round(saa$coefficients ,4)
          caa  <- confint(fit1 )  
          caa <- paste( '(', round(caa[,1],4), ',',round(caa[,2],4), ')') 
         
          TAB <- data.frame(TAB1, caa )
    
   
          colnames(TAB)[3: 5] <- c('t-value', 'p-value', 'CI (95%)') 
         
          
            TAB[,5]<-  as.character(TAB[,5]) 
          
       
     
      }
      }
    
    
    if(input$Password!=PASS){
     TAB <- data.frame(c("Insert Password","Insert Password" ))
      
    }
    
    
  
    TAB 
    
  } )
  
  
  
  
  
  ggPlotRE <- reactive({   
    if(input$Password==PASS)
    {
       
       fit1<- ModelRE()
      
      ### SAmpling WEgihts 
          par(mfrow=c(3,2)) 
          plot(fit1,c(2))
          aa2<- svystdres(fit1 , doplot=T) 
          plot(aa2$stdresids,  scale(fitted(fit1)), xlab='St. Residuals', ylab='St. Predicted Values')
          #aa3 <- svyCooksD(fit1,   doplot=T) 
          aa4 <- svyhat(fit1, doplot=T)
          hist(aa2$stdresids ,  main='Histogram of residuals')
          
       
    }
    
    
    if(input$Password!=PASS){
       df<- data.frame(y=c("Insert Password","Insert Password" ),x=c("Insert Password","Insert Password" ))
       gg<- ggplot(df, aes(x, y ))
       print(gg)
       
    }
   
    
  } )
  
  
    
     
  
  
  output$plotDiag  <- renderPlot({ggPlotRE()}, height = 300*3, width = 600)
 
  
  output$PrintModelOut   <- renderDataTable(  data.frame(TABModel() ), extensions = 'Buttons',  options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv',   'pdf'),
        text = 'Download'
      ))
    
  )  )
  
  
  list2reac   <- reactive({ 
    
    if(input$Password==PASS){
    
      fit1 <- ModelRE() 
      Family         <- input$Family
      
      
      saa  <-  summary(fit1 )
      deviResi   <- saa$deviance
      AICf       <- saa$aic
      DeF        <- saa$df.null - nrow(saa$coeff)
      ChisTest   <- pchisq(deviResi , df=DeF, lower.tail=FALSE)
      
      if(Family=='Linear') {
        ExtraInfo  <-     data.frame( value= c(deviResi ,  AICf,DeF  )  )
      rownames(ExtraInfo) <-   c('Deviance Residual',  'AIC', 'Degrees Freedom'  )
      EI <-    data.frame(ExtraInfo  )
      
      }
        
        
        if(Family=='Logistic')
          {Rsquare    <- psrsq(fit1, method =  "Nagelkerke")
           ExtraInfo  <-     data.frame( value= c(deviResi ,   DeF ,    Rsquare)  )
           rownames(ExtraInfo) <-   c('Deviance Residual',    'Degrees Freedom', 'Pseudo-R2 (Nagelkerke)' )
          EI <-    data.frame(ExtraInfo  )
          
        }
      
      }
  
    
    if(input$Password!=PASS){
     EI <- data.frame(c("Insert Password","Insert Password" ))
      
    }
 
    
    EI
}
  )
  
  
classTableRE   <- reactive({ 
    
    if(input$Password==PASS){
      
      fit1 <-    ModelRE() 
      Family         <- input$Family
      AA  <-  c('message:', 'Only for Logistic Regression')
      
      if(Family=='Logistic'){
        pi <- fitted(fit1)
        Outcome <- fit1$y
        cutp<- optimalCutoff(Outcome, pi) 
        PredictedOutcome  <-  as.numeric(pi >= cutp)
        
        aux<- table( Outcome , PredictedOutcome)
        
        AA   <-  matrix(aux, 2,2)
        
        AA<- data.frame(AA)
        
        colnames(AA) <-  paste('Predicted:', colnames(aux))
        rownames(AA) <-  paste('Actual:', rownames(aux))
        
        
        
        PercCorrect <- c(  AA[1,1],  AA[2,2])/rowSums( AA)
        
        
        
        AA  <- cbind(AA, Perc.Correct=round(PercCorrect*100, 2) )
        
        
      }
    }
    
    if(input$Password!=PASS){
      AA  <- data.frame(c("Insert Password","Insert Password" ))
      
    }
    
    
  AA
  }
  )  
  
  
  
 
  output$tableClas  <-   renderPrint({print(classTableRE()) }  ) 
  
  
  
  
  
  
  output$list2   <-  DT::renderDataTable( list2reac(), extensions = 'Buttons',  options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))
    
  )  ) 
     
  
  
  observeEvent(input$Family, {
    
    if(input$Family=='Logistic'){
      updateSelectInput(session, "YVar",
                        choices = VarModelsBin , selected= VarModelsBin[1])}
    
    if(input$Family=='Linear'){
      updateSelectInput(session, "YVar",
                        choices = VarModelsCon , selected= VarModelsCon[1])}
    
    
  }
  )
  observeEvent(input$XVar,{ 
    VarInt2  <- ''
    Xvar     <- input$XVar
    
    if(length(Xvar) <=1 ){ 
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "InterVar"
                        , choices = ''
                        , selected= NULL)
      
    }
    
    if(length(Xvar) >= 2 ){
      VarInt   <-  combinations(length(Xvar),2,Xvar)
      VarInt2  <-  paste(VarInt[,1], '*' , VarInt[,2], sep='')  
      
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "InterVar"
                        , choices = VarInt2
                        , selected= NULL)
      
    }  })  ## finish observeEvent 
  
  

  

  

  
  
   
   
    
}
  
  
   
 
   



