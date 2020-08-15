
###### for totals

selectVarTotalsUI <- function(id, label = "selectVarTotals", VarTotals,  VarTotalsCat , VarCal) {
  ns <- NS(id)
  
  sidebarLayout( 
    sidebarPanel( 
      
      
      tagList(h2('Estimation of Totals, means and proportions'),
                   br(), 
              actionButton(ns("RefreshButton"),"Refresh!"), 
              textInput(ns('Password'), label = "Password",   value = PASS0),  
              selectInput(ns('estType'), 'National or Regional?', c('National','Regional')  , selected = 'National' ),  
               selectInput(ns('Weights'), label = "Sampling Weights", c( 'Calibrated Weights') , selected = 'Calibrated Weights', 
                                             multiple =FALSE, selectize = TRUE, width = NULL, size = NULL)   ,
              selectizeInput(ns('TotalVarInput1'), label = "Select Variables",  options= list(maxOptions = 2000),choices=VarTotals, selected =VarTotals[1], multiple=FALSE  ), 
              selectizeInput(ns('GroupVarInput'), label = "Group by",  options= list(maxOptions = 2000),choices=c('None',VarTotalsCat[VarTotalsCat!=VarTotals[1]]), selected ='None', multiple=TRUE  ), 
             
              br(),br(),
              tags$hr(style="border-color: black;"),
              "Optional:", br(),
              selectInput(ns('calVarInput'), 'Variables for calibration', VarCal  , selected = VarCal , multiple =TRUE,
                          selectize = TRUE, width = NULL, size = NULL),
              tags$p("*The estimates shown here remove the mising observations from the variables selected" , style = "color:#BD0759;")
              
      ) 
    ),  
    mainPanel( p('Note: National estimates assume that the  sample is informative of the national  population. Calibration
                 uses the main demographic factors  to adjust the sampling weights so that the sample is representative of the population of interest. 
                 An implicit assumption of calibration in the Youth19 survey is that outcomes/variables of interest  do not depend on regional level factors, 
                 but that they can be explained by other factors such as age, gender, etc. 
                 This extrapolation can be done for outcomes/variables where  there is compelling evidence (from experts) that the factors contributing to such 
                 outcomes are very similar in all the regions in NZ after controlling for the calibration variables. 
                 This assumptions should use scientific judgment and common sense to make inferences that go beyond the limitations of statistics.', style = "color:#BD0759;"   ),
                 br() ,
               h2("Categorical Variables") , div(dataTableOutput(ns('TableOut')), style = "font-size:70%"),br(),  br() ,  br() , 
               h2("Continuous Variables") , div(dataTableOutput(ns('TableOutCon')), style = "font-size:70%"), 
               tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}")  ) )
  
}   


selectVarTotals <- function(input, output, session , mydata, NationalTotalsCal, RegionalTotalsCal ,   AuxBinary , AuxCharacter , AuxNumeric , VarCal, N, NReg   ) {

  observeEvent(input$TotalVarInput1,{ 
    Choi <- c('None',VarTotalsCat[VarTotalsCat!=input$TotalVarInput1]) 
    updateSelectizeInput(session = getDefaultReactiveDomain()
                      , inputId = "GroupVarInput"
                      , choices = Choi
                      , selected= 'None') } )
  
  observeEvent(input$Weights,{ 
    if(input$Weights=="Original Weights" ){
      updateSelectInput(session = getDefaultReactiveDomain()
                        , inputId = "calVarInput"
                        , choices = ''
                        , selected= '') }
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
  
  ## finish observeEvent 
  
  
  aaCat <- reactive( {    ## library(DT)
    aux00 <- length(input$TotalVarInput1[input$TotalVarInput1%in% VarTotalsCat]) >0 
 
    if(input$Password==PASS){
      if( aux00){
        
      VarCali       <-    input$calVarInput
      VarTotals    <-    input$TotalVarInput1
      VarGroup    <-    input$GroupVarInput
      wInp         <- input$Weights
      desDummy         <-     (survey::svydesign(id=~1,   weights = ~  1,   data=  mydata()   ))
      
      if(input$estType=='National'){ 
      
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
        if(wInp =='Calibrated Weights'){
        
          aaa           <-    NationalTotalsCal()[VarCali]     
       
          formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
     
          desCal        <-    (survey::calibrate(des1,   formcal  , c(N(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
    
           }
      }
      if(input$estType=='Regional'){ 
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
   
        if(wInp =='Calibrated Weights'){
          aaa           <-    RegionalTotalsCal()[VarCali]     
          formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
          desCal        <-    (survey::calibrate(des1,   formcal  , c(NReg(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
       
           }
      }
      
   
   
     
      if( VarGroup =='None' )
      {
        formtotCat       <-    formula(paste( '~',  paste(  paste("  (as.factor(", VarTotals[VarTotals%in% VarTotalsCat ],"))" ), collapse = '+')))   
        
        TTDummy         <-  round( survey::svytotal( formtotCat  , design = desDummy ,   na.rm=TRUE ), 3)
        
      
      ### ?binary
       
      ### SAmpling WEgihts
      if( wInp=='Original Weights') { 
       
      
        TT <- survey::svytotal( formtotCat  , design = des1 ,   na.rm=TRUE )
        
        aux <- names(TT)
        TT <- data.frame(cbind(   round(TT,3), round(SE(TT),3)))
        colnames(TT) <- c(  'Total.Est', 'SE' )
        ci1  <- round(TT[,1] - 1.96*TT[,2],3)
        ci2  <- round(TT[,1] + 1.96*TT[,2],3)
        TT$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
        
        
        
        TT2 <- survey::svymean( formtotCat  , design = des1 ,   na.rm=TRUE)
        TT2<- data.frame(cbind( round(TT2,3), round(SE(TT2),3)))
        colnames(TT2) <- c(  'Proportion.Est', 'SE.Prop' )
        ci1  <- round(TT2[,1] - 1.96*TT2[,2],3)
        ci2  <- round(TT2[,1] + 1.96*TT2[,2],3)
        TT2$CI_Prop  <- paste( '(' , ci1, ',', ci2 , ')')
        
        
      }
      
      if( wInp=='Calibrated Weights') { 
      
         
        TT <- survey::svytotal( formtotCat , design = desCal ,   na.rm=TRUE)
        aux <- names(TT)
        TT <- data.frame(cbind(   round(TT,3), round(SE(TT),3)))
        colnames(TT) <- c(  'Total.Est', 'SE' )
        ci1  <- round(TT[,1] - 1.96*TT[,2], 3)
        ci2  <- round(TT[,1] + 1.96*TT[,2], 3)
        TT$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
        
        TT2 <- survey::svymean( formtotCat , design = desCal ,   na.rm=TRUE)
        TT2<- data.frame(cbind( round(TT2,3), round(SE(TT2),5)))
        colnames(TT2) <- c(  'Proportion.Est', 'SE.Prop' )
        ci1  <- round(TT2[,1] - 1.96*TT2[,2], 5)
        ci2  <- round(TT2[,1] + 1.96*TT2[,2],5)
        TT2$CI_Prop <- paste( '(' , ci1, ',', ci2 , ')')
    
        
       }
      
        Ta<-  (data.frame(Variable= aux, n=  as.numeric(TTDummy)  ,TT,  TT2 )) 
        
        NameAux <-     c( 'n', 'Total.Est', 'SE' ,  'Proportion.Est', 'SE.Prop' )  
            
        for(bbi in NameAux){ Ta[, bbi] <- as.numeric(Ta[, bbi])} 
        
        
       
      } 
      
      
      if( VarGroup !='None' )
      {
     
        formGroup      <-    formula(paste( '~',  paste(  paste("  (as.factor(", VarGroup[VarGroup%in% VarTotalsCat ],"))" ), collapse = '+')))   
        
       
        ### SAmpling WEgihts
        if( wInp=='Original Weights') { 
          TT2a <- NULL
          TTa <- NULL
          
          VarTot   <- VarTotals[VarTotals%in% VarTotalsCat ]
          NumbGroups  <- length(VarGroup)
           
     
          
         for( i in 1: length(VarTot)){
           VarToti  <- VarTot[i]
           
           Levelsi        <-   names(table(mydata()[,VarToti]))
           
           NoLevels       <-   length(Levelsi  )
           
           
          LevelsAux  <-  paste( 'as.numeric(',VarToti, '==', "'" ,Levelsi, "'", ')' , sep='')
          formtotCati0   <-    formula(paste( '~',   paste(LevelsAux, collapse = '+')))   
        
           TTi         <-   (survey::svyby(  formtotCati0  , formGroup , des1, svytotal , na.rm=T , drop.empty.groups=F ))
          
           TTiDummy         <-   (survey::svyby(  formtotCati0  , formGroup ,  desDummy, svytotal , na.rm=T , drop.empty.groups=F ))
           TTiEstDummy <-  cbind( TTiDummy[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
           TTiGroup    <-  data.frame( TTi[, 1:NumbGroups])
           
           TTiEst <-  cbind( TTi[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
           TTiSE  <-  cbind( TTi[,  (NumbGroups+NoLevels+1):(NumbGroups+2*NoLevels ) ])
           
           TTi0  <-  expand.grid( rownames(TTiGroup), colnames(TTiEst) ) 
           TTi0   <- data.frame(var1=TTi0[[1]], var2=TTi0[[2]])
           
         
            TTi  <-  data.frame( Variable= TTi0[, 2],  TTiGroup[  as.character(TTi0[,1])  ,  1: NumbGroups])
            
           colnames(TTi)[-1] <- paste('Group:' , colnames(TTi)[-1] )
        
         
           TTi$n         <-   round(unlist(TTiEstDummy,3))
           TTi$Total_Est <- round(unlist(TTiEst), 3)
           TTi$SE_Total <- round(unlist(TTiSE), 3)
        
           ci1  <- round(TTi$Total_Est - 1.96*TTi$SE_Total,3)
           ci2  <- round(TTi$Total_Est + 1.96*TTi$SE_Total,3)
           TTi$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
          
           TTa  <-  rbind(  TTa, TTi)
           
        
        
        TT2i         <-   (survey::svyby(  formtotCati0  , formGroup , des1, svymean , na.rm=T , drop.empty.groups=F ))
        
        TT2iGroup    <-  data.frame( TT2i[, 1:NumbGroups])
        TT2iEst <-  cbind( TT2i[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
        TT2iSE  <-  cbind( TT2i[,  (NumbGroups+NoLevels+1):(NumbGroups+2*NoLevels ) ])
        
        TT2i0  <-  expand.grid( rownames(TT2iGroup), colnames(TT2iEst) ) 
        TT2i0   <- data.frame(var1=TT2i0[[1]], var2=TT2i0[[2]])
        
        
        TT2i  <-  data.frame( Variable= TT2i0[, 2],  TT2iGroup[  as.character(TT2i0[,1])  ,  1: NumbGroups])
        
        colnames(TT2i)[-1] <- paste('Group:' , colnames(TT2i)[-1] )
        
        
        TT2i$Prop_Est <- round(unlist(TT2iEst), 3)
        TT2i$SE_Prop <- round(unlist(TT2iSE), 3)
        
        ci1  <- round(TT2i$Prop_Est - 1.96*TT2i$SE_Prop,3)
        ci2  <- round(TT2i$Prop_Est + 1.96*TT2i$SE_Prop,3)
        TT2i$CI_Prop  <- paste( '(' , ci1, ',', ci2 , ')')
        
        TT2a  <-  rbind(  TT2a, TT2i)
      
        
         }
          
           
           
        }
        
        if( wInp=='Calibrated Weights') { 
          TT2a <- NULL
          TTa <- NULL
          
          VarTot   <- VarTotals[VarTotals%in% VarTotalsCat ]
          NumbGroups  <- length(VarGroup)
          
          
          
          for( i in 1: length(VarTot)){
            VarToti  <- VarTot[i]
            
            Levelsi        <-   names(table(mydata()[,VarToti]))
            
            NoLevels       <-   length(Levelsi  )
         
           
            LevelsAux  <-  paste( 'as.numeric(',VarToti, '==', "'" ,Levelsi, "'", ')' , sep='')
            formtotCati0   <-    formula(paste( '~',   paste(LevelsAux, collapse = '+')))   
            
           
            TTi              <-    (survey::svyby(  formtotCati0  , formGroup , desCal, svytotal , na.rm =T , drop.empty.groups=F ))
        
            TTiDummy         <-   (survey::svyby(  formtotCati0  , formGroup ,  desDummy, svytotal ,  na.rm =T , drop.empty.groups=F ))
           
            TTiEstDummy <-  cbind( TTiDummy[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
     
            
            TTiGroup    <-  data.frame( TTi[, 1:NumbGroups])
            TTiEst <-  cbind( TTi[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
            TTiSE  <-  cbind( TTi[,  (NumbGroups+NoLevels+1):(NumbGroups+2*NoLevels ) ])
            
            TTi0  <-  expand.grid( rownames(TTiGroup), colnames(TTiEst) ) 
            TTi0   <- data.frame(var1=TTi0[[1]], var2=TTi0[[2]])
            
           
            TTi  <-  data.frame( Variable= TTi0[, 2],  TTiGroup[  as.character(TTi0[,1])  ,  1: NumbGroups])
            
            colnames(TTi)[-1] <- paste('Group:' , colnames(TTi)[-1] )
            
            TTi$n         <-   round(unlist(TTiEstDummy,3))
            TTi$Total_Est <- round(unlist(TTiEst), 3)
            TTi$SE_Total <- round(unlist(TTiSE), 3)
      
            ci1  <- round(TTi$Total_Est - 1.96*TTi$SE_Total,3)
            ci2  <- round(TTi$Total_Est + 1.96*TTi$SE_Total,3)
            TTi$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TTa  <-  rbind(  TTa, TTi)
            
           
            
            TT2i         <-   (survey::svyby(  formtotCati0  , formGroup , desCal, svymean , na.rm =T , drop.empty.groups=F ))
            
            TT2iGroup    <-  data.frame( TT2i[, 1:NumbGroups])
            TT2iEst <-  cbind( TT2i[,  (NumbGroups+1):(NumbGroups+NoLevels) ])
            TT2iSE  <-  cbind( TT2i[,  (NumbGroups+NoLevels+1):(NumbGroups+2*NoLevels ) ])
            
         
            
            TT2i0  <-  expand.grid( rownames(TT2iGroup), colnames(TT2iEst) ) 
            TT2i0   <- data.frame(var1=TT2i0[[1]], var2=TT2i0[[2]])
            
            
            TT2i  <-  data.frame( Variable= TT2i0[, 2],  TT2iGroup[  as.character(TT2i0[,1])  ,  1: NumbGroups])
            
            colnames(TT2i)[-1] <- paste('Group:' , colnames(TT2i)[-1] )
    
            
            TT2i$Prop_Est <- round(unlist(TT2iEst), 3)
            TT2i$SE_Prop <- round(unlist(TT2iSE), 3)
            
            ci1  <- round(TT2i$Prop_Est - 1.96*TT2i$SE_Prop,3)
            ci2  <- round(TT2i$Prop_Est + 1.96*TT2i$SE_Prop,3)
            TT2i$CI_Prop  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TT2a  <-  rbind(  TT2a, TT2i)
            
            
          }
          
          
        }
        
        
        
        
        Ta<-  (data.frame(  TTa,  TT2a[, (ncol(TT2a) -3 +1): ncol(TT2a)  ] )) 
        
        
        NameAux <-     c( 'n', 'Total_Est' ,'SE_Total' ,'Prop_Est', 'SE_Prop' )  
        
        for(bbi in NameAux){ Ta[, bbi] <- as.numeric(Ta[, bbi])} 
        
        
      }
      
      
      
      }
      
      if(!aux00){ 
     Ta<- data.frame(c('No categorical variables selected'),c('No categorical variables selected')  )
      }
       
     
      
    
    }
    
    if(input$Password!=PASS){ Ta<- data.frame(c('Insert Password'),c('Insert Password')  ) }
   
    Ta
  }
  
  
  ) 
  
  
  
  
  
  aaCon <- reactive( {    ## library(DT)
    
    aux00 <- length(input$TotalVarInput1[input$TotalVarInput1%in% VarTotalsCon]) >0 
     if(input$Password==PASS){
      if( aux00){
      
       
      VarCali       <-    input$calVarInput
       VarTotals    <-    input$TotalVarInput1
      VarGroup      <-    input$GroupVarInput
      wInp          <-    input$Weights
      desDummy      <-    (survey::svydesign(id=~1,   weights = ~  1,   data=  mydata()   ))
   
      
      if(input$estType=='National'){ 
        aaa           <-    NationalTotalsCal()[VarCali]     
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
        if(wInp =='Calibrated Weights'){
          formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
          desCal        <-    (survey::calibrate(des1,   formcal  , c(N(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
        }
      }
      if(input$estType=='Regional'){ 
        aaa           <-    RegionalTotalsCal()[VarCali]     
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
        
        if(wInp =='Calibrated Weights'){
          formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
          desCal        <-    (survey::calibrate(des1,   formcal  , c(NReg(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
        }
      }
      
    
      
   
      
      if( VarGroup =='None' )
      {
        formtotCat  <-   formula(paste( '~',  paste(   VarTotals[VarTotals%in% VarTotalsCon] , collapse = '+')))   
     
        formtotCatNA  <-   formula(paste( '~',  paste(   'as.numeric( !is.na(',  VarTotals[VarTotals%in% VarTotalsCon], '))' , collapse = '+')))   
        
        TTDummy         <-  as.numeric(round( survey::svytotal(     formtotCatNA   , design = desDummy ,   na.rm=TRUE ), 3))
        
        
         ### ?binary
  
        
      ### SAmpling WEgihts
      if( wInp=='Original Weights') { 
        TT <- survey::svytotal( formtotCat  , design = des1 ,   na.rm=TRUE )
        aux <- names(TT)
        TT <- data.frame(     cbind( TTDummy ,  round(TT,3), round(SE(TT),3)))
        colnames(TT) <- c( 'n',  'Total.Est', 'SE' )
        ci1  <- round(TT[,2] - 1.96*TT[,3],3)
        ci2  <- round(TT[,2] + 1.96*TT[,3],3)
        TT$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
        
        
        
        TT2 <- survey::svymean( formtotCat  , design = des1 ,   na.rm=TRUE)
        TT2<- data.frame(cbind( round(TT2,3), round(SE(TT2),3)))
        colnames(TT2) <- c(  'Mean.Est', 'SE.Mean' )
        ci1  <- round(TT2[,1] - 1.96*TT2[,2],3)
        ci2  <- round(TT2[,1] + 1.96*TT2[,2],3)
        TT2$CI_Mean  <- paste( '(' , ci1, ',', ci2 , ')')
        
        
      }
      
      if( wInp=='Calibrated Weights') { 
        TT <- survey::svytotal( formtotCat , design = desCal ,   na.rm=TRUE)
        aux <- names(TT)
        TT <- data.frame(cbind(  TTDummy , round(TT,3), round(SE(TT),3)))
        colnames(TT) <- c( 'n', 'Total.Est', 'SE' )
        ci1  <- round(TT[,2] - 1.96*TT[,3],3)
        ci2  <- round(TT[,2] + 1.96*TT[,3],3)
        TT$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
        
        TT2 <- survey::svymean( formtotCat , design = desCal ,   na.rm=TRUE)
        TT2<- data.frame(cbind( round(TT2,3), round(SE(TT2),3)))
        colnames(TT2) <- c(  'Mean.Est', 'SE.Mean' )
        ci1  <- round(TT2[,1] - 1.96*TT2[,2],3)
        ci2  <- round(TT2[,1] + 1.96*TT2[,2],3)
        TT2$CI_Mean <- paste( '(' , ci1, ',', ci2 , ')')
        
        
      }
      
      
      
        NameAux <- c('n','Total.Est','SE','Mean.Est','SE.Mean' )
        Ta<- (data.frame(Variable= aux,TT,  TT2 )) 
        
         for(bbi in NameAux){
           Ta[, bbi] <- as.numeric( Ta[, bbi])
         }
        
     
      }
      
      
      
      if( VarGroup !='None' )
      {
        
        formGroup      <-    formula(paste( '~',  paste(  paste("  (as.factor(", VarGroup[VarGroup%in% VarTotalsCat ],"))" ), collapse = '+')))   
        
        
        ### SAmpling WEgihts
        if( wInp=='Original Weights') { 
          TT2a <- NULL
          TTa <- NULL
          
          VarTot   <- VarTotals[VarTotals%in% VarTotalsCon ]
          NumbGroups  <- length(VarGroup)
          
        
          
          for( i in 1: length(VarTot)){
            VarToti  <- VarTot[i]
            
           
         
            LevelsAux  <-  paste( 'as.numeric(',VarToti,  ')' , sep='')
            formtotCati0   <-    formula(paste( '~',   paste(LevelsAux, collapse = '+')))   
            
            LevelsAuxNA  <-  paste( 'as.numeric( !is.na(',VarToti,  '))' , sep='')
            formtotCati0NA   <-    formula(paste( '~',   paste(LevelsAuxNA, collapse = '+')))   
            
            
            
            TTi         <-   (survey::svyby(  formtotCati0  , formGroup , des1, svytotal , na.rm =T , drop.empty.groups=F ))
        
            
            TTiDummy         <-   (survey::svyby(  formtotCati0NA  , formGroup , desDummy, svytotal , na.rm =T , drop.empty.groups=F ))
            TTiEstDummy      <-   cbind( TTiDummy [,  (NumbGroups+1)  ])
           
            TTiGroup    <-  as.data.frame( TTi[, 1:NumbGroups])
            colnames(TTiGroup)  <- names( TTi  )[1:NumbGroups]
            
            
            TTiEst <-  cbind( TTi[,  (NumbGroups+1)  ])
            TTiSE  <-  cbind( TTi[,  (NumbGroups+2)  ])
          
            TTi0   <- data.frame(var1=rownames(TTi ), var2= VarToti)
            
            
            TTi  <-  data.frame( Variable= TTi0[, 2],  TTiGroup )
            
            colnames(TTi)[-1] <- paste('Group:' , colnames(TTi)[-1] )
            
            TTi$n         <- round(unlist(TTiEstDummy), 3)
            TTi$Total_Est <- round(unlist(TTiEst), 3)
            TTi$SE_Total <- round(unlist(TTiSE), 3)
            
            ci1  <- round(TTi$Total_Est - 1.96*TTi$SE_Total,3)
            ci2  <- round(TTi$Total_Est + 1.96*TTi$SE_Total,3)
            TTi$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TTa  <-  rbind(  TTa, TTi)
            
        
            
            TT2i         <-   (survey::svyby(  formtotCati0  , formGroup , des1, svymean , na.rm =T , drop.empty.groups=F ))
            
            
            TT2iGroup    <-  as.data.frame( TT2i[, 1:NumbGroups])
            colnames(TT2iGroup)  <- names( TT2i  )[1:NumbGroups]
            TT2iEst <-  cbind( TT2i[,  (NumbGroups+1)  ])
            TT2iSE  <-  cbind( TT2i[,  (NumbGroups+2)  ])
            
            TT2i0   <- data.frame(var1=rownames(TT2i ), var2= VarToti)
            
            
            TT2i  <-  data.frame( Variable= TT2i0[, 2],  TT2iGroup )
            
            colnames(TT2i)[-1] <- paste('Group:' , colnames(TT2i)[-1] )
            
            
            TT2i$Mean_Est <- round(unlist(TT2iEst), 3)
            TT2i$SE_Mean  <- round(unlist(TT2iSE), 3)
            
            ci1  <- round(TT2i$Mean_Est - 1.96*TT2i$SE_Mean,3)
            ci2  <- round(TT2i$Mean_Est + 1.96*TT2i$SE_Mean,3)
            TT2i$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TT2a  <-  rbind(  TT2a, TT2i)
            
            
            
          }
          
          
          
        }
        
        if( wInp=='Calibrated Weights') { 
          TT2a <- NULL
          TTa <- NULL
          
          VarTot   <- VarTotals[VarTotals%in% VarTotalsCon]
          NumbGroups  <- length(VarGroup)
          
          
          
          for( i in 1: length(VarTot)){
            VarToti  <- VarTot[i]
          
            LevelsAux  <-  paste( 'as.numeric(',VarToti,  ')' , sep='')
            formtotCati0   <-    formula(paste( '~',   paste(LevelsAux, collapse = '+')))   
            
            LevelsAuxNA  <-  paste( 'as.numeric( !is.na(',VarToti,  '))' , sep='')
            formtotCati0NA   <-    formula(paste( '~',   paste(LevelsAuxNA, collapse = '+')))   
            
            TTiDummy         <-   (survey::svyby(  formtotCati0NA  , formGroup , desDummy, svytotal , na.rm =T , drop.empty.groups=F ))
            TTiEstDummy      <-   cbind( TTiDummy [,  (NumbGroups+1)  ])
            
            TTi         <-   (survey::svyby(  formtotCati0  , formGroup , desCal, svytotal , na.rm =T , drop.empty.groups=F ))
            
            TTiGroup    <-  as.data.frame( TTi[, 1:NumbGroups])
            colnames(TTiGroup)  <- names( TTi  )[1:NumbGroups]
            TTiEst <-  cbind( TTi[,  (NumbGroups+1)  ])
            TTiSE  <-  cbind( TTi[,  (NumbGroups+2)  ])
            
            TTi0   <- data.frame(var1=rownames(TTi ), var2= VarToti)
            
            
            TTi  <-  data.frame( Variable= TTi0[, 2],  TTiGroup )
            
            colnames(TTi)[-1] <- paste('Group:' , colnames(TTi)[-1] )
            
            TTi$n         <- round(unlist(TTiEstDummy), 3)
            TTi$Total_Est <- round(unlist(TTiEst), 3)
            TTi$SE_Total <- round(unlist(TTiSE), 3)
            
            ci1  <- round(TTi$Total_Est - 1.96*TTi$SE_Total,3)
            ci2  <- round(TTi$Total_Est + 1.96*TTi$SE_Total,3)
            TTi$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TTa  <-  rbind(  TTa, TTi)
            
            
            
            TT2i         <-   (survey::svyby(  formtotCati0  , formGroup , desCal, svymean , na.rm =T , drop.empty.groups=F ))
            
            TT2iGroup    <-  as.data.frame( TT2i[, 1:NumbGroups])
            colnames(TT2iGroup)  <- names( TT2i  )[1:NumbGroups]
            TT2iEst <-  cbind( TT2i[,  (NumbGroups+1)  ])
            TT2iSE  <-  cbind( TT2i[,  (NumbGroups+2)  ])
            
            TT2i0   <- data.frame(var1=rownames(TT2i ), var2= VarToti)
            
            
            TT2i  <-  data.frame( Variable= TT2i0[, 2],  TT2iGroup )
            
            colnames(TT2i)[-1] <- paste('Group:' , colnames(TT2i)[-1] )
            
       
            TT2i$Mean_Est <- round(unlist(TT2iEst), 3)
            TT2i$SE_Mean  <- round(unlist(TT2iSE), 3)
            
            ci1  <- round(TT2i$Mean_Est - 1.96*TT2i$SE_Mean,3)
            ci2  <- round(TT2i$Mean_Est + 1.96*TT2i$SE_Mean,3)
            TT2i$CI_Total  <- paste( '(' , ci1, ',', ci2 , ')')
            
            TT2a  <-  rbind(  TT2a, TT2i)
            
            
            
          }
        }
        
        
        NameAux <-     c( 'n',  'Total_Est' ,'SE_Total' ,'Mean_Est'  ,'SE_Mean') 
        Ta<-  (data.frame(  TTa,  TT2a[, (ncol(TT2a) -3 +1): ncol(TT2a)  ] )) 
        for(bbi in NameAux){ Ta[, bbi] <- as.numeric(Ta[, bbi])} 
        
      }
      
      
      
      }
       
       if(!aux00){ 
         Ta<- data.frame(c('No continuous variables selected'),c('No continuous variables selected')  )
       }
       
       
       
       
     }
    
    if(input$Password!=PASS){ Ta<- data.frame(c('Insert Password'),c('Insert Password')  ) }
    
    Ta
  }
  
  
  ) 
  
  
  
  
  output$TableOut  <- renderDataTable(aaCat(), extensions = 'Buttons',  options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))
    
  )
  )
  
  output$TableOutCon  <- renderDataTable(aaCon(), extensions = 'Buttons',  options = list(
    dom = 'Bfrtip',
    buttons = 
      list('copy', 'print', list(
        extend = 'collection',
        buttons = c('csv', 'excel', 'pdf'),
        text = 'Download'
      ))
    
  )
  )
  
  
  
}

