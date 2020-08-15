

selectVarPlotUI <- function(id, label = "selectVarPlot", VarPlot , VarCal  ) {
  ns <- NS(id)
  
  sidebarLayout( 
    sidebarPanel( 
      
      
      tagList(h2('Plots'),
                br(), 
               textInput( ns('Password') , label = "Password",   value = PASS0), 
              selectInput(ns('PlotType'), label = "Type of Plot",  c( 'barplot','scatter', 'boxplot'), selected = 'barplot', 
                          multiple =FALSE , selectize = TRUE, width = NULL, size = NULL), 
              selectInput(ns('estType'), 'National or Regional?', c('National','Regional')  , selected = 'National' ),  
              selectInput(ns('Weights'), label = "Sampling Weights", c( 'Calibrated Weights') , selected = 'Calibrated Weights', 
                                             multiple =FALSE, selectize = TRUE, width = NULL, size = NULL)   ,
              selectizeInput( ns('PlotVarInput1'), label = "Variable 1",  
                              options= list(maxOptions = 2000),VarPlot , selected = 'ethnic_p6' ), 
              selectizeInput( ns('PlotVarInput2'), label = "Variable 2",  
                              options= list(maxOptions = 2000), c(  VarPlot) , selected = 'GenderFemale' ) , 
              br(),br(),
              tags$hr(style="border-color: black;"),
              "Optional:", br(),
              selectInput(ns('calVarInput'), 'Variables for calibration', VarCal  , selected = VarCal , multiple =TRUE,
                          selectize = TRUE, width = NULL, size = NULL)
              
              
      ) 
    ), 
    mainPanel(p('Note: National estimates assume that the  sample is informative of the national  population. Calibration
                uses the main demographic factors  to adjust the sampling weights so that the sample is representative of the population of interest. 
                An implicit assumption of calibration in the Youth19 survey is that outcomes/variables of interest  do not depend on regional level factors, 
                but that they can be explained by other factors such as age, gender, etc. 
                This extrapolation can be done for outcomes/variables where  there is compelling evidence (from experts) that the factors contributing to such 
                outcomes are very similar in all the regions in NZ after controlling for the calibration variables. 
                This assumptions should use scientific judgment and common sense to make inferences that go beyond the limitations of statistics.', style = "color:#BD0759;"   ),
              br() ,
              h2('Plot' ) ,
              fluidRow(  column( 5,  div(dataTableOutput(ns('TableOut1')), style = "font-size:70%") ),  
                         column(7, downloadButton(ns('downloadPlot0'),'Download Plot'),  plotOutput(ns('plot') ) ) ) 
              
              
    ) )
  
  
  
}














selectVarPlot <- function(input, output, session , mydata, NationalTotalsCal , RegionalTotalsCal,  AuxBinary , AuxCharacter , AuxNumeric  ,VarCal, N, NReg   ) {
  
  
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
   
  
  
  
  observeEvent(input$PlotType,{ 
    if(input$PlotType=="boxplot" ){
      updateSelectizeInput(session = getDefaultReactiveDomain()
                        , inputId = "PlotVarInput1"
                        , choices = VarPlot[ AuxNumeric ]
                        , selected= VarPlot[ AuxNumeric ][1])
      
      updateSelectizeInput(session = getDefaultReactiveDomain()
                     , inputId = "PlotVarInput2"
                     , choices = VarPlot[ !AuxNumeric ]
                     , selected=VarPlot[ !AuxNumeric ][1])
      
      }
  
    
    if(input$PlotType=="scatter" ){
      updateSelectizeInput(session = getDefaultReactiveDomain()
                     , inputId = "PlotVarInput1"
                     , choices = VarPlot[ AuxNumeric ]
                     , selected=VarPlot[ AuxNumeric ][1])
      
      updateSelectizeInput(session = getDefaultReactiveDomain()
                     , inputId = "PlotVarInput2"
                     , choices =  VarPlot[ AuxNumeric ]
                     , selected=VarPlot[ AuxNumeric ][2])
      }
    
    
    if(input$PlotType=="barplot" ){
      updateSelectizeInput(session = getDefaultReactiveDomain()
                     , inputId = "PlotVarInput1"
                     , choices = VarPlot[ !AuxNumeric ]
                     , selected=VarPlot[ !AuxNumeric ][1])
      
      updateSelectizeInput(session = getDefaultReactiveDomain()
                     , inputId = "PlotVarInput2"
                     , choices =   c('None',VarPlot[ !AuxNumeric ])
                     , selected=VarPlot[ !AuxNumeric ][2]) }
    
  })  ## finish observeEvent 
  
  
  
  
  
 tab1re <-    reactive({  if(input$Password==PASS){
     VarCali       <-    input$calVarInput
     VarPlot1    <-    input$PlotVarInput1
     VarPlot2    <-    input$PlotVarInput2 
      
     PlotType    <-    input$PlotType
     ### ?binary
     
     VarPlot12  <- c(VarPlot1, VarPlot2)
     
     
       
     condScatter <-  ( AuxBinary[VarPlot1] | AuxNumeric[VarPlot1] ) & ( AuxBinary[VarPlot2] | AuxNumeric[VarPlot2])
     
     condBoxplota <-  ( AuxBinary[VarPlot1] | AuxNumeric[VarPlot1] ) & ( AuxCharacter[VarPlot2] | AuxBinary[VarPlot2])  
     condBoxplotb <-  ( AuxBinary[VarPlot2] | AuxNumeric[VarPlot2] ) & ( AuxCharacter[VarPlot1] | AuxBinary[VarPlot1])  
     
     condBarplot <-  ( AuxBinary[VarPlot1] | AuxCharacter[VarPlot1] ) & ( AuxBinary[VarPlot2] | AuxCharacter[VarPlot2] ) 
     
     
     if(input$Weights =='Original Weights')
     { 
     if(input$estType=='National'){ 
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
        }
     if(input$estType=='Regional'){ 
        des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
          
     }
     
       
  
    
     if(PlotType=='barplot'){
       if(condBarplot  ) {  
         
         formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
         AA<- as.data.frame(survey::svytable( formtot  , design= des1 ))
        
         mydataProv <- data.frame(AA) 
         xn <-  colnames(mydataProv )[2]
         yn <-  colnames(mydataProv )[3]
         zn <-  colnames(mydataProv )[1] 
         mydataProv  <- mydataProv[order(mydataProv[,2]),]
         
         mydataProv  <- mydataProv[order(mydataProv[,2] , mydataProv[,3] ),]
         mydataProv$Freq  <-  round(mydataProv$Freq)
         for( i in unique(mydataProv[,2])){
           mydataProv$cumFreq[mydataProv[, 2] ==i ]  <- (cumsum(  mydataProv$Freq[mydataProv[, 2] ==i ])) 
         }
         
       
         
         
         out1 <-  mydataProv
         
       } 
       
       if(! (condBarplot)  ) { 
         bbb <-  ('Variables should be categorical ')
          
         out1 <- data.frame(c(bbb, bbb))
       }  
     }
     
     
     
     
     
     if(PlotType=='scatter'){
       
       if( condScatter ) { 
         
         
         formtot1   <-    formula(paste( '~', paste( VarPlot1     , collapse = '+'))  )
         formtot2   <-    formula(paste( '~', paste( VarPlot2    , collapse = '+'))  )
         
         AA1   <-  as.data.frame(survey::svymean( formtot1  , design= des1  ,  na.rm=T  ))
         AA2   <-  as.data.frame(survey::svymean( formtot2  , design= des1  ,  na.rm=T  ))
        
         colnames(AA1)  <- colnames(AA2) <- c('Mean/Proportion','se')
         
         
         out1  <- rbind(round(AA1, 3), round(AA2,3))
         
         
         
         
       }
       
       if(!  condScatter  ) { 
         bbb <- ('Both variables should be numeric ')
         out1 <- data.frame(c(bbb, bbb))
       }  
     }
     
     
     if(PlotType=='boxplot'){ 
       
       if(condBoxplota | condBoxplotb) { 
         
         formtot1   <-    formula(paste( '~', paste( VarPlot1     , collapse = '+'))  )
         formtot2   <-    formula(paste( '~', paste( VarPlot2    , collapse = '+'))  )
         
         AA1   <-  as.data.frame(survey::svymean( formtot1  , design= des1  ,  na.rm=T  ))
         AA2   <-  as.data.frame(survey::svymean( formtot2  , design= des1  ,  na.rm=T  ))
         
         colnames(AA1)  <- colnames(AA2) <- c('Mean/Proportion','se')
         
         
         out1  <-  rbind(round(AA1, 3), round(AA2,3))
         
         
         
       }
        
       
       
       if(! (  condBoxplota |  condBoxplotb) ) { 
         bbb <-  ('One variable  should be numeric and the other should be categorical(or logical) ')
         out1 <- data.frame(c(bbb, bbb))
       }  
     }
     
     
     
     
     
 }
     
     
     
     if(input$Weights =='Calibrated Weights')
     {  
       
       formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )
      
       
       if(input$estType=='National'){ 
          
         aaa           <-    NationalTotalsCal()[VarCali]    
         
         des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
        
         desCal        <-    (survey::calibrate(des1,   formcal  , c(N(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
         
       }
       if(input$estType=='Regional'){ 
         aaa           <-    RegionalTotalsCal()[VarCali]     
         des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
         desCal        <-    (survey::calibrate(des1,   formcal  , c(NReg(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
         
       }
       
   
       
       if(PlotType=='barplot'){
         if(condBarplot  ) {  
           
           formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
           AA<- as.data.frame(survey::svytable( formtot  , design= des1 ))
           formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
           AAcal<-  as.data.frame(survey::svytable( formtot  , design= desCal ))
           
           mydataProv <- data.frame(AAcal) 
           xn <-  colnames(mydataProv )[2]
           yn <-  colnames(mydataProv )[3]
           zn <-  colnames(mydataProv )[1] 
           mydataProv  <- mydataProv[order(mydataProv[,2]),]
           
           mydataProv  <- mydataProv[order(mydataProv[,2] , mydataProv[,3] ),]
           mydataProv$Freq  <-  round(mydataProv$Freq)
           for( i in unique(mydataProv[,2])){
             mydataProv$cumFreq[mydataProv[, 2] ==i ]  <- (cumsum(  mydataProv$Freq[mydataProv[, 2] ==i ])) 
           }
           
           
           
           
           out1 <-  mydataProv
           
           
         } 
         
         if(! (condBarplot)  ) { 
           bbb <-  ('Variables should be categorical ')
           
           out1 <- data.frame(c(bbb, bbb))
         }  
       }
       
       
       
       if(PlotType=='scatter'){
         
         if( condScatter ) { 
           
           
           formtot1   <-    formula(paste( '~', paste( VarPlot1     , collapse = '+'))  )
           formtot2   <-    formula(paste( '~', paste( VarPlot2    , collapse = '+'))  )
           
           AA1   <-  as.data.frame(survey::svymean( formtot1  , design= desCal  ,  na.rm=T  ))
           AA2   <-  as.data.frame(survey::svymean( formtot2  , design= desCal ,  na.rm=T  ))
           
           colnames(AA1)  <- colnames(AA2) <- c('Mean/Proportion','se')
           
           
           out1  <-  rbind(round(AA1, 3), round(AA2,3))
           
           
           
           
           
         }
         
         if(!  condScatter  ) { 
           bbb <- ('Both variables should be numeric ')
           out1 <- data.frame(c(bbb, bbb))
         }  
       }
       
       
       if(PlotType=='boxplot'){ 
         
         if(condBoxplota | condBoxplotb) { 
           
           formtot1   <-    formula(paste( '~', paste( VarPlot1     , collapse = '+'))  )
           formtot2   <-    formula(paste( '~', paste( VarPlot2    , collapse = '+'))  )
           
           AA1   <-  as.data.frame(survey::svymean( formtot1  , design= desCal  ,  na.rm=T  ))
           AA2   <-  as.data.frame(survey::svymean( formtot2  , design= desCal  ,  na.rm=T  ))
           
           
           colnames(AA1)  <- colnames(AA2) <- c('Mean/Proportion','se')
           
           out1  <-  rbind(round(AA1, 3), round(AA2,3))
           
           
           
         }
         
         
         
         if(! (  condBoxplota |  condBoxplotb) ) { 
           bbb <-  ('One variable  should be numeric and the other should be categorical(or logical) ')
           out1 <- data.frame(c(bbb, bbb))
         }  
       }
       
       
     }
     
     
     
     
     out1
     
   }
     
     
   } 
   
   ) 
  
  
 
 
  
   plotre  <- reactive({  if(input$Password==PASS){
     VarCali       <-    input$calVarInput
     VarPlot1    <-    input$PlotVarInput1
     VarPlot2    <-    input$PlotVarInput2 
     PlotType    <-    input$PlotType
     ### ?binary
     
     VarPlot12  <- c(VarPlot1, VarPlot2)
     
     
     condScatter <-  ( AuxBinary[VarPlot1] | AuxNumeric[VarPlot1] ) & ( AuxBinary[VarPlot2] | AuxNumeric[VarPlot2])
     
     condBoxplota <-  ( AuxBinary[VarPlot1] | AuxNumeric[VarPlot1] ) & ( AuxCharacter[VarPlot2] | AuxBinary[VarPlot2])  
     condBoxplotb <-  ( AuxBinary[VarPlot2] | AuxNumeric[VarPlot2] ) & ( AuxCharacter[VarPlot1] | AuxBinary[VarPlot1])  
     
     condBarplot <-  ( AuxBinary[VarPlot1] | AuxCharacter[VarPlot1] ) & ( AuxBinary[VarPlot2] | AuxCharacter[VarPlot2] ) 
     
    
     
     
     
     
     
     if(input$Weights =='Original Weights')
     {
       if(input$estType=='National'){ 
          des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
         }
       if(input$estType=='Regional'){ 
          des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
       
       }
       
       
       
     if(PlotType=='scatter'){
       
       if( condScatter ) { 
         
         mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot1]), x= as.numeric(mydata()[, VarPlot2]), 
                                  w= 1/as.vector(des1$prob), wCal= 1/as.vector(desCal$prob))
         
         ggpp1  <-   ggplot(mydataProv,  aes(x=x, y=y)) + 
           geom_point(  size=  2*mydataProv$w /max(mydataProv$w))+
           geom_smooth(method=lm) + labs(x =VarPlot1, y= VarPlot2)
         
         
         
         
         
       }
       
       if(!  condScatter  ) { 
        bbb <- ('Both variables should be numeric ')
        ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
          geom_text(aes(label= bbb) )
       }  
     }
     
     
     if(PlotType=='boxplot'){ 
       
       if(condBoxplota) { 
         
         mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot1]), x= as.character(mydata()[, VarPlot2]))
         ggpp1<-ggplot(mydataProv, aes (y=y, x= x )) +
           geom_boxplot()
         
         
       }
       
       if(condBoxplotb) {  
         mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot2]), x= as.character(mydata()[, VarPlot1]))
         ggpp1<-ggplot(mydataProv, aes (y=y, x= x )) +
           geom_boxplot()
         
       }
       
       
       if(! (  condBoxplota |  condBoxplotb) ) { 
         bbb <-  ('One variable  should be numeric and the other should be categorical(or logical) ')
         ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
           geom_text(aes(label= bbb) )
       }  
     }
     
     
     
     if(PlotType=='barplot'){
       if(condBarplot  ) {  
         
         formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
         AA<- as.data.frame(survey::svytable( formtot  , design= des1 ))
     
         mydataProv <- data.frame(AA) 
         xn <-  colnames(mydataProv )[2]
         yn <-  colnames(mydataProv )[3]
         zn <-  colnames(mydataProv )[1] 
         mydataProv  <- mydataProv[order(mydataProv[,2]),]
         
         mydataProv  <- mydataProv[order(mydataProv[,2] , mydataProv[,3] ),]
         mydataProv$Freq  <-  round(mydataProv$Freq)
         for( i in unique(mydataProv[,2])){
           mydataProv$cumFreq[mydataProv[, 2] ==i ]  <- (cumsum(  mydataProv$Freq[mydataProv[, 2] ==i ])) 
         }
         
          
         ggpp1 <- ggplot(data=mydataProv , aes_string(x=xn, y=yn, fill=zn)) +
           geom_bar(stat="identity" ) +
           geom_text(aes(y= cumFreq, label=Freq), vjust=1.6, 
                     color="white", size=3.5)+  
           theme_minimal() 
         
         
         
         
       } 
       
       if(! (condBarplot)  ) { 
         bbb <-  ('Variables should be categorical ')
         ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
           geom_text(aes(label= bbb) )
         
      
        }  
     }
     
     }
     
     if(input$Weights == 'Calibrated Weights')
     {     formcal       <-    formula(paste( '~', paste(  VarCali    , collapse = '+'))  )

       
       if(input$estType=='National'){ 
         aaa           <-    NationalTotalsCal()[VarCali]     
         des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightNational,   data=  mydata()   ))
         desCal        <-    (survey::calibrate(des1,   formcal  , c(N(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
       }
       if(input$estType=='Regional'){ 
         aaa           <-    RegionalTotalsCal()[VarCali]     
         des1         <-     (survey::svydesign(id=~id2,  strata=~Wharekura, weights = ~  weightRegional,   data=  mydata()   ))
         desCal        <-    (survey::calibrate(des1,   formcal  , c(NReg(),aaa  ),calfun=c( "raking" )  ) ) ## raking for positive weights
         
       }
       
       
       if(PlotType=='scatter'){
         
         if( condScatter ) { 
           
           mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot1]), x= as.numeric(mydata()[, VarPlot2]), 
                                    w= 1/as.vector(des1$prob), wCal= 1/as.vector(desCal$prob))
           
           
           
           ggpp1  <-   ggplot(mydataProv,  aes(x=x, y=y)) + 
             geom_point(  size=  2*mydataProv$wCal /max(mydataProv$wCal))+
             geom_smooth(method=lm) +   labs(x =VarPlot1, y= VarPlot2)
           
           
           
         }
         
         if(!  condScatter  ) { 
           bbb <- ('Both variables should be numeric ')
           
           ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
             geom_text(aes(label= bbb) )
           
         }  
       }
       
       
       if(PlotType=='boxplot'){ 
         
         if(condBoxplota) { 
           
           mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot1]), x= as.character(mydata()[, VarPlot2]))
           ggpp1<-ggplot(mydataProv, aes (y=y, x= x )) +
             geom_boxplot()
           
           
         }
         
         if(condBoxplotb) {  
           mydataProv <- data.frame(y= as.numeric(mydata()[, VarPlot2]), x= as.character(mydata()[, VarPlot1]))
           ggpp1<-ggplot(mydataProv, aes (y=y, x= x )) +
             geom_boxplot()
           
         }
         
         
         if(! (  condBoxplota |  condBoxplotb) ) { 
           bbb <- ('One variable  should be numeric and the other should be categorical(or logical) ')
           ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
             geom_text(aes(label= bbb) )
         }  
       }
       
       
       
       if(PlotType=='barplot'){
         if(condBarplot  ) {  
           
           
           formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
           
           AA<- as.data.frame(survey::svytable( formtot  , design= des1 ))
           formtot   <-    formula(paste( '~', paste( VarPlot12    , collapse = '+'))  )
           
           
           AAcal<-  as.data.frame(survey::svytable( formtot  , design= desCal ))
           
           
           
           
           ## Calibration 
           mydataProvCal <- data.frame(AAcal) 
           xn <-  colnames(mydataProvCal )[2]
           yn <-  colnames(mydataProvCal )[3]
           zn <-  colnames(mydataProvCal )[1] 
           
           mydataProvCal  <- mydataProvCal[order(mydataProvCal[,2] , mydataProvCal[,3] ),]
           
           mydataProvCal$Freq  <-  round(mydataProvCal$Freq)
           for( i in unique(mydataProvCal[,2])){  
             mydataProvCal$cumFreq[mydataProvCal[, 2] ==i ]  <- (cumsum(  mydataProvCal$Freq[mydataProvCal[, 2] ==i ])) 
           }
           
           
           ggpp1 <- ggplot(data=mydataProvCal , aes_string(x=xn, y=yn, fill=zn)) +
             geom_bar(stat="identity" ) +
             geom_text(aes(y= cumFreq, label=Freq), vjust=1.6, 
                       color="white", size=3.5)+  
             theme_minimal()
           
           
           
         } 
         
         if(! (condBarplot)  ) { 
           bbb <- ('Variables should be categorical ')
           ggpp1 <- ggplot(data=data.frame(x= c(1 ), y= c(1 )) , aes(x=x , y=y   )) +  
             geom_text(aes(label= bbb) )
         }  
       }
       
     }
       
       
     
     print( ggpp1)
     
   }
      
     
   } 
   
   )
   
  
  
  output$plot   <- renderPlot({  plotre() })
  output$downloadPlot0 <- downloadHandler(
    filename = function() { paste("Plot0_", input$PlotVarInput1,'_', input$PlotVarInput2, '.png', sep='') },
    content = function(file) {
      ggsave(file, plotre() , device = "png")
    } )
  
  
  
    
  
  
  
  output$TableOut1 <- renderDataTable( tab1re(), extensions = 'Buttons',  options = list(
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



 

 
  
