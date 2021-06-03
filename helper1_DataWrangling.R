#dir  <- "/Users/clr417/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/0_R Codes/Initial Data/" 
#dirShinydata <- '/Users/clr417/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/ShinyAppYouth19/data/'

# source('/Users/criv292/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/0_R Codes/0.CodeGetData_youth2019Full.R')
# source('/Users/criv292/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/0_R Codes/helper0_CreateSampleWeights.R')



 
#dirShinydata <- '/Users/clr417/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/ShinyAppYouth19/data/'
#dirShinydata <- '/Users/criv292/Dropbox/Indep/Projects/Youth 2019-UoA/App/ShinyAppYouth19/data/'
#setwd('/Users/criv292/Dropbox (Personal)/Indep/Projects/Youth 2019-UoA/App/ShinyAppYouth19/')
 
 

dirShinydata<-   'data/'
 

keyring_unlock(password = "")

 
## Create survey data with weights:   

 
Y19_sample_weights <- 
  cyphr::decrypt(
    readRDS(paste0(dirShinydata, "sample_weights2019.rds")), 
    cyphr::key_sodium(openssl::sha256(charToRaw(keyring::key_get("youth19_secret"))))
  )
  

DataNational        <-  
  cyphr::decrypt(
    readRDS(paste0(dirShinydata, "ECschoolData2019.rds")), 
    cyphr::key_sodium(openssl::sha256(charToRaw(keyring::key_get("youth19_secret"))))
  )



#DataNational <-  ECschoolData2019 
Y19SampleIndividual  <- 
  cyphr::decrypt(
    readRDS(paste0(dirShinydata, "youth2019.rds")), 
    cyphr::key_sodium(openssl::sha256(charToRaw(keyring::key_get("youth19_secret"))))
  )
  
  
## Strata 

Y19SampleIndividual  <- Y19SampleIndividual %>% mutate(strata = if_else(Wharekura == 1, "Wharekura", EducationRegion))
 
Y19SampleIndividual$weight  <- NULL 


 
## Select Eligible: Eligible and Wharakura
 
DataNational <- DataNational  %>% mutate( CondElig= NationalEligible==1 | WharekuraNationalEligible==1, 
                                          id2= paste('id-', ECSchoolID,sep=''),
                                          WharekuraTotal = TotalRoll*Wharekura) 
DataNational <- filter(DataNational,  CondElig==1)
 
## New ids to that match national data
Y19_sample_weights  <- Y19_sample_weights %>% mutate( id2=paste('id-', ECSchoolID,sep='') )
Y19SampleIndividual <- Y19SampleIndividual %>% mutate( id2=paste('id-', ECSchoolID,sep='') )
## Adding variables to individual level data: 
auxDat          <- dplyr::select(DataNational, id2 )   
auxDatWeights   <- dplyr::select(Y19_sample_weights, id2,  sch_weight, stud_weight, weight)   
Y19SampleIndividual2     <-   inner_join(Y19SampleIndividual, auxDat,  by= 'id2'  )
Y19SampleIndividual2     <-   inner_join(Y19SampleIndividual2, auxDatWeights ,  by= 'id2'   )
 
## Observations  with missing    Ethniccity are counted as 'other' as we cant have this as missing for calibration. 
 
Y19SampleIndividual2  <- Y19SampleIndividual2 %>% mutate(ethnic_p6=replace( ethnic_p6 , is.na(ethnic_p6) , 'Other'))
  
## Observations  with missing   sex are removed  as we cant have this as missing for calibration: 46 observations removed

Y19SampleIndividual2  <- filter( Y19SampleIndividual2, !is.na(sex))

#View(Y19SampleIndividual )
#dim(Y19SampleIndividual2) 
 
# Creating Decile variables
auxi  <- unique(as.character(DataNational$Decile))
for(dei in auxi){
  aux           <-  cbind(Y19SampleIndividual2$Decile == dei)
  colnames(aux) <- paste('Decile', dei, sep='')
  Y19SampleIndividual2  <- data.frame(Y19SampleIndividual2,  aux)
  
  aux2          <-  cbind( (DataNational$Decile == dei)*DataNational$TotalRoll)
  colnames(aux2) <- paste('Decile', dei, sep='')
  DataNational   <- data.frame(DataNational,  aux2)
  
}
## Creating useful binary  variables- fro calibration Regionally   
 
Y19SampleIndividual2   <- Y19SampleIndividual2 %>% mutate(WharekuraTotal= Wharekura,
                                                      RegAuckland= EducationRegion=='Auckland',
                                                      RegTaiTokerau=EducationRegion=='Tai Tokerau',
                                                      RegWaikato= EducationRegion=='Waikato', 
                                                      GenderFemale= sex =='Female', 
                                                      Maori  = ethnic_p6 =="Maori",
                                                      European  = ethnic_p6 == 'European',
                                                      Asian = ethnic_p6 == 'Asian',
                                                      Pacific = ethnic_p6 == 'Pacific',
                                                      MELAA   = ethnic_p6 == 'MELAA',
                                                      ## Create groups of 3-4-3 deciles, for calibration: 
                                                      #1-3, 4-7, 8-10
                                                      Decile1_3= Decile <= 3,  
                                                      Decile4_7= Decile > 3 & Decile <=7 , 
                                                      Decile8_10= Decile >  7 & Decile <= 10,
                                                      Decile1_2= Decile <= 2,  
                                                      Decile3_4= Decile > 2 & Decile <=4 , 
                                                      Decile5_6= Decile >  4 & Decile <= 6,
                                                      Decile7_8= Decile >  6 & Decile <= 8,
                                                      Decile9_10= Decile > 8 & Decile <=  10 ,  
                                                      ### Creating age groups that match in DataNationla and survey                                                    
                                                      Age13andUnder = age== '13 and under',
                                                      Age14= age== '14',
                                                      Age15 = age== '15',
                                                      Age16 = age== '16',
                                                      Age17andOver = age== '17 and over', 
                                                      AgeUnder16 =  age== '13 and under'| age== '14' | age== '15',
                                                      Age16andOver =  age== '17 and over' | age== '16',
                                                      weightNational =  weight*10/3, ## Regional fraction
                                                      weightRegional =  weight
                                                       )
 

DataNational   <- DataNational %>% mutate(RegAuckland= EducationRegion=='Auckland',
                                                      RegTaiTokerau=EducationRegion=='Tai Tokerau',
                                                      RegWaikato= EducationRegion=='Waikato', 
                                                      ## Create groups of 3-4-3 deciles, for calibration: 
                                                      Decile1_3=  TotalRoll*(Decile <= 3),  
                                                      Decile4_7= TotalRoll*(Decile > 3 & Decile <=7) , 
                                                      Decile8_10= TotalRoll*(Decile >  7 & Decile <= 10), 
                                                      Decile1_2=  TotalRoll*(Decile <= 2),  
                                                      Decile3_4= TotalRoll*(Decile > 2 & Decile <=4) , 
                                                      Decile5_6= TotalRoll*(Decile >  4 & Decile <= 6),
                                                      Decile7_8= TotalRoll*(Decile > 6 & Decile <=8) , 
                                                      Decile9_10= TotalRoll*(Decile >  8 & Decile <= 10), 
                                                      ### Age matching with survey Only ages eligible in survey 
                                                      Age13andUnder =   Age12 + Age13 + AgeUnder12,
                                                      Age17andOver  = Age17 + Age18 +  Age19  +   AgeOver19 , 
                                                      AgeUnder16 =    Age12 +   Age13 +  Age14 +  Age15,
                                                      Age16andOver  =  Age16 + Age17 +  Age18 +  Age19  +   AgeOver19,
                                                      GenderFemale    = totalFemale, 
                                                      Maori    = totalMaori, 
                                                      European = totalEuropean ,
                                                      Pacific  = totalPacific  ,
                                                      MELAA    = totalMELAA  ,
                                                      Asian    = totalAsian  
                                          )

## VAriables for calibration:
 
VarCalWha <- c('WharekuraTotal', 'Decile1_2' , 'Decile3_4', 'Decile5' ,'Decile6' , 'Decile7','Decile8', 'Decile9'    ,  'Age16andOver' , 'GenderFemale' ,   'Maori' , 'European' , 'Pacific' ,   'Asian'  )

VarCal <- c( 'Decile1_2' , 'Decile3_4', 'Decile5' ,'Decile6' , 'Decile7','Decile8', 'Decile9'    ,  'Age16andOver' , 'GenderFemale' ,   'Maori' , 'European' , 'Pacific' ,   'Asian'  )

VarCalWha <- c('WharekuraTotal','Decile1','Decile2' , 'Decile4' ,  'Decile3', 'Decile5' ,'Decile6' , 'Decile7','Decile8', 'Decile9' ,  
               'Age13andUnder','Age14', 'Age15', 'Age16',  'GenderFemale' ,   'Maori' , 'European' , 'Pacific' ,   'Asian'  )
VarCal  <- c( 'Decile1','Decile2' , 'Decile4' ,  'Decile3', 'Decile5' ,'Decile6' , 'Decile7','Decile8', 'Decile9' ,  
              'Age13andUnder','Age14', 'Age15', 'Age16',  'GenderFemale' ,   'Maori' , 'European' , 'Pacific' ,   'Asian'  )


for(i in 1:length(VarCalWha)){DataNational[, VarCalWha[i]] <- as.numeric(DataNational[, VarCalWha[i]])}
for(i in 1:length(VarCalWha)){Y19SampleIndividual2[, VarCalWha[i]] <- as.numeric(Y19SampleIndividual2[, VarCalWha[i]])}


 
## Ragional data for calibration 
RegionsSurvey <- as.character(unique(Y19SampleIndividual2$EducationRegion) )
RegionalData  <-  (filter(DataNational ,   EducationRegion  %in%RegionsSurvey))
RegionalData$EducationRegion <-   (RegionalData$EducationRegion)

### Trying calibration
 
NSchools              <- nrow(DataNational)
N                     <- sum(DataNational$TotalRoll) 
NReg                  <- sum(RegionalData$TotalRoll) 


## NAtional Totals to be calibrated to  
NationalTotalsCal        <- c(colSums(cbind(DataNational[,VarCalWha])))
RegionalTotalsCal        <- c(colSums(cbind(RegionalData[,VarCalWha])))

# This is just to get the school weights,  
 
 


des1Nat       <-  svydesign(id=~id2,  strata=~strata, weights = ~  weightNational,   data=  Y19SampleIndividual2  )
des1Regional  <-  svydesign(id=~id2,  strata=~strata, weights = ~  weightRegional,   data=  Y19SampleIndividual2  )

formcal      <- formula(paste( '~', paste(VarCalWha  , collapse = '+'))  )
desCalNat    <- calibrate(des1Nat,   formcal  , c(N,NationalTotalsCal ),calfun=c( "raking" )  )  ## raking for positive weights
desCalReg    <- calibrate(des1Regional,   formcal  , c(NReg ,RegionalTotalsCal ),calfun=c( "raking" )  )  ## raking for positive weights

## See example below: first one overestimatees the total number of students A LOT, second is the same as the actual number
sum(1/des1Regional$prob)
sum(1/desCalReg$prob)
sum(RegionalData$TotalRoll)

## Total of  students:
# Sampling weights   - National Expanded
svytotal(~GenderFemale + Maori + Pacific +Wharekura, des1Nat)
# Calibrated weights - National Expanded
svytotal(~GenderFemale + Maori + Pacific + Wharekura , desCalNat)

svyglm( GenderFemale~Maori + Pacific + Wharekura , desCalNat, family=quasibinomial())

aa<-  svyglm(  (GenderFemale)==1~Maori + Pacific + Wharekura , des1Nat, family=quasibinomial())  
 
  


VarExcluded      <- c(1:13, 15:19)
continuosVarList <-c("radScore", "WBScore", "weightNational", "weightRegional")
factorVarList    <-  colnames(Y19SampleIndividual2  )[-VarExcluded ]
factorVarList    <- factorVarList[!factorVarList %in% continuosVarList]     

mydata        <-     Y19SampleIndividual2   
#### This part corrects the type of variables 
###

for( ii in factorVarList  ){
  mydata[,ii ]   <- as.character(as.character(  mydata[,ii ]) )
} 
for( ii in continuosVarList  ){
  mydata[,ii ]   <- as.numeric( (  mydata[,ii ]) )
} 
############

  
## NAtional Totals to be calibrated to  
NationalTotalsCal        <- c(colSums(cbind(DataNational[,VarCal])))
RegionalTotalsCal        <- c(colSums(cbind(RegionalData[,VarCal])))





### Variables for analysis: From intro to end
isBinary  <-  function(x){
  aux1<- names(table(x))
  isBinary <- FALSE
  if(length(aux1)==2 ){
    if(    sum( (aux1) %in% c('0','1')) ==2  |  sum(aux1 %in% c(TRUE, FALSE) ) ==2    ) 
      {
    isBinary <- TRUE
  }}
  return(isBinary)  }

AuxType       <-  sapply(mydata,  class)[-VarExcluded] 
AuxBinary     <-  sapply(mydata,  isBinary) [-VarExcluded] 
VarPlot         <- (colnames(mydata)  )[-VarExcluded] 
AuxCharacter  <-  AuxType =='character' | (AuxType =='factor' &  !AuxBinary)
AuxNumeric    <-  AuxType =='numeric'  | AuxType =='integer'  
 
for(vi in names(AuxBinary[AuxBinary])) { 
aux <- mydata[,vi ] 
auxL <- unique(aux)

if(sum(auxL %in% c('0','1'))==2){
  mydata[,vi ] <-   as.numeric((aux))}

 
if( sum(auxL %in% c('TRUE','FALSE'))==2){
mydata[,vi ] <-   as.numeric(as.logical(aux))}

}
 

VarTotals       <-  VarPlot
VarModels       <-  VarPlot
VarModelsCat    <-  VarPlot[AuxCharacter | AuxBinary  ]
VarModelsCon    <-  VarPlot[AuxNumeric]
VarModelsBin    <-  VarPlot[AuxBinary  ]
VarTotalsCat    <-  VarPlot[AuxCharacter | AuxBinary  ]
VarTotalsCon    <-  VarPlot[AuxNumeric]
VarTotalsBin    <-  VarPlot[AuxBinary  ]




VarInt   <-  combinations(length(VarModels[1:2]),2,VarModels[1:2])
VarInt2  <-  paste(VarInt[,1], '*' , VarInt[,2], sep='') 



#### The following part is for including reference levels in models, 
#a reactive model is built so that the app remembers the  previous choices:
length(unique(as.character(mydata[, VarModels[2]] )))
auxData00       <-  mydata[, VarModelsCat]
sizeX<-  (  sapply( auxData00 , function(x){length(unique(as.character(x[!is.na(x)])))}) )
RefeX<-  unlist(  sapply( auxData00 , function(x){sort(unique(as.character(x[!is.na(x)])))})  )
df  <-  data.frame(Var =rep(names(sizeX), sizeX), Ref=RefeX ) 
df$Ref <- paste(df$Var, df$Ref, sep='_')
auxx   <- rep(F, length(df$Ref));
auxx2  <- cumsum( sizeX)
auxx[as.vector(c(0,auxx2 [-length(auxx2 )])+1)]   <- T
checked <- as.list(auxx)
names(checked) <- df$Ref
 
PASS0 <- ''

PASS <- 
  cyphr::decrypt(
    readRDS(paste0(dirShinydata, "password_analysis.rds")), 
    cyphr::key_sodium(openssl::sha256(charToRaw(keyring::key_get("youth19_secret"))))
  )


 
## Confirming estimates:  
# Sampling weights   - National Expanded
#svytotal(~as.factor(Decile), des1Nat)
# Calibrated weights - National Expanded
#svytotal(~as.factor(Decile), desCalNat)

#aggregate(DataNational$TotalRoll~ DataNational$Decile, FUN='sum')

keyring_lock()
 