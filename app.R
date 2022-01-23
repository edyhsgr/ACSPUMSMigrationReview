##############################################################################################################################
##############################################################################################################################
##R CODE FOR ACS PUMS MIGRATION REVIEW - CALIFORNIA COUNTIES
##
##EDDIE HUNSINGER (AFFILIATION: CALIFORNIA DEPARTMENT OF FINANCE), OCTOBER 2019 (UPDATED JANUARY 2022)
##https://edyhsgr.github.io/eddieh/
##edyhsgr@gmail.com
##
##US Census Bureau 2013 to 2017 American Community Survey (ACS) Public Use Microdata Sample (PUMS) data from IPUMS-USA, University of Minnesota, www.ipums.org.
##
##Some notes: Rogers-Castro fits for these data are just for demonstration, and not intended to be informative about migration for the respective populations. 
##R's approx() function is used for missing data applied in the Rogers-Castro model.
##
##This interface was made with Shiny for R (shiny.rstudio.com). Eddie Hunsinger, September 2019 (updated January 2022). 
##GitHub repository: https://github.com/edyhsgr/ACSPUMSMigrationReview. 
##Rogers-Castro fitting process used: https://applieddemogtoolbox.github.io/#MMSRCode.
##############################################################################################################################
##############################################################################################################################

library(shiny)

ui<-fluidPage(

	tags$h3("Draft ACS PUMS-Based Migration Data Viewer - California Counties - 2013 to 2017 American Community Survey Public Use Microdata Sample data"),
	p(""),
  
hr(),

sidebarLayout(
sidebarPanel(

 selectInput("County", "County",
c(
"Alameda"="1",
"Alpine, Amador, Calaveras, Inyo, Mariposa, Mono, Tuolumne"="3001", #"3",
"Amador"="3001", #"5",
"Butte"="7",
"Calaveras"="3001", #"9",
"Colusa, Glenn, Tehama, Trinity"="11001", #"11",
"Contra Costa"="13",
"Del Norte, Lassen, Modoc, Plumas, Siskiyou"="15001", #"15",
"El Dorado"="17",
"Fresno"="19",
"Glenn"="11001", #"21",
"Humboldt"="23",
"Imperial"="25",
"Inyo"="3001", #"27",
"Kern"="29",
"Kings"="31",
"Lake, Mendocino"="33001", #"33",
"Lassen"="15001", #"35",
"Los Angeles"="37",
"Madera"="39",
"Marin"="41",
"Mariposa"="3001", #"43",
"Mendocino"="33001", #"45",
"Merced"="47",
"Modoc"="15001", #"49",
"Mono"="3001", #"51",
"Monterey"="53001", #"53",
"Napa"="55",
"Nevada, Sierra"="57001", #"57",
"Orange"="59",
"Placer"="61",
"Plumas"="15001", #"63",
"Riverside"="65",
"Sacramento"="67",
"San Benito"="69",
"San Bernardino"="71",
"San Diego"="73",
"San Francisco"="75",
"San Joaquin"="77",
"San Luis Obispo"="79",
"San Mateo"="81",
"Santa Barbara"="83",
"Santa Clara"="85",
"Santa Cruz"="87",
"Shasta"="89",
"Sierra"="57001", #"91",
"Siskiyou"="15001", #"93",
"Solano"="95",
"Sonoma"="97",
"Stanislaus"="99",
"Sutter, Yuba"="101001", #"101",
"Tehama"="11001", #"103",
"Trinity"="11001", #"105",
"Tulare"="107",
"Tuolumne"="3001", #"109",
"Ventura"="111",
"Yolo"="113",
"Yuba"="101001" #"115"
),
),

 selectInput("Sex", "Sex",
c(
"Male"="1",
"Female"="2"
),
),

 selectInput("Hispanic", "Hispanic origin indicator",
c(
"Total"="0",
"Hispanic"="1",
"Not Hispanic"="2"
),
),

#numericInput("MaxAge", "Maximum age", 
#		85, min = 65, max = 90, step = 1,
#),

hr(),

 selectInput("FitMigrationModel", "Fit Rogers-Castro migration model?",
c(
"No"="NO",
"Yes"="YES"
),
),

numericInput("SampleSize", "Sampling size for Rogers-Castro migration model fitter", 
		500, min = 100, max = 10000, step = 100,
),



 selectInput("InOrOut", "Fit In- or Out-movers in model?",
c(
"In"="IN",
"Out (not including international)"="OUT"
),
),

 selectInput("ExcludeStudentAges", "Exclude student ages in model?",
c(
"Yes"="YES",
"No"="NO"
),
),

 sliderInput("StudentAges", "If yes to student ages in model, student ages in model",
                min = 17, max = 23, value = c(18, 20),step=1
),

 selectInput("RetirementFunction", "Include retirement function in model?",
c(
"No"="NO",
"Yes"="YES"
),
),

 selectInput("ElderlyFunction", "Include elderly function in model?",
c(
"No"="NO",
"Yes"="YES"
),
),

hr(),

tags$small(paste0(        
	"US Census Bureau 2013 to 2017 American Community Survey (ACS) Public Use Microdata Sample (PUMS) data from IPUMS-USA, University of Minnesota, www.ipums.org."
	)),
hr(),

tags$small(paste0(        
	"Some notes: Rogers-Castro fits for these data are just for demonstration, and not intended to be informative about migration for the respective populations. 
	R's approx() function is used for missing data applied in the Rogers-Castro model."
	)),
hr(),

tags$small(paste0(        
	"This interface was made with Shiny for R (shiny.rstudio.com). 
	Eddie Hunsinger, September 2019 (updated January 2022). 
	GitHub repository: https://github.com/edyhsgr/ACSPUMSMigrationReview. 
	Rogers-Castro fitting process used: https://applieddemogtoolbox.github.io/#MMSRCode."
	)),

width=3
),

mainPanel(
	plotOutput("plots"),width=3
))
)

#Data<-data.frame(read.table(file="https://u.demog.berkeley.edu/~eddieh/ACSPUMSMigrationReview/USACSMigration/Data/ACSPUMSSelection_2017FiveYearViaIPUMS_CA.csv",header=TRUE,sep=","))
Data<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/ACSPUMSMigrationReview/master/Data/ACSPUMSSelection_2017FiveYearViaIPUMS_CA.csv",header=TRUE,sep=","))

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(2,2))
	
##############################################################################################################################
##############################################################################################################################

MaxAge<-89
agelist<-data.frame(0:110)
names(agelist)<-"Group.1"

Data$COUNTYFIP[Data$PUMA==300 & Data$STATEFIP==6] <- 3001 #Alpine, Amador, Calaveras, Inyo, Mariposa, Mono, Tuolumne
Data$COUNTYFIP[Data$PUMA==1100 & Data$STATEFIP==6] <- 11001 #Colusa, Glenn, Tehama, Trinity
Data$COUNTYFIP[Data$PUMA==1500 & Data$STATEFIP==6] <- 15001 #Del Norte, Lassen, Modoc, Plumas, Siskiyou
Data$COUNTYFIP[Data$PUMA==3300 & Data$STATEFIP==6] <- 33001 #Lake, Mendocino
Data$COUNTYFIP[Data$PUMA==5301 & Data$STATEFIP==6] <- 53001 #Monterey
Data$COUNTYFIP[Data$PUMA==5302 & Data$STATEFIP==6] <- 53001 #Monterey
Data$COUNTYFIP[Data$PUMA==5303 & Data$STATEFIP==6] <- 53001 #Monterey
Data$COUNTYFIP[Data$PUMA==5700 & Data$STATEFIP==6] <- 57001 #Nevada, Sierra
Data$COUNTYFIP[Data$PUMA==10100 & Data$STATEFIP==6] <- 101001 #Sutter, Yuba

Data$MIGCOUNTY1[Data$MIGPUMA1==300 & Data$MIGPLAC1==6] <- 3001 #Alpine, Amador, Calaveras, Inyo, Mariposa, Mono, Tuolumne
Data$MIGCOUNTY1[Data$MIGPUMA1==1100 & Data$MIGPLAC1==6] <- 11001 #Colusa, Glenn, Tehama, Trinity
Data$MIGCOUNTY1[Data$MIGPUMA1==1500 & Data$MIGPLAC1==6] <- 15001 #Del Norte, Lassen, Modoc, Plumas, Siskiyou
Data$MIGCOUNTY1[Data$MIGPUMA1==3300 & Data$MIGPLAC1==6] <- 33001 #Lake, Mendocino
Data$MIGCOUNTY1[Data$MIGPUMA1==5301 & Data$MIGPLAC1==6] <- 53001 #Monterey
Data$MIGCOUNTY1[Data$MIGPUMA1==5302 & Data$MIGPLAC1==6] <- 53001 #Monterey
Data$MIGCOUNTY1[Data$MIGPUMA1==5303 & Data$MIGPLAC1==6] <- 53001 #Monterey
Data$MIGCOUNTY1[Data$MIGPUMA1==5700 & Data$MIGPLAC1==6] <- 57001 #Nevada, Sierra
Data$MIGCOUNTY1[Data$MIGPUMA1==10100 & Data$MIGPLAC1==6] <- 101001 #Sutter, Yuba

if (input$Hispanic=="0") {PopData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex)}
if (input$Hispanic=="1") {PopData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex & HISPAN!=0)}
if (input$Hispanic=="2") {PopData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex & HISPAN==0)}

if (input$Hispanic=="0") {InMigData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}
if (input$Hispanic=="1") {InMigData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex & HISPAN!=0 & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}
if (input$Hispanic=="2") {InMigData<-subset(Data, COUNTYFIP==input$County & STATEFIP==6 & SEX==input$Sex & HISPAN==0 & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}

agg_inmigdata<-aggregate(InMigData$SumOfPERWT, by=list(InMigData$AGE), FUN=sum)
agg_inmigdata<-merge(agelist,agg_inmigdata,by="Group.1",all.x=TRUE)
agg_popdata<-aggregate(PopData$SumOfPERWT, by=list(PopData$AGE), FUN=sum)
agg_popdata<-merge(agelist,agg_popdata,by="Group.1",all.x=TRUE)
if(input$ExcludeStudentAges=="YES") {plot(agg_popdata$x[0:MaxAge],type="l",ylim=c(0,max(agg_popdata$x*1.1,na.rm=TRUE)),xlab="Age",ylab="")}
if(input$ExcludeStudentAges=="NO") {plot(agg_popdata$x[0:MaxAge],type="l",ylim=c(0,max(agg_popdata$x*1.1,na.rm=TRUE)))}
lines(agg_inmigdata$x[0:MaxAge],type="l",col=2)

if (input$Hispanic=="0") {OutMigData<-subset(Data, MIGCOUNTY1==input$County & MIGPLAC1==6 & SEX==input$Sex & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}
if (input$Hispanic=="1") {OutMigData<-subset(Data, MIGCOUNTY1==input$County & MIGPLAC1==6 & SEX==input$Sex & HISPAN!=0 & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}
if (input$Hispanic=="2") {OutMigData<-subset(Data, MIGCOUNTY1==input$County & MIGPLAC1==6 & SEX==input$Sex & HISPAN==0 & (MIGRATE1D==22 | MIGRATE1D==23 | MIGRATE1D==24 | MIGRATE1D==25 | MIGRATE1D==30 | MIGRATE1D==31 | MIGRATE1D==32 | MIGRATE1D==40))}

agg_outmigdata<-aggregate(OutMigData$SumOfPERWT, by=list(OutMigData$AGE), FUN=sum)
agg_outmigdata<-merge(agelist,agg_outmigdata,by="Group.1",all.x=TRUE)
lines(agg_outmigdata$x[0:MaxAge],type="l",col=4)

legend(MaxAge*.65, max(agg_popdata$x*1.05,na.rm=TRUE), legend=c("Population", "In-movers", "Out-movers (domestic)"), col=c("black", "red", "blue"), lty=1, cex=1)

if (input$InOrOut=="IN") {agg_migrate<-agg_inmigdata$x/agg_popdata$x}
if (input$InOrOut=="OUT") {agg_migrate<-agg_outmigdata$x/agg_popdata$x}

plot(agg_inmigdata$x[0:MaxAge]/agg_popdata$x[0:MaxAge],type="l",col=2,xlab="Age",ylab="Migration Rate",ylim=c(0,.5))
lines(agg_outmigdata$x[0:MaxAge]/agg_popdata$x[0:MaxAge],col=4)

legend(MaxAge*.65, .48, legend=c("In", "Out (domestic)"), col=c("red", "blue"), lty=1, cex=1)

#####
##MIGRATION FITTER - FROM https://applieddemogtoolbox.github.io/Toolbox/#SPMMSRCode 
#####
#SIZE OF migprob (DATA BY AGE) USED
SIZE<-MaxAge

migprob<-agg_migrate[1:SIZE]
migprob[1]<-migprob[2]
migprob<-approx(migprob,n=length(migprob))
migprob<-migprob$y

#NUMBER OF TRIES - USED FOR FITTING
TRIES<-input$SampleSize

#PROPORTION TO ITER DISTRIBUTION BOUND SELECTION WITH
BEST<-.015

#CONVERGENCE INDEX
FITTO<-1e-10
###############

###############
##STEP 1 INPUTS
#PROPORTIONALLY ADJUST DATA TO SUM TO 1 - NO PARAMETERS
###############

###############
##STEP 2 INPUTS
#NUMBER OF SMALLEST VALUES TO USE AVERAGE OF AS LEVEL TERM
level<-5
###############

###############
##STEP 3 INPUTS
#MIN AND MAX OF CHILDHOOD AGES TO FIT OVER
childmin<-0
childmax<-16

#HEIGHT OF THE CHILDHOOD CURVE
childparam1tries<-array(runif(TRIES,0,.1))

#RATE OF DESCENT OF THE CHILDHOOD CURVE
childparam2tries<-array(runif(TRIES,0,1))
###############

###############
##STEP 4 INPUTS
#MIN AND MAX OF LABOR FORCE AGES TO FIT OVER
labormin<-17
labormax<-49

#STUDENT AGES TO EXCLUDE - CURRENTLY MUST BE ADJACENT AGES - TO EXCLUDE STUDENT PEAK FROM MODEL CAN SET AS JUST '0'
if(input$ExcludeStudentAges=="YES") {studentages<-c(input$StudentAges[1],input$StudentAges[2])}
if(input$ExcludeStudentAges=="NO") {studentages<-c(0)}

#HEIGHT OF THE LABOR FORCE CURVE
labparam1tries<-array(runif(TRIES,.04,.08))

#RATE OF DESCENT OF THE LABOR FORCE CURVE
labparam2tries<-array(runif(TRIES,.06,.10))

#POSITION OF THE LABOR FORCE CURVE ON THE AGE-AXIS
labparam3tries<-array(runif(TRIES,20,23))

#RATE OF ASCENT OF THE LABOR FORCE CURVE
labparam4tries<-array(runif(TRIES,.1,.5))
###############

###############
##STEP 5 INPUTS
#MIN AND MAX OF RETIREMENT AGES TO FIT OVER
retmin<-50
retmax<-75

#HEIGHT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
retparam1tries<-array(runif(TRIES,.0,.01)) 
if (input$RetirementFunction=="NO") {retparam1tries<-array(runif(TRIES,0,1e-10))}

#RATE OF DESCENT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
retparam2tries<-array(runif(TRIES,2.5,10)) 
if (input$RetirementFunction=="NO") {retparam2tries<-array(runif(TRIES,0,1e-10))}

#POSITION OF THE RETIREMENT CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '55' AND HIGH AS '55+1e-10'
retparam3tries<-array(runif(TRIES,55,65)) 
if (input$RetirementFunction=="NO") {retparam1tries<-array(runif(TRIES,55,55+1e-10))}
###############

###############
##STEP 6 INPUTS
#MIN AND MAX OF ELDERLY AGES TO FIT OVER
eldmin<-75
eldmax<-105

#HEIGHT OF THE ELDERLY CURVE
#TO EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
eldparam1tries<-array(runif(TRIES,0,.02)) #eldparam1tries<-array(runif(TRIES,0,1e-10))

#RATE OF DESCENT OF ELDERLY CURVE
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
eldparam2tries<-array(runif(TRIES,2.5,20)) #eldparam2tries<-array(runif(TRIES,0,1e-10))

#POSITION OF THE ELDERLY CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '100' AND HIGH AS '+1e-10'
eldparam3tries<-array(runif(TRIES,85,105)) #eldparam1tries<-array(runif(TRIES,100,100+1e-10))
###############

###############
###STEP 7 INPUTS
#MIN AND MAX OF STUDENT AGES TO FIT OVER - STUDENT AGES SET ABOVE UNDER STEP 4 INPUTS
#stumin<-min(studentages)
#stumax<-max(studentages+1)
#
##HEIGHT OF STUDENT CURVE
#TO EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
#if(input$IncludeStudentPeak=="YES") {stuparam1tries<-array(runif(TRIES,.001,.1))} 
#if(input$IncludeStudentPeak=="NO") {stuparam1tries<-array((runif(TRIES,0,1e-10)))}
#
##RATE OF DESCENT OF STUDENT CURVE
##TO EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
#if(input$IncludeStudentPeak=="YES") {stuparam2tries<-array(runif(TRIES,0,5))} 
#if(input$IncludeStudentPeak=="NO") {stuparam2tries<-array((runif(TRIES,0,1e-10)))}
#
##POSITION OF THE STUDENT CURVE ON THE AGE-AXIS
##TO EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
#if(input$IncludeStudentPeak=="YES") {stuparam3tries<-array(runif(TRIES,17,21))} 
#if(input$IncludeStudentPeak=="NO") {stuparam3tries<-array((runif(TRIES,0,1e-10)))}
#
##RATE OF ASCENT OF STUDENT CURVE
##TO EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
#if(input$IncludeStudentPeak=="YES") {stuparam4tries<-array(runif(TRIES,0,3))} 
#if(input$IncludeStudentPeak=="NO") {stuparam4tries<-array((runif(TRIES,0,1e-10)))}
###############

if (input$FitMigrationModel=="YES") {
##############################
##FIT TO THE DATA
##############################

##STEP 1 FIT - PROPORTIONAL TO SUM TO 1
step1<-array(,length(migprob))
for (i in 1:length(migprob)) {step1[i]<-migprob[i]/sum(migprob)}

##STEP 2 FIT - MAKE MEAN AGE AND SET LEVEL TERM BASED ON SELECTED NUMBER OF SMALLEST VALUES
step2<-array(,length(step1))
for (i in 1:length(step2)) {if(step1[i] != 0){step2[i]<-step1[i]}}
step2<-array(mean(sort(step2)[1:level]),length(step2))
meanages<-c(0+1:length(step1))

##STEP 3 FIT - SLOPE AND INTERCEPT FOR TRANSFORMATION
#THIS IS DIRECTLY ESTIMATED FIT - I COMMENTED OUT AND DID BY SAMPLING TO MAKE CONSISTENT WITH STEPS 4 THROUGH 7
#step3<-array(,childmax-childmin)
#for (i in 1:length(step3)) {step3[i]<-log(step1[i]-step2[i])}
#meanchildmin<-childmin+1
#childages<-c(childmin+1:childmax)
#childfit<-lm(step3~childages)
#for (i in 1:length(meanages)) {step3[i]<-exp(childfit$coefficients[1])*exp(-(-childfit$coefficients[2])*meanages[i])}
#step3<-step2+step3

##STEP 3 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES 
step3triesfit<-function(childparam1tries,childparam2tries){
step3tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step3tries[1:SIZE,i]<-childparam1tries[i]*exp(-childparam2tries[i]*(meanages[]))}
childresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=childmin)&(meanages[j]<=childmax)) {childresidtries[j,i]<-(step3tries[j,i]-(step1-step2)[j])^2}}}
sumchildresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumchildresidtries[i]<-sum(childresidtries[,i])}
childparam1tries<-runif(TRIES,min(childparam1tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]), max(childparam1tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]))
childparam2tries<-runif(TRIES,min(childparam2tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]), max(childparam2tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]))
childparamtries<-data.frame(sumchildresidtries=sumchildresidtries,childparam1tries=childparam1tries,childparam2tries=childparam2tries)
return(c(step3tries,childparamtries))
}
step3repeatpass<-step3triesfit(childparam1tries,childparam2tries)
ITER<-0
while (abs(max(step3repeatpass$childparam1tries)-min(step3repeatpass$childparam1tries))>FITTO & 
abs(max(step3repeatpass$childparam2tries)-min(step3repeatpass$childparam2tries))>FITTO
)
{step3repeatpass<-step3triesfit(step3repeatpass$childparam1tries,step3repeatpass$childparam2tries)
ITER=ITER+1
}
step3repeatpass$childparam1tries[1]
step3repeatpass$childparam2tries[1]
step3repeatpass$sumchildresidtries[1]
step3best<-array(step1-step2,dim=c(length(step1)))
ITER
step3best[1:SIZE]<-step3repeatpass$childparam1tries[1]*exp(-step3repeatpass$childparam2tries[1]*(meanages[]))
step3<-step2+step3best

##STEP 4 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE  
step4triesfit<-function(labparam1tries,labparam2tries,labparam3tries,labparam4tries){
step4tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step4tries[1:SIZE,i]<-labparam1tries[i]*exp(-labparam2tries[i]*(meanages[]-labparam3tries[i])-exp(-labparam4tries[i]*(meanages[]-labparam3tries[i])))}
labresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=labormin)&(meanages[j]<=labormax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {labresidtries[j,i]<-(step4tries[j,i]-(step1-step3)[j])^2}}}
sumlabresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumlabresidtries[i]<-sum(labresidtries[,i])}
labparam1tries<-runif(TRIES,min(labparam1tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam1tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam2tries<-runif(TRIES,min(labparam2tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam2tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam3tries<-runif(TRIES,min(labparam3tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam3tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam4tries<-runif(TRIES,min(labparam4tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam4tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparamtries<-data.frame(sumlabresidtries=sumlabresidtries,labparam1tries=labparam1tries,labparam2tries=labparam2tries,labparam3tries=labparam3tries,labparam4tries=labparam4tries)
return(c(step4tries,labparamtries))
}
step4repeatpass<-step4triesfit(labparam1tries,labparam2tries,labparam3tries,labparam4tries)
ITER<-0
while (abs(max(step4repeatpass$labparam1tries)-min(step4repeatpass$labparam1tries))>FITTO & 
abs(max(step4repeatpass$labparam2tries)-min(step4repeatpass$labparam2tries))>FITTO &
abs(max(step4repeatpass$labparam3tries)-min(step4repeatpass$labparam3tries))>FITTO &
abs(max(step4repeatpass$labparam4tries)-min(step4repeatpass$labparam4tries))>FITTO
)
{step4repeatpass<-step4triesfit(step4repeatpass$labparam1tries,step4repeatpass$labparam2tries,step4repeatpass$labparam3tries,step4repeatpass$labparam4tries)
ITER=ITER+1
}
step4repeatpass$labparam1tries[1]
step4repeatpass$labparam2tries[1]
step4repeatpass$labparam3tries[1]
step4repeatpass$labparam4tries[1]
step4repeatpass$sumlabresidtries[1]
ITER
step4best<-array(step1-step3,dim=c(length(step1)))
#step4best[1:SIZE]<-step4repeatpass$labparam1tries[1]*exp(-step4repeatpass$labparam2tries[1]*(meanages[]-step4repeatpass$labparam3tries[1])-exp(-step4repeatpass$labparam4tries[1]*(meanages[]-step4repeatpass$labparam3tries[1])))
step4best[1:SIZE]<-round(step4repeatpass$labparam1tries[1],3)*exp(round(-step4repeatpass$labparam2tries[1],3)*(meanages[]-round(step4repeatpass$labparam3tries[1],3))-exp(round(-step4repeatpass$labparam4tries[1],3)*(meanages[]-round(step4repeatpass$labparam3tries[1],3))))

step4<-step3+step4best

##STEP 5 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE   
step5triesfit<-function(retparam1tries,retparam2tries,retparam3tries){
step5tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step5tries[1:SIZE,i]<-retparam1tries[i]*exp(-((meanages[]-retparam3tries[i])/retparam2tries[i])*((meanages[]-retparam3tries[i])/retparam2tries[i]))}
retresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=retmin)&(meanages[j]<=retmax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {retresidtries[j,i]<-(step5tries[j,i]-(step1-step4)[j])^2}}}
sumretresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumretresidtries[i]<-sum(retresidtries[,i])}
retparam1tries<-runif(TRIES,min(retparam1tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam1tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparam2tries<-runif(TRIES,min(retparam2tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam2tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparam3tries<-runif(TRIES,min(retparam3tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam3tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparamtries<-data.frame(sumretresidtries=sumretresidtries,retparam1tries=retparam1tries,retparam2tries=retparam2tries,retparam3tries=retparam3tries)
return(c(step5tries,retparamtries))
}
step5repeatpass<-step5triesfit(retparam1tries,retparam2tries,retparam3tries)
ITER<-0
while (abs(max(step5repeatpass$retparam1tries)-min(step5repeatpass$retparam1tries))>FITTO & 
abs(max(step5repeatpass$retparam2tries)-min(step5repeatpass$retparam2tries))>FITTO & 
abs(max(step5repeatpass$retparam3tries)-min(step5repeatpass$retparam3tries))>FITTO  
)
{step5repeatpass<-step5triesfit(step5repeatpass$retparam1tries,step5repeatpass$retparam2tries,step5repeatpass$retparam3tries)
ITER=ITER+1
}
step5repeatpass$retparam1tries[1]
step5repeatpass$retparam2tries[1]
step5repeatpass$retparam3tries[1]
step5repeatpass$sumretresidtries[1]
ITER
step5best<-array(step1-step4,dim=c(length(step1)))
step5best[1:SIZE]<-step5repeatpass$retparam1tries[1]*exp(-((meanages[]-step5repeatpass$retparam3tries[1])/step5repeatpass$retparam2tries[1])*((meanages[]-step5repeatpass$retparam3tries[1])/step5repeatpass$retparam2tries[1]))
step5<-step4+step5best

##STEP 6 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE   
step6triesfit<-function(eldparam1tries,eldparam2tries,eldparam3tries){
step6tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step6tries[1:SIZE,i]<-eldparam1tries[i]*exp(-((meanages[]-eldparam3tries[i])/eldparam2tries[i])*((meanages[]-eldparam3tries[i])/eldparam2tries[i]))}
eldresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=eldmin)&(meanages[j]<=eldmax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {eldresidtries[j,i]<-(step6tries[j,i]-(step1-step4)[j])^2}}}
sumeldresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumeldresidtries[i]<-sum(eldresidtries[,i])}
eldparam1tries<-runif(TRIES,min(eldparam1tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]),max(eldparam1tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]))
eldparam2tries<-runif(TRIES,min(eldparam2tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]),max(eldparam2tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]))
eldparam3tries<-runif(TRIES,min(eldparam3tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]),max(eldparam3tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]))
eldparamtries<-data.frame(sumeldresidtries=sumeldresidtries,eldparam1tries=eldparam1tries,eldparam2tries=eldparam2tries,eldparam3tries=eldparam3tries)
return(c(step6tries,eldparamtries))
}
step6repeatpass<-step6triesfit(eldparam1tries,eldparam2tries,eldparam3tries)
ITER<-0
while (abs(max(step6repeatpass$eldparam1tries)-min(step6repeatpass$eldparam1tries))>FITTO & 
abs(max(step6repeatpass$eldparam2tries)-min(step6repeatpass$eldparam2tries))>FITTO & 
abs(max(step6repeatpass$eldparam3tries)-min(step6repeatpass$eldparam3tries))>FITTO  
)
{step6repeatpass<-step6triesfit(step6repeatpass$eldparam1tries,step6repeatpass$eldparam2tries,step6repeatpass$eldparam3tries)
ITER=ITER+1
}
step6repeatpass$eldparam1tries[1]
step6repeatpass$eldparam2tries[1]
step6repeatpass$eldparam3tries[1]
step6repeatpass$sumeldresidtries[1]
ITER
step6best<-array(step1-step4,dim=c(length(step1)))
step6best[1:SIZE]<-step6repeatpass$eldparam1tries[1]*exp(-((meanages[]-step6repeatpass$eldparam3tries[1])/step6repeatpass$eldparam2tries[1])*((meanages[]-step6repeatpass$eldparam3tries[1])/step6repeatpass$eldparam2tries[1]))
step6<-step5+step6best

###STEP 7 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE  
#step7triesfit<-function(stuparam1tries,stuparam2tries,stuparam3tries,stuparam4tries){
#step7tries<-array(step1-step2,dim=c(length(step1),TRIES))
#for (i in 1:TRIES) {step7tries[1:SIZE,i]<-stuparam1tries[i]*exp(-stuparam2tries[i]*(meanages[]-stuparam3tries[i])-exp(-stuparam4tries[i]*(meanages[]-stuparam3tries[i])))}
#sturesidtries<-array(0,dim=c(length(step1),TRIES))
#for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=stumin)&(meanages[j]<=stumax)) {sturesidtries[j,i]<-(step7tries[j,i]-(step1-step6)[j])^2}}}
#sumsturesidtries<-array(,TRIES)
#for (i in 1:TRIES) {sumsturesidtries[i]<-sum(sturesidtries[,i])}
#stuparam1tries<-runif(TRIES,min(stuparam1tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam1tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
#stuparam2tries<-runif(TRIES,min(stuparam2tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam2tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
#stuparam3tries<-runif(TRIES,min(stuparam3tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam3tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
#stuparam4tries<-runif(TRIES,min(stuparam4tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam4tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
#stuparamtries<-data.frame(sumsturesidtries=sumsturesidtries,stuparam1tries=stuparam1tries,stuparam2tries=stuparam2tries,stuparam3tries=stuparam3tries,stuparam4tries=stuparam4tries)
#return(c(step7tries,stuparamtries))
#}
#step7repeatpass<-step7triesfit(stuparam1tries,stuparam2tries,stuparam3tries,stuparam4tries)
#ITER<-0
#while (abs(max(step7repeatpass$stuparam1tries)-min(step7repeatpass$stuparam1tries))>FITTO &
#abs(max(step7repeatpass$stuparam2tries)-min(step7repeatpass$stuparam2tries))>FITTO &
#abs(max(step7repeatpass$stuparam3tries)-min(step7repeatpass$stuparam3tries))>FITTO &
#abs(max(step7repeatpass$stuparam4tries)-min(step7repeatpass$stuparam4tries))>FITTO
#)
#{step7repeatpass<-step7triesfit(step7repeatpass$stuparam1tries,step7repeatpass$stuparam2tries,step7repeatpass$stuparam3tries,step7repeatpass$stuparam4tries)
#ITER=ITER+1
#}
#step7repeatpass$stuparam1tries[1]
#step7repeatpass$stuparam2tries[1]
#step7repeatpass$stuparam3tries[1]
#step7repeatpass$stuparam4tries[1]
#step7repeatpass$sumsturesidtries[1]
#ITER
#step7best<-array(step1-step6,dim=c(length(step1)))
#step7best[1:SIZE]<-step7repeatpass$stuparam1tries[i]*exp(-step7repeatpass$stuparam2tries[i]*(meanages[]-step7repeatpass$stuparam3tries[i])-exp(-step7repeatpass$stuparam4tries[i]*(meanages[]-step7repeatpass$stuparam3tries[i])))
#step7<-step6+step7best

##REVIEW FIT
#SQUARED SUM OF RESIDUALS FOR ENTIRE MODEL
squaredsumoffullmodelresiduals<-sum((step4-step1)^2) #squaredsumoffullmodelresiduals<-sum((step7-step1)^2)


##############################
##PLOT THE DATA
##############################

##PLOT ACCUMULATED FIT
if(input$ExcludeStudentAges=="YES") {plot(step1,xlab="Age",ylab="Migration Rate (proportional)",ylim=c(-.005,.035),pch=1,panel.first=c(abline(v=c(input$StudentAges[1],input$StudentAges[2]),col=1,lty=3)))}
if(input$ExcludeStudentAges=="NO") {plot(step1,xlab="Age",ylab="Migration Rate (proportional)",ylim=c(-.005,.035))}
lines(step6,col="black",lwd=3) #lines(step7,col="black",lwd=3)

##PLOT INDIVIDUAL STEP FITTING
#lines(step7-step6,col="yellow",lwd=2,lty=2)
lines(step6-step5,col="orange",lwd=2,lty=2)
lines(step5-step4,col="purple",lwd=2,lty=2)
lines(step4-step3,col="green",lwd=2,lty=2)
lines(step3-step2,col="blue",lwd=2,lty=2)
lines(step2,col="red",lwd=2,lty=2)

##PLOT RESIDUALS
lines(step6-step1,col="dark grey") #lines(step7-step1,col="dark grey")

legend(MaxAge*.65,.035, 
legend=c("Scaled data", "Full model curve", "Level", "Childhood curve", "Labor force curve", "Retirement curve", "Elderly curve", "Full model residuals"), 
col=c("black", "black", "red", "blue", "green", "purple", "orange", "grey"), 
lwd=c(1,2,2,2,2,2,2,1), lty=c(NA,1,2,2,2,2,2,1), pch=c(1,NA,NA,NA,NA,NA,NA,NA), cex=1)
}

},height=1100,width=1100)
	
}

shinyApp(ui = ui, server = server)

