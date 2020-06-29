library(shiny)                             # Load the Shiny library
library(tidyverse)


shinyServer(function(input, output) {      # Set up the Shiny Server
  
  
  #Load calibrator data
  CalibF<-reactive({
    inputFile <- input$data0
    if (is.null(inputFile)) 
    #read.csv("./Data/FDA_Cal_100nM.csv")
    read.csv("./Data/CalibratorsA.csv")
    else(read.csv(inputFile$datapath))
  })
 
  ##CALIBRATOR
  #Get calibrator column names
  varCal<- reactive({
    calCols<-colnames(CalibF()[-1]) #remember to remove the first Time column
  })
  
  #Choose calibrator to analyse  
  output$whatCal<-renderUI({
    selectInput("calCols",
                label= h5("Curve"),
                choices = varCal())
  })
  
  ###TEST DATA
  ##Read raw fluorescence data
  RawF0 <- reactive({
    inputFile <- input$data1
    if (is.null(inputFile)) 
      #read.csv("./Data/FDA_Fig3_416.csv")
      read.csv("./Data/Raw F subdata Da.csv")
    else(read.csv(inputFile$datapath))
  })
  
   
  CalibDat<-reactive({
    CalibDat<-data.frame("Time"= CalibF()[1:input$limitD,1], "F"=CalibF()[1:input$limitD, input$calCols])
  })
  
  RawF <- reactive({
    RawF <- RawF0()[1:(nrow(RawF0())-input$truncpoints),]
  })
  
  output$fileset<- renderUI({
    inputFile <- input$data1
    if (is.null(inputFile)) 
      return(file.path("./Data/Raw F subdata Da.csv"))
    else(file.path(inputFile))
  })

  ##Get vo from fitting
  
  SlopeInt <- reactive({
   # Fi <-CalibF()[1, input$calCols]
    Flu <- CalibF()[, input$calCols]
    Tp <- CalibF()[,1]
    lmPA <- lm(Flu[1:5]~Tp[1:5]) #fit linear model to first 5 points
    vest <- coefficients(lmPA)[2] #estimates for nlm fit
    Fest <- coefficients(lmPA)[1]
    fitslow <- nls(Flu~(vo/kobs)*(1-exp(-kobs*Tp))+Fl0, start = list(vo=vest, kobs=0.04,Fl0=Fest))
    Resfit <- c(coefficients(fitslow)[3], coefficients(fitslow)[1], coefficients(fitslow)[2])
  })
  
  yp <- reactive({
    ypc<-SlopeInt()[1]+CalibDat()[,1]*SlopeInt()[2] #corrected y
  })
  
  
  #Polynomial fitting get coefficients
  Poly5<-reactive({
    Yobs<-CalibDat()[, 2]
    Y<-yp()
    #Fitpoly5<-lm(Y~poly(Yobs, 5, raw = TRUE))
    Fitpoly5<-lm(Y~poly(Yobs, 4, raw = TRUE))
    Fitpoly5$coefficients
  })
  
  #2. H transform
 # fun_htrans<-function(plateData){
 #   yideal_H<-input$alpha*atanh(plateData/input$alpha)
 # }

  #9. polynomial correction function
  fun_poly<-function(y){
    yideal_P<-Poly5()[1]+ 
      Poly5()[2]*y +  
      Poly5()[3]*y^2+
      Poly5()[4]*y^3+
      Poly5()[5]*y^4#+ 
      #Poly5()[6]*y^5
  } 
  
  yideal_P <- reactive({
  Yobs<-CalibDat()[, 2]
  Y<-yp()
  Fitpoly5<-lm(Y~poly(Yobs, 4, raw = TRUE))
  yideal_P<-fun_poly(Yobs)
  })
   
 # yideal_H <- reactive({
  #   Yobs<-CalibDat()[, 2]
  #  yideal_H <- fun_htrans(Yobs)
 # })
 
  
  #Graph of fitted calibrator  
  output$myCalib<-renderPlot({
    if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    yfit <- SlopeInt()[1]+(SlopeInt()[2]/SlopeInt()[3])*(1-exp(-SlopeInt()[3]*CalibDat()[,1]))
    #Draw the graphs
    TatD<-CalibDat()[,1][input$limitD]
    plot(CalibDat()[,1],CalibDat()[,2], pch=2, xlim = c(0, TatD), xlab = "Time", ylab = "Fluorescence")
    lines(CalibDat()[,1], yp(), col = "red")
    lines(CalibDat()[,1], yfit, col = "blue")
    points(CalibDat()[,1], yideal_P(), pch="x", col = "green")
    #points(CalibDat()[,1], yideal_H(), pch="o", col = "cyan")
    text(TatD*0.5, max(yp())*0.1, paste("initial slope =", round(SlopeInt()[2], 4), " and intercept = ", round(SlopeInt()[1], 4)))
  })
 
  
  ##Corrections for raw data
  #Polynomial
  
  RawFP <- reactive ({
    plateF<-RawF()
    plateFP<-plateF[-1] %>%  map_df(~fun_poly(.x)) %>% add_column(plateF[,1], .before = 1) %>% as.data.frame()
    
  })
  #H transform
  
  #RawFH <- reactive({
  #  plateF<-RawF()
  #  plateFH<-plateF[-1] %>%  map_df(~fun_htrans(.x)) %>% add_column(plateF[,1], .before = 1) %>% as.data.frame()
    
 # })
  
  output$myplotsF<-renderPlot({
    if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    plateF<-RawF()
    
    TimeF<-plateF[,1]
    DatwellsF<-plateF[,-1]
    
    absWellsF<-length(DatwellsF[1,])
    samplesF<-names(DatwellsF)
    maxF<-max(DatwellsF)
    maxT<-max(TimeF)
    
    RowNumF<-input$numrows
    
    par(mfrow=c(RowNumF,(absWellsF/RowNumF)))
    par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
    
    switch(input$Transf,
           "none"=plateFT<-plateF,
          # "H-transform"=plateFT<-RawFH(),
           "Polynomial"=plateFT<-RawFP())
    
    for (f in 1: absWellsF ) {
      
      yi<- DatwellsF[,f] 
      #t<-Time
      
      plots<-plot(TimeF, yi, type = "l", col= "blue", lwd = 2, xlim= c(0, maxT), 
                  ylim=c(0, max(plateFT[-1])), xaxt="n", yaxt="n")
      lines(plateFT[,1], plateFT[,f+1],lwd=1, col = "red")
      text(maxT*.1, maxF*.8, samplesF[f])
    }

  })
  
  #Print a table of the polynomial coefficients (optional) 
  output$Fluor<-renderTable({
    Pco<-matrix(c(Poly5()[1], Poly5()[2], Poly5()[3] , Poly5()[4] , Poly5()[5] ), nrow = 1) #,Poly5()[6]
    colnames(Pco)<-c("x0", "x1", "x2", "x3", "x4")#, "x5"
    formatC(Pco)
  })
  
  ##Data preparation
  
 
  
  #5. Converting F to [Thrombin]
  fun_FtoT<-function(Th){
    
    #nowT<-input$CalibT*(Th/(input$TMcomp*input$calSlope))
    nowT<-input$CalibT*(Th/input$calSlope)
  }
 
 
  #Make the derivative curves
  
  
  #Funtion to take first derivative
  #theTime <-reactive({RawF()[2,1]-RawF()[1,1]})
  theTime <-reactive({RawF()[,1]})
  fun_diff<-function(d1){
    res<-diff(d1)/diff(theTime())
    res
  }
  
  #This is the curve to plot
  
  readData <- reactive({
    plateF <- RawF()
    
    switch(input$Transf,
           "none"=plateFC<-plateF,
          # "H-transform"=plateFC<-RawFH(),
           "Polynomial"=plateFC<-RawFP()) 
    #theTime <-RawF()[,1]
    Tdif<-plateFC[,1][-1]
    plateFd<-plateFC[-1] %>%  map_df(~fun_diff(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    plateFdT <- plateFd[-1] %>% map_df(~fun_FtoT(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
    #Late smoothing
    #latePoint <- input$smtail
    lateTimepoint <- length(Tdif)-input$smtail
    lateTime <- Tdif[lateTimepoint]
    fun_latesmooth<-function(ps){
      smoothfit<-supsmu(Tdif, ps)
      #ifelse(Tdif<(length(Tdif)-lateTimepoint), ps, smoothfit$y)
      ifelse(Tdif>lateTime, smoothfit$y, ps)
    }
   
    
    plateFdTs <- plateFdT[-1] %>% map_df(~fun_latesmooth(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
    #6. Alpha-2-M correction
    #latePoint <- input$smtail
    fun_TGcorr<-function(M){
      endTimepoint <- length(M)
      lateTimepoint <- length(M)-input$smtail
      lateTime <- Tdif[lateTimepoint]
      endVal<-mean(M[lateTimepoint:endTimepoint])
      Fdummy<-rep(endVal, length(Tdif))
      Fnew<-ifelse(Tdif>lateTime, Fdummy, M)
      FTm<-endVal*(cumsum(Fnew[1:lateTimepoint])/max(cumsum(Fnew[1:lateTimepoint])))
      Fdummy[1:lateTimepoint]<-FTm
      M-Fdummy
    }
    
    plateFdTsM <- plateFdTs[-1] %>% map_df(~fun_TGcorr(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
    switch(input$a2Mcor, 
           "F"=plateN <- plateFd, 
           "Thrombin"=plateN <- plateFdT,
           "Smooth" = plateN <- plateFdTs,
           "no T-alpha-2M" = plateN <- plateFdTsM)
    
  })
  
  #The T-alpha-2-M-curve
  
  a2MData <- reactive({
    plateM <- RawF()
    
    switch(input$Transf,
           "none"=plateMC<-plateM,
          # "H-transform"=plateMC<-RawFH(),
           "Polynomial"=plateMC<-RawFP()) 
    #theTime <-RawF()[,1]
    Tdif<-plateMC[,1][-1]
    #plateMd<-plateMC[-1] %>%  map_df(~fun_diff(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    plateMdT <- plateMC[-1]  %>%  map_df(~fun_diff(.x)) %>% map_df(~fun_FtoT(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
    #latePoint <- input$smtail
    fun_TaM<-function(C){
      endTimepoint <- length(C)
      lateTimepoint <- length(C)-input$smtail
      lateTime <- Tdif[lateTimepoint]
      endVal<-mean(C[lateTimepoint:endTimepoint])
      Fdummy<-rep(endVal, length(Tdif))
      Fnew<-ifelse(Tdif>lateTime, Fdummy, C)
      FTm<-endVal*(cumsum(Fnew[1:lateTimepoint])/max(cumsum(Fnew[1:lateTimepoint])))
      Fdummy[1:lateTimepoint]<-FTm
      Fdummy
    }
    
    plateMdTM <- plateMdT[-1] %>% map_df(~fun_TaM(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
  })
  
  #First derivative of raw fluorescence for plotting
  platedT <- reactive({
    plateF <- RawF()
    
    #theTime <-RawF()[,1]
    Tdif<-plateF[,1][-1]
    #plateMd<-plateMC[-1] %>%  map_df(~fun_diff(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    plateFdT <- plateF[-1]  %>%  map_df(~fun_diff(.x)) %>% map_df(~fun_FtoT(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
  })
  
  ##Plot the curves
  output$myplotAll<-renderPlot({
    #if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    plate0 <- readData()
    plateM<- a2MData()
    
    Time<-round(plate0[,1], 2)
    Datwells<-plate0[,-1]
   # maxy <- max(Datwells)
    pointsnum<-length(Datwells[,1])-1
    absWells<-length(Datwells[1,])
    samples<-names(Datwells)
    
    initiation<-input$num
    RowNum<-input$numrows
    
    par(mfrow=c(RowNum,(absWells/RowNum)))
    par(mar=c(0.2,0.2,0.2,0.2)) # dimensions for figure
    
    mint<-min(Time)
    
    for (k in 1: absWells ) {
      
      yi<- Datwells[,k] 
      #t<-Time
      
      plots<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, xaxt="n", yaxt="n",  ylim = c(min(Datwells), max(Datwells)))
      lines(plateM[,1], plateM[,k+1],lwd=1, col = "orange")
      lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")
      lines(Time[TabRes()[k,9]:TabRes()[k,10]], yi[TabRes()[k,9]:TabRes()[k,10]],col="green", lwd=3)
      lines(Time[TabRes()[k,10]:TabRes()[k,11]], yi[TabRes()[k,10]:TabRes()[k,11]],col="blue", lwd=3)
      lines(Time[TabRes()[k,11]:TabRes()[k,12]], yi[TabRes()[k,11]:TabRes()[k,12]],col="red", lwd=2)
      
      switch(input$tabRes,
      "2"= abline("h"=TabRes()[k,2], col = "black", lty=2),
      "5"= abline("h"=TabRes()[k,5], col = "black", lty=2),
      "3"= abline("v"=TabRes()[k,3], col = "black", lty=2),
      "6"= abline("v"=TabRes()[k,6], col = "black", lty=2),
      "7"= abline("v"=TabRes()[k,7], col = "black", lty=2),
      "8"= abline("h"=TabRes()[k,8], col = "magenta", lty=2),
     # "4"=polygon(Time[TabRes()[k,3]: TabRes()[k,11]], yi[TabRes()[k,3]: TabRes()[k,11]], col = "khaki")
      "4"=polygon(Time[1: TabRes()[k,12]], yi[1: TabRes()[k,12]], col = "khaki")
    
      )          
    }
    
  })
  
###DO THE ANALYSIS ON THE CURVES
  ##Various functions
  
  TabRes <- reactive({
 
  
  uppy<-function(curve){
    minAbs=curve[1] #use this if not fixing minAbs
    pointmax<-which.max(curve)
    allMaxT<-Timedf[pointmax] #find the corresponding Time and F for this point
    allMaxA<-curve[pointmax]
    upTime<-Timedf[c(1:pointmax)] #vector of time to max
    upAbs<-curve[c(1:pointmax)]  #vector of absorbances to max
    #pcChange<-pcChange*(max(curve)-min(curve))+minAbs
    pcChange<-(pclag*(max(curve)-minAbs))+minAbs
    startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
    startTime<-upTime[startPoint]
    startAbs<-upAbs[startPoint]
    startAbs.i<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,3)
    startTime.i<-round(approx(upAbs, upTime, xout = startAbs.i, ties = mean)$y,3)
    changeA=allMaxA-minAbs
    
    upcurve<-c(minAbs, startTime.i, pcChange, startAbs.i, startPoint, pointmax,allMaxT, allMaxA)
    
  }
  
  downy<-function(curve){
    minAbs=curve[1] #use this if not fixing minAbs
    pointmax<-which.max(curve)
    downTime<-Timedf[-c(1:pointmax)] #vector of time to max
    downAbsR<-curve[-c(1:pointmax)]  #vector of absorbances to max
    #maybe it's best to smooth the whole tail??
    downSmu<-supsmu(downTime, downAbsR)
    downAbs<-downSmu$y
    endAbs<-min(downAbs)#+input$offset
    pointmin<-which(abs(downAbs-endAbs)==min(abs(downAbs-endAbs)))[1]
    #pcChange<-pcChange*(max(curve)-min(curve))+minAbs
    pcChange<-pclag*(max(curve)-minAbs)+minAbs
    decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1]
    decayTime<-downTime[decayPoint]
    decayAbs<-downAbs[decayPoint]
    decayAbs.i<-round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x,3)
    decayTime.i<-round(approx(downAbs, downTime, xout = decayAbs.i, ties = mean)$y,3)
    AUC<-sum(diff(Timedf[1:(pointmax+decayPoint)])*(head(curve[1:(pointmax+decayPoint)],-1)+tail(curve[1:(pointmax+decayPoint)],-1)))/2
    #AUC<-auc(Timedf[1:(pointmax+decayPoint)], curve[1:(pointmax+decayPoint)], type = 'spline')
    
   
    
    downcurve<-c(decayTime, decayTime.i,decayAbs, decayAbs.i, decayPoint, pointmax, AUC, pointmin)
    
  }
  
  pclag<- input$lagChange*0.01
  myDatcorrTaMT <-readData()
  Timedf<-myDatcorrTaMT[,1]
  
  TabResShort<-myDatcorrTaMT[-1] %>%  map_df(~ data.frame(Ao=uppy(.x)[1], Lag=uppy(.x)[2], AUC=downy(.x)[7], Peak=uppy(.x)[8], ttPeak=uppy(.x)[7], ttTail=downy(.x)[2], 
                                                          LagReading=uppy(.x)[3], Startpoint=uppy(.x)[5],
                                                          Pointmax=uppy(.x)[6], DecayPoint=(uppy(.x)[6])+(downy(.x)[5]), MinPoint=downy(.x)[8], Base=(downy(.x)[2]-uppy(.x)[2]))) %>% 
    add_column(Sample=names(myDatcorrTaMT[-1]), .before = 1)

 #write.table(TabResShort , "clipboard",  sep="\t", row.names = FALSE,  col.names=TRUE)
  TabResShort
  })
  
  output$resultsTable<-renderTable({
   TabRes()[,c(1:8, 13)]
    #head(readData())
  })
  
output$plotsTable<-renderTable ({
   if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    RowNum<-input$numrows
    
    
    data<-switch(input$tabRes, 
                 "1"=matrix(TabRes()[,1], byrow=TRUE, nrow=RowNum),
                 "2"=matrix(TabRes()[,2], byrow=TRUE, nrow=RowNum),
                 "3"=matrix(TabRes()[,3], byrow=TRUE, nrow=RowNum),
                 "4"=matrix(TabRes()[,4], byrow=TRUE, nrow=RowNum),
                 "5"=matrix(TabRes()[,5], byrow=TRUE, nrow=RowNum), 
                 "6"=matrix(TabRes()[,6], byrow=TRUE, nrow=RowNum),
                 "7"=matrix(TabRes()[,7], byrow=TRUE, nrow=RowNum), 
                 "8"=matrix(TabRes()[,8], byrow=TRUE, nrow=RowNum),
               

    )
    
    colnames(data) =as.character(1:(length(data)/RowNum))
    #write.table(data, "clipboard", sep="\t", col.names=F, row.names=F) #OPTIONAL- live if local
    #write.table(data, "TscopeRRes.txt", sep="\t", col.names=F, row.names=F)
    data
    
  })

##Plot of single curve
#Also need to select well

var<- reactive({
  colnames(readData()[-1])
})



output$what<-renderUI({
  selectInput("colmnames",
              label= h5("Select a column of absorbance data"),
              choices = var())
}) 

output$nowwhat<-renderUI({
  selectInput("colmnames",
              label= h5("Copy a column of absorbance data"),
              choices = var())
}) 

#plot for single curve
output$myplot<-renderPlot({
  if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  plate0 <- readData()
  plateM<- a2MData()
  
  TaM<-plateM[,input$colmnames]
  
  Time<-round(plate0[,1], 2)
  Datwells<-plate0[,-1]
  
  yi<-Datwells[,input$colmnames] # Rather than get and attach, this just reads in the column chosen earlier
  
  k<-which(TabRes()[, 1]==input$colmnames)
 
  mint<-min(Time)
  
 
    
    plots<-plot(Time, yi, type = "l", col= "slategrey", lwd = 3, ylim = c(min(Datwells), max(Datwells)*1.1), ylab = "Thrombin or Fluorescence")
    lines(plateM[,1], plateM[,k+1],lwd=1, col = "orange")
    lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")
    lines(Time[TabRes()[k,9]:TabRes()[k,10]], yi[TabRes()[k,9]:TabRes()[k,10]],col="green", lwd=3)
    lines(Time[TabRes()[k,10]:TabRes()[k,11]], yi[TabRes()[k,10]:TabRes()[k,11]],col="blue", lwd=3)
    lines(Time[TabRes()[k,11]:TabRes()[k,12]], yi[TabRes()[k,11]:TabRes()[k,12]],col="red", lwd=2)
    
  
           "2"= abline("h"=TabRes()[k,2], col = "black", lty=2)
           "5"= abline("h"=TabRes()[k,5], col = "black", lty=2)
           "3"= abline("v"=TabRes()[k,3], col = "black", lty=2)
           "6"= abline("v"=TabRes()[k,6], col = "black", lty=2)
           "7"= abline("v"=TabRes()[k,7], col = "black", lty=2)
           "8"= abline("h"=TabRes()[k,8], col = "magenta", lty=2)
           # "4"=polygon(Time[TabRes()[k,3]: TabRes()[k,11]], yi[TabRes()[k,3]: TabRes()[k,11]], col = "grey80")
           #"4"=polygon(Time[1: TabRes()[k,12]], yi[1: TabRes()[k,12]], col = "grey90")
           
  
})

output$curveTable<-renderTable({
  if(is.null(input$colmnames)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
  TabNames<-colnames(TabRes())
  ColNam<-c("Parameter", "Result")
  
  All.Res<-TabRes() %>% filter(Sample == input$colmnames) %>% select(1:7)
  All.Res.Tab<-cbind(TabNames[1:7], t(All.Res))
  colnames(All.Res.Tab)<-ColNam
  
  All.Res.Tab
})


output$rawData<-renderTable({
  rawData <- switch(input$whatRaw,
  "Raw fluorescence"=RawF(),
  "Thrombin"=platedT(), #uncorreted
  "Corrected"=readData()
  )
  
  #write.table( rawData[,input$colmnames], "clipboard",  sep="\t", row.names = FALSE,  col.names=FALSE)
  rawData
})

setsTab<-reactive({
  
  setTable<-matrix(c( 
    "Date", format(Sys.Date(), "%d %b %Y"), "Time", format(Sys.time(), "%X"),
    
    "Read interval s", round(RawF()[2,1]-RawF()[1,1], 4),  "% Change for end of lag", input$lagChange,
    
    "Calibrator settings", "", "Points for calibrator fit", input$limitD, 
    
    "Concentration of calibrator", input$CalibT, "Calibrator rate", input$calSlope,
    
    "Data analysis", "", "", "",
    
    "Truncate data", input$truncpoints, "Points to smooth tail", input$smtail,
    
    "Correction method", input$Transf, "T-alpha-2M correction", input$a2Mcor

  ),
  
  byrow=TRUE, nrow=7)
  
  colnames(setTable)<-c("Parameter", "Value", "Parameter", "Value")
  setTable
})



output$settings<-renderTable({
  #if(is.null(input$colmnames)){return(NULL)}
  setTable<-setsTab()
  
  setTable
})



})
