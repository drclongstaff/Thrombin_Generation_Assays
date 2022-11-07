library(shiny)                             # Load the Shiny library
library(dplyr)
library(tibble)
library(purrr)
#library(clipr)
library(readxl)

#This function is for loading user data
load_file <- function(NAME, PATH, SHEET){
  
  ext <- tools::file_ext(NAME)
  switch(ext,
         "xlsx"= read_excel(PATH, SHEET),
         csv = vroom::vroom(PATH, delim = ",", show_col_types = FALSE),
         tsv = vroom::vroom(PATH, delim = "\t"),
         txt = vroom::vroom(PATH, show_col_types = FALSE),
         validate("Invalid file. Please upload a .csv or .txt file")
  )
}

shinyServer(function(input, output) {      # Set up the Shiny Server
  
  
  #Load calibrator data
  CalibF0<-reactive({
    inputFile <- input$data0
    if (is.null(inputFile)) 
    read.csv("./Data/CalibratorsA.csv")
    else(
      load_file(input$data0$name, input$data0$datapath, input$sheetc) %>% 
        as.data.frame()
    )
    
  })
  
  CalibF <- reactive({
    
    CalibF0() %>% select(1, input$calstart:input$calend)
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
  
  
  CalibDat<-reactive({
    CalibDat<-data.frame("Time"= CalibF()[1:input$limitD,1], "F"=CalibF()[1:input$limitD, input$calCols])
  })
  
  #Here we fit the polynomial for the calibrator
  Poly4<-reactive({
    Yobs<-CalibDat()[, 2]
    TcF<-CalibDat()[, 1]
    Fitpoly4<-lm(Yobs~poly(TcF, 5, raw = TRUE))
    Fitpoly4$coefficients
  })
  
  #Already fitted calib data
  #Just need to get values for vo and intercept
  #output$calvo <- renderTable({
  SlopeInt1 <- reactive({
    A <- Poly4()[1] 
    B <- Poly4()[2]
    C <- Poly4()[3] 
    D <- Poly4()[4] 
    E <- Poly4()[5] 
    Ff <- Poly4()[5] 
    f <- expression(A+B*x+C*x^2+D*x^3+E*x^4+Ff*x^5)
    df <- D(f,'x')
    x <- 0
    newvo <- eval(df)
    newint <- eval(f)
    
    as.numeric(c(newint, newvo))
    
  })
  
  
  output$mytest <- renderPlot({
    if(is.null(input$calCols)){return(NULL)}
    par(mfrow = c(2,2))  # Split the plotting panel into a 2 x 2 grid
    #par(mar=c(0,0,0,0)) # dimensions for figure
    Yobs<-CalibDat()[, 2]
    TcF<-CalibDat()[, 1]
    Fitpoly4<-lm(Yobs~poly(TcF, 5, raw = TRUE))
   # plot(Fitpoly4)  # Plot the model information)
    plot(Fitpoly4$fitted, Fitpoly4$residuals, xlab="Fitted", ylab="Residuals", bty="n")
    abline(h=0, lty=2, col="red")
    plot(CalibThrombin()[,1], CalibThrombin()[,2], type="l", col="blue", xlab="Time", ylab="Thrombin nM", bty="n")
    abline(h=input$CalibT, lty=2, col="red")
    plot(CalibThrombin()[,1], cumsum(Fitpoly4$residuals), xlab="Time", ylab="cusum residuals", bty="n")
    abline(h=0, lty=2, col="red")
    qqnorm(Fitpoly4$residuals, main="", bty="n")
    qqline(Fitpoly4$residuals, lty=2)
  })
  
  # yp is the perfect straight line of the initial slope
  yp <- reactive({
    
    ypc<-SlopeInt1()[1]+CalibDat()[,1]*SlopeInt1()[2] #corrected y
    
  })
  
  
  
  #This is the fitting to the plot Yobs vs the perfect straight line
  #Polynomial fitting get coefficients
  Poly5<-reactive({
    Yobs<-CalibDat()[, 2]
    Y<-yp()
    #Fitpoly5<-lm(Y~poly(Yobs, 4, raw = TRUE))
    Fitpoly5<-lm(Y~poly(Yobs, 5, raw = TRUE))
    Fitpoly5$coefficients
  })
  
 #polynomial correction function
  fun_poly<-function(y){
    yideal_P<-Poly5()[1]+ 
      Poly5()[2]*y +  
      Poly5()[3]*y^2+
      Poly5()[4]*y^3+
      Poly5()[5]*y^4+ 
    Poly5()[6]*y^5
  } 
 
  
  yideal_P <- reactive({
    Yobs<-CalibDat()[, 2]
    Y<-yp()
    Fitpoly5<-lm(Y~poly(Yobs, 5, raw = TRUE))
    yideal_P<-fun_poly(Yobs)
  })
  
  
 
  #To generate the thrombin curve for the calibrator
  
 CalibThrombin <- reactive ({
  
   CalibThrombin<-CalibDat()[-1] %>%  map_df(~fun_poly(.x)) %>% 
    map_df(~fun_FtoT(.x)) %>% 
    add_column(CalibDat()[,1], .before = 1) %>% as.data.frame()
   diffres <- diff(CalibThrombin[,2])/diff(CalibThrombin[,1])
   cbind((CalibThrombin[,1]), diffres)
   
 }) 
  
   #Graph of fitted calibrator  
  output$myCalib<-renderPlot({
    if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    
    maxX <- as.double(max(CalibDat()[1], na.rm = TRUE))
    maxY <- as.double(max(CalibDat()[2], na.rm = TRUE))
    TatD<-CalibDat()[,1]#[input$limitD]
    
    #yfit <- Poly4()[1]+Poly4()[2]*TatD+Poly4()[3]*TatD^2+Poly4()[4]*TatD^3+Poly4()[5]*TatD^4
    yfit <- Poly4()[1]+Poly4()[2]*TatD+Poly4()[3]*TatD^2+Poly4()[4]*TatD^3+Poly4()[5]*TatD^4+Poly4()[6]*TatD^5
    
   
    
    #Draw the graphs
    plot(CalibF()[,1], CalibF()[, input$calCols], 
         pch="+", xlim = c(0, maxX), ylim=c(0, maxY), xlab = "Time", ylab = "Fluorescence",
         #main =  paste("Calibrator : initial slope =", round(SlopeInt1()[2], 4), " and intercept = ", round(SlopeInt1()[1], 4)))
         main =  paste("Calibrator : initial slope =", round(SlopeInt1()[2], 4)))
    points(CalibDat()[,1], yp(), col = "red")
    lines(CalibDat()[,1], yfit, col = "magenta")
    points(CalibDat()[,1], yideal_P(), pch="x", col = "olivedrab")
    legend("bottomright", legend=c("raw","fit", "initial rate", "corrected"), pch=c("+","_", "O", "X"), col = c("black","magenta", "red",  "olivedrab"))
  })
  
  
  
  
  ###TEST DATA
  ##Read raw fluorescence data
  RawF0 <- reactive({
    inputFile <- input$data1
    if (is.null(inputFile)) 
    read.csv("./Data/NewDat3.csv")#[c(1, 2:29)]
    else(
      load_file(input$data1$name, input$data1$datapath, input$sheetd) %>% 
        #load_file(input$data1$datapath) %>% 
        as.data.frame()
    )
    
  })
  

  #This is truncated or columns chosen 
  #and is responsive when the truncation is changed
  RawF <- reactive({
    #RawF0A <- RawF0()
    RawF0A <- RawF0()[c(1, input$datstart:input$datend)]
    RawF <- RawF0A[1:(nrow(RawF0())-input$truncpoints),]
  })
  
 #Put mfrow code here
  
  output$value <- renderUI({ print(14) })
 
  
   #This is for the settings table state calibrators 
  output$calset<- renderUI({
    inputFile <- input$data0$name
    if (is.null(inputFile)) 
      return(basename("./Data/CalibratorsA.csv"))
    else(basename(inputFile))
  }) 
  
  #This is for the settings table stae raw data
  output$fileset<- renderUI({
    inputFile <- input$data1$name
    if (is.null(inputFile)) 
      return(basename("./Data/NewDat3.csv"))
    else(basename(inputFile))
  })

  
  ##Corrections for raw data using polynomial function above
  
  RawFP <- reactive ({
    plateFP<-RawF()[-1] %>%  map_df(~fun_poly(.x)) %>% add_column(RawF()[,1], .before = 1) %>% as.data.frame()
    
  })
 
  #These are the plots of raw data with or without polynomial correction
  output$myplotsF<-renderPlot({
    if(is.null(input$calCols)){return(NULL)} # To stop this section running and producing an error before the data has uploaded
    
    TimeF<-RawF()[,1]
    DatwellsF<-RawF()[,-1]
    
    absWellsF<-length(DatwellsF[1,])
    samplesF<-names(DatwellsF)
    maxF<-max(DatwellsF)
    maxT<-max(TimeF)
    
    RowNumF<-input$numrows
    
    par(mfrow=c(RowNumF,(absWellsF/RowNumF)))
    par(mar=c(0.6,3.2,0.2,0.2)) # dimensions for figure
    
    switch(input$Transf,
           "none"=plateFT<-RawF(),
           "Polynomial"=plateFT<-RawFP())
    
    for (f in 1: absWellsF ) {
      
      yi<- DatwellsF[,f] 
      
      plots<-plot(TimeF, yi, type = "l", col= "blue", lwd = 2, xlim= c(0, maxT), 
                  ylim=c(0, max(plateFT[-1])),    axes=FALSE)
      axis(1, seq(0, maxT, maxT), pos=0)
      axis(2, seq(0,maxF,maxF),las=2, pos=0,cex.axis=1.2, col.axis="red")
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
  
  
  ##More transformations
  #Converting Fluorescence to Thrombin
  
  fun_FtoT<-function(Th){
    
    nowT<-input$CalibT*(Th/input$calSlope)
  }
 
 
  #Make the derivative curves
  
  fun_diff<-function(d1){
    res<-diff(d1)/diff(RawF()[,1])
    res
  }
  
  
  readData <- reactive({
    plateF <- RawF()
    
    switch(input$Transf,
           "none"=plateFC<-plateF,
            "Polynomial"=plateFC<-RawFP()) 
    Tdif<-plateFC[,1][-1]
    plateFd<-plateFC[-1] %>%  map_df(~fun_diff(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    plateFdT <- plateFd[-1] %>% map_df(~fun_FtoT(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
    
    #Late smoothing
    lateTimepoint <- length(Tdif)-input$smtail
    lateTime <- Tdif[lateTimepoint]
    fun_latesmooth<-function(ps){
      smoothfit<-supsmu(Tdif, ps)
      ifelse(Tdif>lateTime, smoothfit$y, ps)
    }
    
    plateFdTs <- plateFdT[-1] %>% map_df(~fun_latesmooth(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
   
    #6. Alpha-2-M correction
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
           #"Raw" = plateF,
           "F"=plateN <- plateFd, 
           "Thrombin"=plateN <- plateFdT,
           "Smooth" = plateN <- plateFdTs,
           "no T-alpha-2M" = plateN <- plateFdTsM)
    
    #write_clip(plateN)
    plateN
    
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
    plateFdT <- plateF[-1]  %>%  map_df(~fun_diff(.x)) %>% map_df(~fun_FtoT(.x)) %>% add_column(Tdif, .before = 1) %>% as.data.frame()
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
    pcChange<-(pclag*(max(curve)-minAbs))+minAbs
    startPoint<-which(abs(upAbs-pcChange)==min(abs(upAbs-pcChange)))[1]
    #This prevents crashing if there are blank wells
    thresh <- 0.005
    ifelse(max(curve)-min(curve)<thresh,
           startAbs<-upAbs[startPoint],
           startAbs<-round(approx(upTime, upAbs, xout = pcChange, ties = mean)$x,3)
    )
    
    #StartTime is fitted if abs > threshold, otherwise is closest point
    #This prevents crashing if there are blank wells
    ifelse(max(curve)-min(curve)<thresh,
           startTime<-upTime[startPoint],
           startTime<-round(approx(upAbs, upTime, xout = startAbs, ties = mean)$y,3)
    )
    
    changeA=allMaxA-minAbs
    
    upcurve<-c(minAbs, startTime, pcChange, startAbs, startPoint, pointmax,allMaxT, allMaxA)
    
  }
  
  downy<-function(curve){
    pointmax<-which.max(curve)
    downTime<-Timedf[-c(1:pointmax)] #vector of time to max
    downAbsR<-curve[-c(1:pointmax)]  #vector of absorbances to max
    #maybe it's best to smooth the whole tail??
    downSmu<-supsmu(downTime, downAbsR)
    downAbs<-downSmu$y
    endAbs<-min(downAbs)#+input$offset
    #THIS IS NEW. SHOULD THE minAbs BE THE MIN ABS FOR THE DOWN CURVE?
    #DOES SOLVE THE PROBLEM OF POINTMIN BEING WRONG
    minAbs <- endAbs #try it this way incase the curve does not go back to zero
    ##which point closest to minimum
    pointmin<-which(abs(downAbs-endAbs)==min(abs(downAbs-endAbs)))[1]
    pcChange<-pclag*(max(curve)-minAbs)+minAbs
    
    #NEW decaypoint is where set% lysis occurs
    #if_else(curve[length(curve)]>=pcChange, decayPoint <- length(curve), decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] )
    if_else(downAbs[length(downAbs)]>=pcChange, decayPoint <- length(downAbs), decayPoint<-which(abs(downAbs-pcChange)==min(abs(downAbs-pcChange)))[1] )
    #New decayabs
    if_else(max(curve)-min(curve)<0.005,
            decayAbs<-downAbs[decayPoint],
            decayAbs<-round(approx(downTime, downAbs, xout = pcChange, ties = mean)$x,3)
    )
    
    #StartTime is fitted if abs > threshold, otherwise is closest point
    #This prevents crashing if there are blank wells
    #New decaytime
    ifelse(max(curve)-min(curve)<0.005,
           decayTime<-downTime[decayPoint],
           decayTime<-round(approx(downAbs, downTime, xout = decayAbs, ties = mean)$y,3)
    )
    
    #This AUC is with %lag and decay 
    AUC<-sum(diff(Timedf[1:(pointmax+decayPoint)])*(head(curve[1:(pointmax+decayPoint)],-1)+tail(curve[1:(pointmax+decayPoint)],-1)))/2
    #AUC<-auc(Timedf[1:(pointmax+decayPoint)], curve[1:(pointmax+decayPoint)], type = 'spline')
    
   # downcurve<-c(decayTime, decayTime.i,decayAbs, decayAbs.i, decayPoint, pointmax, AUC, pointmin)
    downcurve<-c(decayTime, decayTime,decayAbs, decayAbs, decayPoint, pointmax, AUC, pointmin)
    
  }
  
  pclag<- input$lagChange*0.01 #changes the number to %
  myDatcorrTaMT <-readData()
  Timedf<-myDatcorrTaMT[,1]
  
  TabResShort<-myDatcorrTaMT[-1] %>%  map_df(~ data.frame(Ao=uppy(.x)[1], Lag=uppy(.x)[2], AUC=downy(.x)[7], Peak=uppy(.x)[8], ttPeak=uppy(.x)[7], ttTail=downy(.x)[2], 
                                                          LagReading=uppy(.x)[3], Startpoint=uppy(.x)[5],
                                                          Pointmax=uppy(.x)[6], DecayPoint=(uppy(.x)[6])+(downy(.x)[5]), MinPoint=downy(.x)[8], 
                                                          Base=(downy(.x)[2]-uppy(.x)[2]), LagtoPeak =uppy(.x)[7]-uppy(.x)[2])) %>% 
    add_column(Sample=names(myDatcorrTaMT[-1]), .before = 1)
  #clipr::write_clip(TabResShort) #REMOVE BEFORE UPLOAD
  TabResShort
  })
  
  output$resultsTable<-renderTable({
    
    
    TabRes()[,c(1:8, 13,14)]
    
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
    #lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")#shows thrombin curve
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
           "10"= abline("v"=TabRes()[k,10], col = "blue", lty=1),
           "4"=polygon(Time[TabRes()[k,9]: TabRes()[k,11]], yi[TabRes()[k,9]: TabRes()[k,11]], col = "khaki")
           #"4"=polygon(Time[1: TabRes()[k,12]], yi[1: TabRes()[k,12]], col = "khaki")
           
    )          
  }
  
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
    #lines(platedT()[,1], platedT()[, k+1], lty=2, col = "green")
    lines(Time[TabRes()[k,9]:TabRes()[k,10]], yi[TabRes()[k,9]:TabRes()[k,10]],col="green", lwd=3)
    lines(Time[TabRes()[k,10]:TabRes()[k,11]], yi[TabRes()[k,10]:TabRes()[k,11]],col="blue", lwd=3)
    lines(Time[TabRes()[k,11]:TabRes()[k,12]], yi[TabRes()[k,11]:TabRes()[k,12]],col="red", lwd=2)
    
  
           "2"= abline("h"=TabRes()[k,2], col = "black", lty=2)
           "5"= abline("h"=TabRes()[k,5], col = "black", lty=2)
           "3"= abline("v"=TabRes()[k,3], col = "black", lty=2)
           "6"= abline("v"=TabRes()[k,6], col = "black", lty=2)
           "7"= abline("v"=TabRes()[k,7], col = "black", lty=2)
           "8"= abline("h"=TabRes()[k,8], col = "magenta", lty=2)
           #"10"= abline("v"=TabRes()[k,10], col = "blue", lty=1)
           # "4"=polygon(Time[TabRes()[k,9]: TabRes()[k,11]], yi[TabRes()[k,9]: TabRes()[k,11]], col = "grey80")
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
                    "Raw fluorescence"= RawF(),
                    "Analysed" =  readData()
                    )
 #clipr::write_clip(rawData)
  clipr::write_clip(rawData)[-1]#To remove time column if necessary
  rawData
})

output$calData <- renderTable({
  calData <- CalibF()
  
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
  setTable<-setsTab()
  
  setTable
})



})
