library(data.table)
library(ggplot2)
library(shiny)
library(grid)
library(gridExtra)
library(rjson)


time2Tc <- function(tim){  
  b <- tim - tim%/%10^12*10^12
  # hhmmssffffff
  ms <- b%%10^6; b <-(b-ms)/10^6
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600+ms/(3600*10^6))
}
hhmmss2Tc <- function(tim){  
  # hhmmss
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600)
}
Tc2str <- function(Tc){  
  hh <- floor(Tc)
  b <- (Tc-hh)*60
  mm <- floor(b)
  ss <- (b-mm)*60
  a <- ISOdatetime(0,1,1,hh,mm,ss)
  str <- format(a, "%H:%M:%OS2")
  return(str)
}

readmsgs <- function(){  
  msgs <- fread('Capture_20110701_ES_OUT.txt', header = FALSE, 
                col.names = c('Time','Prod','Type','Action','Level','Price','Size','KI','OrdCt','FIXMsgID'))
  # filters for market hours
  msgs[,Tc:= time2Tc(Time)]
  msgs <- msgs[Tc %between% c(15,20.25) & Prod == 'ESU1']
  msgs[,Price:= Price/100]
  msgs <- msgs[order(Tc)]
}


plotsuspectmsgs <- function(msgs,rngTc,Nl){  
  Tc = msgs$Tc
  Type = msgs$Type
  Action = msgs$Action
  Level = msgs$Level
  Price = msgs$Price
  Size = msgs$Size
  OrdCt = msgs$OrdCt   
  FTc = rngTc[1]
  LTc = rngTc[2]
  xt = which(Type==2)
  
  #type 2 are Trades
  cx = which(Type==2 & Tc>=FTc)
  #first Trade in interval [FTc, LTc]
  Tick = (Price - Price[cx[1]])*4
  cx = which(Level==1 & Type==0 & Tc>=FTc)
  #first Bid in interval [FTc, LTc]
  Bid0 = Tick[cx[1]]
  cx = which(Level==1 & Type==1 & Tc>=FTc)
  #first Offer in interval [FTc, LTc]
  Offer0 = Tick[cx[1]]
  
  # convert to seconds
  Tc = (Tc - FTc)*3600
  LTc = (LTc-FTc)*3600
  FTc = 0
  dT = (LTc-FTc)/8
  #define limits for plots
  rngTc = c(FTc,LTc)
  limTc = c(FTc-dT,LTc+dT)
  limSize = c(0,max(Size))
  limOrd = c(0,max(OrdCt))
  limTick = c(min(Tick[xt]),max(Tick[xt]))
  
  grid.newpage()
  par(mfrow=c(3,2))
  par(cex = 0.8)
  par(mar = c(2, 2, 1.5, 1),  oma = c(1.5, 0.5, 0.5, 0.5))
  #cl <- topo.colors(5)
  cl <- rainbow(7)
  ac = "turquoise3"
  ac = "black"
  
  
  plot(0,0,xlim=limTc,ylim = limSize,type = "n",xlab="sec",ylab="",col.lab=ac)
  title(main="Bid Size", col.main=ac, font.main=1)
  #axis(2, labels=, pos=, lty=, col=, las=, tck=, ...)
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
  for (i in 1:Nl){
    cx = which(Tick==Bid0+1-i & Type==0)
    lines(Tc[cx],Size[cx],col = cl[i],type='s')
  }
  
  plot(0,0,xlim=limTc,ylim = limSize,type = "n",xlab="sec",ylab="",col.lab=ac)
  title(main="Offer Size", col.main=ac, font.main=1)
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
  for (i in 1:Nl){
    cx = which(Tick==Offer0-1+i & Type==1)
    lines(Tc[cx],Size[cx],col = cl[i],type='s')
    a = Tc[cx]
  }
  
  plot(0,0,xlim=limTc,ylim = limOrd,type = "n",xlab="sec",ylab="",col.lab=ac)
  title(main="Bid Order Count", col.main=ac, font.main=1)
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
  for (i in 1:Nl){
    cx = which(Tick==Bid0+1-i & Type==0)
    lines(Tc[cx],OrdCt[cx],col = cl[i],type='s')
  }
  
  plot(0,0,xlim=limTc,ylim = limOrd,type = "n",xlab="sec",ylab="",col.lab=ac)
  title(main="Offer Order Count", col.main=ac, font.main=1)
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
  for (i in 1:Nl){
    cx = which(Tick==Offer0-1+i & Type==1)
    lines(Tc[cx],OrdCt[cx],col = cl[i],type='s')
  }
  
  plot(Tc[xt],Tick[xt],xlim=limTc,ylim=limTick,pch='.',type='s',xlab="sec", col="red",ylab="",col.lab=ac)
  title(main="Trade Price (in Ticks)", col.main=ac, font.main=1)  
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
  
  plot(Tc[xt],Size[xt],xlim=limTc,pch='.',type='h',xlab="sec", col="red",ylab="",col.lab=ac)
  title(main="Trade Size", col.main=ac, font.main=1)  
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("black"), lty=2, lwd=1)
}



plotprice <- function(msgs,rngTc){  
  Tc <- msgs$Tc
  Price <- msgs$Price
  Size <- msgs$Size
  FTc <- rngTc[1]
  LTc <- rngTc[2]
  
  #define limits for plots
  limTc = c(15,20.25)
  limPrice = c(min(Price),max(Price))
  limSize = c(min(Size),max(Size))
  
  grid.newpage()
  par(mfrow=c(3,1))
  par(cex = 0.8)
  par(mar = c(2, 2, 1.5, 1),  oma = c(1.5, 0.5, 0.5, 0.5))
  cl <- rainbow(7)
  ac = "turquoise3"
  ac = "black"
  
  plot(Tc,Price,xlim=limTc,ylim = limPrice,type='l',xlab="time",ylab="",col.lab=ac)
  title(main="Price", col.main=ac, font.main=1)
  axis(1, col.axis=ac); axis(2, col.axis=ac)
  abline(v=rngTc, col=c("red"), lty=1, lwd=1)
}


plot6P <- function(i,Nl){
  d <- DT[i,]
  FTc <- d$FTc
  LTc <- d$LTc
  dT = max((LTc - FTc),5/3600)
  msgsP <- msgs[Tc %between% c(FTc-dT,LTc+dT)]
  plotsuspectmsgs(msgsP,c(FTc,LTc),Nl)
}
plot1P <- function(i){
  d <- DT[i,]
  FTc <- d$FTc
  LTc <- d$LTc
  plotprice(msgsT,c(FTc,LTc))
}
printmsgs <- function(i){
  d <- DT[i,]
  s <- sprintf("Duration = %6.3f sec",d$Duration)
  return(s)
}


printdf <- function(i){
  c <-DT[i,]
  Time <- c(Tc2str(c$FTc),Tc2str(c$LTc))
  Level <- c(paste0(c$FSide,c$FLevel),paste0(c$LSide,c$LLevel))
  dSize <- format(c(c$FdSz,c$LdSz), nsmall=0, trim = FALSE, width=5)
  dCount <- format(c(c$FdCt,c$LdCt), nsmall=0, trim = FALSE, width=3)
  df <- data.frame(Time=Time, Level=Level, dSize=dSize, dCount=dCount)
  row.names(df) <- c("Start","End")
  colnames(df) <- c("Time","Level","ChgSize","ChgCount")
  return(df)
}

printdf1 <- function(i){
  c <-DT[i,]
  df <- data.frame(Time=c(Tc2str(c$FTc),Tc2str(c$LTc)), Level=c(paste0(c$FSide,c$FLevel),paste0(c$LSide,c$LLevel)), 
                   dSize=c(c$FdSz,c$LdSz), dCount=c(c$FdCt,c$LdCt))
  df$dCount <- format(df$dCount, nsmall=0, justify = "right", trim = FALSE, width=3)
  df$dSize <- format(df$dSize, nsmall=0, justify = "right", trim = FALSE, width=5)
  row.names(df) <- c("Start","End")
  colnames(df) <- c("Time","Level","ChgSize","ChgCount")
  return(df)
}


msgs <- readmsgs()
msgsT <- msgs[Type==2,]
msgsT <- msgsT[seq(1, nrow(msgsT), 100),]




suspect.JSON <- fromJSON(file="result1.json")
N = length(suspect.JSON)
DT <- data.frame(Duration=NA,FLevel=NA,FPrice=NA,FdCt=NA,FdSz=NA,FMID=NA,FTc=NA,FSide=NA,
                 LLevel=NA,LPrice=NA,LdCt=NA,LdSz=NA,LMID=NA,LTc=NA,LSide=NA)

for (i in 1:N){
  c <- suspect.JSON[i]$'';
  cF = c$First
  cL = c$Last
  DT[i,] = list(c$Duration, 
                cF$Level,cF$Price/100,cF$deltaCt,cF$deltaSz,cF$FIXMsgId,time2Tc(cF$Time),cF$Side,
                cL$Level,cL$Price/100,cL$deltaCt,cL$deltaSz,cL$FIXMsgId,time2Tc(cL$Time),cL$Side)
}

DT <- data.table(DT)   
DT <- DT[order(FTc)]
DT <- DT[FTc>=15]  
N <- nrow(DT)


server <- function(input, output) {
  
  output$plot6P <- renderPlot({
    plot6P(input$CaseNum,input$LevelNum)
  })
  
  output$plot1P <- renderPlot({
    plot1P(input$CaseNum)
  })
  
  output$text <- renderUI({ 
    ss <- paste("<b>",printmsgs(input$CaseNum),"</b>")
    HTML(ss)
  })
  
  output$text1 <- renderText({ 
    printmsgs(input$CaseNum)
  })
  
  output$table <- renderTable({ 
    printdf(input$CaseNum)
  })
  
}  






#shinyApp(ui = ui, server = server)
