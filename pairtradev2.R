#Required libraries
library("quantmod")
library("tseries")

pairtest <- function(hisse,stocks){
  len <- length(stocks)
  # 2 combinations of all stocks given.
  a <- combn(stocks,2)
  i=1
  cat("\n\n",deparse(substitute(stocks)),stocks)

  sym <- len*(len-1)/2
  for (i in 1:sym){
    fiyat <- subset(hisse, select = c(a[1,i],a[2,i]))
  
    # Carry out linear regression on the two price serie
    comb1 = lm(fiyat[,1]~fiyat[,2])
    comb2 = lm(fiyat[,2]~fiyat[,1])
    # Performs the ADF test using a single lag order. 
    # Picks the one with lower adf stat.
    adfresult1 <- adf.test(comb1$residuals, k=2)
    adfresult2 <- adf.test(comb2$residuals, k=2)
    # Check adfstatistic to obtain better results
    if(adfresult1$statistic < adfresult2$statistic){
      comb = comb1
      adfresult = adfresult1
    } else{
      comb = comb2
      adfresult = adfresult2
      hold = fiyat[,1]
      fiyat[,1] = fiyat[,2]
      fiyat[,2] = hold
      hold = a[1,i]
      a[1,i] = a[2,i]
      a[2,i] = hold 
    }
    gun = nrow(fiyat)
    date <- as.Date(hisse$Date, "%d/%m/%Y")
    startDate <- as.Date("2017", "%Y")
    today <- as.Date(Sys.Date(), "%m/%d/%Y")

    b <- scale(comb$residuals, center = TRUE, scale = TRUE)
    maxmean <- mean(rollmax(b[which(b>0)],100))
    minmean <- mean(rollapply(b[which(b<0)],100,min))

    zEntry <- maxmean * 0.8
    zProfit <- maxmean * 0.5
    zStop <- maxmean * 1.1

    nzEntry <- minmean * 0.5
    nzProfit <-  minmean * 0.8
    nzStop <- minmean * 1.1

    zScore <- tail(b,1)
    price <- rbind(fiyat,0)
    price[gun+1,1] = fiyat[gun,1] 
    price[gun+1,2] = fiyat[gun,2] 

    if(adfresult$p.value <= 0.01){
      if( (zScore > zEntry  & zScore < zStop) | (zScore < nzEntry & zScore > nzStop) ){

        Relative <- fiyat[,1]/fiyat[,2]
        roll <- rollmean(Relative,100,align="right",fill = 0)
        hedgeRatio <- comb$coefficients[2] / (tail(fiyat[,1],1)/tail(fiyat[,2],1))

        if( (tail(roll,1) >= tail(Relative,1)) ){
          cat("\nShort",a[2,i],tail(fiyat[,2],1),"Long",a[1,i],tail(fiyat[,1],1),hedgeRatio)        
        }else{
          cat("\nShort",a[1,i],tail(fiyat[,1],1),"Long",a[2,i],tail(fiyat[,2],1))
        }
        # Plot closing prices
        par(mfrow = c(3,1))
        plot(date,fiyat[,1], type="l",xlim=c(startDate,today),ylim=range(tail(fiyat[,1],300)), xlab="Date", ylab="Prices", col="blue")
        legend("bottomleft", c(paste(a[1,i],"LHS"), paste(a[2,i],"RHS")), lty=c(1, 1), col=c("blue", "red"), bg="white" )  
        par(new=T)
        plot(date,fiyat[,2], type="l",xlim=c(startDate,today),ylim=range(tail(fiyat[,2],300)), axes=F, xlab="", ylab="", col="red")
        mtext(paste("Closing Prices",a[1,i],a[2,i]))
        axis(side=4)
        
        # Plot relative price
        plot(date,Relative,xlim=c(startDate,today),ylim=range(tail(Relative,300)),xlab="Date", type="l", ylab="",col="gray25")
        par(new=T)
        plot(date,roll, type="l",xlim=c(startDate,today),ylim=range(tail(Relative,300)), axes=F, xlab="", ylab="", col="red")
        legend("bottomleft", c("Relative Performance", "Rolling Mean"), lty=c(1,1), col=c("gray25", "red"), bg="white")   
        mtext(paste("Relative Performance:",a[1,i],"/",a[2,i]))

        # Plot spread
        plot(date,b, type = "l",xlim=c(startDate,today), ylim=range(tail(b,300)), xlab="Date", ylab="",col="gray60")
        abline(h = mean(b) , col = "red", lty = 2)
        abline(h = zEntry , col = "seagreen", lty = 2)
        abline(h = nzEntry , col = "seagreen", lty = 2)
        legend("bottomleft", c("Entry Level", "Historical Mean", "Spread"), lty=c(2,2,1), col=c("seagreen", "red", "gray40"), bg="white" )      
        mtext(paste(zScore/zEntry,"Z Score of Spread")) 
      }
    }
  }
}

#Load data
setwd("Z:/Research/WORKING/Intern/Berke/Pair")
hisse <- read.csv(file="bist.csv", header=TRUE, sep=",")

pdf("PairTrade.pdf",width=20,height=12)

BANKA <- c("XU030","AKBNK","ALBRK","DENIZ","FINBN","GARAN","HALKB","ICBCT","ISCTR","SKBNK","TSKB","VAKBN","YKBNK")
pairtest(hisse,BANKA)

SA <- c("SAHOL","AVISA","BRISA","AKCNS","AKBNK","KORDS","CRFSA","TKNSA","CIMSA","YUNSA","AKGRT")
pairtest(hisse,SA)

KOC <- c("KCHOL","FROTO","TOASO","TTRAK","ARCLK","AYGAZ", "OTKAR","TATGD", "TUPRS")
pairtest(hisse,KOC)

ANADOLU <- c("AGHOL","AEFES","ASUZU", "MGROS","CCOLA","ADEL")
pairtest(hisse,ANADOLU)

TELCO <- c("XU030","TCELL","TTKOM")
pairtest(hisse,TELCO)

dev.off()

##bankalar
## aghol sahol kchol vs tuprs arclk subsidiaries
## 3lü parça pair 1/2 tuprs

