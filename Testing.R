library("quantmod")
library("tseries")

ploting <- function(date,fiyat1,fiyat2,startDate,today,ticker1,ticker2,Relative,roll,b,zEntry,nzEntry){
  
  # Plot closing prices
  plot(date,fiyat1, type="l",xlim=c(startDate,today),ylim=range(tail(fiyat1,300)), xlab="Date", ylab="Prices", col="blue")
  legend("bottomleft", c(paste(ticker1,"LHS"), paste(ticker2,"RHS")), lty=c(1, 1), col=c("blue", "red"), bg="white" )  
  par(new=T)
  plot(date,fiyat2, type="l",xlim=c(startDate,today),ylim=range(tail(fiyat2,300)), axes=F, xlab="", ylab="", col="red")
  mtext(paste("Closing Prices",ticker1,ticker2))
  axis(side=4)
  
  # Plot relative price
  plot(date,Relative,xlim=c(startDate,today),ylim=range(tail(Relative,300)),xlab="Date", type="l", ylab="",col="gray25")
  par(new=T)
  plot(date,roll, type="l",xlim=c(startDate,today),ylim=range(tail(Relative,300)), axes=F, xlab="", ylab="", col="red")
  legend("bottomleft", c("Relative Performance", "Rolling Mean"), lty=c(1,1), col=c("gray25", "red"), bg="white")  	
  mtext(paste("Relative Performance:",ticker1,"/",ticker2))
  
  # Plot spread
  plot(date,b, type = "l",xlim=c(startDate,today), ylim=range(tail(b,300)), xlab="Date", ylab="",col="gray60")
  abline(h = mean(b) , col = "red", lty = 2)
  abline(h = zEntry , col = "seagreen", lty = 2)
  abline(h = nzEntry , col = "seagreen", lty = 2)
  legend("bottomleft", c("Entry Level", "Historical Mean", "Spread"), lty=c(2,2,1), col=c("seagreen", "red", "gray40"), bg="white" )  		
  mtext("Z Score of Spread")
  
}
pairtest <- function(hisse,stocks){
  options <- c("XU030","AKBNK","ARCLK","EKGYO","EREGL","GARAN","HALKB","ISCTR","KCHOL","KRDMD","PETKM","PGSUS","SAHOL","SISE","TCELL","THYAO","TOASO","TTKOM","TUPRS","VAKBN","YKBNK")
  # 2 combinations of all stocks given.
  a <- combn(stocks,2)
  i=1
  
  while (i < (length(a)/2)+1){
  	fiyat <- subset(hisse, select = c(a[1,i],a[2,i]))
  
  	# Carry out linear regression on the two price series
  	comb1 = lm(fiyat[,1]~fiyat[,2])
  	comb2 = lm(fiyat[,2]~fiyat[,1])
  	# Performs the ADF test using a single lag order. 
  	# Picks the one with lower adf stat.
  	adfresult1 <- adf.test(comb1$residuals, k=2)
  	adfresult2 <- adf.test(comb2$residuals, k=2)
  	# Check both sides to obtain better stats.
  	if (adfresult1$statistic < adfresult2$statistic){
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
  
  	if(adfresult$p.value <= 0.01){
  		gun = nrow(fiyat)
  		date <- as.Date(hisse$Date, "%d/%m/%Y")
  		startDate <- as.Date("2016", "%Y")
  		today <- as.Date(Sys.Date(), "%m/%d/%Y")
  
  		Relative <- fiyat[,1]/fiyat[,2]
  		Relative[is.infinite(Relative)] <- 0
  		Relative[is.nan(Relative)] <- 0
  		Relative[is.na(Relative)] <- 0	
  		
  		roll <- rollmean(Relative,100,align="right",fill = 0)
  		hedgeRatio <- comb$coefficients[2] / (tail(fiyat[,1],1)/tail(fiyat[,2],1))
  		b <- scale(comb$residuals, center = TRUE, scale = TRUE)
  		maxmean <- mean(rollmax(b[which(b>0)],100))
  		minmean <- mean(rollapply(b[which(b<0)],100,min))
  		
  		zEntry <- maxmean * 0.7
  		nzEntry <- minmean * 0.7
  		
  		zScore <- tail(b,1)
  		price <- rbind(fiyat,0)
  		price[gun+1,1] = fiyat[gun,1] 
  		price[gun+1,2] = fiyat[gun,2] 
  
		if(zScore > zEntry){
			if(a[1,i] %in% options == TRUE){
				ploting(date,fiyat[,1],fiyat[,2],startDate,today,a[1,i],a[2,i],Relative,roll,b,zEntry,nzEntry)
				if(tail(roll,1) > tail(Relative,1)){
					cat("\nShort",a[2,i],tail(fiyat[,2],1),"Long",a[1,i],tail(fiyat[,1],1))
				}else{
					cat("\nShort",a[1,i],tail(fiyat[,1],1),"Long",a[2,i],tail(fiyat[,2],1))
				
			}
		} 
		if(zScore < nzEntry ){
			if(a[2,i] %in% options == TRUE){
				ploting(date,fiyat[,1],fiyat[,2],startDate,today,a[1,i],a[2,i],Relative,roll,b,zEntry,nzEntry)
				if(tail(roll,1) > tail(Relative,1)){
					cat("\nShort",a[2,i],tail(fiyat[,2],1),"Long",a[1,i],tail(fiyat[,1],1))				
				}else{
					cat("\nShort",a[1,i],tail(fiyat[,1],1),"Long",a[2,i],tail(fiyat[,2],1))
				}
			}
		}
  	}
	i <-  i + 1
  }
}

#Load data
setwd("C:\\Intern\\Berke\\Cointegration\\Data")
hisse <- read.csv(file="bist.csv", header=TRUE, sep=",")

ARACI <- c("XU030","GEDIK","GLBMD","INFO","ISMEN","OSMEN")
pairtest(hisse,ARACI)

BANKA <- c("XU030","AKBNK","ALBRK","DENIZ","FINBN","GARAN","HALKB","ICBCT","ISCTR","KLNMA","SKBNK","TSKB","VAKBN","YKBNK")
pairtest(hisse,BANKA)

BILISIM <- c("XU030","ASELS","NETAS","ALCTL","ARENA","KAREL","DGATE","FONET","KRONT","LOGO","ESCOM","PKART","DESPC","LINK","INDES","ARMDA")
pairtest(hisse,BILISIM)

ELEKTRIK <- c("XU030","AKENR","AKSEN","AKSUE","AYEN","BMELK","ODAS","ZOREN")
pairtest(hisse,ELEKTRIK)

FAKTORING <- c("XU030","CRDFA","GARFA","ISFIN","LIDFA","SEKFK","VAKFN","FFKRL")
pairtest(hisse,FAKTORING)

GIDA <- c("XU030","AEFES","ALYAG","AVOD","BANVT","CCOLA","DARDL","EKIZ","ERSU","FRIGO","IZTAR","KENT","KERVT","KNFRT","KRSAN","KRSTL","MERKO","OYLUM","PENGD","PETUN","PINSU","PNSUT","SELGD","TACTR","TATGD","TBORG","TKURU","TUKAS","ULKER","ULUUN","VANGD","YAPRK")
pairtest(hisse,GIDA)

GIRISIM <- c("XU030","EGCYO","EGLYO","GDKGS","GOZDE","HDFGS","ISGSY","RHEAG","VERTU")
pairtest(hisse,GIRISIM)

GMYO <- c("XU030","AGYO","AKFGY","AKMGY","AKSGY","ALGYO","ATAGY","AVGYO","DGGYO","DZGYO","EKGYO","HLGYO","IDGYO","ISGYO","KLGYO","KRGYO","MRGYO","MSGYO","NUGYO","OZGYO","OZKGY","PAGYO","PEGYO","RYGYO","SNGYO","SONME","SRVGY","TRGYO","TSGYO","VKGYO","YGGYO","YGYO","YKGYO")
pairtest(hisse,GMYO)

HIZMET <- c("XU030","AKGUV","FLAP","SNKRN","MCTAS")
pairtest(hisse,HIZMET)

ILETISIM <- c("XU030","TCELL","TTKOM")
pairtest(hisse,ILETISIM)

INSAAT <- c("XU030","ANELE","EDIP","ENKAI","KUYAS","ORGE","SANEL","TURGG","YYAPI")
pairtest(hisse,INSAAT)

KIRTASIYE <- c("ADEL","SERVE")
pairtest(hisse,KIRTASIYE)

MADEN <- c("XU030","IHLGM","IPEKE","KOZAL","KOZAA","PRKME","METAL")
pairtest(hisse,MADEN)

METAL <- c("XU030","BRSAN","BURCE","COMDO","CELHA","CEMAS","CEMTS","CUSAN","DMSAS","ERBOS","EREGL","IZMDC","KRDMA","KRDMB","KRDMD","OZBAL","SARKY","TUCLK","ASCEL","BURVA","EMNIS","ISDMR","MAKTK")
pairtest(hisse,METAL)

MKYO <- c("XU030","ATLAS","ECBYO","EUKYO","ETYAT","EUYO","GRNYO","ISYAT","OYAYO","VKFYO")
pairtest(hisse,MKYO)

MOBILYA <- c("XU030","DGKLB","GENTS","DMISH","ORMA","YATAS","YONGA")
pairtest(hisse,MOBILYA)

ORMAN <- c("XU030","ALKA","BAKAB","DGZTE","DGKLB","DURDO","GENTS","HURGZ","IHGZT","KARTN","TIRE","OLMIP","PRZMA","SAMAT","VKING","IPEKE","DOBUR")
pairtest(hisse,ORMAN)

SIGORTA <- c("XU030","AKGRT","ANHYT","ANSGR","AVISA","GUSGR","HALKS","RAYSG")
pairtest(hisse,SIGORTA)

SPOR <- c("XU030","BJKAS","FENER","GSRAY","TSPOR")
pairtest(hisse,SPOR)

TAS <- c("XU030","ADANA","ADBGR","ADNAC","AFYON","AKCNS","ANACM","ASLAN","AYES","BASCM","BOLUC","BRKSN","BSOKE","BTCIM","BUCIM","CIMSA","CMBTN","CMENT","DENCM","DOGUB","EGPRO","EGSER","EPLAS","GOLTS","INTEM","IZOCM","KONYA","KUTPO","MRDIN","NIBAS","NUHCM","PIMAS","SISE","TRKCM","UNYEC","USAK","YBTAS")
pairtest(hisse,TAS)

TEKNOLOJI <- c("XU030","ALCTL","ANELT","ARENA","ARMDA","ASELS","DGATE","DESPC","ESCOM","FONET","INDES","KAREL","KRONT","LINK","LOGO","NETAS","PKART","TCELL","TTKOM")
pairtest(hisse,TEKNOLOJI)

TEKSTIL <- c("XU030","ARSAN","ATEKS","BISAS","BLCYT","BOSSA","BRKO","BRMEN","DAGI","DERIM","DESA","DIRIT","ESEMS","HATEK","KORDS","KRTEK","LUKSK","MAVI","MEMSA","MNDRS","RODRG","SKTAS","SNPAM","VAKKO","YATAS","YUNSA")
pairtest(hisse,TEKSTIL)

TICARET <- c("XU030","ADESE","BIMAS","BIZIM","BMEKS","CRFSA","DOAS","INTEM","KIPA","MAVI","MEPET","MGROS","MIPAZ","PIMAS","PSDTC","SANKO","SELEC","TGSAS","TKNSA","VAKKO","AKPAZ","UZERB")
pairtest(hisse,TICARET)

TURIZM <- c("XU030","AVTUR","AYCES","ETILR","KSTUR","MAALT","MARTI","MERIT","METUR","NTTUR","PKENT","TEKTU","ULAS","UTPYA")
pairtest(hisse,TURIZM)

ULASTIRMA <- c("XU030","BEYAZ","CLEBI","DOCO","GSDDE","PGSUS","RYSAS","THYAO","TAVHL")
pairtest(hisse,ULASTIRMA)