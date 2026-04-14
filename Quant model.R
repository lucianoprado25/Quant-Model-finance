# ==========================================
# Quantitative Finance Models in R
# Author: Luciano Prado
# ==========================================
# Sections:
# 1. Data Processing
# 2. Return Computation
# 3. Factor Models (CAPM, FF)
# 4. Time Series (ARMA/ARIMA)
# 5. GARCH Models
# 6. Risk Metrics (VaR, ES)


install.packages("e1071")
install.packages("tseries")
install.packages("psych")
install.packages("pastecs")
library(e1071)
library(tseries)
library(psych)
library(pastecs)



# 1. Data Processing
#change the working directory 
#settle our path with a function "stock_name" that we will change everytime we need,that will allow us to work on a firm without having to retype all the code
directory_path = ""data/NKE.csv""
stock_name = "DJI1"
file_path = paste(directory_path,stock_name,".csv",sep='')
data_stocks=read.csv(file_path)#import data
head(data_stocks)
#specify the colum with Adj.close data
AC <- data_stocks[, c("Date","Adj.Close")] #AC=data_stocks$Adj.Close
AC=na.omit(AC) #remove NAs as it will affect the calculations
Date = as.Date(AC$Date)
AC = AC$Adj.Close

# 2. Return Computation
#statistics
mean(AC)
exp(mean(log(AC)))
median(AC)
var(AC)
sd(AC)
IQR(AC)
quantile(AC)
library(e1071)
skewness(AC)
kurtosis(AC)

## ------------------------------------------------------------------------
#summary of one column/variable in a dataframe
summary(AC)

## ----fig.cap=c("Price Histogram","Price Histogram with Normal Curve")----
#plot the histogram of the prices (figure-5.1)

# Check the range of valid dates in your data
plot(Date,AC, ylab = "Adjusted Close price", xlab= "DATE",main = paste("Evolution du cours de",stock_name),las=1, type='l')
hist(AC, prob = TRUE, main= paste(stock_name,"HISTOGRAM"), las=1) #default breaks
hist(AC, breaks = 20, prob = TRUE, xaxp= c(0,180,12), yaxp= c(0,0.5,8), main =paste(stock_name,"HISTOGRAM"), las=1 )
curve(dnorm(x, mean = mean(AC), sd = sd(AC)), col = "darkblue", lwd = 2, add = TRUE) 

## ----fig.cap="Quantile-Quantile Plot"------------------------------------
#plot quantiles of DJI (figure-5.3)
qqnorm(AC,main = paste(stock_name,"Normal Q-Q Plot"), las=1)
#plot quantiles from the theoretical normal distribution
qqline(AC,col="red")

data_cs1=read.csv(file_path)
head(data_cs1) #check the imported data

## ------------------------------------------------------------------------
#selecting first 10 price series including the date column
data_cs1.1=data_cs1[,c(1:7)]
#data cleaning-remove NAs
data_cs1.1=na.omit(data_cs1.1)
colnames(data_cs1.1)# see the columns present in the data
summary(data_cs1.1) #notice the Date variable
#check class of dates which will be factor ( treated as factor by default)	
class(data_cs1.1$Date)
#convert dates to class Date
data_cs1.1$Date=as.Date(data_cs1.1$Date,format="%Y-%m-%d")
class(data_cs1.1$Date)
summary(data_cs1.1) #notice the Date variable

## ------------------------------------------------------------------------
#Analyse avec volumes vendus (partie ajoutée)

data_stocks=read.csv(file_path)#import data
head(data_stocks)
#specify the colum with DJI data
AC=data_stocks$Adj.Close
AC=na.omit(AC) #remove NAs as it will affect the calculations

summary(AC)


## ------------------------------------------------------------------------
ptor=function(data,date.pos=1,date.format="%d/%m/%Y",
              log.ret=TRUE, percntg=TRUE)
{  
  n =nrow(data)  # number of observations  
  # creates a negative index in case a date is to be excluded
  ndx =if (date.pos) -date.pos else 1:ncol(data)  
  if(percntg){ multiplicator=100}
  if (log.ret) # in case of log-returns   
  {     
    #log-returns
    ret =log(data[2:n,ndx]/data[1:(n-1),ndx])*multiplicator   
  }
  else 
    #in case of simple returns  
  {    
    #returns  
    ret=(data[2:n,ndx]/data[1:(n-1),ndx]-1)*multiplicator   
  }   
  if (date.pos) # in case some date is in the dataframe   
  {    
    #returns a dataframe with the date in R format in the first position
    return(data.frame(Date=as.Date(data[2:n,date.pos],
                                   format=date.format),ret))   
  }  
  #returns the returns, but no date is defined
  else return(ret) 
} 

## ------------------------------------------------------------------------
data_cs1.1r=ptor(data_cs1.1,date.pos=1,date.format="%Y-%m-%d")
head(data_cs1.1r)
write.csv(data_cs1.1r$Adj.Close,file=paste(stock_name,"taux.csv",sep = ''))
plot(data_cs1.1r$Date,data_cs1.1r$Adj.Close, ylab = "Taux de rentabilité", xlab= "DATE", main=paste("Rentabilité de l'action",stock_name),type='l',las=1, col='lightskyblue')

## ------------------------------------------------------------------------
library(psych)#load the required package
args(describe)#arguments for describe function
#use describe to calculate descriptive stats for data_cs1.1r
desc1=describe(data_cs1.1r[,6:6])#note we dont pass the date column
#check the output
head(desc1)
#the above output is in long format, we can transpose it get column format
desc1.t=t(desc1)
head(desc1.t)

## ------------------------------------------------------------------------
write.csv(desc1.t,file=paste(stock_name,"cree.csv",sep = ''))

## ------------------------------------------------------------------------
#Boxplot graphic
boxplot(AC, las= 1, ylab= "Adjusted close values", main= paste(stock_name,"ADJUSTED CLOSE BOXPLOT"),col="moccasin", outline = FALSE)

## ------------------------------------------------------------------------
require(pastecs)# note library and require can both be used to include a package
#detach the package pastecs its useful to avoid any conflicts (e.g psych and Hmisc have 'describe' function with two different behaviours
detach("package:psych",unload=TRUE)
#use stat.desc in default arguments
desc2=stat.desc(data_cs1.1r[,2:7])
desc2#note no skewness/kurtosis


## ------------------------------------------------------------------------
desc2.sk=stat.desc(data_cs1.1r[,6:6],norm=TRUE)
print(round(desc2.sk,4))#round the values just for display purposes
#save the stats in a csv
write.csv(desc2.sk,"desc2_pastecs.csv")

## ----fig.height=20,fig.width=15,fig.cap="Return Plots",out.height="18cm",out.widht="12cm"----
n=ncol(data_cs1.1r)#calculate number of data columns
s.names=c(colnames(data_cs1.1r)) #get the names of each return series
#generate a series of colors
clrs=seq(1:10) 
#graphical parameter to plot 10 plots on one screen (2 rows X 5 columns)
dev.new()
par(mfrow=c(3,2)) 
#figure-PLOT return of the 10 stocks
for(i in 2:n) {  
  plot(data_cs1.1r$Date,data_cs1.1r[,i],type="l",main=paste(stock_name,s.names[i]),col=clrs[(i-1)],xlab="Date",ylab="Return") 
}

## ----fig.height=10,fig.width=8,fig.cap="Q-Q Plots",out.width="12cm",out.height="18cm",results="hold",fig.height=15----
#graphical parameter to plot 10 plots on one screen (2 rows X 5 columns)
par(mfrow=c(3,2)) 
#figure-QQ plot - normality test
for(i in 2:n) {  
  qqnorm(data_cs1.1r[,i],main=paste(stock_name,s.names[i]))
  qqline(data_cs1.1r[,i], col="red")
}



# 3. Factor Models (CAPM, FF)


library(e1071)
library(tseries)
library(psych)
library(pastecs)
library(quantreg)
library(GRS.test)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)
library(tseries)
library(psych)
library(quantreg)

#change the working directory to the folder containing 6_Portfolios_2x3.csv or provide the full path with the filename 
PORTEFEUILLE=read.csv("/Users/lucianoprado/Desktop/CODE R /pat_R/Portfolios_2x3.CSV",sep=",",skip=15,header=TRUE) #import data
PORTEFEUILLE=PORTEFEUILLE[-(1159:8601),]
colnames(PORTEFEUILLE)[1]="Date"

#convert dates to R date class
PORTEFEUILLE$Date <- as.Date(paste(PORTEFEUILLE$Date, "01", sep = ""), format = "%Y%m%d")

#convertir les portefeuilles en format numerique
PORTEFEUILLE$SMALL.LoBM=as.numeric(PORTEFEUILLE$SMALL.LoBM)
PORTEFEUILLE$ME1.BM2=as.numeric(PORTEFEUILLE$ME1.BM2)
PORTEFEUILLE$SMALL.HiBM=as.numeric(PORTEFEUILLE$SMALL.HiBM)
PORTEFEUILLE$BIG.LoBM=as.numeric(PORTEFEUILLE$BIG.LoBM)
PORTEFEUILLE$ME2.BM2=as.numeric(PORTEFEUILLE$ME2.BM2)
PORTEFEUILLE$BIG.HiBM=as.numeric(PORTEFEUILLE$BIG.HiBM)


FFFACTORS=read.csv("/Users/lucianoprado/Desktop/CODE R /pat_R/F-F_Research_Data_5_Factors_2x3.csv",skip=3,header=TRUE,row.names=NULL) #import data
colnames(FFFACTORS)[1]="Date"

#convert dates to R date class
FFFACTORS$Date <- as.Date(paste(FFFACTORS$Date, "01", sep = ""), format = "%Y%m%d")

#first check if the dates in PORTEFEUILLE and FFFACTORS are same using setdiff
setdiff(PORTEFEUILLE$Date,FFFACTORS$Date)

#If there is differencies in dates beetwen the two datasets
merged_data <- merge(PORTEFEUILLE, FFFACTORS, by = "Date", all.x = TRUE)
merged_data = na.omit(merged_data)
head(merged_data)

#With no difference in dates we combine the two datasets by columns. 
#DATA_PFFF=cbind(PORTEFEUILLE, FFACTORS)
#DATA_PFFF= DATA_PFFF [,-9]
#head(DATA_PFFF)

#save the data for further analysis in an excel file (export the new database)
write.csv(merged_data,file="DATA1.csv",row.names=FALSE)

DATA1=read.csv("DATA1.csv") #import the data

#create conditional subset of observations in Fama French factor data
DATA2=DATA1[DATA1$Date>=as.Date("2017-01-01")&DATA1$Date<=as.Date("2022-12-01"),]
#check the subset
head(DATA2)
tail(DATA2)
#remove NAs
DATA2=na.omit(DATA2)

## ------------------------------------------------------------------------
lreg1=lm(formula= SMALL.LoBM ~Mkt.RF,data=DATA2) #comparaison entre le rendement du portefeuille et le rendement du marché  
lreg1
summary(lreg1)

## ----fig.cap="Linear Regression Diagnostic Plots"------------------------
#first plot SMALL LoBM and Mkt.RF returns
plot(DATA2$SMALL.LoBM,DATA2$Mkt.RF)
plot(DATA2$SMALL.LoBM,DATA2$Mkt.RF, xlab=" SMALL.LoBM return", ylab= "Mkt.RF return")
#add the regression line 
abline(lreg1,col="blue")

# we set the graphical parameter as the plot  function for lm object creates 4 plots
par(mfrow = c(2, 2))
plot(lreg1)


## MODELES DE REGRESSION ----------------------------------------------------------------

#create another column with SMALL.LoBM-RF (difference entre la rentabilite de SMALL.LoBM et le taux sans risque)
DATA2$SMALL.LoBM.RF=DATA2$SMALL.LoBM-DATA2$RF

##CAPM model
CAPM_lreg=lm(SMALL.LoBM.RF~Mkt.RF,data=DATA2)
summary(CAPM_lreg)
## ----fig.cap="CAPM residuals check"-----------------------------------
par(mfrow=c(2,2))
plot(CAPM_lreg)

library(stargazer)
stargazer(CAPM_lreg,summary=TRUE,title="OLS Results",type="text",no.space=TRUE)


##3FM model
FF3FM_lreg=lm(SMALL.LoBM.RF~Mkt.RF+SMB+HML,data=DATA2)
summary(FF3FM_lreg)

## ----fig.cap=" 3FM residuals check"-----------------------------------
par(mfrow=c(2,2))
plot(FF3FM_lreg)

## ------------------------------------------------------------------------
library(stargazer)
stargazer(FF3FM_lreg,summary=TRUE,title="OLS Results",type="text",no.space=TRUE)

##5FM model
FF5FM=lm(SMALL.LoBM.RF~Mkt.RF+SMB+HML+CMA+RMW,data=DATA2)
summary(FF5FM)

## ----fig.cap=" 5FM residuals check"-----------------------------------
par(mfrow=c(2,2))
plot(FF5FM)

## ------------------------------------------------------------------------
library(stargazer)
stargazer(FF5FM,summary=TRUE,title="OLS Results",type="text",no.space=TRUE)

anova(CAPM_lreg,FF3FM_lreg,FF5FM) #comparaison entre les modèles



#generate Q-Q Plots 
par(mfrow=c(2,2))
#SMALL.LoBM.RF
qqnorm(DATA2$SMALL.LoBM.RF,main=" SMALL.LoBM ")
qqline(DATA2$ SMALL.LoBM.RF,col=2)
#Mkt.RF.RF
qqnorm(DATA2$Mkt.RF,main="Mkt.RF.RF")
qqline(DATA2$Mkt.RF,col=2)
#SMB
qqnorm(DATA2$SMB,main="SMB")
qqline(DATA2$SMB,col=2)
#HML
qqnorm(DATA2$HML,main="HML")
qqline(DATA2$HML,col=2)
#RMW
qqnorm(DATA2$RMW,main="RMW")
qqline(DATA2$RMW,col=2)
#CMA
qqnorm(DATA2$CMA,main="CMA")
qqline(DATA2$CMA,col=2)


## ------------------------------------------------------------------------
#load the package
library(quantreg)
#create a vector of quantiles
tau=c(0.05,0.25,0.5,0.75,0.95)
ff_qreg=rq(SMALL.LoBM.RF~Mkt.RF+SMB+HML,tau=tau,data=DATA2)
ff_qreg

## ----resume des regressions quantile par quantile-------------------------------------------------------
sum.ff_qreg=summary(ff_qreg)
sum.ff_qreg

## ----fig.cap="Quantile Estimate Plots"-----------------------------------
plot(sum.ff_qreg)



# 4. Time Series (ARMA/ARIMA)



#load packages 
library(e1071)
library(tseries)
library(psych)
library(pastecs)
library(quantreg)
library(GRS.test)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)
library(tseries)
library(psych)
library(xts)
library(fArma)
library(forecast)

#Don't forget to define the working directory

#Read the .csv file with the data
NKE <- read.csv("/Users/lucianoprado/Desktop/CODE R /pat_R/NKE.csv")

#Define the date 
NKE$Date= as.Date(NKE$Date)

#Delete unwanted columns  
NKE <- subset(NKE, select = -c(Open))
NKE <- subset(NKE, select = -c(High))
NKE <- subset(NKE, select = -c(Low))
NKE <- subset(NKE, select = -c(Close))
NKE <- subset(NKE, select = -c(Volume))


#Create d1 as a times series  
d1=xts(NKE$Adj.Close,order.by=NKE$Date)


#figure-9.1 : NKE Adjusted Closing Prices
plot(d1, ylab = "y", xlab = "x")


#Calculate the returns of a stock 
ptor=function(data,date.pos=1,date.format="%d/%m/%Y",
              log.ret=TRUE, percntg=TRUE)
{  
  n =nrow(data)  # number of observations  
  # creates a negative index in case a date is to be excluded
  ndx =if (date.pos) -date.pos else 1:ncol(data)  
  if(percntg){ multiplicator=100}
  if (log.ret) # in case of log-returns   
  {     
    #log-returns
    ret =log(data[2:n,ndx]/data[1:(n-1),ndx])*multiplicator   
  }
  else 
    #in case of simple returns  
  {    
    #returns  
    ret=(data[2:n,ndx]/data[1:(n-1),ndx]-1)*multiplicator   
  }   
  if (date.pos) # in case some date is in the dataframe   
  {    
    #returns a dataframe with the date in R format in the first position
    return(data.frame(Date=as.Date(data[2:n,date.pos],
                                   format=date.format),ret))   
  }  
  #returns the returns, but no date is defined
  else return(ret) 
} 

## ------------------------------------------------------------------------
#NKE's stock returns
NKE_return=ptor(NKE,date.pos=1,date.format="%Y-%m-%d")

head(NKE_return)



#Creation of a time series with NKE's stock Returns
d2=xts(NKE_return$ret,order.by=NKE_return$Date)

## ----fig
plot(d2, main="NKE Log Returns")


## ------------------------------------------------------------------------
#ARMA with d2 -> returns 
#First check the stationarity of the series
#ADF test for NKE return
adf.test(d2)

#KPSS test for NKE return
kpss.test(d2)



## ------------------------------------------------------------------------
#Autocorrelation test
#Ljung-Box test on return series for 20 lags
Box.test(d2,lag=20,type="Ljung")



## -----------------------------------------------------------------------
# finding p,q lags
#finding order q (MA part)
acf(d2, main="ACF to find q")
#finding order p (AR part)
pacf(d2, main="PACF to find p")


## ------------------------------------------------------------------------
#to find the best fit model with a loop, based on AIC criterion
#set the best order to default
best_order <- c(0, 0)
#set best AIC to Inf to accept any value
best_aic <- Inf
# check till p=5 and q=5
for (i in 0:5) { 
  for (j in 0:5) {
    #fit the model
    fit=arma(d2,order=c(i,j)) 
    #extract AIC from the summary method
    fit_aic = summary(fit)$aic 
    #compare the AIC with previous AIC
    if (fit_aic<best_aic) { 
      best_aic = fit_aic 
      best_order=c(i,j) } 
  }
} 
print(c(best_order,best_aic))

## ------------------------------------------------------------------------
#install.packages
library(fArma)
fit1=armaFit(~arma(4,4),data=d2,description="ARMA(4,4)")

par(mfrow=c(2,2))
#checking the model's residuals. If the model is perfect, residulas should be normal IID
summary(fit1)

#6 Step ahead (6 month) forecast
p1=predict(fit1,n.ahead=6)
#extract predicted values
p1$pred



## ------------------------------------------------------------------------
#ARIMA with d1 -> adjusted closing prices
#First check the stationarity of the series
#ADF test for NKE return
adf.test(d1)

#KPSS test for NKE return
kpss.test(d1)


#no stationarity -> diff()
d3 <- na.omit(diff(d1))

#Check the stationarity again with ADF et KPSS
adf.test(d3)
kpss.test(d3)


#Still not stationary -> diff()
d4 <- na.omit(diff(d3))

#Check the stationarity again with ADF et KPSS
adf.test(d4)
kpss.test(d4)
#=>stationnaire

plot(d4, main="After diff")
plot(d1, main="Before diff")

#finding order q (MA part)
acf(d4, main="ACF to find q")
#finding order p (AR part)
pacf(d4, main="PACF to find p")



## ------------------------------------------------------------------------
#to find the best fit model with a loop, based on AIC criterion
#set the best order to default
best2_order <- c(0, 0)
#set best AIC to Inf to accept any value
best2_aic <- Inf
#check till p=5 and q=5
for (i in 0:5) { 
  for (j in 0:4) {
    #fit the model
    fit2=arma(d3,order=c(i,j)) 
    #extract AIC from the summary method
    fit2_aic = summary(fit2)$aic 
    #compare the AIC with previous AIC
    if (fit2_aic<best2_aic) { 
      best2_aic = fit2_aic 
      best2_order=c(i,j) } 
  }
} 
#fit2=arma(d3,order=c(0,4))
#fit2_aic = summary(fit2)$aic
#if (fit2_aic<best2_aic) { 
#best2_aic = fit2_aic 
#best2_order=c(i,j) }  }


print(c(best2_order,best2_aic))



#----------

Close_d1 <- d1[1:110]
test_d1 <- d1[111:121]

arimaModel_1=arima(Close_d1,order=c(7,0,0))
arimaModel_2=arima(Close_d1,order=c(6,0,0))
arimaModel_3=arima(Close_d1,order=c(8,0,0))


print(arimaModel_1)
print(arimaModel_2)
print(arimaModel_3)


forecast1 <- forecast(arimaModel_1,10)
plot(forecast1)

forecast2 <- forecast(arimaModel_2,10)
plot(forecast2)

forecast3 <- forecast(arimaModel_3,10)
plot(forecast3)

#We compare the forecast wiithcheckr the real dat&
plot(test_d1)

#------------------------- 
##checking the model's residuals. If the model is perfect, residulas should be normal IID
checkresiduals(arimaModel_1)

#6 Step ahead (6 month) forecast

arimaModel=arima(d1,order=c(4,2,2))

forecast <- forecast(arimaModel,10)
plot(forecast)
install.packages("e1071")
install.packages("tseries")
install.packages("psych")
install.packages("pastecs")
install.packages("quantreg")
install.packages("GRS.test")
install.packages("quantmod")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("broom")
install.packages("xts")
install.packages("forecast")
install.packages("rugarch")

##############

library(e1071)
library(tseries)
library(psych)
library(pastecs)
library(quantreg)
library(GRS.test)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)
library(tseries)
library(psych)
library(xts)
library(forecast)
library(rugarch)

#############

nke_cours=read.csv("/Users/lucianoprado/Desktop/CODE R /pat_R/nke.csv")
nke_cours=nke_cours[,-c(2:5,7)]
nke_cours$Date=as.Date(nke_cours$Date)
nke_ts=xts(nke_cours$Adj.Close,order.by = nke_cours$Date)
plot(nke_ts, main = "nke Closing Prices", ylab = "Cours", xlab = "Années", las=1)

#############

ptor=function(data,date.pos=1,date.format="%Y-%m-%d",
              log.ret=TRUE,percntg=TRUE)
{  
  n =nrow(data)
  ndx =if (date.pos) -date.pos else 1:ncol(data)  
  if(percntg){ multiplicator=100}
  if (log.ret)
  {     
    ret =log(data[2:n,ndx]/data[1:(n-1),ndx])*multiplicator   
  }
  else 
  {    
    ret=(data[2:n,ndx]/data[1:(n-1),ndx]-1)*multiplicator   
  }   
  if (date.pos)
  {    
    return(data.frame(Date=as.Date(data[2:n,date.pos],
                                   format=date.format),ret))   
  }  
  else return(ret) 

}  

###############

# 5. GARCH Models


nke_ret = ptor(nke_cours, date.pos = 1, date.format = "%Y-%m-%d")
write.csv(nke_ret, file = 'Taux de rentabilité logarithmique - nke')
plot(nke_ret,  type ="l", ylab = "taux de rentabilté", xlab = "Date", main = "Rentabilité - nke", las=1,)

################

args(ugarchspec)
garch_spec=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0)))

fit_garch=ugarchfit(spec=garch_spec,data=nke_ret)
fit_garch

################

plot(fit_garch,which=10, las=1)
plot(fit_garch,which=3, las=1)
plot(fit_garch,which=8, las=1)
plot(fit_garch,which=9, las=1)

fore1=ugarchforecast(fit_garch,n.ahead=10)
show(fore1)

fit_garch2=ugarchfit(spec=garch_spec,data=nke_ret,out.sample=10)
fore2=ugarchforecast(fit_garch2,n.ahead=1,n.roll=9)
show(fitted(fore2))
show(sigma(fore2))

#2:Time Series Prediction (rolling)
plot(fore2,which=2, las=1)
#4:Sigma Prediction (rolling)
plot(fore2,which=4, las=1)


# 6. Risk Metrics (VaR, ES)



install.packages("devtools")
devtools::install_github("mdancho84/tidyquant")
install.packages("tidyquant")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
remove.packages("ggplot2") # Unisntall ggplot
install.packages("ggplot2") # Install it again
install.packages("quantmod")

############

library(tidyquant)
library(timetk)
library(ggplot2)
library(magrittr)
library(dplyr)

############

NKE <- tq_get("NKE",                    
              from = '2018-01-01',
              to = "2023-01-01",
              get = "stock.prices")

NKE %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  ggtitle("NKE from 2009 to 2021") +
  labs(x = "Date", "Price") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(x = "Date", y = "Adjusted Price") +
  theme_bw()

# Calculate daily returns

NKE_daily_returns <- NKE %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "nke_returns")

NKE_daily_returns %>%
  ggplot(aes(x = date, y = tte_returns)) +
  geom_line() +
  theme_classic() +
  labs(x = "Date", y = "Daily returns") +
  ggtitle("Daily Returns for NKE") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) 

#To get a sense of how extreme the returns can be we can plot a histogram

NKE_daily_returns %>%
  ggplot(aes(x = tte_returns)) +
  geom_histogram(binwidth = 0.015) +
  theme_classic() +
  labs(x = "Daily returns") +
  ggtitle("Daily Returns for NKE") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) +
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")

#calculating the VaR and ES

daily_return=NKE_daily_returns$tte_returns
VaR(daily_return,p=0.99, method= "historical")
ES(daily_return,p=0.99, method= "historical")




