# Retrospective analysis summary for  my.syndromic


```r
#loading and preparing the data to work with
load("my.syndromic.RData")
require(vetsyn)
matrix.days <- x@observed
matrix.week.full <- convert_days_to_week(x@observed,x@dates)
matrix.week <- matrix.week.full[,-(1:2),drop=FALSE]
frequency=365
```

## Cattle

```r
#create time series
s=1
days    <-  matrix.days[,s]
days.ts <-  ts(days, start = c(x@dates$year[1], x@dates$yday[1]),
               frequency = frequency)
t = 1:length(days)

week    <-  matrix.week[,s]
week.ts <-  ts(week, start = c(matrix.week.full[1,2],as.numeric(substr(as.character(matrix.week.full[1,1]),7,8))),
               frequency = 52)
t.week <- 1:length(week)
```


```r
#Plot series
plot(days.ts , xlab="Days", main="Daily")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
plot(week.ts , xlab="Weeks",main="Weeks")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png)

### Summary statistics

```r
#Percentiles
quantile(days,probs=c(0,0.25,0.5,0.75,1),names=FALSE)
```

```
## [1]  0  1  3  7 42
```

```r
round(mean(days),4)
```

```
## [1] 5.0815
```

```r
#Number of days at minimum value
(countInMin <- length(which(days == min(days))))
```

```
## [1] 231
```

```r
(percentInMin <- round(((countInMin)/(length(days)))*100,2))
```

```
## [1] 15.82
```


```r
#ACF and PACF
acf(days,main="ACF for daily data")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
pacf(days,main="PACF for daily data")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

### Crude (visual) assessment of temporal effects

```r
boxplot(days ~ x@dates$dow, main="Day of the Week")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
boxplot(days ~ x@dates$month, main = "Month")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png)

```r
boxplot(days ~ x@dates$year, main = "Year")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-3.png)

### POISSON  Regression with DOW, month or sin/cos wave

```r
distribution="poisson"

month.f = as.factor (x@dates$month)
dow.f <- as.factor (x@dates$dow)
cos = cos (2*pi*t/frequency)
sin = sin (2*pi*t/frequency)
tminus1<-c(days[1],days[1:(length(days)-1)])
tminus2<-c(days[1:2],days[1:(length(days)-2)])
tminus3<-c(days[1:3],days[1:(length(days)-3)])
tminus4<-c(days[1:4],days[1:(length(days)-4)])
tminus5<-c(days[1:5],days[1:(length(days)-5)])
tminus6<-c(days[1:6],days[1:(length(days)-6)])
tminus7<-c(days[1:7],days[1:(length(days)-7)])

fit1 = glm(days~ dow.f, family=distribution)
fit1AR1 = glm(days~ dow.f + tminus1, family=distribution)
fit1AR2 = glm(days~ dow.f + tminus1+ tminus2, family=distribution)
fit1AR3 = glm(days~ dow.f + tminus1+ tminus2 + tminus3, family=distribution)
fit1AR4 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit1AR5 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit1AR6 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit1AR7 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit2 = glm(days~ dow.f+ month.f, family=distribution)
fit2AR1 = glm(days~ dow.f+ month.f + tminus1, family=distribution)
fit2AR2 = glm(days~ dow.f+ month.f + tminus1+ tminus2, family=distribution)
fit2AR3 = glm(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3, family=distribution)
fit2AR4 = glm(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit2AR5 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit2AR6 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit2AR7 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit3 = glm(days~ dow.f+ cos + sin, family=distribution)
fit3AR1 = glm(days~ dow.f+ cos + sin + tminus1, family=distribution)
fit3AR2 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2, family=distribution)
fit3AR3 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3, family=distribution)
fit3AR4 = glm(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit3AR5 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit3AR6 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit3AR7 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)
```



```r
#Printing AICs
AR1 <- c(fit1=fit1$aic,fit1AR1=fit1AR1$aic,fit1AR2=fit1AR2$aic,fit1AR3=fit1AR3$aic,
                          fit1AR4=fit1AR4$aic,fit1AR5=fit1AR5$aic,fit1AR6=fit1AR6$aic,fit1AR7=fit1AR7$aic)
AR2 <- c(fit2=fit2$aic,fit2AR1=fit2AR1$aic,fit2AR2=fit2AR2$aic,fit2AR3=fit2AR3$aic,
                          fit2AR4=fit2AR4$aic,fit2AR5=fit2AR5$aic,fit2AR6=fit2AR6$aic,fit2AR7=fit2AR7$aic)
AR3 <- c(fit3=fit3$aic,fit3AR1=fit3AR1$aic,fit3AR2=fit3AR2$aic,fit3AR3=fit3AR3$aic,
                          fit3AR4=fit3AR4$aic,fit3AR5=fit3AR5$aic,fit3AR6=fit3AR6$aic,fit3AR7=fit3AR7$aic)


print(AR1)
```

```
##     fit1  fit1AR1  fit1AR2  fit1AR3  fit1AR4  fit1AR5  fit1AR6  fit1AR7 
## 11320.02 11031.93 10937.39 10771.22 10670.93 10643.27 10474.98 10076.07
```

```r
print(AR2)
```

```
##      fit2   fit2AR1   fit2AR2   fit2AR3   fit2AR4   fit2AR5   fit2AR6   fit2AR7 
## 10677.927 10571.687 10550.648 10473.384 10428.468 10422.736 10300.719  9948.802
```

```r
print(AR3)
```

```
##      fit3   fit3AR1   fit3AR2   fit3AR3   fit3AR4   fit3AR5   fit3AR6   fit3AR7 
## 10713.309 10595.676 10570.996 10486.401 10436.669 10429.683 10309.455  9961.839
```


```r
plot(t,days, type="l",main="Poisson regression")
lines(fit1$fit, col="red"   , lwd=2)
lines(fit2$fit, col="blue"  , lwd=2)
lines(fit3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                  c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
### Negative Binomial Regression with DOW, month or sin/cos wave
```

```r
require(MASS)
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
fitNB1 = glm.nb(days~ dow.f)
fitNB1AR1 = glm.nb(days~ dow.f + tminus1)
fitNB1AR2 = glm.nb(days~ dow.f + tminus1+ tminus2)
fitNB1AR3 = glm.nb(days~ dow.f + tminus1+ tminus2 + tminus3)
fitNB1AR4 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB1AR5 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB1AR6 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB1AR7 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB2 = glm.nb(days~ dow.f+ month.f)
fitNB2AR1 = glm.nb(days~ dow.f+ month.f + tminus1)
fitNB2AR2 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2)
fitNB2AR3 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3)
fitNB2AR4 = glm.nb(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB2AR5 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB2AR6 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB2AR7 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB3 = glm.nb(days~ dow.f+ cos + sin)
fitNB3AR1 = glm.nb(days~ dow.f+ cos + sin + tminus1)
fitNB3AR2 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2)
fitNB3AR3 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3)
fitNB3AR4 = glm.nb(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB3AR5 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB3AR6 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB3AR7 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)
```



```r
#Printing AICs
AR_NB1 <- c(fitNB1=fitNB1$aic,fitNB1AR1=fitNB1AR1$aic,fitNB1AR2=fitNB1AR2$aic,fitNB1AR3=fitNB1AR3$aic,
                          fitNB1AR4=fitNB1AR4$aic,fitNB1AR5=fitNB1AR5$aic,fitNB1AR6=fitNB1AR6$aic,fitNB1AR7=fitNB1AR7$aic)
AR_NB2 <- c(fitNB2=fitNB2$aic,fitNB2AR1=fitNB2AR1$aic,fitNB2AR2=fitNB2AR2$aic,fitNB2AR3=fitNB2AR3$aic,
                          fitNB2AR4=fitNB2AR4$aic,fitNB2AR5=fitNB2AR5$aic,fitNB2AR6=fitNB2AR6$aic,fitNB2AR7=fitNB2AR7$aic)
AR_NB3 <- c(fitNB3=fitNB3$aic,fitNB3AR1=fitNB3AR1$aic,fitNB3AR2=fitNB3AR2$aic,fitNB3AR3=fitNB3AR3$aic,
                          fitNB3AR4=fitNB3AR4$aic,fitNB3AR5=fitNB3AR5$aic,fitNB3AR6=fitNB3AR6$aic,fitNB3AR7=fitNB3AR7$aic)
print(AR_NB1)
```

```
##    fitNB1 fitNB1AR1 fitNB1AR2 fitNB1AR3 fitNB1AR4 fitNB1AR5 fitNB1AR6 fitNB1AR7 
##  7882.081  7831.151  7812.160  7782.583  7768.701  7765.351  7731.473  7642.834
```

```r
print(AR_NB2)
```

```
##    fitNB2 fitNB2AR1 fitNB2AR2 fitNB2AR3 fitNB2AR4 fitNB2AR5 fitNB2AR6 fitNB2AR7 
##  7773.364  7759.367  7755.545  7742.727  7738.949  7740.321  7714.477  7637.937
```

```r
print(AR_NB3)
```

```
##    fitNB3 fitNB3AR1 fitNB3AR2 fitNB3AR3 fitNB3AR4 fitNB3AR5 fitNB3AR6 fitNB3AR7 
##  7769.198  7752.750  7748.104  7732.507  7728.242  7729.317  7704.399  7626.556
```


```r
plot(t,days, type="l",main="Negative Binomial Regression")
lines(fitNB1$fit, col="red"   , lwd=2)
lines(fitNB2$fit, col="blue"  , lwd=2)
lines(fitNB3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


## Chicken

```r
#create time series
s=2
days    <-  matrix.days[,s]
days.ts <-  ts(days, start = c(x@dates$year[1], x@dates$yday[1]),
               frequency = frequency)
t = 1:length(days)

week    <-  matrix.week[,s]
week.ts <-  ts(week, start = c(matrix.week.full[1,2],as.numeric(substr(as.character(matrix.week.full[1,1]),7,8))),
               frequency = 52)
t.week <- 1:length(week)
```


```r
#Plot series
plot(days.ts , xlab="Days", main="Daily")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

```r
plot(week.ts , xlab="Weeks",main="Weeks")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-2.png)

### Summary statistics

```r
#Percentiles
quantile(days,probs=c(0,0.25,0.5,0.75,1),names=FALSE)
```

```
## [1] 0 0 0 1 9
```

```r
round(mean(days),4)
```

```
## [1] 0.7055
```

```r
#Number of days at minimum value
(countInMin <- length(which(days == min(days))))
```

```
## [1] 883
```

```r
(percentInMin <- round(((countInMin)/(length(days)))*100,2))
```

```
## [1] 60.48
```


```r
#ACF and PACF
acf(days,main="ACF for daily data")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

```r
pacf(days,main="PACF for daily data")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-2.png)

### Crude (visual) assessment of temporal effects

```r
boxplot(days ~ x@dates$dow, main="Day of the Week")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

```r
boxplot(days ~ x@dates$month, main = "Month")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-2.png)

```r
boxplot(days ~ x@dates$year, main = "Year")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-3.png)

### POISSON  Regression with DOW, month or sin/cos wave

```r
distribution="poisson"

month.f = as.factor (x@dates$month)
dow.f <- as.factor (x@dates$dow)
cos = cos (2*pi*t/frequency)
sin = sin (2*pi*t/frequency)
tminus1<-c(days[1],days[1:(length(days)-1)])
tminus2<-c(days[1:2],days[1:(length(days)-2)])
tminus3<-c(days[1:3],days[1:(length(days)-3)])
tminus4<-c(days[1:4],days[1:(length(days)-4)])
tminus5<-c(days[1:5],days[1:(length(days)-5)])
tminus6<-c(days[1:6],days[1:(length(days)-6)])
tminus7<-c(days[1:7],days[1:(length(days)-7)])

fit1 = glm(days~ dow.f, family=distribution)
fit1AR1 = glm(days~ dow.f + tminus1, family=distribution)
fit1AR2 = glm(days~ dow.f + tminus1+ tminus2, family=distribution)
fit1AR3 = glm(days~ dow.f + tminus1+ tminus2 + tminus3, family=distribution)
fit1AR4 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit1AR5 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit1AR6 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit1AR7 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit2 = glm(days~ dow.f+ month.f, family=distribution)
fit2AR1 = glm(days~ dow.f+ month.f + tminus1, family=distribution)
fit2AR2 = glm(days~ dow.f+ month.f + tminus1+ tminus2, family=distribution)
fit2AR3 = glm(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3, family=distribution)
fit2AR4 = glm(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit2AR5 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit2AR6 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit2AR7 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit3 = glm(days~ dow.f+ cos + sin, family=distribution)
fit3AR1 = glm(days~ dow.f+ cos + sin + tminus1, family=distribution)
fit3AR2 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2, family=distribution)
fit3AR3 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3, family=distribution)
fit3AR4 = glm(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit3AR5 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit3AR6 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit3AR7 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)
```



```r
#Printing AICs
AR1 <- c(fit1=fit1$aic,fit1AR1=fit1AR1$aic,fit1AR2=fit1AR2$aic,fit1AR3=fit1AR3$aic,
                          fit1AR4=fit1AR4$aic,fit1AR5=fit1AR5$aic,fit1AR6=fit1AR6$aic,fit1AR7=fit1AR7$aic)
AR2 <- c(fit2=fit2$aic,fit2AR1=fit2AR1$aic,fit2AR2=fit2AR2$aic,fit2AR3=fit2AR3$aic,
                          fit2AR4=fit2AR4$aic,fit2AR5=fit2AR5$aic,fit2AR6=fit2AR6$aic,fit2AR7=fit2AR7$aic)
AR3 <- c(fit3=fit3$aic,fit3AR1=fit3AR1$aic,fit3AR2=fit3AR2$aic,fit3AR3=fit3AR3$aic,
                          fit3AR4=fit3AR4$aic,fit3AR5=fit3AR5$aic,fit3AR6=fit3AR6$aic,fit3AR7=fit3AR7$aic)


print(AR1)
```

```
##     fit1  fit1AR1  fit1AR2  fit1AR3  fit1AR4  fit1AR5  fit1AR6  fit1AR7 
## 3618.540 3593.905 3588.496 3589.602 3591.593 3591.474 3592.038 3559.945
```

```r
print(AR2)
```

```
##     fit2  fit2AR1  fit2AR2  fit2AR3  fit2AR4  fit2AR5  fit2AR6  fit2AR7 
## 3598.829 3581.735 3579.706 3581.586 3582.961 3584.232 3585.637 3557.410
```

```r
print(AR3)
```

```
##     fit3  fit3AR1  fit3AR2  fit3AR3  fit3AR4  fit3AR5  fit3AR6  fit3AR7 
## 3621.707 3597.113 3591.779 3592.926 3594.913 3594.816 3595.395 3563.334
```


```r
plot(t,days, type="l",main="Poisson regression")
lines(fit1$fit, col="red"   , lwd=2)
lines(fit2$fit, col="blue"  , lwd=2)
lines(fit3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                  c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)

```r
### Negative Binomial Regression with DOW, month or sin/cos wave
```

```r
require(MASS)
fitNB1 = glm.nb(days~ dow.f)
fitNB1AR1 = glm.nb(days~ dow.f + tminus1)
fitNB1AR2 = glm.nb(days~ dow.f + tminus1+ tminus2)
fitNB1AR3 = glm.nb(days~ dow.f + tminus1+ tminus2 + tminus3)
fitNB1AR4 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB1AR5 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB1AR6 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB1AR7 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB2 = glm.nb(days~ dow.f+ month.f)
fitNB2AR1 = glm.nb(days~ dow.f+ month.f + tminus1)
fitNB2AR2 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2)
fitNB2AR3 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3)
fitNB2AR4 = glm.nb(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB2AR5 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB2AR6 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB2AR7 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB3 = glm.nb(days~ dow.f+ cos + sin)
fitNB3AR1 = glm.nb(days~ dow.f+ cos + sin + tminus1)
fitNB3AR2 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2)
fitNB3AR3 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3)
fitNB3AR4 = glm.nb(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB3AR5 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB3AR6 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB3AR7 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)
```



```r
#Printing AICs
AR_NB1 <- c(fitNB1=fitNB1$aic,fitNB1AR1=fitNB1AR1$aic,fitNB1AR2=fitNB1AR2$aic,fitNB1AR3=fitNB1AR3$aic,
                          fitNB1AR4=fitNB1AR4$aic,fitNB1AR5=fitNB1AR5$aic,fitNB1AR6=fitNB1AR6$aic,fitNB1AR7=fitNB1AR7$aic)
AR_NB2 <- c(fitNB2=fitNB2$aic,fitNB2AR1=fitNB2AR1$aic,fitNB2AR2=fitNB2AR2$aic,fitNB2AR3=fitNB2AR3$aic,
                          fitNB2AR4=fitNB2AR4$aic,fitNB2AR5=fitNB2AR5$aic,fitNB2AR6=fitNB2AR6$aic,fitNB2AR7=fitNB2AR7$aic)
AR_NB3 <- c(fitNB3=fitNB3$aic,fitNB3AR1=fitNB3AR1$aic,fitNB3AR2=fitNB3AR2$aic,fitNB3AR3=fitNB3AR3$aic,
                          fitNB3AR4=fitNB3AR4$aic,fitNB3AR5=fitNB3AR5$aic,fitNB3AR6=fitNB3AR6$aic,fitNB3AR7=fitNB3AR7$aic)
print(AR_NB1)
```

```
##    fitNB1 fitNB1AR1 fitNB1AR2 fitNB1AR3 fitNB1AR4 fitNB1AR5 fitNB1AR6 fitNB1AR7 
##  3354.848  3342.538  3339.719  3341.248  3343.069  3344.285  3345.269  3330.264
```

```r
print(AR_NB2)
```

```
##    fitNB2 fitNB2AR1 fitNB2AR2 fitNB2AR3 fitNB2AR4 fitNB2AR5 fitNB2AR6 fitNB2AR7 
##  3354.169  3346.993  3345.838  3347.764  3348.942  3350.592  3352.254  3339.809
```

```r
print(AR_NB3)
```

```
##    fitNB3 fitNB3AR1 fitNB3AR2 fitNB3AR3 fitNB3AR4 fitNB3AR5 fitNB3AR6 fitNB3AR7 
##  3358.083  3345.717  3342.994  3344.538  3346.352  3347.672  3348.723  3333.464
```


```r
plot(t,days, type="l",main="Negative Binomial Regression")
lines(fitNB1$fit, col="red"   , lwd=2)
lines(fitNB2$fit, col="blue"  , lwd=2)
lines(fitNB3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)


## Swine

```r
#create time series
s=3
days    <-  matrix.days[,s]
days.ts <-  ts(days, start = c(x@dates$year[1], x@dates$yday[1]),
               frequency = frequency)
t = 1:length(days)

week    <-  matrix.week[,s]
week.ts <-  ts(week, start = c(matrix.week.full[1,2],as.numeric(substr(as.character(matrix.week.full[1,1]),7,8))),
               frequency = 52)
t.week <- 1:length(week)
```


```r
#Plot series
plot(days.ts , xlab="Days", main="Daily")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

```r
plot(week.ts , xlab="Weeks",main="Weeks")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-2.png)

### Summary statistics

```r
#Percentiles
quantile(days,probs=c(0,0.25,0.5,0.75,1),names=FALSE)
```

```
## [1]  0  0  0  1 29
```

```r
round(mean(days),4)
```

```
## [1] 0.7575
```

```r
#Number of days at minimum value
(countInMin <- length(which(days == min(days))))
```

```
## [1] 1042
```

```r
(percentInMin <- round(((countInMin)/(length(days)))*100,2))
```

```
## [1] 71.37
```


```r
#ACF and PACF
acf(days,main="ACF for daily data")
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

```r
pacf(days,main="PACF for daily data")
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-2.png)

### Crude (visual) assessment of temporal effects

```r
boxplot(days ~ x@dates$dow, main="Day of the Week")
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

```r
boxplot(days ~ x@dates$month, main = "Month")
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-2.png)

```r
boxplot(days ~ x@dates$year, main = "Year")
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-3.png)

### POISSON  Regression with DOW, month or sin/cos wave

```r
distribution="poisson"

month.f = as.factor (x@dates$month)
dow.f <- as.factor (x@dates$dow)
cos = cos (2*pi*t/frequency)
sin = sin (2*pi*t/frequency)
tminus1<-c(days[1],days[1:(length(days)-1)])
tminus2<-c(days[1:2],days[1:(length(days)-2)])
tminus3<-c(days[1:3],days[1:(length(days)-3)])
tminus4<-c(days[1:4],days[1:(length(days)-4)])
tminus5<-c(days[1:5],days[1:(length(days)-5)])
tminus6<-c(days[1:6],days[1:(length(days)-6)])
tminus7<-c(days[1:7],days[1:(length(days)-7)])

fit1 = glm(days~ dow.f, family=distribution)
fit1AR1 = glm(days~ dow.f + tminus1, family=distribution)
fit1AR2 = glm(days~ dow.f + tminus1+ tminus2, family=distribution)
fit1AR3 = glm(days~ dow.f + tminus1+ tminus2 + tminus3, family=distribution)
fit1AR4 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit1AR5 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit1AR6 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit1AR7 = glm(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit2 = glm(days~ dow.f+ month.f, family=distribution)
fit2AR1 = glm(days~ dow.f+ month.f + tminus1, family=distribution)
fit2AR2 = glm(days~ dow.f+ month.f + tminus1+ tminus2, family=distribution)
fit2AR3 = glm(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3, family=distribution)
fit2AR4 = glm(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit2AR5 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit2AR6 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit2AR7 = glm(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)

fit3 = glm(days~ dow.f+ cos + sin, family=distribution)
fit3AR1 = glm(days~ dow.f+ cos + sin + tminus1, family=distribution)
fit3AR2 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2, family=distribution)
fit3AR3 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3, family=distribution)
fit3AR4 = glm(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)
fit3AR5 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)
fit3AR6 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)
fit3AR7 = glm(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)
```



```r
#Printing AICs
AR1 <- c(fit1=fit1$aic,fit1AR1=fit1AR1$aic,fit1AR2=fit1AR2$aic,fit1AR3=fit1AR3$aic,
                          fit1AR4=fit1AR4$aic,fit1AR5=fit1AR5$aic,fit1AR6=fit1AR6$aic,fit1AR7=fit1AR7$aic)
AR2 <- c(fit2=fit2$aic,fit2AR1=fit2AR1$aic,fit2AR2=fit2AR2$aic,fit2AR3=fit2AR3$aic,
                          fit2AR4=fit2AR4$aic,fit2AR5=fit2AR5$aic,fit2AR6=fit2AR6$aic,fit2AR7=fit2AR7$aic)
AR3 <- c(fit3=fit3$aic,fit3AR1=fit3AR1$aic,fit3AR2=fit3AR2$aic,fit3AR3=fit3AR3$aic,
                          fit3AR4=fit3AR4$aic,fit3AR5=fit3AR5$aic,fit3AR6=fit3AR6$aic,fit3AR7=fit3AR7$aic)


print(AR1)
```

```
##     fit1  fit1AR1  fit1AR2  fit1AR3  fit1AR4  fit1AR5  fit1AR6  fit1AR7 
## 4846.252 4838.922 4835.396 4675.913 4666.283 4630.157 4626.843 4623.932
```

```r
print(AR2)
```

```
##     fit2  fit2AR1  fit2AR2  fit2AR3  fit2AR4  fit2AR5  fit2AR6  fit2AR7 
## 4644.673 4645.964 4647.851 4543.044 4541.366 4520.610 4512.690 4513.506
```

```r
print(AR3)
```

```
##     fit3  fit3AR1  fit3AR2  fit3AR3  fit3AR4  fit3AR5  fit3AR6  fit3AR7 
## 4758.070 4755.620 4755.592 4617.012 4610.667 4580.082 4576.684 4575.989
```


```r
plot(t,days, type="l",main="Poisson regression")
lines(fit1$fit, col="red"   , lwd=2)
lines(fit2$fit, col="blue"  , lwd=2)
lines(fit3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                  c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)

```r
### Negative Binomial Regression with DOW, month or sin/cos wave
```

```r
require(MASS)
fitNB1 = glm.nb(days~ dow.f)
fitNB1AR1 = glm.nb(days~ dow.f + tminus1)
fitNB1AR2 = glm.nb(days~ dow.f + tminus1+ tminus2)
fitNB1AR3 = glm.nb(days~ dow.f + tminus1+ tminus2 + tminus3)
fitNB1AR4 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB1AR5 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB1AR6 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB1AR7 = glm.nb(days~ dow.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB2 = glm.nb(days~ dow.f+ month.f)
fitNB2AR1 = glm.nb(days~ dow.f+ month.f + tminus1)
fitNB2AR2 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2)
fitNB2AR3 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3)
fitNB2AR4 = glm.nb(days~ dow.f+ month.f +
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB2AR5 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB2AR6 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB2AR7 = glm.nb(days~ dow.f+ month.f  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)

fitNB3 = glm.nb(days~ dow.f+ cos + sin)
fitNB3AR1 = glm.nb(days~ dow.f+ cos + sin + tminus1)
fitNB3AR2 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2)
fitNB3AR3 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3)
fitNB3AR4 = glm.nb(days~ dow.f  + cos + sin + 
                tminus1+ tminus2+ tminus3+ tminus4)
fitNB3AR5 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)
fitNB3AR6 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)
fitNB3AR7 = glm.nb(days~ dow.f+ cos + sin  +
                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)
```



```r
#Printing AICs
AR_NB1 <- c(fitNB1=fitNB1$aic,fitNB1AR1=fitNB1AR1$aic,fitNB1AR2=fitNB1AR2$aic,fitNB1AR3=fitNB1AR3$aic,
                          fitNB1AR4=fitNB1AR4$aic,fitNB1AR5=fitNB1AR5$aic,fitNB1AR6=fitNB1AR6$aic,fitNB1AR7=fitNB1AR7$aic)
AR_NB2 <- c(fitNB2=fitNB2$aic,fitNB2AR1=fitNB2AR1$aic,fitNB2AR2=fitNB2AR2$aic,fitNB2AR3=fitNB2AR3$aic,
                          fitNB2AR4=fitNB2AR4$aic,fitNB2AR5=fitNB2AR5$aic,fitNB2AR6=fitNB2AR6$aic,fitNB2AR7=fitNB2AR7$aic)
AR_NB3 <- c(fitNB3=fitNB3$aic,fitNB3AR1=fitNB3AR1$aic,fitNB3AR2=fitNB3AR2$aic,fitNB3AR3=fitNB3AR3$aic,
                          fitNB3AR4=fitNB3AR4$aic,fitNB3AR5=fitNB3AR5$aic,fitNB3AR6=fitNB3AR6$aic,fitNB3AR7=fitNB3AR7$aic)
print(AR_NB1)
```

```
##    fitNB1 fitNB1AR1 fitNB1AR2 fitNB1AR3 fitNB1AR4 fitNB1AR5 fitNB1AR6 fitNB1AR7 
##  3166.393  3166.181  3166.120  3130.599  3130.730  3119.240  3117.250  3118.443
```

```r
print(AR_NB2)
```

```
##    fitNB2 fitNB2AR1 fitNB2AR2 fitNB2AR3 fitNB2AR4 fitNB2AR5 fitNB2AR6 fitNB2AR7 
##  3134.479  3136.475  3138.171  3119.364  3121.027  3115.965  3115.515  3117.030
```

```r
print(AR_NB3)
```

```
##    fitNB3 fitNB3AR1 fitNB3AR2 fitNB3AR3 fitNB3AR4 fitNB3AR5 fitNB3AR6 fitNB3AR7 
##  3149.948  3151.235  3152.119  3120.543  3121.891  3113.951  3112.711  3114.350
```


```r
plot(t,days, type="l",main="Negative Binomial Regression")
lines(fitNB1$fit, col="red"   , lwd=2)
lines(fitNB2$fit, col="blue"  , lwd=2)
lines(fitNB3$fit, col="green" , lwd=2)
legend("topleft",pch=3,col=c("red","blue","green"),
                c("Day-of-Week", "Day-of-Week+Month","Day-of-Week+sin/cos"))
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34-1.png)


