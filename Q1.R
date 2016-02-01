#autoregression order, solved
#month, weekday/weekends or each day?, hour,half hour (may not important), demand
#Cross validation (how much reduction of MSE worth an new variable addtion, in prediction?),
#above read 521 book
#regularization, AIC, BIC
#multicollinearity
#natural logarithmic differences, solved
#neural network required variables
#seasonality: detect, lags? 
#remove outliers important！ solved
#price freeze, unknown reason
#external source

#check missing data, 2 types

###############
# check packages
check_packages = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}

check_packages(c("stats","devtools","utils","lubridate","dplyr","tseries","neuralnet", "stringr"))

##############
data = read.csv('/Users/luohaosheng/Desktop/Question/Tios/Test/australia-victoria-energy.csv')
#don't forget to change the path when running the code
data = dplyr::select(data,SETTLEMENTDATE,TOTALDEMAND,RRP)%>%
  mutate(YEAR=year(SETTLEMENTDATE),MONTH=month(SETTLEMENTDATE),DAY=day(SETTLEMENTDATE),HOUR = hour(SETTLEMENTDATE),MINUTE = minute(SETTLEMENTDATE),
         TIME = paste(HOUR, MINUTE),WDAY=wday(SETTLEMENTDATE,label=T),WDAY2=1)
data[data$WDAY%in%c("Sat","Sun"),"WDAY2"]=0

year = transmute(data,YEAR=year(SETTLEMENTDATE))
year[year$YEAR!=2014,] #only one 2015
nrow(data[duplicated(data$SETTLEMENTDATE),]) # no duplicate time
(n = nrow(data))
#therefore no type 2 missing data one 364*24*2 = 17520
data[!complete.cases(data),] # no type 1 missing data


plot(data$RRP,type="l",xlab="Index",ylab="RRP",main="The Time-series of RRP",xlim=c(1,n))
plot(data$TOTALDEMAND,type="l")
plot(data$RRP, data$TOTALDEMAND, type = "p", pch = 20)
adf.test(data$TOTALDEMAND) #both are stationary
adf.test(data$RRP) #both are stationary

#logistic may not stable, remove the outliers

par(mfrow=c(1,2))
plot(data[600:850,"RRP"], type="l")
plot(data[600:850,"TOTALDEMAND"],type="l")


#seasonality within a year, due to month itself, cause weather changes
plot(data[,"RRP"],type="l",ylim=c(-5,100))
plot(data[,"TOTALDEMAND"],type="l",ylim=c(-5,100))

#seasonality within a month

datetime <- seq(
  from=as.POSIXct("2014-4-1 0:00"),
  to=as.POSIXct("2014-4-30 23:30"),
  by="30 mins"
)  
plot(datetime,data[data$MONTH==4,"RRP"], type="l", xaxs='i',xlab="Index", 
     ylab="RRP", main="The Time-series of RRP in April",xlim = as.POSIXct(c("2014-4-1 0:00","2014-4-30 23:30")))
plot(data[data$MONTH==4,"TOTALDEMAND"], type="l", xlab="Index", 
     ylab="RRP", main="The Time-series of Total Demand in April")

datetime = seq(from=as.POSIXct("2014-4-5 0:00"), to=as.POSIXct("2014-4-13 23:30"),
               by="30 mins")  
plot(datetime, data[(data$MONTH==4)&(data$DAY>4)&(data$DAY<14),"RRP"], type="l",  
     xaxs='i', xlab="Date",ylab="RRP", main="The Time-series of RRP during April 5th - 13th")
plot(datetime, data[(data$MONTH==4)&(data$DAY>4)&(data$DAY<14),"TOTALDEMAND"], type="l",  
     xaxs='i', xlab="Date",ylab="Total Demand", main="The Time-series of Total Demand during April 5th - 13th")


#Holiday database: http://www.officeholidays.com/countries/australia/victoria/2014.php
#Public holiday only
Holiday = as.data.frame(matrix(c(1,1,1,27,3,10,4,18,4,21,4,25,6,9,11,4,12,25,12,26), 
                 nrow = 10, ncol = 2, byrow=T))
colnames(Holiday) = c("MONTH","DAY")
Holiday$holiday = 1

# 14–17 January 2014 - Melbourne records 4 consecutive days of temperatures exceeding 41 °C (106 °F), two of which exceed 43 °C (109 °F)
# 16 January 2014 - Victoria power outage, but no negative price (ignore, b/c smoothing)
# 17 January 2014 - negative price, but may due to the recovery of some power plant (ignore, b/c smoothing)
# 28 January 2014- high demand, hot weather bushfire, no outage, higher order of demand! high price
# 2014/02/02 hot weather bushfire, high demand, power outage, price spike and negative price
# 2014/09/09 floods and thunder storms 
# 10/26/2014-10/27/2014 - thunder storm, negative price, major power outages
#Buyers are actually getting money and electricity from sellers. However, you need to keep in mind that if a producer is willing to accept negative prices, this means it is less expensive for him to keep their power plants online than to shut them down and restart them later.
#https://www.epexspot.com/en/company-info/basics_of_the_power_market/negative_prices
#Can consider holidays

#smooth Jan 14-17, all weekday, keep the remaining high demand case, use higher order. 
data2 = data
hot_index = rownames(data[data$MONTH==1&data$DAY%in%14:17,])
normal_wday = data[((data$MONTH!=1)|!(data$DAY%in%14:17))&data$WDAY2==1,]
normal_wday = group_by(normal_wday,TIME)
normal_wday = as.data.frame(summarise(normal_wday,TOTALDEMAND=mean(TOTALDEMAND),RRP=mean(RRP)))
normal_wday = rbind(normal_wday,normal_wday,normal_wday,normal_wday)
data2[hot_index,c("TOTALDEMAND","RRP")]=normal_wday[c("TOTALDEMAND","RRP")]
data2$LCTOTD = NA #create log change of demand and price, pending pricing due to negative price
data2[2:n,"LCTOTD"] = log(data2$TOTALDEMAND[2:n])-log(data2$TOTALDEMAND[1:(n-1)])
data2 = merge(x = data2, y = Holiday, by = c("MONTH","DAY"), all.x = TRUE) #create holiday
data2[is.na(data2$holiday),"holiday"] = 0
data2 = arrange(data2,YEAR,MONTH,DAY,HOUR,MINUTE)

#acf, pacf
#acf(data2$RRP,lag.max = 96, type = "correlation")
#acf(data2$RRP,lag.max = 96, type = "partial")
#AR（4，5） model, no need to include MA 另外为了比较神经网络和ARIMA，input保证长一个样才有意义
#结果与AEMO印证
#不用单独列出来了，显得太死板学术



#factorization
data2$MONTH = as.factor(data2$MONTH)

#create TOTD higher orders, scale down
data2$TOTD1 = data2$TOTALDEMAND/1000
data2$TOTD2 = data2$TOTD1^2
data2$TOTD3 = data2$TOTD1^3 #should scale down, to avoid 小数点保存问题

#create RRP lags
data2[,c("L1RRP","L2RRP","L3RRP","L4RRP","L336RRP","L337RRP","L338RRP","L339RRP",
         "L340RRP")]=NA
col = which(colnames(data2)=="L1RRP") #first column of lags of RRP
col_RRP = which(colnames(data2)=="RRP")
for(j in c(1:4,336:340)){
  i=j+1
  data2[i:n,col]=data2[1:(n-j),col_RRP]
  col=col+1
}

#create LCTOTD lags
data2[,c("L1LCTOTD","L2LCTOTD","L3LCTOTD","L4LCTOTD","L336LCTOTD","L337LCTOTD",
         "L338LCTOTD","L339LCTOTD","L340LCTOTD")]=NA 
#LC: log change
col = which(colnames(data2)=="L1LCTOTD") #first column of lags of LCTOTD
col_LCTOTD = which(colnames(data2)=="LCTOTD")
for(j in c(1:4,336:340)){
  i=j+1
  data2[i:n,col]=data2[1:(n-j),col_LCTOTD]
  col=col+1
}

# create dataset for RRP model training
data3 = dplyr::select(data2, RRP, TOTD1:TOTD3, MONTH, TIME, WDAY2, holiday, L1RRP:L340RRP)
data3 = data3[complete.cases(data3),]
name_data3 = names(data3)
lagname_data3 = c("L1RRP","L2RRP","L3RRP","L4RRP","L336RRP","L337RRP","L338RRP","L339RRP","L340RRP")
# create dataset for error analysis
ANN = ARIMA = as.data.frame(matrix(0,3,10))
colnames(ANN) = colnames(ARIMA) = c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10")
rownames(ANN) = rownames(ARIMA) = c("MSE","MAPE","Corr") #MAE? 
#Corr to analyze the model accuracy with the %change of a value data in AEMO

# 10 models for ANN
M_p1 = "TOTD1 + TOTD2 + TOTD3 +"
M_p2 = paste(lagname_data3, collapse = " + ")
M = vector("list", 10)
M[[1]] = as.formula("RRP ~ TOTD1")
M[[2]] = as.formula("RRP ~ TOTD1 + TOTD2 + TOTD3")
#M[[3]] = as.formula(paste("RRP ~", "TOTD1 +", M_p2))
M[[3]] = as.formula(paste("RRP ~", M_p2))
M[[4]] = as.formula(paste("RRP ~",M_p1, M_p2))
M[[5]] = as.formula(paste("RRP ~ TOTD1 + holiday + WDAY2 +", M_p2))
M[[6]] = as.formula(paste("RRP ~", M_p1,"holiday + WDAY2 +",M_p2))
M[[7]] = as.formula(paste("RRP ~ TOTD1 + MONTH + TIME +", M_p2))
M[[8]] = as.formula(paste("RRP ~", M_p1,"MONTH + TIME +",M_p2))
M[[9]] = as.formula(paste("RRP ~ TOTD1 + MONTH + TIME + holiday + WDAY2 +", M_p2))
M[[10]] = as.formula(paste("RRP ~", paste(name_data3[!name_data3 %in% "RRP"], collapse = " + ")))
hiddenstr = list(0, 1, 4, 6, 6, 7, 6, 7, 7, c(6, 2)) #its nodes

#random cross-validation 5%
t = 10
for(i in 1:t){
  index = sample(1:nrow(data3),round((1-1/t)*nrow(data3)))
  train.cv = data3[index,]
  test.cv = data3[-index,]
  for(j in 1:10){
    nn = neuralnet(M[[j]],data=train.cv,hidden=hiddenstr[[j]],linear.output=T)
    pr.nn = compute(nn,test.cv[,all.vars(M[[j]][[3]])]) #再检查一下，头晕的时候写的！
    
    tss = pr.nn$net.result
    
    ln = lm(M[[j]],data=train.cv)
    pr.ln = cbind(rep(1,nrow(test.cv)),as.matrix(test.cv[,all.vars(M[[j]][[3]])]))%*%as.matrix(ln$coefficients)
    
  }





}

#data4 is just a temp df for trial to predict TOTD with neural network
#should add some other factors, based on the model selected for RRP
data4 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD,WDAY2)
data4 = data4[complete.cases(data4),]
n_data4 = nrow(data4)
name_data4 = names(data4)
f = as.formula(paste("LCTOTD ~", paste(name_data4[!name_data4 %in% "LCTOTD"], collapse = " + ")))
nn_LCTOTD = neuralnet(f,data=data4,hidden=6,linear.output=T)
data4[(nrow(data4)+1):(nrow(data4)+96),"WDAY2"] = 1
data4[nrow(data4),"WDAY2"] = 0
lag_index = c(1:4,336:340)
for(j in 1:96){
  for(i in seq(1:(ncol(data4)-2))){
    data4[(j+n_data4),(i+1)]=data4[(j+n_data4-lag_index[i]),1]
  }
}
rownames(data4) = seq(1:nrow(data4))


######

for(i in 1:96){
    data4[(i+n_data4),1] = compute(nn_LCTOTD, data4[(i+n_data4),all.vars(f[[3]])])$net.result
    for(j in 1:4){
      data4[(i+n_data4+j),(1+j)]=data4[(i+n_data4),1]
    }
}
data4 = data4[complete.cases(data4),]

pr1 = as.data.frame(exp(data4[(n_data4+1):nrow(data4),"LCTOTD"]))
start = data2[nrow(data2),"TOTALDEMAND"]
for(i in 1:nrow(pr1)){
pr1[i,"PRTOTD"]=start*prod(pr1[1:i,1])
}

plot(pr1$PRTOTD,type="l")
plot(data2[4225:4272,"TOTALDEMAND"],type="l")

########################
data4 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD)
data4 = data4[complete.cases(data4),]
n_data4 = nrow(data4)
name_data4 = names(data4)
f = as.formula(paste("LCTOTD ~", paste(name_data4[!name_data4 %in% "LCTOTD"], collapse = " + ")))
nn_LCTOTD = neuralnet(f,data=data4,hidden=4,linear.output=T)



lag_index = c(1:4,336:340)
for(j in 1:96){
  for(i in seq(1:(ncol(data4)-1))){
    data4[(j+n_data4),(i+1)]=data4[(j+n_data4-lag_index[i]),1]
  }
}
rownames(data4) = seq(1:nrow(data4))

######

nn_LCTOTD = compute(nn_LCTOTD,data4[(17179-48*5):17179,all.vars(f[[3]])])$net.result
pr1 = as.data.frame(exp(nn_LCTOTD))
start = data2[nrow(data2),"TOTALDEMAND"]
for(i in 1:nrow(pr1)){
  pr1[i,"PRTOTD"]=start*prod(pr1[1:i,1])
}

plot(pr1$PRTOTD,type="l")

######

for(i in 1:96){
  data4[(i+n_data4),1] = compute(nn_LCTOTD, data4[(i+n_data4),all.vars(f[[3]])])$net.result
  for(j in 1:4){
    data4[(i+n_data4+j),(1+j)]=data4[(i+n_data4),1]
  }
}
data4 = data4[complete.cases(data4),]

pr1 = as.data.frame(exp(data4[(n_data4+1):nrow(data4),"LCTOTD"]))
start = data2[nrow(data2),"TOTALDEMAND"]
for(i in 1:nrow(pr1)){
  pr1[i,"PRTOTD"]=start*prod(pr1[1:i,1])
}

plot(pr1$PRTOTD,type="l")
plot(data2[4225:4272,"TOTALDEMAND"],type="l")