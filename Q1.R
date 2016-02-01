#month, weekday/weekends or each day?, hour,half hour (may not important), demand
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
data = read.csv('~/Tios/Homework/australia-victoria-energy.csv')
#don't forget to change the path when running the code
data = dplyr::select(data,SETTLEMENTDATE,TOTALDEMAND,RRP)%>%
  mutate(YEAR=year(SETTLEMENTDATE),MONTH=month(SETTLEMENTDATE),DAY=day(SETTLEMENTDATE),HOUR = hour(SETTLEMENTDATE),MINUTE = minute(SETTLEMENTDATE),
         TIME = paste(HOUR, MINUTE),WDAY=wday(SETTLEMENTDATE,label=T),WDAY2=0.1)
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
#adf.test(data$TOTALDEMAND) #both are stationary
#adf.test(data$RRP) #both are stationary

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
Holiday$holiday = 0.1

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
normal_wday = data[((data$MONTH!=1)|(!data$DAY%in%14:17))&data$WDAY2==0.1,]
normal_wday = group_by(normal_wday,HOUR,MINUTE)
normal_wday = as.data.frame(summarise(normal_wday,TOTALDEMAND=mean(TOTALDEMAND),RRP=mean(RRP)))
normal_wday = rbind(normal_wday,normal_wday,normal_wday,normal_wday)
data2[hot_index,c("TOTALDEMAND","RRP")]=normal_wday[c("TOTALDEMAND","RRP")]
data2[data2$RRP<0,"RRP"]=0.01
data2[data2$RRP>80,"RRP"]=80
data2[2:n,"LCRRP"] = log(data2$RRP[2:n])-log(data2$RRP[1:(n-1)])


#create log change of demand and price
data2[2:n,"LCTOTD"] = log(data2$TOTALDEMAND[2:n])-log(data2$TOTALDEMAND[1:(n-1)])
data2 = merge(x = data2, y = Holiday, by = c("MONTH","DAY"), all.x = TRUE) #create holiday
data2[is.na(data2$holiday),"holiday"] = 0
data2 = arrange(data2,YEAR,MONTH,DAY,HOUR,MINUTE)

data2$MONTH=as.factor(data2$MONTH)



#acf, pacf
#acf(data2$RRP,lag.max = 96, type = "correlation")
#acf(data2$RRP,lag.max = 96, type = "partial")
#AR（4，5） model, no need to include MA 另外为了比较神经网络和LN，input保证长一个样才有意义
#结果与AEMO印证
#不用单独列出来了，显得太死板学术


#create TOTD higher orders, scale down

data2[2:nrow(data2),"LCTOTD2"] = (data2[2:nrow(data2),"LCTOTD"])^2
data2[2:nrow(data2),"LCTOTD3"] = (data2[2:nrow(data2),"LCTOTD"])^3


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
data3 = dplyr::select(data2, LCRRP, LCTOTD, LCTOTD2, LCTOTD3, WDAY2, holiday, MONTH)
data3 = data3[complete.cases(data3),]

name_data3 = names(data3)
# create dataset for error analysis
ANN = LN = as.data.frame(matrix(0,3,4))
colnames(ANN) = colnames(LN) = c("M1","M2","M3","M4")
rownames(ANN) = rownames(LN) = c("MSE","MAPE","Corr") #MAE? 
#Corr to analyze the model accuracy with the %change of a value data in AEMO

# 10 models for ANN
M = vector("list", 4)
M[[1]] = as.formula("LCRRP ~ LCTOTD + LCTOTD2 + LCTOTD3")
M[[2]] = as.formula("LCRRP ~ LCTOTD + LCTOTD2 + LCTOTD3 + holiday")
M[[3]] = as.formula("LCRRP ~ LCTOTD + LCTOTD2 + LCTOTD3 + WDAY2")
M[[4]] = as.formula("LCRRP ~ LCTOTD + LCTOTD2 + LCTOTD3 + WDAY2 + holiday")
hiddenstr = list(2, 2, 2, 3) #its nodes or (2,2,2)?

#random cross-validation 5%
t = 10
for(i in 1:t){
  index = sample(1:nrow(data3),round(1/t*nrow(data3)))
  index_error = index - 1
  train.cv = data3[-index,]
  test.cv = data3[index,]
  rrp.cv = data2[index_error,"RRP"]
  for(j in 1:4){
    nn = neuralnet(M[[j]],data=train.cv,hidden=hiddenstr[[j]],linear.output=T)
    pr.nn = exp(compute(nn,test.cv[,all.vars(M[[j]][[3]])])$net.result)
    pr.nn.RRP = rrp.cv*pr.nn
    ANN["MSE",j] = sum((pr.nn.RRP-data2[index,"RRP"])^2)/length(index)/t
    
    ln = lm(M[[j]],data=train.cv)
    pr.ln = exp(cbind(rep(1,nrow(test.cv)),as.matrix(test.cv[,all.vars(M[[j]][[3]])]))%*%as.matrix(ln$coefficients))
    pr.ln.RRP = rrp.cv*pr.ln
    LN["MSE",j] = LN["MSE",j]+sum((pr.ln.RRP-data2[index,"RRP"])^2)/length(index)/t
    
  }
}

#data4 is just a temp df for trial to predict TOTD with neural network
#should add some other factors, based on the model selected for RRP
data4 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD,WDAY2)
data4 = data4[complete.cases(data4),]
n_data4 = nrow(data4)
name_data4 = names(data4)
f = as.formula(paste("LCTOTD ~", paste(name_data4[!name_data4 %in% "LCTOTD"], collapse = " + ")))
nn_LCTOTD = neuralnet(f,data=data4,hidden=5,linear.output=T)
data4[(n_data4+1):(n_data4+96),"WDAY2"] = 0.1
data4[nrow(data4),"WDAY2"] = 0
lag_index = c(1:4,336:340)
for(j in 1:96){
  for(i in seq(1:(ncol(data4)-2))){
    data4[(j+n_data4),(i+1)]=data4[(j+n_data4-lag_index[i]),1]
  }
}
rownames(data4) = seq(1:nrow(data4))


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

######################## no weekday
data4 = dplyr::select(data2,LCTOTD,L1LCTOTD:L340LCTOTD)
data4 = data4[complete.cases(data4),]
n_data4 = nrow(data4)
name_data4 = names(data4)
f = as.formula(paste("LCTOTD ~", paste(name_data4[!name_data4 %in% "LCTOTD"], collapse = " + ")))
nn_LCTOTD = neuralnet(f,data=data4,hidden=5,linear.output=T)

lag_index = c(1:4,336:340)
for(j in 1:96){
  for(i in seq(1:(ncol(data4)-1))){
    data4[(j+n_data4),(i+1)]=data4[(j+n_data4-lag_index[i]),1]
  }
}
rownames(data4) = seq(1:nrow(data4))
#within-sample
#nn_LCTOTD = compute(nn_LCTOTD,data4[(17179-48*5):17179,all.vars(f[[3]])])$net.result
#pr1 = as.data.frame(exp(nn_LCTOTD))
#start = data2[nrow(data2),"TOTALDEMAND"]
#for(i in 1:nrow(pr1)){
#  pr1[i,"PRTOTD"]=start*prod(pr1[1:i,1]) 
#}
#plot(pr1$PRTOTD,type="l")



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
plot(data2[4225:4320,"TOTALDEMAND"],type="l")
