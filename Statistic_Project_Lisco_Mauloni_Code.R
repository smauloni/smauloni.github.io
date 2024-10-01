#
#NEEDED TOOLS
#

library(dplyr)
library(caret)
library(glmnet)
library(ggplot2)
library(ggthemes)
library(see)
library(performance)
library(patchwork)

compute_change = function(x)
{
  v = c()
  for (i in 1:(length(x))){
    r = (x[i]-x[i-1])/x[i-1]*100
    v[i] = r
  }
  return(v)
}

compute_change_DFF = function(x)
{
  v = c()
  for (i in 1:(length(x))){
    r = (x[i]-x[i-1])
    v[i] = r
  }
  return(v)
}

remove_percentage = function(x)
{
  res = gsub("%","",x)
  return(res)
}


#
#SCRIPT
#

path = "/Users/stefano/Library/Mobile Documents/com~apple~CloudDocs/Mathematical Statistics/Statistics Project/"
file_names = c("----SHEL Prova dal 2010.csv", "----Brent Oil Futures prova 2010.csv", 
        "----Crude Oil WTI Futures prova 2010.csv", "----Natural Gas Futures prova 2010.csv", 
        "----ETF iShares Clean Energy ICLN.csv", "----S&P 500 Historical Data.csv", "----Henry Nat Gas.csv",
        "----Refiners ETF (CRAK).csv", "----DFF.csv")

for (i in 1:9){
  if (i<8){temp = read.csv(paste(path, file_names[i], sep=""), sep = ";")}
  else {temp = read.csv(paste(path, file_names[i], sep=""))}
  attach(temp)
  names(temp)
  assign(paste("data", i, sep=""), temp)
}

vec <- c("SHEL", "Brent", "Crude_WTI", "Natural_Gas", "Clean_Energy", "SP500")
for (i in 1:6){
    nam = paste("data", i, sep="")
    temp = get(nam)
    nam = paste(nam, "copy", sep="")
    colnames(temp)[1] = paste("Date", i, sep="")
    cng = paste("C_", vec[i], sep="")
    colnames(temp)[7] = cng
    temp[cng] = as.numeric(remove_percentage(temp[,cng]))
    colnames(temp)[2] = paste("P_", vec[i], sep="")
    temp = temp[c(1,2,7)]
    assign(nam, temp)
}

rm(i, nam, cng, temp, vec, file_names, path)

data7copy = data7
colnames(data7copy)[1] = "Date7"
colnames(data7copy)[2] = "P_HenryGas"
data7copy$P_HenryGas = as.numeric(data7copy$P_HenryGas)
data7copy$C_HenryGas = compute_change(data7copy$P_HenryGas)

data8copy = data8
colnames(data8copy)[1] = "Date8"
colnames(data8copy)[5] = "P_Refiners"
data8copy$C_Refiners = compute_change(data8copy$P_Refiners)
data8copy = data8copy[c(1,5,8)]

data9copy = data9
colnames(data9copy)[1] = "Date9"
colnames(data9copy)[2] = "P_FedRates"
data9copy$C_FedRates = compute_change_DFF(data9copy$P_FedRates)


#1=SHEL, 2=Brent, 3=Crude, 4=Nat Gas, 5=iShares Clean Energy,  6=S&P 500, 
#7=Henry hub Gas, 8=CRAK ETF Refiners, 9=Tassi fed

#MERGE THE DATA
df = left_join(x=data1copy, y=data2copy, by=join_by(Date1 == Date2))
df = left_join(x=df, y=data3copy, by=join_by(Date1 == Date3))
df = left_join(x=df, y=data4copy, by=join_by(Date1 == Date4))
df = left_join(x=df, y=data5copy, by=join_by(Date1 == Date5))
df = left_join(x=df, y=data6copy, by=join_by(Date1 == Date6))
df = left_join(x=df, y=data9copy, by=join_by(Date1 == Date9))
df = left_join(x=data8copy, y=df, by=join_by(Date8 == Date1))
df = left_join(x=df, y=data7copy, by=join_by(Date8 == Date7))

#df = df[c(2:19)]

df = na.omit(df)

#df = df[(df$Change1 >= -6 & df$Change1<= 6),]
#df = df[seq(600, 900),]
#df$Price1 = df$Price1/df$Price1[1]


#REGRESSIONS
regr <- lm(C_SHEL ~ C_Refiners + C_Brent + C_Crude_WTI + C_Natural_Gas + C_Clean_Energy + C_FedRates + C_SP500 + C_HenryGas, data = df)
summary(regr)
plot(regr)

regr2 = lm(P_SHEL ~ P_Refiners + P_Brent + P_Crude_WTI + P_Natural_Gas + P_Clean_Energy + P_FedRates + P_SP500 + P_HenryGas, data = df)
summary(regr2)
plot(regr2)


#KOLMOGOROV-SMIRNOFF TEST
res = residuals(regr)
# empirical cdf
x_lims = 1.2 * c(min(res),max(res))
plot(ecdf(res), pch=".", main = "Empirical distribution function vs. standard normal", xlim = x_lims)
# standard normal cdf
x_vec = seq(x_lims[1], x_lims[2], by = 0.1)
lines(x_vec, pnorm(x_vec, mean=mean(res), sd=sd(res)))
ks_test = ks.test(res, "pnorm", mean=mean(res), sd=sd(res))
alpha = .05
ks_test$p.value < alpha
#FALSE: do not reject H0
hist(res)


#STEP METHOD TO MODEL SELECTION FOR: __CHANGE__
intercept <- lm(C_SHEL ~ 1, data = df)
all <- regr
forward <- step(intercept, direction='forward', scope=formula(all), trace=0)
forward$anova
forward$coefficients

backward <- step(intercept, direction='backward', scope=formula(all), trace=0)
backward$anova
backward$coefficients

both <- step(intercept, direction='both', scope=formula(all), trace=0)
both$anova
both$coefficients


#STEP METHOD TO MODEL SELECTION FOR: __PRICE__
intercept2 <- lm(P_SHEL ~ 1, data = df)
all2 <- regr2
forward2 <- step(intercept2, direction='forward', scope=formula(all2), trace=0)
forward2$anova
forward2$coefficients

backward2 <- step(intercept2, direction='backward', scope=formula(all2), trace=0)
backward2$anova
backward2$coefficients

both2 <- step(intercept2, direction='both', scope=formula(all2), trace=0)
both2$anova
both2$coefficients


#
#   T-TEST
#

#divide the df into two,
#each containing either the first or second semester for all years (2010-2023)

compute_log = function(x)
{
  v = c()
  for (i in 1:(length(x))){
    r = log(x[i]/x[i-1])
    v[i] = r
  }
  return(v)
}

#compute log returns
df_log = df[c(1, 4)]
df_log$log_return = compute_log(df_log$P_SHEL)
df_log$Date8 <- as.Date(df_log$Date8, format= "%Y-%m-%d")

sem1 = df_log[0,]
sem2 = sem1
for (i in 10:23){
  mex = paste("20", i, sep="")
  temp1 = subset(df_log, Date8 >= paste(mex,"-01-01", sep="") & Date8 <= paste(mex,"-06-30", sep=""))
  sem1 = rbind(sem1, temp1)
  temp2 = subset(df_log, Date8 > paste(mex,"-06-30", sep="") & Date8 <= paste(mex,"-12-31", sep=""))
  sem2 = rbind(sem2, temp2)
}


#perform the test
alpha = .05
# different variances
t.test(sem1$log_return, sem2$log_return, alternative = "two.sided", paired = F, var.equal = F, conf.level = 1-alpha)
# reject H_0

#also one-sided
t.test(sem1$log_return, sem2$log_return, null= ,alternative = "two.sided", paired = F, var.equal = T, conf.level = 1-alpha)


# plots
a = sem1[c(3)]
a = cbind(a, sem2[c(3)][seq(1,496),])
colnames(a)[1] = "sem1"
colnames(a)[2] = "sem2"
a = na.omit(a)

ggplot(a, aes(x=a$sem1)) + geom_histogram(color = "#192594", fill = "#4fa2e3", binwidth = 0.008) +
  labs(x = "SHEL log-return", y = "Frequency", title = "Histogram of Shell log-returns in semester 1") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(a, aes(x=a$sem2)) + geom_histogram(color = "#192594", fill = "#4fa2e3", binwidth = 0.008) +
  labs(x = "SHEL log-return", y = "Frequency", title = "Histogram of Shell log-returns in semester 2") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())


#
# PLOT
#


#df2 = df[(df$P_SHEL > 55),]


df %>%
  ggplot(aes(df$P_SHEL, df$P_Brent, color=C_SHEL)) + #1
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Brent", x = "Price of SHEL", y = "Price of Brent oil futures",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_Refiners, color=C_SHEL)) + #2
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Refiners", x = "Price of SHEL", y = "Price of Refiners ETF",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_Crude_WTI, color=C_SHEL)) +#3
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Crude WTI", x = "Price of SHEL", y = "Price of Crude WTI oil futures",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_Natural_Gas, color=C_SHEL)) + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Natural Gas", x = "Price of SHEL", y = "Price of Natural Gas futures",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_Clean_Energy, color=C_SHEL)) + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Clean Energy ETF", x = "Price of SHEL", y = "Price of iShares ETF",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_SP500, color=C_SHEL)) + #4
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/S&P 500", x = "Price of SHEL", y = "Value of S&P 500",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$P_SHEL, df$P_FedRates, color=C_SHEL)) + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Federeal funds rates", x = "Price of SHEL", y = "Value of DFF",  color = "SHEL % Change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

df %>%
  ggplot(aes(df$C_SHEL, df$C_Refiners, color=P_SHEL)) + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Correlation SHEL/Refiners", x = "% Change of SHEL", y = "% Change in Refiners ETF",  color = "SHEL Price") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())




#FITTED VS RESIDUALS, CHANGES
regr %>%
  ggplot(aes(regr$fitted.values, regr$residuals, color=df$P_SHEL), color = "blue") + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Fitted vs Residuals, Regression on Changes",x = "Fitted", y = "Residuals", color = "SHEL price") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlim(-7,7) +
  ylim(-10,10)

#FITTED VS RESIDUALS, PRICES
regr2 %>%
  ggplot(aes(regr2$fitted.values, regr2$residuals, color=df$C_SHEL), color = "blue") + 
  geom_point(size = 1.5, alpha = 0.8) + 
  labs(title="Fitted vs Residuals, Regression on Price",x = "Fitted", y = "Residuals", color = "SHEL % change") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
  #xlim(-7,7) +
  #ylim(-10,10)


#QQPLOT
ggplot(regr, aes(sample = regr$residuals)) + stat_qq(distribution = qnorm, col = "#4fa2e3") +
  stat_qq_line(line.p = c(0.25, 0.75), col = "#88dd88") +
  xlab("Normal Scores") + ylab("Residuals") +
  theme_fivethirtyeight() +
  labs(title = "Regression on Changes, QQPlot") +
  theme(axis.title = element_text())

ggplot(regr2, aes(sample = regr2$residuals)) + stat_qq(distribution = qnorm, col = "#4fa2e3") +
  stat_qq_line(line.p = c(0.25, 0.75), col = "#88dd88") +
  xlab("Normal Scores") + ylab("Residuals") +
  theme_fivethirtyeight() +
  labs(title = "Regression on Prices, QQPlot") +
  theme(axis.title = element_text())


  
#HISTOGRAM SHEL
ggplot(df, aes(x=df$C_SHEL, color = df$P_SHEL)) + geom_histogram(color = "#192594", fill = "#4fa2e3", binwidth = 0.4) +
  labs(x = "SHEL % Change", y = "Frequency", title = "Histogram of Shell stock price changes") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())


#HISTOGRAM RESIDUALS
ggplot(regr, aes(x=regr$residuals, color = df$C_SHEL)) + geom_histogram(color = "#192594", fill = "#4fa2e3", binwidth = 0.4) +
  labs(x = "Residuals", y = "Frequency", title = "Residuals on regression on % changes") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  xlim(-10,10)

ggplot(regr2, aes(regr2$residuals, color = df$P_SHEL)) + geom_histogram(color = "#192594", fill = "#4fa2e3", binwidth = 0.4) +
  labs(x = "Residuals", y = "Frequency", title = "Residuals on regression on prices") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  xlim(-10,10)


#plot(df$C_SHEL, predict(regr)[seq(1, 994)], xlim=c(-2.5,2.5), ylim=c(-15,15), col = "blue", alpha = 0.5)
#par(new=TRUE)
#plot(df$C_SHEL, predict(both)[seq(1, 994)], xlim=c(-2.5,2.5), ylim=c(-15,15), col = "red")


#PLOT OF THE 2 DIFFERENT REGRESSIONS
ggplot(df[seq(200,900),], aes(df$C_SHEL[seq(200,900)])) +
  geom_point(aes(df$C_SHEL[seq(200,900)], predict(regr)[seq(200,900)]), size = 1.5, alpha = 0.8, color = "#ff6666", shape=1) +
  geom_point(aes(df$C_SHEL[seq(200,900)], predict(both)[seq(200,900)]), size = 1.5, alpha = 0.8, color="#6666ff", shape=1) +
  theme_fivethirtyeight() +
  geom_abline(aes(intercept = 0, slope = 1), color = "#88dd88") +
  labs(title = "Difference between regressions", x = "", y = "") +
  theme(axis.title = element_text()) +
  xlim(-7,7) + ylim(-6,6)

ggplot(df[seq(200,900),], aes(df$P_SHEL[seq(200,900)])) +
  geom_point(aes(df$P_SHEL[seq(200,900)], predict(both2)[seq(200,900)]), size = 1.5, alpha = 0.8, color="#6666ff", shape=1) +
  theme_fivethirtyeight() +
  geom_abline(aes(intercept = 0, slope = 1), color = "#88dd88") +
  labs(title = "Difference between regressions", x = "", y = "") +
  theme(axis.title = element_text())

#PRICE OF SHELL OVER TIME
ggplot(df, aes(x=as.numeric(row.names(df)), y=df$P_SHEL)) + geom_line(color = "#4fa2e3") +
  labs(title = "SHEL price over time", x = "Index", y = "Price") +
  theme_fivethirtyeight() +
  ylim(0,70)
