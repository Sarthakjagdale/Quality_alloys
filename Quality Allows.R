#####################################################
### Created: 11/22/2019
### Author : Jean Philippe Matondo,Kuan-Chieh Chen, Marco Antonio Gutierrez Armas,
### Sarthak Jagdale,Shreenidhi Kotwal
###
### Web Analytics at Quality Alloys, Inc.Team 11
#####################################################
library(readxl)
library(data.table)
library(ggplot2)
library(plotly)
library(moments)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(lmtest)
library(rsq)
#####################################################
### 1. Import Datasets
#####################################################
full <- read_excel("Web Analytics Case Student Spreadsheet Final.xls",sheet = "Full")
head(full)
initial <- read_excel("Web Analytics Case Student Spreadsheet Final.xls",sheet = "Initial ")
head(initial)
promo <- read_excel("Web Analytics Case Student Spreadsheet Final.xls",sheet = "PROMO")
head(promo)
pre <- read_excel("Web Analytics Case Student Spreadsheet Final.xls",sheet = "PRE")
head(pre)
post <- read_excel("Web Analytics Case Student Spreadsheet Final.xls",sheet = "POST")
head(post)
######################################################
weekly_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet = "Weekly Visits",
                            range = "A5:H71")
weekly_visits
fin <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet = "Financials", range = "A5:E71")
fin
lbs <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet = "Lbs. Sold", range = "A5:B295")
lbs
visits <- read_excel("Web Analytics Case Student Spreadsheet.xls",sheet = "Daily Visits", range =
                       "A5:B467")
visits
######################################################
### 2. Overview of where we are !!!
######################################################
# Unique Visits
graph_1 <- ggplot(weekly_visits, aes(`Week (2008-2009)`, `Unique Visits`)) +
  geom_bar(stat="identity", width = 0.9, fill="tomato1") +
  labs(title="Unique Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
graph_1
# Revenue
graph_2 <- ggplot(fin, aes(`Week (2008-2009)`, `Revenue`))+
  geom_bar(stat="identity", width = 0.5, fill="Blue") +
  labs(title="Revenue Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
graph_2
# Profit
graph_3 <- ggplot(fin, aes(`Week (2008-2009)`, `Profit`)) +
  geom_bar(stat="identity", width = 0.5, fill="Green") +
  labs(title="Profit Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
graph_3
#Lbs. Sold
graph_4 <- ggplot(fin, aes(`Week (2008-2009)`, `Lbs. Sold`)) +
  geom_bar(stat="identity", width = 0.5, fill="yellow") +
  labs(title="Revenue Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
graph_4
####################################################
### 3. summary statistic with mean
####################################################
#### Mean Revenue
ini_rev<- mean(initial$Revenue)
pre_rev<- mean(pre$Revenue)
promo_rev<- mean(promo$Revenue)
post_rev<- mean(post$Revenue)
Revenue <- c(ini_rev,pre_rev,promo_rev,post_rev)
Revenue
#### Mean Profit
ini_profit<- mean(initial$Profit)
pre_profit<- mean(pre$Profit)
promo_profit<- mean(promo$Profit)
post_profit<- mean(post$Profit)
profit <- c(ini_profit, pre_profit, promo_profit, post_profit)
profit
#### Mean visit
ini_visit<- mean(initial$Visits)
pre_visit<- mean(pre$Visits)
promo_visit<- mean(promo$Visits)
post_visit<- mean(post$Visits)
visit<- c(ini_visit, pre_visit, promo_visit, post_visit)
visit
#### Mean Post
ini_lbs<- mean(initial$Lbs.Sold)
pre_lbs<- mean(pre$Lbs.Sold)
promo_lbs<- mean(promo$Lbs.Sold)
post_lbs<- mean(post$Lbs.Sold)
LBS <- c(ini_lbs, pre_lbs, promo_lbs, post_lbs)
LBS
##### Total mean in data frame for different periods
table_1 <-data.frame(Revenue,profit,visit,LBS)
table_1
#############################################
#### 4.Analysis
############################################
barplot(Revenue,
        main = "REVENUE",
        names.arg = c("INITIAL","PRE-PROMO","PROMO","POST-PROMO"),
        ylab = "Thousands",
        col =heat.colors(6),
        horiz = FALSE)
barplot(profit,
        main = "PROFITS",
        names.arg = c("INITIAL","PRE-PROMO","PROMO","POST-PROMO"),
        col = heat.colors(7),#c("darkgreen","darkorange", "red", "darkred" ),
        horiz = FALSE)
barplot(visit,
        main = "Number of Visits",
        names.arg = c("INITIAL","PRE-PROMO","PROMO","POST-PROMO"),
        col = c("red","darkred", "darkgreen", "red"),
        horiz = FALSE)
barplot(LBS,
        main = "LBS Sold",
        names.arg = c("INITIAL","PRE-PROMO","PROMO","POST-PROMO"),
        ylim = c(0,20000),
        col = c("darkgreen","darkgreen", "darkorange", "darkred"),
        horiz = FALSE)
### bounce rate
initial_brate<- initial$Bounce_Rate
pre_brate<- pre$Bounce_Rate
post_brate<- post$Bounce_Rate
promo_brate<- promo$Bounce_Rate
### Bounce rate
boxplot(initial_brate,pre_brate,promo_brate,post_brate,
        main = "Bounce rate comparision",
        at = c(1,2,4,5),
        names = c("Initial", "Pre", "Promo", "Post"),
        las = 2,
        col = c("Blue","Green","Orange","Red"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
### LBS solde to profit
ggplot(full, aes(x=Lbs.Sold, y=Profit)) +
  geom_point(size=2, colour="blue")+
  geom_smooth(method = "lm",color="red",
              formula = y ~ x, parse= T)+geom_point()
### visit to Revenue
ggplot(full, aes(x=Visits, y=Revenue)) +
  geom_point(size=2, colour="blue")+
  geom_smooth(method = "lm",color="red",
              formula = y ~ x, parse= T)+geom_point()
### visit to LBS
ggplot(full, aes(x=Visits, y=Lbs.Sold)) +
  geom_point(size=2, colour="blue")+
  geom_smooth(method = "lm",color="red",
              formula = y ~ x, parse= T)+geom_point()
##############################################
### 5. Statistical summary
#############################################
data.frame(unclass(summary(initial)), check.names = FALSE, stringsAsFactors = FALSE)
data.frame(unclass(summary(pre)), check.names = FALSE, stringsAsFactors = FALSE)
data.frame(unclass(summary(promo)), check.names = FALSE, stringsAsFactors = FALSE)
data.frame(unclass(summary(post)), check.names = FALSE, stringsAsFactors = FALSE)