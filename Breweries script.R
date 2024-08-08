##PROFIT ANALYSIS

##KEY PERFROMANCE INDEX
#1) Profit worth of breweries for both territories.
#2) Compare the total Profit between the two territories.
#3) Country with the highest profit in 2019.
#4) Year with the highest profit.
#5) Month in the three years with the least profit generated.
#6) Minimum profit in the month of December 2018.
#7) Compare the profit in percentage for each month in 2019( Using a pie chart).
#8) Brand that generated the highest profit in Senegal.


##Importing data into RStudio
library(readxl)
breweries<-read_excel("C:\\Users\\NNADI C FAITHFUL\\Desktop\\International-Breweries.xlsx")
View(breweries)

##CALCULATING TOTAL PROFIT
Total_Profit<-sum(breweries$PROFIT)
View(Total_Profit)


##COMPARING THE PROFIT OF THE TWO TERRITORIES
library(dplyr)
anglophone<-breweries%>%group_by(COUNTRIES)%>%filter(COUNTRIES==c("Ghana","Nigeria"))%>%summarise(sum(PROFIT))
anglophone
anglophone_c<-anglophone%>%summarise(`sum(PROFIT)`)
anglophone_c

francophone<-breweries%>%group_by(COUNTRIES)%>%filter(COUNTRIES==c("Togo","Senegal","Benin"))%>%summarise(sum(PROFIT))
francophone
francophone_c<-francophone%>%summarise(`sum(PROFIT)`)

compareAF<-rbind(anglophone_c,francophone_c)
compareAF<-cbind(compareAF,Territory=c("anglophone","francophone"))
comapareAF

##COUNTRY THAT GENERATED THE HIGHEST PROFIT IN 2019
Highest_Profit_2019<-breweries%>%group_by(COUNTRIES)%>%filter(YEARS==2019)%>%summarise(PROFIT)
Highest_Profit_2019


##YEAR WITH THE HIGHEST PROFIT
library(dplyr)
highest_year<-breweries%>%group_by(YEARS)%>%summarise(max(PROFIT))
highest_year

##MONTH WITH THE LEAST PROFIT
Month_least_profit<-breweries%>%group_by(MONTHS)%>%summarise(min(PROFIT))
Month_least_profit

##LEAST PROFIT IN THE MONTH OF DECEMBER 2018
Dec_least_profit<-breweries%>%filter(MONTHS=="December" & YEARS==2018)%>%summarise(min(PROFIT))
Dec_least_profit


##PROFIT IN PERCENTAGE OF EACH MONTH IN 2019
library(dplyr)
Profit_2019<-breweries%>%group_by(MONTHS)%>%filter(YEARS==2019)%>%summarise(sum(PROFIT))
Profit_2019
Colours<-c("red","blue","purple","pink","orange","black","white","brown","yellow","green","gray","magenta")
Percent_2019<-Profit_2019$`sum(PROFIT)`/sum(Profit_2019$`sum(PROFIT)`)
Percent_2019<-Percent_2019*100
Percent_2019
Percent_2019<-round(Percent_2019,2)
Percent_2019

pie(Profit_2019$`sum(PROFIT)`,main = "Profit in 2019", col = Colours,labels = Percent_2019)


##BRAND THAT GENERATED THE HIGHEST PROFIT IN SENEGAL
library(dplyr)
Brand_Profit<-breweries%>%group_by(BRANDS)%>%filter(COUNTRIES == "Senegal")%>%summarise(sum(PROFIT))
Brand_Profit
```