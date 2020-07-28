remove(list = ls())
Customer_1=read.csv("D:\\download\\1.3 - R Case Studies (All 3 case studies for Foundation R)\\R case study 2 (Credit card)\\Customer Acqusition.csv",stringsAsFactors =F)
spend=read.csv("D:\\download\\1.3 - R Case Studies (All 3 case studies for Foundation R)\\R case study 2 (Credit card)\\spend.csv",stringsAsFactors =F)
Repayment=read.csv("D:\\download\\1.3 - R Case Studies (All 3 case studies for Foundation R)\\R case study 2 (Credit card)\\Repayment.csv",stringsAsFactors =F)
names(Customer_1)
names(spend)
names(Repayment)
str(Customer_1)
str(spend)
str(Repayment)

names(Customer_1)
names(Repayment)
names(spend)
require(dplyr)   #(dplyr)
#install.packages("lubridate",dependencies = T) 
require(lubridate)
require(data.table)
Cust_spend=left_join(Customer_1,spend,by="Customer")
View(Cust_spend)
Cust_spend_Repay=left_join(Cust_spend,Repayment,by="Customer")
View(Cust_spend_Repay)
names(Cust_spend_Repay)

# 1.	In the above dataset,
# a.	Incase age is less than 18, replace it with mean of age value
Cust_spend_Repay$Age[Cust_spend_Repay$Age<18]=mean(Cust_spend_Repay$Age)
names(Cust_spend_Repay)
Cust_spend_Repay["X"]=NULL
# b.	Incase spend amount is more than the limit, replace it with 50% of that customer's limit. 
# (customer's limit provided in acquisition table is the per transaction limit on his card)
Cust_spend_Repay$Amount.x[Cust_spend_Repay$Amount.x>Cust_spend_Repay$Limit]=Cust_spend_Repay$Limit*0.5
# c.	Incase the repayment amount is more than the limit, replace the repayment with the limit

Cust_spend_Repay$Amount.y[Cust_spend_Repay$Amount.y>Cust_spend_Repay$Limit]=Cust_spend_Repay$Limit

# #=================================================================================
# 
# 2.	From the above dataset create the following summaries:
#   a.	How many distinct customers exist?
distinct(Cust_spend_Repay,Customer)
#   b.	How many distinct categories exist?
distinct(Cust_spend_Repay,Type)
str(Cust_spend_Repay)
View(Cust_spend_Repay)
names(Cust_spend_Repay)
#   c.	What is the average monthly spend by customers?
    # %B %d %Y,12-Jan-04
Cust_spend_Repay$Month.y=as.Date(Cust_spend_Repay$Month.y,format="%d-%b-%y")
Cust_spend_Repay$Month.x=as.Date(Cust_spend_Repay$Month.x,format="%d-%b-%y")
min(Cust_spend_Repay$Month.x)
max(Cust_spend_Repay$Month.x)
max(Cust_spend_Repay$Month.x)-min(Cust_spend_Repay$Month.x)


time_length(difftime( max(Cust_spend_Repay$Month.x),
                      min(Cust_spend_Repay$Month.x)), "month")


sum(Cust_spend_Repay$Amount.x)/(
  time_length(difftime( max(Cust_spend_Repay$Month.x),
                        min(Cust_spend_Repay$Month.x)), "month"))
#-------------------------------------------------------------------------------
spend$Month <- as.Date(spend$Month,format= "%d-%b-%y")
spend %>%group_by(Customer,month = lubridate::month(Month),year = lubridate::year(Month)) %>% 
  summarize(month_average = mean(Amount))
#-----------------------------------------------------------------------
spend$Month_Year<-format(as.Date(spend$Month),"%Y-%m")       #Important
AverageMonthlySpend <- spend %>% group_by(Customer,Month_Year) %>% dplyr::summarise(MonthlySpend=sum(Amount))
#   d.	What is the average monthly repayment by customers?

time_length(difftime( max(Cust_spend_Repay$Month.y),
                      min(Cust_spend_Repay$Month.y)  ), "month")

sum(Cust_spend_Repay$Amount.y)/(
  time_length(difftime( max(Cust_spend_Repay$Month.y),
                        min(Cust_spend_Repay$Month.y)), "month"))
#-----------------------------------------------------------------------------
Repayment$Month <- as.Date(Repayment$Month,format="%d-%b-%y")
Repayment$Month_Year <- format(as.Date(Repayment$Month),"%Y-%m")
AverageMonthlyRepayment <- Repayment %>% group_by(Customer,Month_Year) %>% dplyr::summarise(MonthlyRepayment=sum(Amount))
AverageMonthlyRepayment
#   e.	If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?
#(Profit is defined as interest earned on Monthly Profit. Monthly Profit = Monthly repayment - Monthly spend. 
#Interest is earned only on positive profits and not on negative amounts)
spend$MonthNAME <- lubridate::month(spend$Month,label=T,abbr=T)
spend$Year <- lubridate::year(spend$Month)
MonthlySpend <- spend %>% group_by(Month_Year) %>% dplyr::summarise(TotalSpend=sum(Amount))
(MonthlySpend$Profit <- (MonthlySpend$TotalSpend*0.029))
#----------------------------------------------------------------------------------
rep_amt <- Repayment %>%group_by(Customer,month = lubridate::month(Month),year = lubridate::year(Month),Amount)
spend_amt <- spend %>%group_by(Customer,month = lubridate::month(Month),year = lubridate::year(Month),Amount)
Repayment$bank_monthly_profit = rep_amt$Amount - spend_amt$Amount
Repayment$intrest_bank_earned <- ifelse(Repayment$bank_monthly_profit >
                                          0,round((Repayment$bank_monthly_profit*2.9)/100,digits = 2),0)
# f.	What are the top 5 product types?
spend%>%group_by(Type)%>%summarize(Frequency = n())%>%dplyr::arrange(desc(Frequency))
head(spend%>%group_by(Type)%>%summarize(Frequency = n())%>%dplyr::arrange(desc(Frequency)),5)
#   g.	Which city is having maximum spend?
Cust_spend %>% dplyr::group_by(City) %>% 
  dplyr::summarize(Sum_TS = sum(Amount))
#   h.	Which age group is spending more money?
Cust_spend$Age_Group <- ifelse(Cust_spend$Age < 30,"Young Age" , ifelse(Cust_spend$Age > 30 &
                                                                          Cust_spend$Age < 55 ,"Middle Age","Old age"  ))
Cust_spend%>% dplyr::group_by(Age_Group) %>% 
  dplyr::summarize(Sum_TS = sum(Amount))
#   i.	Who are the top 10 customers in terms of repayment?
  
top10_repayment <- 	Repayment %>% dplyr::group_by(Customer) %>% 
  dplyr::summarize(Tot_repayment =sum(Amount))
head(dplyr::arrange(top10_repayment,desc(top10_repayment$Tot_repayment)),10)
#3.Calculate the city wise spend on each product on yearly basis. 
#Also include a graphical representation for the same.
library(ggplot2)
q3 <- Cust_spend %>% dplyr::group_by(City,product = Cust_spend$Product, year = spend_amt$year) %>%dplyr::summarize(Tot_spend =sum(Amount))
ggplot(q3,aes(x = City, y = Tot_spend, color = product)) + 
  geom_point()+facet_grid(year~.)
#q.4.a--graph for city wise month wise amount spend
ggplot(spend_amt) +geom_bar(aes(x =  Cust_spend$City, fill = spend_amt$month))
ggplot(spend_amt) + aes(x = month, y = spend_amt$Amount, fill =  Cust_spend$City) + 
  geom_bar(stat = "identity",position = "stack")
#q.4.b--yearly spend on air tickets
q4b <- spend_amt %>% dplyr::group_by(Type ,year) %>%dplyr::summarize(Tot_spend =sum(Amount))
q4b_graph <- q4b[q4b$Type == "AIR TICKET",c("year","Tot_spend")]
ggplot(q4b_graph) +geom_col(aes(x = year, y = Tot_spend))
#q.4.c--comparing monthly spend for each products
Cust_spend$Amount=round(Cust_spend$Amount)
ggplot2::ggplot(Cust_spend) +aes(x = Cust_spend$Product,  y = spend_amt$month)+
  geom_col(color = "indianred")
#q.5-- 
z10$Repayment_month
class(z10$Repayment_month)
z10$Repayment_month =  as.Date(z10$Repayment_month, format = '%d - %b - %y')


Final = function(data, Product, Time)
{
  if(Time == 'Monthly' | Time == 'monthly')
  {
    p = z10[z10$Product== Product,] %>%
      group_by(Customer,City, month(Repayment_month)) %>%
      summarise(TotalPayments = sum(Repayment_amt))
    
    top_10 = top_n(p,10)
    
  }
  
  else if (Time == 'Yearly' | Time == 'yearly')
  {
    p = z10[z10$Product== Product,] %>%
      group_by(Customer,City, year(Repayment_month)) %>%
      summarise(TotalPayments = sum(Repayment_amt))
    
    top_10 = top_n(p,10)
  }
  
  else{
    stop(paste("The Time period should be Yearly or Monthly, instead found", Time, "instead"))
    
  }
  return(top_10)
}









