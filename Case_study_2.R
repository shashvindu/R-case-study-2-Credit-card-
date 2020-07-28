install.packages("dplyr")
library(dplyr)

#Q1
#a

z1 = read.csv("F:\\Study related\\AnalytixLabs\\R\\Case Study\\R case study 2 (Credit card)\\Customer Acqusition.csv")
mean(z1$Age)

z1$Age[z1$Age<18] = mean(z1$Age)

#b
z2 = read.csv("F:\\Study related\\AnalytixLabs\\R\\Case Study\\R case study 2 (Credit card)\\spend.csv")

z3 = read.csv("F:\\Study related\\AnalytixLabs\\R\\Case Study\\R case study 2 (Credit card)\\Repayment.csv")

z4 = left_join(z1,z2,by = "Customer")

z = left_join(z4,z3, by= "Customer")

z = rename(z, "spend_amt" = "Amount.x")
z = rename(z, "spend_month" = "Month.x")
z = rename(z, "Repayment_amt" = "Amount.y")
z = rename(z, "Repayment_month" = "Month.y")

z$spend_amt = ifelse(z$spend_amt > z$Limit, .5*z$Limit,z$spend_amt)

#c

z$Repayment_amt = ifelse(z$Repayment_amt > z$Limit, z$Limit,z$Repayment_amt)

#----------------------------------------------------------------------------------------------------

#Q2
#a

length(unique(z$Customer))

#b

length(unique(z$Type))

#c

z_gp = z%>%
  group_by(Customer)%>%
  summarise(Avg_monthly_spent = mean(spend_amt))

#d

z_gp1 = z%>%
  group_by(Customer)%>%
  summarise(Avg_monthly_repayment_amt = mean(Repayment_amt))

#e

z$Monthly_Profit = z$Repayment_amt - z$spend_amt

Profit = z%>%
         filter(Monthly_Profit>0)%>%
        mutate(v=0.029*Monthly_Profit)
               

z10 = filter(z,Monthly_Profit>0)

z10$Profit = z10$Monthly_Profit * 0.029


#months(as.Date(z$Repayment_month, format = '%d- %b-%y'))

#f

q2 = z10 %>%
  group_by(Product, Type) %>%
  summarise(Total_earned_Profit = sum(Profit)) %>%
  head(arrange(desc(Total_earned_Profit)), n=5)

#g

q3 = z10 %>%
  group_by(City) %>%
  summarise(City_wise_spend_amt = sum(spend_amt)) %>%
  head(arrange(desc(City_wise_spend_amt)), n =1)

#h

z10$Age_Group = ifelse(z10$Age<28, "Under 28", ifelse(z10$Age<38,"Under 38",ifelse(z10$Age<48, "Under 48",
                       ifelse(z10$Age<58, "Under 58",ifelse(z10$Age<68, "Under 68", ifelse(z10$Age<78, "Under 78","Above 78"))))))
  
q4 = z10 %>%
  group_by(Age_Group) %>%
  summarise(Age_grp_wise_amt_spent = sum(spend_amt)) %>%
  arrange(desc(Age_grp_wise_amt_spent)) %>%
  head(Age_grp_wise_amt_spent, n=5)
  

#i

q5 = z10 %>%
  group_by(Customer) %>%
  summarise(Total_repayment = sum(Repayment_amt)) %>%
  arrange(desc(Total_repayment)) %>%
  head(Total_repayment, n = 10)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Q3

install.packages("lubridate")
library(lubridate)

options(scipen=777)

y1 = as.Date(z$spend_month, format = '%d- %b-%y')

y = z %>%
    group_by(City,Product,Year =  year(as.Date(spend_month, format = '%d- %b-%y')))%>%
    summarise(Spending_summary = sum(spend_amt))

class(z$spend_month)


library(ggplot2)


ggplot(y , aes(x = City, y = Spending_summary)) + geom_bar(stat= "identity", position= "dodge") + facet_wrap(~Product)
#------------------------------------------------------------------------------------------------------------------------------

#Q5

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

Final(z10,'Gold','monthly')
    






















































  
  


















  
































