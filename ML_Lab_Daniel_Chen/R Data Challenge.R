
# For this R review section, we mainly focus on data cleaning and data visualizations.
# For problems 2 and 3, you should mainly use **dplyr** and **ggplot2** to construct some 
# plots and also provide **brief interpretations** about your findings.

## Problem 1: Dataset Import & Cleaning
# The data comes from a global company, including orders from 2012 to 2015. Import the dataset **Order** and do some basic EDA. 
# Check **"Profit"** and **"Sales"** in the dataset, convert these two columns to numeric data.

#############
library(data.table)
#setwd("C:/Users/asus/Desktop/Python/R")
#order = read.csv(file= "R Data Challenge I/Orders.csv")

order = fread(input = "R Data Challenge I/Orders.csv", header = TRUE,stringsAsFactors=FALSE) #data.table
View(order)

#select profit and sales columns
order1 <- order[, .(Sales, Profit)]

# order1$Sales <- as.numeric(gsub("[$,]","",order1$Sales))
# order1$Profit <- as.numeric(gsub("[$,]","",order1$Profit))

order1[,c("Sales", "Profit"):= list(as.numeric(gsub("[$,]","",Sales)), as.numeric(gsub("[$,]","",Profit)))] #remove $ and ,

View(order1)
class(order1$Sales)
class(order1$Profit)

#slides:

number.cleaner = function(x){
  word = unlist(strsplit(x, split='$', fixed = TRUE))
  word = paste0(word[1], word[2])
  word = as.numeric(gsub(",","",word))
}

order$New_sales = sapply(order$Sales, number.cleaner)
order$New_profit = sapply(order$Profit, number.cleaner)

#############

## Problem 2: Inventory Management
# Retailers that depend on seasonal shoppers have a particularly challenging 
# job when it comes to inventory management. Your manager is making plans for next year's inventory.
# He wants you to answer the following questions:
# 1. Is there any seasonal sales trend in your company? 
# 2. Is there any seasonal trend of **different categories ** of products?

# **Note:** Each order has a column called Quantity.

#############
library(dplyr)
library(ggplot2)
order = fread(input = "R Data Challenge I/Orders.csv", header = TRUE,stringsAsFactors=FALSE)

order1 <- order[, .(Order.Date, Category, Quantity, Sub.Category)]
order1[,c("Order.Date") := list(as.Date(Order.Date, "%m/%d/%y"))] # use as.Date to set order date 
order1[,c("Order.Month") := list(month(Order.Date))] # use month() to get order month , 
                                                     # order$Order.Month <- format(order$Order.Date, "%m")

order2 = order1 %>% group_by(Order.Month, Category) %>% summarize(Quantity = sum(Quantity)) # group by and summarize
#order5 = order2 %>% group_by(Order.Month) %>% summarize(Quantity = sum(Quantity)) # group by and summarize

View(order3) 
#plot 
ggplot(order2, aes(Order.Month, Quantity)) + geom_point(aes(col= Category)) #scatter plot
ggplot(order2, aes(Order.Month, Quantity)) + geom_bar(aes(fill= Category), stat="identity", position="dodge")

#there seems to be an increase in purchases in different categories as the year progresses
#taking a closer look at sub categories might explain why 
order3 = order1 %>% group_by(Order.Month, Sub.Category) %>% summarize(Quantity = sum(Quantity)) # group by and summarize
ggplot(order3, aes(Order.Month, Quantity)) + geom_point(aes(col= Sub.Category)) #scatter plot

library(googleVis)
ord = order2[,c("Order.Month", "Quantity")]
ord$Furniture = ifelse(order2$Category == "Furniture", order2$Quantity, NA)
ord$Office_Supplies = ifelse(order2$Category == "Office Supplies", order2$Quantity, NA)
ord$Technology = ifelse(order2$Category == "Technology", order2$Quantity, NA)

ord$Quantity = NULL
scatter= gvisScatterChart(ord) #[,c("Order.Month", "Quantity")])
plot(scatter)
#
my_options <- list(width="1200px", height="600px",       # with options
                   title="Seasonal Sales Trends",
                   hAxis="{title:'Month'}",
                   vAxis="{title:'Quantity'}")
#with actions
my_options$explorer <- "{actions:['dragToZoom', 'rightClickToReset']}"
plot(gvisScatterChart(ord,options=my_options))

#furniture and technology have a close correlation
#overall a rise in quantity affects all categories 

#slides:

colMeans(order[,c("Quantity", "New_sales", "New_profit")])
#  Quantity  New_sales New_profit 
#  3.476545 246.490685  28.610970

colSums(order[,c("Quantity", "New_sales", "New_profit")])
#  Quantity  New_sales New_profit 
#  178312   12642507    1467457 

order$Order.Date = as.Date(order$Order.Date, "%m/%d/%y") # use as.Date to set order date 
order$Order.Month = format(order$Order.Date, "%m")
time <- order %>% 
  group_by(Order.Date) %>%
  summarize(daily_quantities = sum(Quantity),
            daily_sales = sum(New_sales),
            daily_profit = sum(New_profit))

ggplot(time, aes(Order.Date, daily_quantities)) + geom_line() + xlab("Time") + ylab("Daily Orders Quantity") + theme_bw() + geom_smooth()

ggplot(time, aes(Order.Date, daily_profit)) + geom_line() + xlab("Time") + ylab("Daily Orders Quantity") + theme_bw() + geom_smooth()

#graph the 12 months
order$Order.Month = format(order$Order.Date, "%m")
month <- order %>% 
  group_by(Order.Month) %>%
  summarize(monthly_quantities = sum(Quantity),
            monthly_sales = sum(New_sales),
            monthly_profit = sum(New_profit))
ggplot(month, aes(Order.Month, monthly_quantities, group = 1)) + geom_line() + xlab("Time") + ylab("Monthly Orders Quantity") + theme_bw()


#############

## Problem 3: Why did customers make returns?
# Your manager required you to give a brief report (**Plots + Interpretations**) on returned orders from the **Returns** dataset.
# 1. How much profit did we lose for each year?
# 2. How many customer returned more than once? more than 10 times? 
# 3. Which regions are more likely to return orders?
# 4. Which categories (sub-categories) of products are more likely to be returned?

# *Hint*: 
# 1. Import **Returns.csv**
# 2. Merge the **Returns** dataframe you imported with the **Orders** dataframe.

#############
ordera = fread(input = "R Data Challenge I/Orders.csv", header = TRUE,stringsAsFactors=FALSE)
returnsa = fread(input = "R Data Challenge I/Returns.csv", header = TRUE,stringsAsFactors=FALSE)

#question 1
#merge
returnsa1 = merge(ordera, returnsa, by.x = "Order.ID", by.y = "Order ID") 
#grab needed col
ret = returnsa1[,c("Order.Date", "Profit", "Customer.ID", "Region.x", "Category", "Sub.Category")]

#revise date and set profit to numeric
ret[,c("Profit"):= list(as.numeric(gsub("[$,]","",Profit)))] #remove $ and ,
ret[,c("Order.Date") := list(as.Date(Order.Date, "%m/%d/%y"))] # use as.Date to set order date 
ret[,c("Order.Year") := list(year(Order.Date))] # order year 
ret[,c("LostProfit") := list(ifelse(Profit < 0, as.numeric(Profit), 0))]  #lost profit 
ret2= ret %>% group_by(Order.Year) %>% summarize(LostProfit = sum(LostProfit))  

#ret[,c("Lost Profit") := NULL] #clean

#question 2
ret = returnsa1[,c("Order.Date", "Profit", "Customer.ID", "Region.x", "Category", "Sub.Category")]
ret2= ret %>% group_by(Customer.ID) %>% summarize(returns = n())  #all customers that returned 
#more than once
count(ret2[ret2$returns > 10,])

View(ret2)
?summarise
class(ret2)

#question 3
ret = returnsa1[,c("Order.Date", "Profit", "Customer.ID", "Region.x", "Category", "Sub.Category")]
ret2= ret %>% group_by(Region.x) %>% summarize(returns = n()) %>% arrange(desc(returns))  #all customers that returned 
head(ret2)

#question 4
ret = returnsa1[,c("Order.Date", "Profit", "Customer.ID", "Region.x", "Category", "Sub.Category")]
ret2= ret %>% group_by(Category) %>% summarize(returns = n()) %>% arrange(desc(returns))  #all customers that returned 
ret2
ret2= ret %>% group_by(Sub.Category) %>% summarize(returns = n()) %>% arrange(desc(returns))  #all customers that returned 

#slides:
library(lubridate)
library(DT)

ret3= ret %>% group_by(Customer.ID) %>% summarize(returns = n()) %>% group_by(returns) %>% summarize(num_of_cust = n()) 
ggplot(ret3, aes(x=as.factor(returns), y = num_of_cust)) + geom_bar(stat="identity", aes(fill=as.factor(returns)))



#############
# 1. How much profit did we lose for each year?  Order.Date Profit
#       2012     -5773.
#       2013     -7713.
#       2014     -7439.
#       2015    -15908.

# 2. How many customer returned more than once? more than 10 times? Customer.ID
# count(ret2[ret2$returns > 1,]) = 547
# count(ret2[ret2$returns > 10,]) = 2

# 3. Which regions are more likely to return orders? Region.x
# ret2= ret %>% group_by(Region.x) %>% summarize(returns = n()) %>% arrange(desc(returns)) 
# Central America       248
# Western Europe        233
# Western US            177
# Oceania               154
# Southeastern Asia     140
# Eastern US            134

# 4. Which categories (sub-categories) of products are more likely to be returned? Category, Sub.Category
# Office Supplies    1348
# Technology          445
# Furniture           427
#
# Binders          269
# Art              217
# Storage          212
# Paper            150
# Chairs           147
# Phones           145
# Accessories      138
# Labels           137
# Furnishings      135
# Bookcases        104
# Supplies         103
# Fasteners        102
# Copiers           99
# Envelopes         99
# Machines          63
# Appliances        59
# Tables            41























