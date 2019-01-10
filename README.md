# Instacart-Market-Basket-Analysis
Using R Apriori Algorithm for purchasing association analysis


install.packages("readr")
install.packages("arules")
install.packages("RColorBrewer")
install.packages("arulesViz")
install.packages("grid")
library(readr)
library(arules)
library(dplyr)
library(RColorBrewer)
library(grid)
library(arulesViz)

order_products__prior <- read_csv("order_products__prior.csv")
products <- read_csv("products.csv")

#use only the first 50000 records in the analysis
mydata <- order_products__prior[1:50000,1:2]
mydatamerge <- merge(mydata, products, by= "product_id")
mydataarrange <- arrange(mydatamerge, order_id)
mydatafinal<- mydataarrange[,c(2,3)]


#splitting the data
dt <- split(mydatafinal$product_name, mydatafinal$order_id)

#converting data to a class of transactions
dt2 = as(dt, "transactions")
summary(dt2)

#itemfrequency
itemFrequency(dt2, type = "relative")

#plotting
itemFrequencyPlot(dt2, topN=5, col= brewer.pal(8, 'Pastel2'), type="absolute")

#rules
rules = apriori(dt2, parameter = list(support = 0.001, confidence = 0.8, minlen = 3))
plot(rules, control=list(col=brewer.pal(11, "Spectral")), main = "")
plot(rules, method="grouped", control=list(type= "items", main=""))
plot(rules, method="graph", control=list(type= "items", main=""))


#look at rules
options(digits = 2)
rules<- sort(rules, by="lift", decreasing = TRUE)
inspect(head(sort(rules, by = "lift"), 10))

#Get summary information
summary(rules)

#converting rules into the omnipotent data frame
rules1 = as(rules, "data.frame")
inspect(subset(rules, subset = rhs %pin% "Banana"))
inspect(subset(rules, subset = rhs %pin% "Banana"), by = "lift")
plot(subset(rules, subset = rhs %pin% "Banana"),method="grouped", control=list(type= "items", main=""))
plot(subset(rules, subset = rhs %pin% "Banana"),method="graph", control=list(type= "items", main=""))
plot(subset(rules, subset = rhs %pin% "Banana"),method="paracoord", control=list(type= "items", main=""))
plot(subset(rules, subset = rhs %pin% "Banana"), control=list(col=brewer.pal(11, "Spectral")), main = "")

#subset only the first 10 rules by lift
subrules <- head(sort(rules, by="lift"), 10)
plot(subrules, method="graph", control=list(type= "items", main=""))


#What are customers likely to buy before they purchase "Banana"
rules1 <- apriori(data = dt, parameter = list(supp= 0.001, conf= 0.8), appearance = list(default="lhs", rhs="Banana"), control = list(verbose=F))
rules1 <-sort(rules, decreasing = TRUE, by="confidence")
inspect(rules1[1:10])

#Scatter plot of rules
plot(rules1, control = list(col=brewer.pal(11,"Spectral")), main="", jitter=0)


#What are customers likely to buy before they purchase "Organic Banana"
rules2 <- apriori(data = dt, parameter = list(supp= 0.001, conf= 0.8), appearance = list(default="lhs", rhs="Bag of Organic Bananas"), control = list(verbose=F))
rules2 <-sort(rules2, decreasing = TRUE, by="confidence")
inspect(rules2[1:5])

#Scatter plot of rules
plot(rules2, control = list(col=brewer.pal(11,"Spectral")), main="", jitter=0)
plot(subset(rules, subset = rhs %pin% "Bag of Organic Bananas"),method="paracoord", control=list(type= "items", main=""))

