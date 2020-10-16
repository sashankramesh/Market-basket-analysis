# 
# R code for AFS Project Term 2 Jan to Apr 2020
# Project - OCBC Customer data
# 
# ----------------------------------------------------
# Version & Date created: v0.1; 18/04/2020
# Author: Sarthak S
# Comments: To perform Merchant Affinity Analysis (Merchant Basket Analysis)
# 
# ----------------------------------------------------
# Edits to code - 
# Version & Date:
# Author:
# Comments:
# 
# ----------------------------------------------------
# Install & Define Libraries
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("readxml")
install.packages("openxlsx")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("dplyr")
install.packages("RColorBrewer")

library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(openxlsx)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

getwd()
setwd("C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules")
getwd()

# ----------------------------------------------------
# Flow - 
# 1 - Import i/p files
# 2 - Data transformation to usable format
# 3 - Create transaction dataset
# 4 - Perform Merchant affinity - (a) Transaction summary & (b) Rules mining
# 5 - Export result
# 6 - Visualisation of results
# 
# ----------------------------------------------------
# Start of Code proper
# ----------------------------------------------------
# 1 - Import Input Files required
MS_data <- read.csv("Master dataset v5.csv", header = TRUE, fill = TRUE)

# colnames(MS_data, do.NULL = TRUE, prefix = "col")
colnames(MS_data)[colnames(MS_data) %in% c("ï..PartyID")] <- 
  c("PartyID")
# colnames(MS_data, do.NULL = TRUE, prefix = "col")

# ----------------------------------------------------
# 2 - Data transformation to usable format
# Merchant Name
MS_data$New.M.Name = as.factor(MS_data$New.M.Name)
# Date
MS_data$Date <- as.Date(MS_data$Transaction_Date, "%d/%m/%Y")
# Transaction value
MS_data$Amount <- as.numeric(MS_data$Transaction_amount)

# Check data types
# glimpse(MS_data)

# Split dataset into parts for each customer cluster
unique(MS_data$Customer.Segment)

Cluster_Pot_Upsell <- subset(MS_data, Customer.Segment=="Potential Upsell")
Cluster_Pros_Churn <- subset(MS_data, Customer.Segment=="Prospective Churn")
Cluster_Low_Value <- subset(MS_data, Customer.Segment=="Low Value")
Cluster_Churn <- subset(MS_data, Customer.Segment=="Churn")
Cluster_Prem_Cust <- subset(MS_data, Customer.Segment=="Premium Customer")

# ----------------------------------------------------
# 3 - Create transaction dataset for each cluster
# Main
Transaction_Data<- ddply(MS_data, c("PartyID"),
                         function(MS_data)paste(MS_data$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_Data$PartyID <- NULL
#Transaction_Data$Date <- NULL
colnames(Transaction_Data)[colnames(Transaction_Data) == "V1"] <- "Items"
# glimpse(Transaction_Data)

# -------------
# Clusters - Potential Upsell
Transaction_PU<- ddply(Cluster_Pot_Upsell, c("PartyID"),
                         function(Cluster_Pot_Upsell)
                           paste(Cluster_Pot_Upsell$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_PU$PartyID <- NULL
#Transaction_PU$Date <- NULL
colnames(Transaction_PU)[colnames(Transaction_PU) == "V1"] <- "Items"
glimpse(Transaction_PU)

# -------------
# Clusters - Prospective Churn
Transaction_PC<- ddply(Cluster_Pros_Churn, c("PartyID"),
                       function(Cluster_Pros_Churn)
                         paste(Cluster_Pros_Churn$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_PC$PartyID <- NULL
#Transaction_PC$Date <- NULL
colnames(Transaction_PC)[colnames(Transaction_PC) == "V1"] <- "Items"
# glimpse(Transaction_PC)

# -------------
# Clusters - Low Value 
Transaction_LV<- ddply(Cluster_Low_Value, c("PartyID"),
                       function(Cluster_Low_Value)
                         paste(Cluster_Low_Value$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_LV$PartyID <- NULL
#Transaction_LV$Date <- NULL
colnames(Transaction_LV)[colnames(Transaction_LV) == "V1"] <- "Items"
# glimpse(Transaction_LV)

# -------------
# Clusters - Churn
Transaction_CH<- ddply(Cluster_Churn, c("PartyID"),
                       function(Cluster_Churn)
                         paste(Cluster_Churn$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_CH$PartyID <- NULL
#Transaction_CH$Date <- NULL
colnames(Transaction_CH)[colnames(Transaction_CH) == "V1"] <- "Items"
# glimpse(Transaction_CH)

# -------------
# Clusters - Premium Customer 
Transaction_PR<- ddply(Cluster_Prem_Cust, c("PartyID"),
                       function(Cluster_Prem_Cust)
                         paste(Cluster_Prem_Cust$New.M.Name, collapse = ","))

# Nullify columns - PartyID & Date; Rename cols
Transaction_PR$PartyID <- NULL
#Transaction_PR$Date <- NULL
colnames(Transaction_PR)[colnames(Transaction_PR) == "V1"] <- "Items"
# glimpse(Transaction_PR)

# -------------
# Export Transaction dataset
write.csv(Transaction_Data,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket.csv", quote = FALSE, row.names = FALSE)
write.csv(Transaction_PU,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PU.csv", quote = FALSE, row.names = FALSE)
write.csv(Transaction_PC,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PC.csv", quote = FALSE, row.names = FALSE)
write.csv(Transaction_LV,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_LV.csv", quote = FALSE, row.names = FALSE)
write.csv(Transaction_CH,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_CH.csv", quote = FALSE, row.names = FALSE)
write.csv(Transaction_PR,"C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PR.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket.csv', format = 'basket', sep=',')
tr_PU <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PU.csv', format = 'basket', sep=',')
tr_PC <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PC.csv', format = 'basket', sep=',')
tr_LV <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_LV.csv', format = 'basket', sep=',')
tr_CH <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_CH.csv', format = 'basket', sep=',')
tr_PR <- read.transactions('C:/Users/Sarthak/Downloads/SMU/Courses/Analytics in Financial Services-Merged Section/Project/MBA Rules/merchant_basket_PR.csv', format = 'basket', sep=',')

# ----------------------------------------------------
# 4 - Perform Merchant affinity - (a) Transaction summary & (b) Rules mining
# PU - Potential Upsell
# PC - Potential Churn
# LV - Low Value
# CH - Churn
# PR - Premium Customers
# (a) Transaction Summary
summary(tr)
summary(tr_PU)
summary(tr_PC)
summary(tr_LV)
summary(tr_CH)
summary(tr_PR)

# (b) Rules mining - Min Support as 0.01, confidence as 0.5
# Main (47 rules)
association.rules_tr <- apriori(tr, parameter = list(supp=0.01, conf=0.50,maxlen=10))
summary(association.rules_tr)
inspect(head(sort(association.rules_tr, by= "lift"), 20))

# Clusters - Potential Upsell (62 rules)
association.rules_tr_PU <- apriori(tr_PU, parameter = list(supp=0.01, conf=0.50,maxlen=10))
summary(association.rules_tr_PU)
inspect(head(sort(association.rules_tr_PU, by= "lift"), 20))

# Clusters - Prospective Churn (8 rules)
association.rules_tr_PC <- apriori(tr_PC, parameter = list(supp=0.01, conf=0.20,maxlen=10))
summary(association.rules_tr_PC)
inspect(head(sort(association.rules_tr_PC, by= "lift"), 20))

# Clusters - Low Value (93 rules)
association.rules_tr_LV <- apriori(tr_LV, parameter = list(supp=0.01, conf=0.50,maxlen=10))
summary(association.rules_tr_LV)
inspect(head(sort(association.rules_tr_LV, by= "lift"), 20))

# Clusters - Churn (4 rules)
association.rules_tr_CH <- apriori(tr_CH, parameter = list(supp=0.01, conf=0.10,maxlen=10))
summary(association.rules_tr_CH)
inspect(head(sort(association.rules_tr_CH, by= "lift"), 20))

# Clusters - Premium Customer (11 rules)
association.rules_tr_PR <- apriori(tr_PR, parameter = list(supp=0.05, conf=0.30,maxlen=10))
summary(association.rules_tr_PR)
inspect(head(sort(association.rules_tr_PR, by= "lift"), 20))

# ----------------------------------------------------
# 5 - Export result
# PU - Potential Upsell
# PC - Potential Churn
# LV - Low Value
# CH - Churn
# PR - Premium Customers

write(association.rules_tr, file = "Rules_overall.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

write(association.rules_tr_PU, file = "Rules_PU.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

write(association.rules_tr_PC, file = "Rules_PC.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

write(association.rules_tr_LV, file = "Rules_LV.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

write(association.rules_tr_CH, file = "Rules_CH.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

write(association.rules_tr_PR, file = "Rules_PR.csv", 
      sep = ",", quote = TRUE, row.names = FALSE)

# ----------------------------------------------------
# 6 - Visualisation of results
# PU - Potential Upsell
# PC - Potential Churn
# LV - Low Value
# CH - Churn
# PR - Premium Customers

# Interactive graph with plotly
plotly_arules(association.rules_tr)

subRules<-association.rules_tr[quality(association.rules_tr)$confidence>0.4]
# Scatterplot
plot(subRules)

# Two Key Plot
plot(subRules,method="two-key plot")

# Interactive scatter graph - with zoom, filtering and rule read options
plot(head(sort(association.rules_tr, by= "lift"), 20), 
     measure = c("support", "lift"), 
     shading = "confidence",interactive = TRUE)

# Flowchart connector graph
plot(head(sort(association.rules_tr, by= "lift"), 20), 
     method = "graph", control = list(type= "Items"))

# Interactive HTML graph
top10Rules <- head(association.rules_tr, n = 10, by = "confidence")
plot(top10Rules, method = "graph",  engine = "htmlwidget")

# Parallel Co-ordinates plot
Top20Rules_L<-head(association.rules_tr, n=20, by="lift")
plot(Top20Rules_L, method="paracoord")

# Using Merchant transaction/baseket data - freq plot
# For the top 20 Merchants, create Absolute & Relative Freq Plot
itemFrequencyPlot(tr, topN=20, type="absolute",
                  col=brewer.pal(8,'Pastel2'), 
                  main="Top 20 Merchants Absolute Freq Plot")

itemFrequencyPlot(tr, topN=20, type="relative",
                  col=brewer.pal(8,'Pastel2'), 
                  main="Top 20 Merchants Relative Freq Plot")

