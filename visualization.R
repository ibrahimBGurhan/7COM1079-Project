library(readr)
library(dplyr)
food <- read_csv("Food_Supply_Quantity_kg_Data.csv")

#Categorize food types
food$animal <- rowSums(food[,c("Animal fats", "Animal Products", "Aquatic Products, Other", "Eggs", "Fish, Seafood", "Meat", "Milk - Excluding Butter", "Miscellaneous", "Offals")], na.rm=TRUE)
food$plant <- rowSums(food[,c("Alcoholic Beverages", "Cereals - Excluding Beer", "Fruits - Excluding Wine", "Oilcrops", "Pulses", "Spices", "Starchy Roots", "Stimulants", "Sugar & Sweeteners", "Sugar Crops", "Treenuts", "Vegetable Oils", "Vegetables", "Vegetal Products")], na.rm=TRUE)
food$total <- rowSums(food[,c("animal", "plant")], na.rm=TRUE)

#Remove null rows
food<-food[!(food$Deaths==0),]
food<-food[!is.na(food$Deaths),]

#Plot
pdf("visualization.pdf")
plot(food$plant, food$Deaths, pch = 19, frame = TRUE, col = "blue", main = "Mortality rate from Covid-19 vs plant-based product consumption rate", xlab = "Plant-based product consumption rate (%)", ylab = "Mortality rate from Covid-19 (%)")
abline(lm(food$Deaths ~ food$plant), col = "red")
dev.off()