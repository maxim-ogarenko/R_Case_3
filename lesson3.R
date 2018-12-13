if(!file.exists("Lesson3")) dir.create("./myRcourse/Lesson3")
setwd("C:/Users/Admin/Documents/myRcourse/Lesson3")
library(dplyr)

rm(list = ls())

sales_units17 <- read.csv("sales_units2017.csv", header = TRUE, sep = ";")
sales_units18 <- read.csv("sales_units2018.csv", header = TRUE, sep = ";")
cogs <- read.csv("cog_per_unit.csv", header = TRUE, sep = ";", dec = ",")
stock17 <- read.csv("stock_units2017.csv", header = TRUE, sep = ";")
stock18 <- read.csv("stock_units2018.csv", header = TRUE, sep = ";")

skus <- c(as.character(sales_units17[, 1]), 
         as.character(sales_units18[, 1]),
         as.character(stock17[, 1]), 
         as.character(stock18[, 1]), 
         as.character(cogs$SKU))
skus <- data.frame(unique(skus))

colnames(skus) <- "SKU"
names(stock17)[1] <- "SKU"
names(stock18)[1] <- "SKU"
names(sales_units17)[1] <- "SKU"
names(sales_units18)[1] <- "SKU"

stock <- merge(skus, stock17, all = TRUE, by = "SKU")
stock <- merge(stock, all = TRUE, stock18)
stock[is.na(stock)] <- 0
str(stock)

sales <- merge(skus, sales_units17, all = TRUE, by = "SKU")
sales <- merge(sales, sales_units18, all = TRUE, by = "SKU")
sales[, -1][sales[, -1] < 0] <- 0
sales[is.na(sales)] <- 0
daily_sales <- sales[, -1] / 30
str(sales)

cogs <- merge(skus, cogs, all = TRUE, by = "SKU")
cogs[is.na(cogs)] <- 0
str(cogs)

inv_days <- as.matrix(stock[, -1] / daily_sales)
inv_days[!is.finite(inv_days)] <- 0
inv_days <- data.frame(skus, inv_days)
str(inv_days)

inventory <- as.matrix(stock[, -1] * cogs$COG)
inventory <- data.frame(skus, inventory)
inventory_totals <- unname(round(colSums(inventory[, -1])/10^6, 1))

deficit_days <- 90
deficit <- deficit_days - inv_days[, -1]
deficit[deficit < 0] <- 0
deficit <- deficit * daily_sales * cogs$COG
deficit <- data.frame(skus, deficit)
str(deficit)
deficit_totals <- unname(round(colSums(deficit[, -1])/10^6, 1))

overstock_days <- 150
overstock <- inv_days[, -1] - overstock_days
overstock[overstock < 0] <- 0
overstock <- overstock * daily_sales * cogs$COG
overstock <- data.frame(skus, overstock)
overstock_totals <- unname(round(colSums(overstock[, -1])/10^6, 1))

report <- data.frame(colnames(stock)[2:11], 
                     inventory_totals,
                     deficit_totals, 
                     overstock_totals)
colnames(report) <- c("Month", "Inventory", "Deficit", "Overstock")
{print("Inventory, Deficits and Overstocks, in mln Rub") 
report}

par(mfrow = c(3, 1))
barplot(height = inventory_totals, 
        names.arg = report$Month, 
        ylim = c(0, 80),
        ylab = "Запасы, в млн. руб.", 
        cex.names = 0.8,
        las = 2, 
        main = "Динамика запасов, в млн. руб.")
barplot(height = deficit_totals, 
        names.arg = report$Month, 
        ylim = c(0, 80),
        ylab = "Дефицит, в млн. руб.", 
        cex.names = 0.8,
        las = 2, 
        main = "Динамика дефицита, в млн. руб.")
barplot(height = overstock_totals, 
        names.arg = report$Month, 
        ylim = c(0, 80),
        ylab = "Излишек, в млн. руб.", 
        cex.names = 0.8,
        las = 2, 
        main = "Динамика излишка, в млн. руб.")