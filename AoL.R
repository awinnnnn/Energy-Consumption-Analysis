data <- read.csv('power-consumption.csv')

# Format date
#install.packages("lubridate")
library(lubridate)
data$Datetime <- mdy_hm(data$Datetime)
data$Date <- as.Date(data$Datetime)
data$Time <- format(data$Datetime, "%H:%M:%S")
data$Month <- month(data$Datetime)

# Categorize Time of Day and Season
data$TimeOfDay <- cut(as.numeric(format(data$Datetime, "%H")),
                      breaks = c(-Inf, 1, 12, 18, 24),
                      labels = c("Night", "Morning", "Afternoon", "Evening"),
                      right = FALSE)
data$Season <- cut(data$Month,
                      breaks = c(1, 3, 6, 10, 12, Inf),
                      labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
                      right = FALSE)

# Categorize Power Consumption
zone1_q1 <- unname(quantile(data$PowerConsumption_Zone1, probs = c(0.25)))
zone1_q2 <- unname(quantile(data$PowerConsumption_Zone1, probs = c(0.75)))
zone2_q1 <- unname(quantile(data$PowerConsumption_Zone2, probs = c(0.25)))
zone2_q2 <- unname(quantile(data$PowerConsumption_Zone2, probs = c(0.75)))
zone3_q1 <- unname(quantile(data$PowerConsumption_Zone3, probs = c(0.25)))
zone3_q2 <- unname(quantile(data$PowerConsumption_Zone3, probs = c(0.75)))
data$PowerCategory_Zone1 <- cut(data$PowerConsumption_Zone1, 
                                breaks = c(-Inf, zone1_q1, zone1_q2, Inf),
                                labels = c("Low_Con", "Medium_Con", "High_Con"))
data$PowerCategory_Zone2 <- cut(data$PowerConsumption_Zone2, 
                                breaks = c(-Inf, zone2_q1, zone2_q2, Inf),
                                labels = c("Low_Con", "Medium_Con", "High_Con"))
data$PowerCategory_Zone3 <- cut(data$PowerConsumption_Zone3, 
                                breaks = c(-Inf, zone3_q1, zone3_q2, Inf),
                                labels = c("Low_Con", "Medium_Con", "High_Con"))

# Categorize Temperature, Humidity, and Wind Speed
temp_q1 <- unname(quantile(data$Temperature, probs = c(0.25)))
temp_q2 <- unname(quantile(data$Temperature, probs = c(0.75)))
data$TempCategory <- cut(data$Temperature, 
                         breaks = c(-Inf, temp_q1, temp_q2, Inf),
                         labels = c("Temp_Low", "Temp_Medium", "Temp_High"))

humid_q1 <- unname(quantile(data$Humidity, probs = c(0.25)))
humid_q2 <- unname(quantile(data$Humidity, probs = c(0.75)))
data$HumidCategory <- cut(data$Humidity, 
                         breaks = c(-Inf, humid_q1, humid_q2, Inf),
                         labels = c("Humid_Low", "Humid_Medium", "Humid_High"))

wind_q1 <- unname(quantile(data$WindSpeed, probs = c(0.25)))
wind_q2 <- unname(quantile(data$WindSpeed, probs = c(0.75)))
data$WindCategory <- cut(data$WindSpeed, 
                         breaks = c(-Inf, wind_q1, wind_q2, Inf),
                         labels = c("Wind_Low", "Wind_Medium", "Wind_High"))

################################################################################
# Association Rules Mining
#install.packages("arules")
library(arules)

transaction_columns <- data[, c("TempCategory", "HumidCategory", "WindCategory",
                                "Season", "TimeOfDay", "PowerCategory_Zone3")]

transactions <- apply(transaction_columns, 2, function(column) {
  as.character(na.omit(column)) 
})

transaction.list <- split(transactions, data$Datetime)
transaction.list <- as(transaction.list, "transactions")

# Apriori
rules_apriori <- apriori(transaction.list, parameter = list(supp = 0.05, conf = 0.7),
                 appearance = list(rhs = c("Low_Con", "Medium_Con", "High_Con"), default = "lhs"))
inspect(rules_apriori)

# Eclat
freq_eclat <- eclat(transaction.list, parameter = list(support = 0.05, 
                                                       target = "frequent itemsets"))
rules_eclat <- ruleInduction(freq_eclat, transaction.list, confidence = 0.7)
rules_eclat <- subset(rules_eclat, rhs %in% c("Low_Con", "Medium_Con", "High_Con"))
inspect(rules_eclat)

# FP Growth
library("rCBA")
train <- sapply(transaction_columns, as.factor)
train <- data.frame(train, check.names=FALSE)
txns <- as(train, "transactions")
rules = rCBA::fpgrowth(txns, support=0.05, confidence=0.7, maxLength=3, consequent="PowerCategory_Zone3",
                       parallel=FALSE)
prunedRules <- rCBA::pruning(train, rules, method="m2cba", parallel=FALSE)
inspect(prunedRules)

################################################################################
# Data Visualizations
# Temperature
hist_data <- hist(data$Temperature, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < temp_q1, "green", 
                     ifelse(hist_data$mids < temp_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Temperature Distribution",
     xlab = "Value", ylab = "Frequency")

legend("topright", legend = c("<14.41", "14.41-22.89", ">22.89"), 
       fill = c("green", "yellow", "red"))

# Humidity
hist_data <- hist(data$Humidity, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < humid_q1, "green", 
                     ifelse(hist_data$mids < humid_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Humidity Distribution",
     xlab = "Value", ylab = "Frequency")

legend("topleft", legend = c("<58.31", "58.31-81.4", ">81.4"), 
       fill = c("green", "yellow", "red"))

# Wind Speed
hist_data <- hist(data$WindSpeed, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < wind_q1, "green", 
                     ifelse(hist_data$mids < wind_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Wind Speed Distribution",
     xlab = "Value", ylab = "Frequency")

legend("center", legend = c("<0.078", "0.078-4.915", ">4.915"), 
       fill = c("green", "yellow", "red"))

# Zone 1
hist_data <- hist(data$PowerConsumption_Zone1, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < zone1_q1, "green", 
                     ifelse(hist_data$mids < zone1_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Power Consumption Zone 1",
     xlab = "Value", ylab = "Frequency")

legend("topright", legend = c("Low", "Medium", "High"), 
       fill = c("green", "yellow", "red"))

# Zone 2
hist_data <- hist(data$PowerConsumption_Zone2, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < zone2_q1, "green", 
                     ifelse(hist_data$mids < zone2_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Power Consumption Zone 2",
     xlab = "Value", ylab = "Frequency")

legend("topright", legend = c("Low", "Medium", "High"), 
       fill = c("green", "yellow", "red"))

# Zone 3
hist_data <- hist(data$PowerConsumption_Zone3, breaks = 100, plot = FALSE)
bin_colors <- ifelse(hist_data$mids < zone3_q1, "green", 
                     ifelse(hist_data$mids < zone3_q2, "yellow", "red")) 

plot(hist_data, col = bin_colors, main = "Power Consumption Zone 3",
     xlab = "Value", ylab = "Frequency")

legend("topright", legend = c("Low", "Medium", "High"), 
       fill = c("green", "yellow", "red"))
