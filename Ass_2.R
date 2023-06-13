install.packages("carData")
library(carData)

str(Prestige)
ls(Prestige)
View(Prestige)

dim(Prestige)
head(Prestige)
tail(Prestige)

max(Prestige$income)
min(Prestige$income)
rownames(Prestige)[Prestige[, "income"] == max(Prestige[, "income"])]
rownames(Prestige)[Prestige[, "income"] == min(Prestige[, "income"])]

mean(Prestige$income > 25000) * 100
#or
length(Prestige$income[Prestige$income > 25000]) / length(Prestige$income) * 100

ab2000_be10000 <- sum((Prestige$income > 20000 & Prestige$income < 10000)/ length(Prestige$income)) * 100


#Question 2
bp_income_acc_occup <- boxplot(income~type, data = Prestige, col = c("beige", "pink", "orange", "blue"))

hist(Prestige$income, xlab = "Income($)", col = "beige")


#c
Prestige$type <- unique(Prestige$type)
a<-unique(Prestige$type)

Prestige$income <- as.numeric(as.character(Prestige$income))
Prestige$income <- Prestige$income[!is.na(Prestige$income)]

par(mfrow = c(2, 2))

for (i in 1:length(a))
  
{
  data <- Prestige$income[a == a[i]]
    hist(data, col = "beige", xlab = "Income", ylab = "Frequency",
       main = paste("Income Distribution -", a[i]))
    }

average_income <- t.test(Prestige$income, alpha = 0.05)
testaov <- aov(income~type, data = Prestige)
summary(testaov)

#e
occup <- Prestige[c("nursing.aides", "physio.therapsts", "pharmacists", "pilots", 
           "taxi.drivers", "longshoremen", "typesetters", "bookbinders"),]
#f
typeoccu <- table(Prestige$type)
piegraph <- pie(x = typeoccu, labels = c("bc", "prof", "wc"),
                radius = 1.5)
#g
par(mfrow = c(2, 2))

for (i in 1:length(a))
  
{
  data <- Prestige$income[a == a[i]]
  barplot(data, col = "orange", xlab = "Income", ylab = "Frequency",
       main = paste("Income Distribution -", a[i]))
}


#3
#a
install.packages("MASS")
library(MASS)
install.packages("ggplot2")
library(ggplot2)
plot(x = Prestige$income, y = Prestige$prestige, 
     xlab = "Income", ylab = "Prestige", pch = 2, main = "carData package: Prestige data")

#b
?cor.test
correla <- cor.test(Prestige$income, Prestige$prestige)
conf_interval <- correla$conf.int
co.efficient <- correla$estimate
