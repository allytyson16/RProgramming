###week 5

#function

"Mean_fun" <- function(Data)
{
  n <- length(Data) #sample size
  SUM <- sum(Data)
  Mu <- (1/n) * SUM
}
  
"MeanVar" <- function(Data)
{
  n <- length(data)
  Mean_x <- Mean_fun(Data)
  Median_x <- median(Data)
  Variance_x <- var(Data)
  Tab <- c(n, Mean_x, Median_x, Variance_x)
  names(tab) <- c("n", "Mean", "Median",
                  "Variance")
  return(Tab)
}

#sampling
##install.packages("insuranceData"). ## install life contingency, insurance, etc packages,

?Cars93
Mean_fun(Data = Cars93$Price)   #why is there an error?
mean(Cars93$Price)
MeanVar(data = Cars93$Price)




install.packages("psych")
install.packages("evt0")
install.packages("ReIns")
