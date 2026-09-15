## Installation of the Libraries
#install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")
#install.packages("dplyr", repos = "http://cran.us.r-project.org")
#install.packages("tidyquant", repos = "http://cran.us.r-project.org")
#install.packages("quantmod", repos = "http://cran.us.r-project.org")
#install.packages("tseries", repos = "http://cran.us.r-project.org")
#install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyquant)
library(quantmod)
library(tseries)
library(tidyverse)

# Input parameters
symbol_name <<- c("BMRI.JK", "BBCA.JK", "BBRI.JK")  # try to put Your tickers!!
vahy <<- c(0.2,0.2,0.2)
FROM <<- "2023-10-02"   # change to Your dates!!!
TO <<- "2023-12-29"

# Data download
# preparing one table in common for all the downloaded tickers - You can change 
for (i in 1:length(symbol_name)) {
  prac <<- Ad(getSymbols(symbol_name[i], from = FROM, to = TO,auto.assign=FALSE))
  if (i==1) {
    price <<-prac
  } else{
    price <<- merge(price,prac)
  }
}
rm(prac)    # prac is just temporary variable to remove
colnames(price) <- symbol_name  #puting the names of the shares
str(price)

# Daily returns
return_a <<- CalculateReturns(price, method="log")
summary(return_a)
hist(return_a$BMRI.JK,main = "Daily returns")
hist(return_a$BBCA.JK,main = "Daily returns")
hist(return_a$BBRI.JK,main = "Daily returns")

# Mengganti data hilang dengan median
for(i in 1:dim(return_a)[2]){
  return_a[,i][is.na(return_a[,i])] <- median(return_a[,i],na.rm = TRUE)
}
return2_a <<- return_a[-1,] # Menghapus baris pertama yang awalnya mengandung NA

# VaR of one share - historical and variance-covariance method
# compute the 95th percentile of each column
# historical method
Qh <<- VaR(return2_a, p=0.95, method="gaussian")
Qh

#variance-covariance method
# set the parameters
# calculate the 5th percentile
Qv <<- VaR(return2_a, p=0.95, method="historical")
Qv

# create a histogram
hist(return2_a[,1], 
     breaks = seq(min(return2_a[,1])-0.02, max(return2_a[,1])+0.02, by = 0.01), # specify the bin width
     main = paste(symbol_name[1]," VaR hist =", round(Qh[,1],3), sep = " ", "VaR cov = ", round(Qv[,1],3)),
     freq=FALSE,
     xlab = "returns", # add a label to the x-axis
     ylab = "Frequency", # add a label to the y-axis
     col = "blue", # specify the color of the bars
     border = "white", # specify the color of the border of the bars
     xlim = c(min(return2_a[,1]) - 0.05, max(return2_a[,1]) + 0.05) # set the x-axis limits
)

abline(v = Qh[,1], col = "red", lwd = 2)
abline(v = Qv[,1], col = "green", lwd = 2)

curve(dnorm(x, mean = mean(return2_a[,1]), sd = sd(return2_a[,1])), 
      col = "red", 
      add = TRUE)

# BBCA #
# create a histogram
hist(return2_a[,2], 
     breaks = seq(min(return2_a[,2])-0.02, max(return2_a[,2])+0.02, by = 0.01), # specify the bin width
     main = paste(symbol_name[2]," VaR hist =", round(Qh[,2],3), sep = " ", "VaR cov = ", round(Qv[,2],3)),
     freq=FALSE,
     xlab = "returns", # add a label to the x-axis
     ylab = "Frequency", # add a label to the y-axis
     col = "blue", # specify the color of the bars
     border = "white", # specify the color of the border of the bars
     xlim = c(min(return2_a[,2]) - 0.05, max(return2_a[,2]) + 0.05) # set the x-axis limits
)

abline(v = Qh[,2], col = "red", lwd = 2)
abline(v = Qv[,2], col = "green", lwd = 2)

curve(dnorm(x, mean = mean(return2_a[,2]), sd = sd(return2_a[,2])), 
      col = "red", 
      add = TRUE)

# BMRI #
hist(return2_a[,3], 
     breaks = seq(min(return2_a[,3])-0.02, max(return2_a[,3])+0.02, by = 0.01), # specify the bin width
     main = paste(symbol_name[3]," VaR hist =", round(Qh[,3],3), sep = " ", "VaR cov = ", round(Qv[,3],3)),
     freq=FALSE,
     xlab = "returns", # add a label to the x-axis
     ylab = "Frequency", # add a label to the y-axis
     col = "blue", # specify the color of the bars
     border = "white", # specify the color of the border of the bars
     xlim = c(min(return2_a[,3]) - 0.05, max(return2_a[,3]) + 0.05) # set the x-axis limits
)

abline(v = Qh[,3], col = "red", lwd = 2)
abline(v = Qv[,3], col = "green", lwd = 2)

curve(dnorm(x, mean = mean(return2_a[,3]), sd = sd(return2_a[,3])), 
      col = "red", 
      add = TRUE)

# Complex portfolio
summary(return2_a)
chart.Correlation(return_a)

# Computation of VaR
# Historical method
hist_var_2 <- VaR(return2_a, p=0.95, method="historical")
hist_var_2

# Variance-covariance method
varcovar_var_2 <- VaR(return2_a, p=0.95, method="gaussian")
varcovar_var_2

# VaR of Portfolio
# preparing one table in common for all the downloaded tickers - You can change 
valueAtRisk <-VaR(return2_a, weights = vahy, p=0.95, portfolio_method = "component", method = "gaussian")
valueAtRisk

# Expected shortfall/Conditional VaR of the portfolio
# symbol_name <<- c("BMRI.JK", "BBCA.JK", "BBRI.JK")
# preparing one table in common for all the downloaded tickers - You can change 
ExpectedShortFallHist <-ETL(return2_a, weights = vahy, p=0.95, portfolio_method = "component", method = "historical")  # probe to change "historical" to "gauss"
ExpectedShortFallHist
ExpectedShortFallVarcorr <-ETL(return2_a, weights = vahy, p=0.95, portfolio_method = "component", method = "gaussian")  # probe to change "historical" to "gauss"
ExpectedShortFallVarcorr
