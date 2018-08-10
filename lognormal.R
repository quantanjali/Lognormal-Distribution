#*************************************************************************************************************************************************************
# Developer 	: Kumari Anjali
# Analysis of Lognormal Distribution of Stock Price 
#**************************************************************************************************************************************************************

#  Normal Distribution and Stock Prices 
#  In finance, price of any stock follow a lognormal distribution. 
#  The stock price at time t+1 is as shown in the following formula: 
#  St +1 = St exp((r - 1/2 σ2) Δt + σ ∈√Δt
#  St 	: stock price at t, St+1 :stock price at t+1, 
#  r 		: expected annual stock return, σ : annualized volatility, 
#  T 		: time in Years (1 year), 
#  n 		: number of steps involved in calculation 
#  Δt 	: dt, size of the unit step size =T/n, 
#  ε 		: distribution term with a zero mean, (a random value from a normal sample) 


# Installing Library Function, if required

if (!require(ggplot2)) install.packages('ggplot2')
if (!require(Matrix)) install.packages('Matrix')
if (!require(sde)) install.packages('sde')
if (!require(data.table)) install.packages('data.table')

library(sde)
library(Matrix)
library(ggplot2)
library(data.table)


##############################################################################################################
#  Consider the following values for this project: 
#  St = 10$ ,  r = 0.15 (15% expected return per year) , σ =0.20 (20% annual volatility in prices), T = 1 year, N=100, ε =0.15 
##############################################################################################################

stock_price = 10		# St,		the stock price at t
return = 0.15			# r,		Expected stock return per year
volatility = 0.2			# σ,		the annual volatility
time = 1					# T,		time in Years
steps = 100				# N,		the number of steps involved in calculation
dt = time/steps			# Δt,		Delta change in time
epsilon = 0.15			# ε 		distribution term with a zero mean, (a random value from a normal sample


##############################################################################################################
# Starting with the initial stock price St as specified, and considering 100 Steps, 
# Calculate the expected value of the stock price at the end of every successive Δt interval of time 
##############################################################################################################

stock_price = 10	; return = 0.15	; volatility = 0.2;  time = 1;  steps = 100

X = matrix(0, nrow =100, ncol=1,)
for (i in 1: 100) 
 {
 	epsilon = 0.15
 	X[i, ] = stock_price * exp((((return - (0.5*(volatility^2)))*(time/(101-i))) + (volatility * epsilon * sqrt((time/(101-i))))))
 }

X

##############################################################################################################

# Plot the entire movement of prices over the T period under observation 

##############################################################################################################

plot(X, type = "l", col = "blue",
						 main = "Simulation of expected Stock price movement : Epsilon = 0.15, ", 
						 xlab ="Time period", 
						 ylab = "Stock Price")

##############################################################################################################
# Instead of considering a fixed ε as in the previous steps, 
# randomly assign values to ε from a standard normal distribution. 
##############################################################################################################

# Method to simulate stock price forecast over a 1-year period, having 100 step using Matrix & Looing, 
# ε randomly assign values from a standard normal distribution
#****************************************************************************************************************************************************
stock_price = 10	; return = 0.15	; volatility = 0.2;  time = 1;  steps = 100	
epsilon = rnorm(100, mean = 0, sd = 1)

X = matrix(0, nrow =100, ncol=1)
for (i in 1: 100) 
 		{
 			X[i,] = stock_price * exp((((return - (0.5*(volatility^2)))*(time/(101-i))) + (volatility * epsilon[i] * sqrt((time/(101-i))))))
 		}
X

var(X)

plot(X, type = "l", col = "blue",
						 main = "Simulation of Stock price movement : Matrix Method, Epsilon: random normal ", 
						 xlab ="Time period", 
						 ylab = "Stock Price")
						 
						 

# Alternate method to simulate stock price forecast over a 1-year period, having 100 step using standard package & library function 
#****************************************************************************************************************************************************

library(sde)

stock_price =10 ; return=0.15 ; volatility =0.2 ; time = 1 ; steps=100; trials=1 ; dt = time/steps

epsilon = seq(0,time,by = dt)
X = matrix(rep(0, length(epsilon)*trials), nrow=trials)

for (i in 1:trials) 
 {
 	X[i,]= GBM(x= stock_price, r=return,sigma=volatility,T=time,N=steps)
 }

X

#  Plot of each trajectory of prices as a separate line 

ymax=max(X); ymin=min(X) #bounds for simulated prices

plot(epsilon, X[1,], t='l', ylim=c(ymin, ymax), col="blue", main = "Stock price movement using Standard Package", ylab="Price P(t)", xlab="time t")
for(i in 2:trials)
 {
 	lines(epsilon, X[i,], t='l', ylim=c(ymin, ymax), col=i)
 }


##############################################################################################################
# Perform 5 trials of 100 steps each to plot the probable movement of stock prices over a 1-year period. 
# Plot each trajectory of prices as a separate line 
##############################################################################################################

# 5 trails to simulate stock price forecast over a 1-year period, having 100 step using Matrix & Looing
#****************************************************************************************************************************************************
stock_price = 10	; return = 0.15	; volatility = 0.2;  time = 1;  steps = 100	

X = matrix(0, nrow =100, ncol=5)
for (j in 1:5){
	for (i in 1: 100) 
 		{
 			epsilon = rnorm(1, mean = 0, sd = 1)
 			X[i,j] = stock_price * exp((((return - (0.5*(volatility^2)))*(time/(101-i))) + (volatility * epsilon* sqrt((time/(101-i))))))
 		}
 	}

X

ymax=max(X); ymin=min(X) #bounds for simulated prices
matplot(X, type = "l", ylim=c(ymin, ymax), main = " 5 trails to simulate stock price: Matrix & Looping method", ylab="Price P(t)", xlab="time t")


# How wide a variance is noticeable in the final year- end price of the stock for the 5 separate trials
#**************************************************************************************************************

f <- c(X[100,1:5])
variation <- c(f-10)
variation


#  ALTERNATE METHOD (More Accurate) : 5 trails to simulate stock price forecast over a 1-year period, having 100 step using Standard Library package
#****************************************************************************************************************************************************

library(sde)

# Perform 5 trials of 100 steps each to plot the probable movement of stock prices over a 1-year period. 

stock_price =10 ; return=0.15 ; volatility =0.2 ; time = 1 ; steps=100; trials=5 ; dt = time/steps

epsilon = seq(0,time,by = dt)
X = matrix(rep(0, length(epsilon)*trials), nrow=trials)

for (i in 1:trials) 
 {
 	X[i,]= GBM(x= stock_price, r=return,sigma=volatility,T=time,N=steps)
 }

X

#  Plot of each trajectory of prices as a separate line 

ymax=max(X); ymin=min(X) #bounds for simulated prices

plot(epsilon, X[1,], t='l', ylim=c(ymin, ymax), col=1, main = " 5 trails to simulate stock price: 100 steps: Standard Library " , ylab="Price P(t)", xlab="time t")
for(i in 2:trials)
 {
 	lines(epsilon, X[i,], t='l', ylim=c(ymin, ymax), col=i)
 }

##############################################################################################################
# THE END 
##############################################################################################################
