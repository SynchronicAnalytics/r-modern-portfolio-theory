# CalculateBeta
# Reads a directory of stock return files, calculates the adjusted Beta of the stock, and ranks them.
# DVS 11/20/2016 - 12/11/201

# Preliminaries

library(stats)
library(xts)			# Extended time series objects
library(nortest)		# For ad.test
library(lmtest)			# For Durbin-Watson autocorrelation test

rm(list=ls()) 
options(digits=5)
setwd("/Users/davidschwab/StockDownload/Data")

# Functions

outliers.iqr <- function(x, multiplier = 3)
{
	# Returns the lower and upper bounds for outliers as determined by a multiplier * IQR
	# Assumes no particular distribution
	
	# Takes a vector x and an IQR multiplier
	# Returns a two-element list with lbound,ubound as members
	
	temp <- quantile(x, probs=c(.25,.75), names=FALSE) 
	iqr <- temp[2] - temp[1]
	lbound <- temp[1] - multiplier * iqr
	ubound <- temp[2] + multiplier * iqr
	return(list(lbound=lbound,ubound=ubound))
}

# Risk-free rate and avg market return

rf.rate <- .018255405675  			# 10 year treasury (geometric mean, YTD)
#mean.market.return <- mean(spx.file.subset$return) * 252
#var.market.return <- var(spx.file.subset$return)

# Get list of files with stock returns and read each file as a data frame in the
# environment "stocks"
	
files <- list.files()
files.count <- length(files)	# Store as variable so we don't recalculate it each loop iteration

stocks <- new.env()
for (i in 1:files.count)
{
	assign(files[i], read.csv(files[i]), pos = stocks)
}

# Make a list of all the data frames and add the stock names

stock.list <- lapply(ls(stocks), function(x) {get(x, envir=stocks)})
names(stock.list) <- ls(stocks)

# Subset YTD returns

stock.list.subset <- lapply(stock.list, function (x) {
		subset(x, as.Date(Date) >= '2016-01-01', select = c("Date", "Adj.Close"))
})

# Transform each data frame into an xts object

for(i in 1:length(stock.list.subset))
{
	temp.date <- as.Date(as.character(stock.list.subset[[i]]$Date), "%Y-%m-%d")
	stock.list.subset[[i]] <- xts(stock.list.subset[[i]]$Adj.Close, temp.date)
	colnames(stock.list.subset[[i]]) <- c("Adj.Close")
}

# Calculate market return on all stocks

for(i in 1:length(stock.list.subset))
{
	stock.list.subset[[i]]$Return <- log(stock.list.subset[[i]]$Adj.Close / lag(stock.list.subset[[i]]$Adj.Close))
}

# Get rid of NAs (usually first observation, since no lagged value)

for(i in 1:length(stock.list.subset))
{
	stock.list.subset[[i]] <- stock.list.subset[[i]][!is.na(stock.list.subset[[i]]$Return)]
}

# Get outliers for each stock, then trim

stock.list.outliers <- lapply(stock.list.subset, function (x) {outliers.iqr(x$Return,3)})

stock.list.trimmed <- lapply(stock.list.subset, function (x) {
	temp <- outliers.iqr(x$Return,3)
	subset(x, Return > temp$lbound & Return < temp$ubound, select = "Return")
})

# Strip SPX out of the list of stocks to analyze, and make it a separate object

spx.stock.return <- subset(stock.list.trimmed[["SPX"]], select = "Return")
stock.list.return <- stock.list.trimmed[names(stock.list.trimmed) != "SPX"]

# Test each stock return for normality, then adjust for multiple comparisons
# Returns TRUE or FALSE for each stock, depending on whether test is passed

normal.test <- lapply(stock.list.return, function (x) {ad.test(as.numeric(x))})
normal.test.adj <- lapply(normal.test, function (x) {
	temp <- p.adjust(x$p.value, method="hochberg", n=length(normal.test))
	return (temp > 0.05)
})

# Subset the stocks that are approximately normal using normal.test.adj as a filter

stock.list.data <- stock.list.return[unlist(normal.test.adj)]

# Estimate Betas for each stock

Beta.model <- lapply(stock.list.data, function (x){
	model.data <- merge(spx.stock.return, x)
	colnames(model.data) <- c("market.return", "stock.return")
	lm(stock.return ~ market.return, data = model.data)
})

# Test model residuals for normality, heteroskedasticity, and autocorrelation

Beta.model.residuals <- lapply(Beta.model, residuals)

# Normality

normal.test <- lapply(Beta.model.residuals, ad.test)
normal.test.adj <- lapply(normal.test, function (x) {
	temp <- p.adjust(x$p.value, method="hochberg", n=length(normal.test))
	return (temp > 0.05)
})

Beta.model.normal <- Beta.model[unlist(normal.test.adj)]

# Auto-correlation

ac.test <- lapply(Beta.model.normal, dwtest)
ac.test.adj <- lapply(ac.test, function (x) {
	temp <- p.adjust(x$p.value, method="hochberg", n=length(ac.test))
	return (temp > 0.05)
})

Beta.model.uncor <- Beta.model.normal[unlist(ac.test.adj)]

# Combine model estimates and diagnostics into a new list
# Include both model SEE and an annualized SEE, since we'll use it to adjust Beta

portfolio <- lapply(Beta.model.uncor, function (x) {
	data.frame(
		Alpha=coef(x)[1],
		Beta=coef(x)[2],
		Model.see=summary(x)$sigma,
		Model.see.annual=summary(x)$sigma * sqrt(252),
		Model.r.squared=summary(x)$r.squared
		)
})

# Adjust each Beta using Vasicek's method
# Beta.mean and Beta.var are over all the Betas in the portfolio

Beta.mean <- mean(sapply(portfolio, function(x) {x$Beta}))
Beta.var <- var(sapply(portfolio, function(x) {x$Beta}))

portfolio.adj <- lapply(portfolio, function (x) {
	beta.weight.1 <- (x$Model.see.annual)^2 / ((x$Model.see.annual)^2 + Beta.var)
	beta.weight.2 <- Beta.var / ((x$Model.see.annual)^2 + Beta.var)
	x$Beta.adj <- (beta.weight.1 * Beta.mean) + (beta.weight.2 * x$Beta)
	x
})

# Get rid of poor estimates
	
portfolio.fit <- portfolio.adj[unlist(sapply(portfolio.adj, function(x) {x$Model.r.squared > .5}))]

# Calculated estimated returns for each stock, then rank them using standard MPT method
# of excess return to Beta

mean.spx.return <- mean(spx.stock.return) * 252
var.spx.return <- var(spx.stock.return)					# we'll need this for the portfolio cutoff

portfolio.est.return <- lapply(portfolio.fit, function (x) {
	x$est.return <- x$Alpha + (x$Beta.adj * mean.spx.return)
	x
})

portfolio.ranking <- lapply(portfolio.est.return, function(x) {
	x$ranking <-(x$est.return - rf.rate) / x$Beta.adj
	x
})

# Sort by decreasing rank (just for convenience)

portfolio.ranking <- portfolio.ranking[order(-sapply(portfolio.ranking, function(x) {x$ranking}))]

# Estimate cut-off value for inclusion of stocks into the optimal portfolio
# Then subset optimal portfolio

# Need to convert our list to a data frame for this part (for the cumsum() function)

stocks <- names(portfolio.ranking)
values <- matrix(unlist(portfolio.ranking),nrow=length(portfolio.ranking), byrow=TRUE)
portfolio.optimal <- data.frame(stocks,values)
colnames(portfolio.optimal) <- c("Stock", "Alpha", "Beta", "Model.see", "Model.see.anual","Model.r.squared", "Beta.adj", "est.return", "ranking")

# Now estimate the cutoff; attach() just makes the calculation easier to follow

attach(portfolio.optimal)
cutoff.numerator <- ((est.return - rf.rate) * Beta.adj) / Model.see^2
cutoff.denom <- Beta.adj^2 / Model.see^2
detach(portfolio.optimal)

cutoff <- (var.spx.return * cumsum(cutoff.numerator)) / (1 + var.spx.return * cumsum(cutoff.denom))
portfolio.cutoff <- cbind(portfolio.optimal, cutoff)
portfolio.final <- subset(portfolio.cutoff, ranking > cutoff)

# Estimate percentage to invest in each stock in optimal portfolio

alpha.prime <- portfolio.final$est.return - (rf.rate + portfolio.final$Beta.adj*(mean.spx.return - rf.rate))
portfolio.final$z <- alpha.prime / portfolio.final$Model.see

portfolio.final$invest.pct <- portfolio.final$z / sum(portfolio.final$z)


