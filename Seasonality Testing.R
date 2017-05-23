trainiq<-read.csv("trainalliq.csv",header = TRUE)
trainsj<-read.csv("trainallsj.csv",header = TRUE)
#finding the period of the data
library(forecast)
findfrequency(trainsj$total_cases)
findfrequency(trainiq$total_cases)

#measure trend and seasonality on a [0,1] scale
decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl$time.series[,2]
    season <- x.stl$time.series[,1]
    remainder <- x - trend - season
  }
  else #Nonseasonal data
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,
              transform=transform,lambda=lambda))
}
decomp(trainsj$total_cases)

# f1 maps [0,infinity) to [0,1]
f1 <- function(x,a,b)
{
  eax <- exp(a*x)
  if (eax == Inf)
    f1eax <- 1
  else
    f1eax <- (eax-1)/(eax+b)
  return(f1eax)
}

# f2 maps [0,1] onto [0,1]
f2 <- function(x,a,b)
{
  eax <- exp(a*x)
  ea <- exp(a)
  return((eax-1)/(eax+b)*(ea+b)/(ea-1))
}

measures <- function(x)
{
  require(forecast)
  
  N <- length(x)
  freq <- findfrequency(x)
  x <- ts(x,f=freq)
  
  # Decomposition
  decomp.x <- decomp(x)
  
  # Adjust data
  if(freq > 1)
    fits <- decomp.x$trend + decomp.x$season
  else # Nonseasonal data
    fits <- decomp.x$trend
  adj.x <- decomp.x$x - fits + mean(decomp.x$trend, na.rm=TRUE)
  
  # Backtransformation of adjusted data
  if(decomp.x$transform)
    tadj.x <- InvBoxCox(adj.x,decomp.x$lambda)
  else
    tadj.x <- adj.x
  
  # Trend and seasonal measures
  v.adj <- var(adj.x, na.rm=TRUE)
  if(freq > 1)
  {
    detrend <- decomp.x$x - decomp.x$trend
    deseason <- decomp.x$x - decomp.x$season
    trend <- ifelse(var(deseason,na.rm=TRUE) < 1e-10, 0, 
                    max(0,min(1,1-v.adj/var(deseason,na.rm=TRUE))))
    season <- ifelse(var(detrend,na.rm=TRUE) < 1e-10, 0,
                     max(0,min(1,1-v.adj/var(detrend,na.rm=TRUE))))
  }
  else #Nonseasonal data
  {
    trend <- ifelse(var(decomp.x$x,na.rm=TRUE) < 1e-10, 0,
                    max(0,min(1,1-v.adj/var(decomp.x$x,na.rm=TRUE))))
    season <- 0
  }
  
  m <- c(trend,season)
  names(m) <- c("trend","seasonal")
  
  return(m)
}
measures(trainsj$total_cases)
measures(trainiq$total_cases)

#Decomposition further shows seasonality
plot(as.ts(trainsj$total_cases))
plot(as.ts(trainiq$total_cases))


ts_sj = ts(trainsj$total_cases, frequency = 52)
decompose_sj = decompose(ts_sj, "multiplicative")
plot(as.ts(decompose_sj$seasonal))
plot(as.ts(decompose_sj$trend))
plot(as.ts(decompose_sj$random))
plot(decompose_sj)

ts_iq = ts(trainiq$total_cases, frequency = 52)
decompose_iq = decompose(ts_iq, "multiplicative")
plot(as.ts(decompose_iq$seasonal))
plot(as.ts(decompose_iq$trend))
plot(as.ts(decompose_iq$random))
plot(decompose_iq)

#tbats model automatically determine if a seasonal pattern is present
#seasonal will be TRUE if a seasonal model is chosen and otherwise FALSE
fit_sj <- tbats(ts_sj)
seasonal <- !is.null(fit$seasonal)

fit_iq <- tbats(ts_iq)
seasonal <- !is.null(fit$seasonal)
