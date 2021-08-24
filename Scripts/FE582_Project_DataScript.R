# Data Retrieval Script

# Admin tasks
# install.packages("corrplot")

# Load libraries and set working directory (for csv outputting)
{
  library(quantmod)
  library(broom)
  library(corrplot)
  library(fBasics)
  library(fTrading)
  
  setwd("C:/S18_Projects")
}

# Define Functions
{
  data_pull   <- function(type, tick, from, to)           # Pull stock/etf/indicator data
  {
    if(type == "indicator")
    {
      cnames <- c("Data")
      getSymbols.FRED(tick, env = .GlobalEnv, return.class = "xts")
      temp <- data.frame(get(tick))
      colnames(temp) <- cnames
      #rm(get(tick))
      temp
    }
    else if(type == "stock/etf")
    {
      cnames <- c("Open","High","Low","Close","Volume","Adjusted")
      getSymbols(tick, from = from, to = to)
      if(tick == "^GSPC") { temp <- data.frame(GSPC) }
      else { temp <- data.frame(get(tick)) }
      colnames(temp) <- cnames
      temp
    }
    else { bad <- "Invalid data type to pull"; bad }
  }
  
  comp.data   <- function(type, dlist, cnames, from, to)  # Put data sets together, match by date sequence
  {
    # First check for time frame (days, months, years)
    if(type == "d")
    {
      dates <- seq(as.Date(from), as.Date(to), by = "day")      # Create a data list for subsetting
      dates <- as.character(dates)                              # Make data character type for compatibility
    }
    else if (type == "m")
    {
      dates <- seq(as.Date(from), as.Date(to), by = "month")    # Create a data list for subsetting
      dates <- as.character(dates)                              # Make data character type for compatibility
    }
    else 
    {
      dates <- seq(as.Date(from), as.Date(to), by = "quarter")  # Create a data list for subsetting
      dates <- as.character(dates)                              # Make data character type for compatibility
    }
    
    for(i in 1:length(dlist))
    {
      print(noquote(paste("Data Frame:",i," - ",cnames[i])))
      if(i == 1)
      {
        temp.df             <- dlist[[i]]
        values              <- data.frame(temp.df[dates, ])
        rownames(values)    <- dates
        new.lst             <- data.frame(values)
      }
      else
      {
        temp.df             <- dlist[[i]]
        values              <- data.frame(temp.df[dates, ])
        rownames(values)    <- dates
        new.lst <- cbind(new.lst, values)
      }
    }
    
    colnames(new.lst) <- cnames
    new.lst
  }
  
  cor.agg     <- function(data) # Generate a correlation matrix for a full data set
  {
    params <- colnames(data)
    cor.matrix <- data.frame(matrix(ncol = 1))
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        temp <- rbind(temp, cor(as.numeric(data[,i]), as.numeric(data[,j])))
      }
      cor.matrix <- cbind(cor.matrix, temp)
    }
    cor.matrix <- cor.matrix[,-1]
    colnames(cor.matrix) <- params; rownames(cor.matrix) <- params
    cor.matrix
    
  }
  
  reg.agg     <- function(data) # Generates a regression model matrix for an entire data set
  {
    reg.matrix <- data.frame(matrix(nrow=length(data)))
    names <- colnames(data)
    
    for(i in 1: length(data))
    {
      temp <- data.frame()
      for(j in 1:length(data))
      {
        model <- lm(as.numeric(data[,i])~as.numeric(data[,j]))
        sum   <- summary(model)
        stats <- glance(sum)
        r     <- stats$r.squared
        temp <- rbind(temp, r)
      }
      reg.matrix <- cbind(reg.matrix, temp)
    }
    reg.matrix <- reg.matrix[,-1]
    colnames(reg.matrix) <- names
    rownames(reg.matrix) <- names
    reg.matrix
  }
  
  calc.ret    <- function(data) # Calculate returns for 1 column data frame
  {
    rnames <- rownames(data); rnames <- rnames[-1]
    temp <- c()
    
    for(i in 1:length(data[,1]))
    {
      temp <- c(temp, log(data[i,1]/data[i-1,1]))
    }
    new.df <- data.frame(temp)
    colnames(new.df) <- c("Data")
    rownames(new.df) <- rnames
    new.df
  }
  
  calc.ret.df <- function(data) # Calculate return for 2+ column data frame
  {
    rnames <- rownames(data); rnames <- rnames[-1]
    cnames <- colnames(data)
    new.df <- data.frame(matrix(nrow = length(data[,1])-1))
    
    for(i in 1:length(data))
    {
      temp <- c()
      for(j in 1:length(data[,i]))
      {
        temp <- c(temp, log(data[j,i]/data[j-1,i]))
      }
      new.df <- cbind(new.df, temp)
    }
    new.df <- new.df[-1]
    colnames(new.df) <- cnames
    rownames(new.df) <- rnames
    new.df
  }
  
  strip.param <- function(data, param) # Stri adjusted price column from ohlc objects (standard equity)
  {
    rnames <- rownames(data)
    new.df <- data.frame(data[,param])
    colnames(new.df) <- "Data"
    rownames(new.df) <- rnames
    new.df
  }
  
  summarize <- function(df)
  {
    new <- data.frame(matrix(ncol=1))
    cnames <- colnames(df)
    rnames <- c("min","q1","median","mean","q3","max","std","skew","kurt","sw-test")
    for(i in 1:length(df))
    {
      sum   <- summary(df[,i]); sum <- glance(sum)
      min   = sum$minimum*100
      q1    = sum$q1*100
      med   = sum$median*100
      mu    = sum$mean*100
      q3    = sum$q3*100
      max   = sum$maximum*100
      sd    = sd(df[,i])*100
      skew  = skewness(df[,i])
      kurt  = kurtosis(df[,i]) + 3
      norm1 = shapiro.test(df[,i])
      norm1 = norm1$p.value
      temp  <- c(min,q1,med,mu,q3,max,sd,skew,kurt,norm1)
      new   <- cbind(new, temp)
    }
    new <- new[-1]
    rownames(new) <- rnames
    colnames(new) <- cnames
    new
  }
}

# Data Full time frame
{
  to          <- as.character(Sys.Date())             # Today
  from        <- as.character(as.Date(to) - (365*10)) # Start Day - Default for 10 years
}

# Set Data to retrieve
{
  indexes      <- c("^GSPC","DJIA","NDAQ")
  sectors      <- c("XLF","XLK","XLV","XLE","XLI","XLY","XLP","XLU","XLB","XBI","XLRE")
  indicators   <- c("A191RL1Q225SBEA","FEDFUNDS","VIXCLS","UMCSENT","DCOILWTICO","CSUSHPINSA","DTWEXM","GFDEGDQ188S","UNRATE",
                   "GOLDAMGBD228NLBM","MHHNGSP","PCOPPUSDM","PWHEAMTUSDM","PMAIZMTUSDM","PSHRIUSDM","PBEEFUSDQ","PBANSOPUSDM",
                   "PSOYBUSDM","PPOULTUSDM","PCOCOUSDM","PBARLUSDM","PSUGAISAUSDM","PCOTTINDUSDM","PIORECRUSDM","PALUMUSDM",
                   "PCOALAUUSDM","PRUBBUSDM","BAMLHYH0A0HYM2TRIV","TOTALSA") 
  t.rates     <- c("GS1","GS2","GS3","GS5","GS7","GS10","GS30")
  x.rates     <- c("DEXUSEU","DEXJPUS","DEXCHUS","DEXCAUS","DEXMXUS","DEXKOUS","DEXBZUS","EXINUS","DEXSZUS")
}

# Load Price data - Daily Default - If adding params, update
{
  # Load index data
  mkt.SP500   <- data_pull("stock/etf", indexes[1], from, to)
  mkt.DJIA    <- data_pull("indicator", indexes[2], from, to); remove(DJIA)
  mkt.Nasdaq  <- data_pull("stock/etf", indexes[3], from, to)
  index_list <- c("mkt.SP500","mkt.DJIA","mkt.Nasdaq")
  
  # Load sector ETF data
  s.Fin     <- data_pull("stock/etf", sectors[1],  from, to) # Financial   Sector ETF
  s.Tech    <- data_pull("stock/etf", sectors[2],  from, to) # Technology  Sector ETF
  s.HC      <- data_pull("stock/etf", sectors[3],  from, to) # Health Care Sector ETF
  s.Energy  <- data_pull("stock/etf", sectors[4],  from, to) # Energy      Sector ETF
  s.Ind     <- data_pull("stock/etf", sectors[5],  from, to) # Industrial  Sector ETF
  s.CD      <- data_pull("stock/etf", sectors[6],  from, to) # Cons. Disc. Sector ETF
  s.CS      <- data_pull("stock/etf", sectors[7],  from, to) # Cons. Stpl. Sector ETF
  s.Util    <- data_pull("stock/etf", sectors[8],  from, to) # Utilities   Sector ETF
  s.Mat     <- data_pull("stock/etf", sectors[9],  from, to) # Materials   Sector ETF
  s.Bio     <- data_pull("stock/etf", sectors[10], from, to) # Biotech     Sector ETF
  s.RE      <- data_pull("stock/etf", sectors[11], from, to) # Real Est.   Sector ETF
  sector_list <- c("s.Fin","s.Tech","s.HC","s.Energy","s.Ind","s.CD","s.CS","s.Util","s.Mat","s.Bio") #s.RE
  
  # Load exchange rate data 
  rates.us_euro   <- data_pull("indicator", x.rates[1], from, to) # US/EURO
  rates.jap_us    <- data_pull("indicator", x.rates[2], from, to) # JAPAN/US
  rates.china_us  <- data_pull("indicator", x.rates[3], from, to) # CHINA/US
  rates.canada_us <- data_pull("indicator", x.rates[4], from, to) # CANADA/US
  rates.mexico_us <- data_pull("indicator", x.rates[5], from, to) # MEXICO/US
  rates.skorea_us <- data_pull("indicator", x.rates[6], from, to) # South Korea/US
  rates.brazil_us <- data_pull("indicator", x.rates[7], from, to) # BRAZIL/US
  rates.india_us  <- data_pull("indicator", x.rates[8], from, to) # INDIA/US
  rates.swiss_us  <- data_pull("indicator", x.rates[9], from, to) # SWITZERLAND/US
  rates_list <- c("us/euro","jap/us","china/us","canada/us","mexico/us","skorea/us","brazil/us","india/us","swiss/us")
  remove(list = x.rates)
  
  # Load Indicator data
  ind.GDP      <- data_pull("indicator", indicators[1],  from, to) # US GDP growth                      - quarterly (1st)
  ind.Fedfunds <- data_pull("indicator", indicators[2],  from, to) # Federal Funds (interest rate) rate - monthly   (1st)
  ind.Vix      <- data_pull("indicator", indicators[3],  from, to) # Volatility index daily value       - daily
  ind.Um_sent  <- data_pull("indicator", indicators[4],  from, to) # Sentiment Index (U.M.)             - monthly   (1st)
  ind.Oil      <- data_pull("indicator", indicators[5],  from, to) # Crude oil, price per barrel        - daily
  ind.Home_p   <- data_pull("indicator", indicators[6],  from, to) # Shiller home price index value     - monthly   (1st)
  ind.USD      <- data_pull("indicator", indicators[7],  from, to) # USD (broad basket index) value     - daily
  ind.Debt_Fed <- data_pull("indicator", indicators[8],  from, to) # Public debt as % of US GDP         - quarterly (1st)
  ind.Unemply  <- data_pull("indicator", indicators[9],  from, to) # Unemployment rate                  - monthly   (1st)
  ind.gold_p   <- data_pull("indicator", indicators[10], from, to) # Gold price (per ounce) (london)    - daily
  ind.bondhy   <- data_pull("indicator", indicators[11], from, to) # BofAML US HY total return index    - daily
  ind.autosale <- data_pull("indicator", indicators[12], from, to) # Total Vehicle Sales                - monthly
  indicator_list <- c("gdp","fedfunds","vix","um_sent","oil","home_p","usd","debt_fed","unemply","gold_p","bondhy","autosale")
  remove(list = indicators)
  
  # Load Treasury data 
  ust1        <- data_pull("indicator", t.rates[1], from, to) # UST 1-year rate - monthly
  ust2        <- data_pull("indicator", t.rates[2], from, to) # UST 2-year rate - monthly
  ust3        <- data_pull("indicator", t.rates[3], from, to) # UST 3-year rate - monthly
  ust5        <- data_pull("indicator", t.rates[4], from, to) # UST 5-year rate - monthly
  ust7        <- data_pull("indicator", t.rates[5], from, to) # UST 7-year rate - monthly
  ust10       <- data_pull("indicator", t.rates[6], from, to) # UST 10-year rate - monthly
  ust30       <- data_pull("indicator", t.rates[7], from, to) # UST 30-year rate - monthly
  
  ind.GDP <- ind.GDP/100 # GDP change per quarter, adjust for % as decimal
  
}

# Strip volumes for equity (ohlc) data sets - Put into a volume data frame
{
  ohlc.lst <- list(mkt.SP500,mkt.Nasdaq,s.Fin,s.Tech,s.HC,s.Energy,s.Ind,s.CD,s.CS,s.Util,s.Mat,s.Bio) #,s.RE)
  cnames   <- c("sp500","nasdaq","s.fin","s.tech","s.hc","s.energy","s.ind","s.cd","s.cs","s.util","s.mat","s.bio")
  rnames   <- rownames(mkt.SP500)
  
  volume.data <- data.frame(matrix(ncol=1))
  
  for(i in 1:length(ohlc.lst))
  {
    volume.data <- cbind(volume.data, strip.param(ohlc.lst[[i]], "Volume"))
  }
  
  volume.data <- volume.data[-1]
  rownames(volume.data) <- rnames
  colnames(volume.data) <- cnames
}

# Strip prices for equity (ohlc) data sets
{
  # Index Data
  mkt.SP500   <- strip.param(mkt.SP500  , "Adjusted")
  mkt.Nasdaq  <- strip.param(mkt.Nasdaq , "Adjusted")
  
  # Sector ETF 
  s.Fin       <- strip.param(s.Fin      , "Adjusted")
  s.Tech      <- strip.param(s.Tech     , "Adjusted")
  s.HC        <- strip.param(s.HC       , "Adjusted")
  s.Energy    <- strip.param(s.Energy   , "Adjusted")
  s.Ind       <- strip.param(s.Ind      , "Adjusted")
  s.CD        <- strip.param(s.CD       , "Adjusted")
  s.CS        <- strip.param(s.CS       , "Adjusted")
  s.Util      <- strip.param(s.Util     , "Adjusted")
  s.Mat       <- strip.param(s.Mat      , "Adjusted")
  s.Bio       <- strip.param(s.Bio      , "Adjusted")
  s.RE        <- strip.param(s.RE       , "Adjusted")
}

# Compiled Data sets for scrubbing - If adding params, update
{
  data.d   <- list(mkt.DJIA,mkt.Nasdaq,mkt.SP500,s.Bio,s.CD,s.CS,s.Energy,s.Fin,s.HC,s.Ind,s.Mat,s.Tech,s.Util,s.RE,
                   ind.gold_p,ind.Oil,ind.USD,ind.Vix,
                   rates.brazil_us,rates.canada_us,rates.china_us,rates.india_us,rates.jap_us,rates.mexico_us,rates.skorea_us,rates.swiss_us,rates.us_euro)
  data.d.c <- c("djia","nasdaq","sp500","s.bio","s.cd","s.cs","s.energy","s.fin","s.hc","s.ind","s.mat","s.tech","s.util","s.RE",
                "gold.p","oil","usd","vix",
                "brazil/us","canada/us","china/us","india/us","jap/us","mexico/us","skorea/us","swiss/us","us/euro")
  
  data.m   <- list(ind.Fedfunds,ind.Um_sent,ind.Home_p,ind.Unemply,ind.bondhy,
                   ust1,ust2,ust3,ust5,ust7,ust10,ust30,
                   ind.autosale)
  data.m.c <- c("fedfunds","umsent","homep","unemply","bondhy","ust1","ust2","ust3","ust5","ust7","ust10","ust30","autosale")
  
  data.q   <- list(ind.GDP,ind.Debt_Fed)
  data.q.c <- c("gdp","debtfed")
}

# Calculate Daily Returns - If adding params, check for column omit (any data series not "daily" should be omittied here)
{
  price.d       <- comp.data("d",data.d,data.d.c,"2008-3-07","2018-3-07") # Combine Data Sets
  omit.d        <- c(14,22)                                               # Identify columns to remove
  price.d       <- price.d[-omit.d]                                       # Remove columns of abnormal data
  price.d       <- na.omit(price.d)                                       # Remove rows with NA data
  return.d      <- calc.ret.df(price.d)                                   # Create Returns Datatable
  return.d.corr <- cor(return.d)                                          # Create a correlation Matrix
  corrplot(return.d.corr, method = "circle", main = "Daily")                              # Plot the Correlations
}

# Calculate Weekly Returns
{
  return.w  <- data.frame(matrix(ncol=1))
  rnames    <- rownames(price.d)
  cnames    <- colnames(price.d)
  
  for(i in 1:length(price.d))
  {
    temp <- data.frame(price.d[,i])
    rownames(temp) <- rnames                                             # Apply row names to mirror .xts data set
    temp <- periodReturn(temp, period = "weekly", type = "log")
    
    return.w <- cbind(return.w, temp)
  }
  
  rnames <- rownames(return.w)
  return.w <- data.frame(return.w)
  return.w <- return.w[-1]
  colnames(return.w) <- cnames
  
  return.w.corr <- cor(return.w)
  corrplot(return.w.corr, method = "circle", main = "Weekly")
  
}

# Calculate Monthly Returns
{
  
  # Combine monthly Price Data
  price.m <- comp.data("m",data.m,data.m.c,"2008-3-01","2018-3-01") # Combine Data Sets specific monthly
  price.m <- na.omit(price.m)
  
  m.rnames <- rownames(price.m)
  
  return.m1 <- calc.ret.df(price.m)
  
  dates <- rownames(return.m1)
  dates <- as.Date(dates)
  dates <- format(dates, format = "%y-%m") 
  
  rownames(return.m1) <- dates
  
  # Calculate Monthly returns for daily data set
  return.m  <- data.frame(matrix(ncol=1))
  rnames    <- rownames(price.d)
  cnames    <- colnames(price.d)
  
  for(i in 1:length(price.d))
  {
    temp <- data.frame(price.d[,i])
    rownames(temp) <- rnames
    temp <- periodReturn(temp, period = "monthly", type = "log")
    
    return.m <- cbind(return.m, temp)
  }
  
  rnames   <- rownames(return.m)
  return.m <- data.frame(return.m)
  return.m <- return.m[-1]
  colnames(return.m) <- cnames
  
  dates <- rownames(return.m)
  dates <- as.Date(dates)
  dates <- format(dates, format = "%y-%m") 
  
  rownames(return.m) <- dates
  
  return.m <- merge(return.m, return.m1, by = "row.names") # Merge for same time frame
  return.m <- data.frame(return.m)
  
  rnames <- return.m[1]
  rnames <- unlist(rnames)
  return.m <- return.m[-1]
  rownames(return.m) <- rnames
  
  return.m.corr <- cor(return.m)
  corrplot(return.m.corr, method = "circle", main = "Monthly")
  
}

# Create Summary Tables 
{
  return.d.sum <- summarize(return.d)
  return.w.sum <- summarize(return.w)
  return.m.sum <- summarize(return.m)
}

# Create Regression Tables
{
  return.d.reg <- reg.agg(return.d)
  return.w.reg <- reg.agg(return.w)
  return.m.reg <- reg.agg(return.m)
}

# Create Output Files for used in analysis scripts
{
  write.csv(price.d,       file = "dailyprices.csv")
  write.csv(return.d,      file = "dailyreturns.csv")
  write.csv(volume.data,   file = "dailyvolume.csv")
  write.csv(return.d.sum,  file = "dailysummary.csv")
  write.csv(return.d.corr, file = "dailycorrelation.csv")
  write.csv(return.d.reg,  file = "dailyregression.csv")
  
  write.csv(return.w,      file = "weeklyreturns.csv")
  write.csv(return.w.sum,  file = "weeklysummary.csv")
  write.csv(return.w.corr, file = "weeklycorrelation.csv")
  write.csv(return.w.reg,  file = "weeklyregression.csv")
  
  write.csv(price.m,       file = "monthlyprices.csv")
  write.csv(return.m,      file = "monthlyreturns.csv")
  write.csv(return.m.sum,  file = "monthlysummary.csv")
  write.csv(return.m.corr, file = "monthlycorrelation.csv")
  write.csv(return.m.reg,  file = "monthlyregression.csv")
  
}

# End of Script...