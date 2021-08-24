

fit <- lda(s.bio~usd+oil+gold.p, data = datalist[[1]], na.action = "na.omit")
fit




### (2) Conduct K-means clustering analysis -------------------------------------------------------


### 

# Analyze Returns data


# Analyze Correlation data



### (4) Multi-variate Linear Regression models ------------------------------------------------------
data.bio <- return.m[c(4,14:38)]
base     <- lm(s.bio~1, data = data.bio); full     <- lm(s.bio~., data = data.bio)
model    <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s <- summary(model); print(model.s)

data.cd  <- return.m[c(5,14:38)]
base     <- lm(s.cd~1, data = data.cd); full     <- lm(s.cd~., data = data.cd)
model    <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s  <- summary(model); print(model.s)

data.cs  <- return.m[c(6,14:38)]
base     <- lm(s.cs~1, data = data.cs); full     <- lm(s.sc~., data = data.cs)
model    <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s  <- summary(model); print(model.s)

data.energy  <- return.m[c(7,14:38)]
base         <- lm(s.energy~1, data = data.energy); full     <- lm(s.energy~., data = data.energy)
model        <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s      <- summary(model); print(model.s)

data.fin     <- return.m[c(8,14:38)]
base         <- lm(s.fin~1, data = data.fin); full     <- lm(s.fin~., data = data.fin)
model        <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s      <- summary(model); print(model.s)

data.hc      <- return.m[c(9,14:38)]
base         <- lm(s.hc~1, data = data.hc); full     <- lm(s.hc~., data = data.hc)
model        <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 1)
model.s      <- summary(model); print(model.s)









