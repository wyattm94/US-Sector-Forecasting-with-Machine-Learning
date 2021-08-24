### Functions used for FE582 Final Project


##Functions used for data manipulation and classification

# Generates a correlation matrix for a full data set
cor.agg         <- function(data) 
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
  return(cor.matrix)
}

# Generates a regression model matrix for an entire data set
reg.agg         <- function(data) 
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

# Adds classification of up/down for etf returns 
classify.etfs   <- function(data, col.list) 
{
  out <- list()
  start = col.list[length(col.list)] + 1
  end   = ncol(data)
  
  out.tags <- c()
  for(i in 1:length(col.list))
  {
    target     <- data[,col.list[i]]
    out.tags   <- c(out.tags, names(data)[col.list[i]])
    move.tag   <- paste(names(data)[col.list[i]],"move",sep="")
    class.list <- rep("down", length(target)) ; class.list[target > 0] = "up"
    
    temp       <- data.frame(data[,1],data[,col.list[i]],class.list,data[,start:end])
    cnames     <- c(names(data)[c(1,col.list[i])],move.tag,names(data)[start:end])
    
    colnames(temp) <- cnames
    out[[i]] <- temp 
  }
  names(out) <- out.tags
  return(out)
}


## Functions used for analyzing and subsetting data set correlations for clustering analysis

# Ranks (orders) a correlation/regression (r^2) matrix or data frame by rows
rank.correg     <- function(data)
{
  obs.table <- data
  cnames    <- colnames(obs.table)
  
  orders.name  <- NULL
  orders.value <- NULL
  
  for(i in 1:nrow(obs.table))             # Go by row
  {
    temp  <- obs.table[i,]                # Current row
    
    tcnames <- cnames
    order.n <- c() 
    order.v <- c()
    
    for(j in 1:length(temp))              # Inside current row
    {
      cm = 1                              # Current Max
      for(k in 1:length(temp))            # Check all rows for max, repeat with length -1 until order established
      {
        if(abs(temp[k]) > abs(temp[cm])) { cm = k } 
      }
      
      order.n <- c(order.n, tcnames[cm]) # Add current max correlation "name" to list
      order.v <- c(order.v, temp[,cm])   # Add current max correlation "value" to list
      temp    <- temp[-cm]               # Remove current max corr. elem/name, then retest
      tcnames <- tcnames[-cm]
    }
    
    orders.name   <- rbind(orders.name, order.n)
    orders.value  <- rbind(orders.value, order.v)
  }
  
  rownames(orders.name)  <- rownames(obs.table) # ETF target names added
  rownames(orders.value) <- rownames(obs.table)
  
  out <- list(orders.name, orders.value) ; names(out) <- c("n","v")
  return(out)
}

# Plots ranked variables by absolute value (magnitude of relationship > direction of relationship), name and actuall value (as labels)
plot.ranks      <- function(obs, type, dname, dval)
{
  for(i in 1:length((obs)))
  {
    print(obs)
    val      <- signif(as.numeric(dval[obs[1],]),2)
    val.abs  <- abs(val)  
    name     <- as.character(dname[obs[1],]) 
    
    if(val[1] == 1.00) # Drop correlation with self
    {
      val     <- val[-1]
      val.abs <- val.abs[-1]
      name    <- name[-1]
    }
    
    t <- NULL
    if(type == "correlation") { t = "correlation" }
    if(type == "regression")  { t = "r^2" }
    
    
    plot(val.abs, ylab = t, type = "p", pch = 20, col = "black", main = paste(obs,type,"indicator rankings"))
    text(val.abs, name             , pos=1, offset=0.3, cex=0.7,  col="blue")
    text(val.abs, as.character(val), pos=3, offset=0.3, cex=0.7,  col="red")
  }
  
}

# Ranks a data frame or matrix data set --> CALLS: corr.agg() + reg.agg() + plot.ranks() + rank.correg())
analyze.ranks   <- function(data, title, type, inds = NULL, mincorr, top=NULL,print=FALSE,plot=FALSE,plotc=FALSE,plotr=FALSE)
{
  line <- noquote("-------------------------------------------------------------------------")
  
  print(line)
  print(noquote(paste("Ranking Correlations for",title)))
  print(noquote(paste("(1) Building Correlation + Regression (R^2) Tables...")))
  
  cors.r <- cor.agg(data) ; cors.r <- round(cors.r, 4)
  regs.r <- reg.agg(data) ; regs.r <- round(regs.r, 4)
  
  cors = regs = NULL
  
  if(!(is.null(inds))) # Seperate data into obs and exp data
  {
    cors <- cors.r[1:inds,(inds+1):ncol(cors.r)]
    regs <- regs.r[1:inds,(inds+1):ncol(regs.r)]
  }
  else { cors = cors.r ; regs = regs.r }
  
  print(noquote(paste("(2) Ranking Correlations and R^2 values + Creating Summary Data Frame...")))
  
  cd <- rank.correg(cors)
  rd <- rank.correg(regs)

  nc <- cd[[1]] ; nr <- rd[[1]]
  vc <- cd[[2]] ; vr <- rd[[2]]
  
  # Create DF --> FULL DATA SET
  cor.namerank.df <- NULL
  reg.namerank.df <- NULL
  
  rn <- c() ; cn <- c()
  
  for(i in 1:nrow(nc))
  {
    target <- rownames(nc)[i]
    r1 <- paste(target,"~")
    r2 <- paste("value.",i,sep="")
    
    cor.namerank.df <- rbind(cor.namerank.df, nc[i,]) 
    cor.namerank.df <- rbind(cor.namerank.df, vc[i,])
    
    reg.namerank.df <- rbind(reg.namerank.df, nr[i,])
    reg.namerank.df <- rbind(reg.namerank.df, vr[i,])
    
    rn <- c(rn, r1, r2)
    cn <- c(cn, i)
  }
  
  cn <- cn[1:ncol(nc)]
  
  rownames(cor.namerank.df) <- rn ; colnames(cor.namerank.df) <- cn 
  rownames(reg.namerank.df) <- rn ; colnames(reg.namerank.df) <- cn 
  
  tc <- rownames(nc) ; type.c <- "correlation"
  tr <- rownames(nr) ; type.r <- "regression"
  
  print(noquote(paste("(3) Subsetting abs(Correlations) >=",mincorr,"...")))
  
  cor.sub <- list()
  
  for(i in 1:nrow(nc))
  {
    q      <- as.numeric(quantile(vc[i,])[4])
    subn   <- nc[i,which(abs(vc[i,]) >= mincorr)]
    subv   <- vc[i,which(abs(vc[i,]) >= mincorr)]
    subr   <- vr[i,which(nr[i,] %in% subn)]
    sub.df <- data.frame(subn, subv, subr)
    colnames(sub.df) <- c("data","corr","r2")
    
    cor.sub[[i]] <- sub.df
  }
  names(cor.sub) <- tc
  
  if(print == TRUE)
  {
    print(line)
    titles <- names(cor.sub)
    for(i in 1:length(cor.sub))
    { print(noquote(paste("For",titles[i],": (",paste(cor.sub[[i]][[1]],collapse=", "),")"))) }
  }
  
  if(plot == TRUE) 
  { 
    if(plotc == TRUE && plotr == TRUE)
    {
      par(mfrow=c(1,2))
      for(i in 1:length(nc)) 
      { 
        plot.ranks(rownames(nc)[i], type.c, nc, vc) 
        plot.ranks(rownames(nr)[i], type.r, nr, vr)
      } 
    }
    else if((plotc == TRUE && plotr == FALSE))
    {
      par(mfrow=c(1,1))
      for(i in 1:length(nc))  { plot.ranks(rownames(nc)[i], type.c, nc, vc)  }
    }
    else if((plotc == FALSE && plotr == TRUE))
    {
      par(mfrow=c(1,1))
      for(i in 1:length(nr))  { plot.ranks(rownames(nr)[i], type.r, nr, vr)  }
    }
  }
  
  if(type == "subset") { out <- list(df=cor.sub,rcor=cors.r,scor=cors,rreg=regs.r,sreg=regs); return(out) }
  else if(type == "top" && !(is.null(top)))
  {
    print(noquote(paste("(4) Selecting up to the top",top,"correlated partners...")))
    if(print == TRUE) { print(line) }
    
    selects <- NULL ; rn <- c()
    
    for(i in 1:length(cor.sub))
    {
      sdf <- cor.sub[[i]]
      n   <- min(top, nrow(sdf))
      
      newdf <- data.frame(sdf[1:n,1],sdf[1:n,2],sdf[1:n,3])
      colnames(newdf) <- c("etf","corr","r2")
      
      new.sub[[i]] <- newdf
    }
    names(new.sub) <- tc
    out <- list(df=new.sub,rcor=cors.r,scor=cors,rreg=regs.r,sreg=regs); return(out)
  }
  else if(type == "top" && is.null(top)) { print(noquote("ERROR: top paramter is NULL (return full subsets)")); return(cor.sub) }
  else
  {
    print(noquote(paste("(4) Selecting the best pairs...")))
    if(print == TRUE) { print(line) }
    
    pairs <- NULL ; rn <- c()
    
    for(i in 1:length(cor.sub))
    {
      t <- names(cor.sub)[i]
      
      s <- cor.sub[[i]]
      b <- s[which.max(s$r2),]
      
      if(print == TRUE) { print(noquote(paste("Best Pair:",t,"+",b$etf,"based on corr =",b$corr,"and r^2 =",b$r2))) }
      
      rn <- c(rn, paste("Pair.",i,sep=""))
      
      pairs <- rbind(pairs,c(t,as.character(b$etf),b$corr,b$r2))
    }
    
    rownames(pairs) <- rn ; colnames(pairs) <- c("A","B","Corr","R^2")
    
    out <- list(pairs, cors, cor.namerank.df, regs, reg.namerank.df)
    names(out) <- c("subsets","cormatrix","corrankdf","regmatrix","regrankdf")
    
    return(out)
  }
}


### Functions used for clustering/hierarchal Analysis

wssplot         <- function(data, nc, main) 
{
  ### Function Definition:
  #   Plot Within Sum of Squares (WSS) plot (aka. "scree" or "elbow" plot)
  
  ### Parametrs:
  #   data = data in df or matrix form
  #   nc   = max number of clusters to test up to
  #   main = plotting title
  
  ### Output:
  #   WSS plot to identify best k selection by decreasing value changes as k -> nc
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares", 
       main = paste(main,"WSS ('elbow') Plot"))
}

pc.wss.analysis <- function(data, nc, title, print = TRUE, plot = TRUE)
{
  ### Function Definition:
  #   Analyze the Principal Components and Within Sum of Squares changes for kmeans models 
  
  ### Parametrs:
  #   data  = data in df or matrix form
  #   nc    = max number of clusters to test up to
  #   title = plotting title
  #   print = print resulting output
  #   plot  = plot resulting output
  
  ### Output:
  #   PCA analysis (plot + DF of # components vs. %variance explained in data)
  #   WSS plot for identifying best k means cluster amount
  
  pc  <- prcomp(data)
  sum <- summary(pc)
  imp <- data.frame(t(sum$importance))
  
  loc.25 <- which(floor(imp[,3]*100) >= 25)
  loc.50 <- which(floor(imp[,3]*100) >= 50)
  loc.75 <- which(floor(imp[,3]*100) >= 75)
  loc.85 <- which(floor(imp[,3]*100) >= 85)
  loc.95 <- which(floor(imp[,3]*100) >= 95)
  
  if(length(loc.25) > 1) { loc.25 = loc.25[1] }
  if(length(loc.50) > 1) { loc.50 = loc.50[1] }
  if(length(loc.75) > 1) { loc.75 = loc.75[1] }
  if(length(loc.85) > 1) { loc.85 = loc.85[1] }
  if(length(loc.95) > 1) { loc.95 = loc.95[1] }
  
  if(print == TRUE)
  {
    print(noquote(paste("# Number of PCs needed to explain 25% of the variance:",loc.25)))
    print(noquote(paste("# Number of PCs needed to explain 50% of the variance:",loc.50)))
    print(noquote(paste("# Number of PCs needed to explain 75% of the variance:",loc.75)))
    print(noquote(paste("# Number of PCs needed to explain 85% of the variance:",loc.85)))
    print(noquote(paste("# Number of PCs needed to explain 95% of the variance:",loc.95)))
    print(noquote("------------------------------------------------------------------"))
  }
  
  if(plot == TRUE)
  {
    yticks = c(0,0.25,0.50,0.75,0.85,0.95,1)
    
    plot(imp$Cumulative.Proportion, type = "b", 
         xlab = "Number of Components",
         ylab = "Cumulative Variance Explained",
         axes = FALSE,
         main = paste(title,"PCA analysis"))
    
    
    axis(1)
    axis(2, at = yticks, labels = yticks, las = 2)
    
    abline(h=0.25  , col="red")
    abline(v=loc.25, col= "red")
    abline(h=0.50,   col="yellow")
    abline(v=loc.50, col="yellow")
    abline(h=0.75  , col="blue")
    abline(v=loc.75, col="blue")
    abline(h=0.85  , col="green")
    abline(v=loc.85, col="green")
    abline(h=0.95  , col="green")
    abline(v=loc.95, col="green")
    
    wssplot(data, nc, title)
  }
}

test.kmeans     <- function(data, k0, k1, add.labels = FALSE)
{
  ### Function Definition:
  #   Analze Kmeans models from k0 to k1 clusters (visual plot analysis) 
  
  ### Parametrs:
  #   data       = data in df or matrix form
  #   k0         = starting number of clusters
  #   k1         = max clusters to try up to
  #   add.labels = add text (variable) labels to clustered points (caution for large data sets)
  
  ### Output:
  #   Kmeans analysis plotted clusters using PCA for all k's from k0 to k1
  
  kseq = seq(k0,k1,1)
  for(i in kseq)
  {
    cl     <- kmeans(x = data, centers = i, iter.max = 20, nstart = 20)
    PCA    <- prcomp(data)$x
    title  <- paste("Kmean Clustering (Distance) with k =",i)
    
    if(add.labels == TRUE)
    {
      plot(PCA, col = cl$cluster, pch = 20, cex = 2, main = title)
      text(x=PCA[,1], y=PCA[,2], cex=0.5, pos=4, labels=(rownames(data)))
    }
    else { plot(PCA, col = cl$cluster, pch = 20, cex = 2, main = title) }
  }
}

test.hc         <- function(data, k0, k1)
{
  for(i in k0:k1)
  {
    hc     <- hclust(data, method = "complete")
    hc.cut <- cutree(hc, k = i)
    
    title  <- paste("Hierarchal Clustering with k =",i)
    
    par(mfrow=c(1,1)) ; plot(hc, main = title) ; rect.hclust(hc, k = i, border = "red")
  }
}

run.clustering  <- function(data, k, type, max.elem.out = 5, add.labels = FALSE)
{
  if(type == "kmeans")
  {
    cl     <- kmeans(x = data, centers = k, iter.max = 20, nstart = 20)
    PCA    <- prcomp(data)$x
    title  <- paste("Kmean Clustering with k =",k)
    if(add.labels == TRUE)
    {
      plot(PCA, col = cl$cluster, pch = 20, cex = 2, main = title)
      text(x=PCA[,1], y=PCA[,2], cex=0.5, pos=4, labels=(rownames(data)))
    }
    else { plot(PCA, col = cl$cluster, pch = 20, cex = 2, main = title) }
    
    # Data Frame of obs. and cluster it is in
    clusters <- data.frame(cl$cluster)
    clusters <- data.frame(rownames(clusters), clusters[,1])
    
    for(i in 1:k)
    {
      curr.cluster <- clusters[which(clusters[,2] == as.character(i)),]
      elem.check   <- min(max.elem.out,nrow(curr.cluster))
      elems.out    <- paste(curr.cluster[1:elem.check,1],collapse=" ")
      print(noquote(paste("Cluster ",i," contents (up to ",max.elem.out,") :",elems.out,sep="")))
    }
    
    out <- list(cl, clusters)
    return(out)
  }
  else
  {
    hc     <- hclust(data, method = "complete")
    hc.cut <- cutree(hc, k = k)
    
    title  <- paste("Hierarchal Clustering with k =",k)
    
    par(mfrow=c(1,1)) ; plot(hc, main = title) ; rect.hclust(hc, k = k, border = "red")
    
    clusters <- data.frame(hc.cut)
    clusters <- data.frame(rownames(clusters), clusters[,1])
    
    for(i in 1:k)
    {
      curr.cluster <- clusters[which(clusters[,2] == as.character(i)),]
      elem.check   <- min(max.elem.out,nrow(curr.cluster))
      elems.out    <- paste(curr.cluster[1:elem.check,1],collapse=" ")
      print(noquote(paste("Cluster ",i," contents (up to ",max.elem.out,") :",elems.out,sep="")))
    }
    
    out <- list(hc, clusters)
    return(out)
  }
}

# Function used for Create Data Subets for each ETF by the 4 cluster techniques (+ full data)

create.subs     <- function(sublist, kd, hd, kc, hc, raw)
{
  line <- noquote("-------------------------------------------------------------------------")
  
  # Step 1 - Create 2 column data frames for cluster
  print(line)
  print(noquote("Step 1: Data Adjustments for input paramaters..."))
  
  subs <- sublist$df
  mcor <- sublist$rcor

  type <- c("kd","hd","kc","hc")
  
  # Step 2 - Apply Cluster tags for all 4 classifications (make new list of 10 df)
  print(noquote("Step 2: Attaching Cluster Tags..."))
  
  tags.list <- list()
  for(i in 1:length(subs))
  {
    obs <- as.character(subs[[i]][,1])
    new <- NULL
    for(j in 1:length(obs))
    {
      d1 = kd[kd[,1] == obs[j],2]
      d2 = hd[hd[,1] == obs[j],2]
      c1 = kc[kc[,1] == obs[j],2]
      c2 = hc[hc[,1] == obs[j],2]
      c.tag <- c(obs[j],d1,d2,c1,c2) # 4 tags (classifications) per corr elem, per target indicator
      new <- rbind(new, c.tag)
    }
    tags.list[[i]] <- new
  }
  names(tags.list) <- names(subs)
  
  # Step 3 - Filter the data sub sets by cluster groups vs. correlation to etf target - Lists of subset data (by name)
  print(noquote("Step 3: Filtering parameter lists by best parameter and attempt colinearity removal by cluster..."))
  
  complete.lists <- list()
  for(i in 1:length(tags.list))
  {
    etf  <- names(tags.list)[i]
    temp <- tags.list[[i]]
    new.subs  <- list()
    
    for(j in 2:5)
    {
      clusters <- unique(temp[,j]) # number of clusters in model
      sub2  <- c()
      for(k in 1:length(clusters))
      {
        group <- temp[which(temp[,j] == clusters[k]),1] # Get elem (names) in the current per cluster (elems in same cluster)
        
        if(length(group) == 1) { sub2 <- c(sub2,group) } # If only 1 elem, keep it (hypothesis - no collinearity when alone)
        else
        {
          cors <- abs(mcor[which(rownames(mcor) == etf), which(colnames(mcor) %in% group)]) # Organize elems in group by correlation to etf
          loc.max  <- as.numeric(which.max(cors)) 
          elem.max <- names(cors)[loc.max]
          
          sub2 <- c(sub2, elem.max)
          
          # Now check for other potential candidates in the cluster by correlation % with selected paramater
          items <- names(cors)[-loc.max]
          for(q in 1:length(items))
          {
            act <- raw[[i]]
            
            select <- as.numeric(act[,elem.max])
            test   <- as.numeric(act[,items[q]])
            c.test <- cor.test(select, test)
            
            if(c.test$p.value < 0.05)
            {
              if(abs(c.test$statistic) < 0.50) { sub2 <- c(sub2, elem.max) }
              else { next }
            }
            else { next }
          }
        }
      }
      new.subs[[j-1]]   <- sub2
    }
    names(new.subs)     <- type
    complete.lists[[i]] <- new.subs
  }
  names(complete.lists) <- names(tags.list)
  
  # Step 4 - Return list of 10 finalized etf subset lists (lists of 4 df, so a 10 element list of 4 element lists of data frames)
  print(noquote("Step 4: Creating Final Data Output list (List with data sets)..."))
  
  final.subsets <- list()
  for(i in 1:length(complete.lists))
  {
    temp1 <- complete.lists[[i]]
    comp.list <- list()
    
    act <- raw[[i]]
    move.tag   <- paste(names(tags.list)[i],"move",sep="")
    
    for(j in 1:length(temp1))
    {
      temp2 <- temp1[[j]]
      comp.list[[j]]   <- act[,c(names(tags.list)[i],move.tag,temp2)]
    }
    names(comp.list)   <- type
    final.subsets[[i]] <- comp.list
  }
  names(final.subsets) <- names(tags.list)
  return(final.subsets)
}

# Functions used to construct + Predict Linear Models and Decision Trees

build.lm        <- function(lst, raw, sum = FALSE)
{
  line = noquote("-------------------------------------------------------------------------")
  complete.list <- list()
  sectors    <- names(lst)
  types      <- names(lst[[1]])
  
  for(i in 1:length(lst))
  {
    temp1 <- lst[[i]] # List of 4 cluster classification tables
    model.list <- list()
    
    if(sum == TRUE) { print(noquote(paste("Creating Linear Models for:",sectors[i]))) }
    
    for(j in 1:length(temp1))
    {
      temp2 <- temp1[[j]] # Data list for each of 4 models
      
      target <- names(temp2)[1] 
      elems  <- names(temp2)[3:ncol(temp2)]
      
      base.form <- as.formula(paste(target,"~",1))
      full.form <- as.formula(paste(target,"~",paste(elems,collapse="+")))
      
      base <- lm(base.form, data = temp2)
      full <- lm(full.form, data = temp2)
      
      model <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = 0)
      
      if(sum == TRUE)
      {
        cf.p  <- c()
        coeff <- model$coefficients[-1]
        s     <- summary(model)
        
        if(length(coeff) > 3) { cf.p <- c(names(coeff)[1:3],"...") }
        else { cf.p <- c(names(coeff)) }
        
        r   <- round(s$r.squared,2)
        std <- round(s$sigma,2)
        
        print(noquote(paste("Model (",types[j],") ",target," ~ ",paste(cf.p,collapse="+")," (",length(coeff)," coeffs.) has r^2 . Sigma = ",r," . ",std,sep="")))
      }
      
      model.list[[j]] <- model
    }
    
    # Create from full data set
    act <- raw[[i]]
    
    target <- names(act)[2]
    elems  <- names(act)[4:ncol(act)] # 4 because raw data includes the date
    
    base.form <- as.formula(paste(target,"~",1))
    full.form <- as.formula(paste(target,"~",paste(elems,collapse="+")))
    
    base <- lm(base.form, data = act)
    full <- lm(full.form, data = act)
    
    model <- step(base, scope = list(lower = base, upper = full), direction = "forward", trace = FALSE)
    
    if(sum == TRUE)
    {
      cf.p  <- c()
      coeff <- model$coefficients[-1]
      s     <- summary(model)
      
      if(length(coeff) > 3) { cf.p <- c(names(coeff)[1:3],"...") }
      else { cf.p <- c(names(coeff)) }
      
      r   <- round(s$r.squared,2)
      std <- round(s$sigma,2)
      
      print(noquote(paste("Model (F)  ",target," ~ ",paste(cf.p,collapse="+")," (",length(coeff)," coeffs.) has r^2 . Sigma = ",r," . ",std,sep="")))
    }
    
    model.list[[j+1]]  <- model
    names(model.list)  <- c(types,"fulldata")
    
    complete.list[[i]] <- model.list 
    if(sum == TRUE) { print(line) }
  }
  names(complete.list) <- sectors
  return(complete.list)
}

pred.lm         <- function(models, tests)
{
  cluster.list <- list()
  
  for(i in 1:length(models)) # Major Model list
  {
    etfm    <- models[[i]]        # Taregt ETF models (4)
    target  <- names(models)[i]   # Target ETF Tag (Name)
    clusts  <- names(etfm)        # Cluster names (4)
    tst     <- tests[[i]]         # ETF Data for testing
    tst.act <- tst[,target]       # Actual return data for ETF being tested
    
    move.tag <- paste(target,"move",sep="") # Target Class data name
    tst.cls <- tst[,move.tag]       # Movement Classification data
    
    cluster.res <- list()
    
    for(j in 1:length(etfm)) # Model per clustering technique used
    {
      cm    <- etfm[[j]]                     # Specific Cluster Model
      pred  <- predict.lm(cm, newdata = tst) # Numerical Vector
      
      pred.class <- rep("down", length(pred)) ; pred.class[pred > 0] = "up"
      pred.mse   <- mean((pred-tst.act)^2)
      class.acc  <- (sum(pred.class == tst.cls)/length(pred))*100
      class.err  <- 100 - class.acc
      
      cluster.temp <- list(pred, pred.class, pred.mse, class.acc, class.err)
      names(cluster.temp) <- c("prednum","predclass","predmse","classacc","classerr")
      cluster.res[[j]] <- cluster.temp
    }
    
    names(cluster.res) <- clusts
    cluster.list[[i]] <- cluster.res
  }
  
  names(cluster.list) <- names(models)
  
  # Comparision table for reference
  comp.table <- data.frame(matrix(nrow=5))
  
  for(i in 1:length(cluster.list))
  {
    temp1 <- cluster.list[[i]]
    
    acc.list <- c()
    for(j in 1:length(temp1))
    {
      temp2 <- temp1[[j]]
      
      ca <- temp2$classacc
      acc.list <- c(acc.list, ca)
    }
    comp.table <- cbind(comp.table, acc.list)
  }
  comp.table <- comp.table[,-1]
  rownames(comp.table) <- names(models[[i]])
  colnames(comp.table) <- names(models)
  
  out <- list(cluster.list, comp.table)
  names(out) <- c("predictions","comparision")
  
  return(out)
}

build.dt        <- function(lst, raw, print, plot)
{
  line = "-----------------------------------------------------------------------------------------------------"
  complete.list <- list()
  sectors    <- names(lst)
  types      <- names(lst[[1]])
  
  for(i in 1:length(lst))
  {
    temp1 <- lst[[i]] # List of 4 cluster classification tables
    model.list <- list()
    
    for(j in 1:length(temp1))
    {
      temp2  <- temp1[[j]] # Data list for each of 4 models
      
      target <- names(temp2)[2] 
      elems  <- names(temp2)[3:ncol(temp2)]
      
      form   <- as.formula(paste(target,"~",paste(elems,collapse="+")))
      
      model  <- rpart(form, method = "class", data = temp2)
      
      if(print == TRUE)
      {
        vip <- model$variable.importance
        vn  <- names(vip)
        vv  <- as.numeric(vip)
        print(noquote(paste("Decision Tree: ",target," (",types[j],") - Ranked Variable Importance:")))
        print(vip)
      }
      
      if(plot == TRUE) { prp(model) }
      
      model.list[[j]] <- model
    }
    
    # Create from full data set
    act <- raw[[i]]
    
    target <- names(act)[3] 
    elems  <- names(act)[4:ncol(act)]
    
    form   <- as.formula(paste(target,"~",paste(elems,collapse="+")))
    model  <- rpart(form, method = "class", data = act)
    
    if(print == TRUE)
    {
      vip <- model$variable.importance
      vn  <- names(vip)
      vv  <- as.numeric(vip)
      print(noquote(paste("Decision Tree: ",target," (F)  - Ranked Variable Importance:")))
      print(vip)
    }
    
    if(plot == TRUE) { prp(model) }
    
    model.list[[j+1]] <- model
    names(model.list) <- c(types,"fulldata")
    
    complete.list[[i]] <- model.list 
    if(print == TRUE) { print(line) }
  }
  names(complete.list) <- sectors
  return(complete.list)
}

pred.dt         <- function(models, tests)
{
  cluster.list <- list()
  
  for(i in 1:length(models)) # Major Model list
  {
    etfm    <- models[[i]]        # Taregt ETF models (4)
    target  <- names(models)[i]   # Target ETF Tag (Name)
    clusts  <- names(etfm)        # Cluster names (4)
    tst     <- tests[[i]]         # ETF Data for testing
    
    move.tag <- paste(target,"move",sep="") # Target Class data name
    tst.cls  <- tst[,move.tag]    # Movement Classification data
    
    cluster.res <- list()
    
    for(j in 1:length(etfm)) # Model per clustering technique used
    {
      dm    <- etfm[[j]]                                     # Specific Cluster Model
      pred  <- predict(dm, newdata = tst, type = "class")    # Numerical Vector
      
      class.acc  <- (sum(pred == tst.cls)/length(pred))*100
      class.err  <- 100 - class.acc
      
      cluster.temp <- list(pred, class.acc, class.err)
      names(cluster.temp) <- c("predclass","classacc","classerr")
      cluster.res[[j]] <- cluster.temp
    }
    
    names(cluster.res) <- clusts
    cluster.list[[i]]  <- cluster.res
  }
  
  names(cluster.list) <- names(models)
  
  # Comparision table for reference
  comp.table <- data.frame(matrix(nrow=5))
  
  for(i in 1:length(cluster.list))
  {
    temp1 <- cluster.list[[i]]
    
    acc.list <- c()
    for(j in 1:length(temp1))
    {
      temp2 <- temp1[[j]]
      
      ca <- temp2$classacc
      acc.list <- c(acc.list, ca)
    }
    comp.table <- cbind(comp.table, acc.list)
  }
  comp.table <- comp.table[,-1]
  rownames(comp.table) <- names(models[[i]])
  colnames(comp.table) <- names(models)
  
  out <- list(cluster.list, comp.table)
  names(out) <- c("predictions","comparision")
  
  return(out)
}

# Functions used to perform backtesting on model predictions and market/etf holding portfolios

sim.hold        <- function(data, type, title, list.loc = NULL, add.cum = FALSE, capital = 10000000, print.sum = TRUE, print.all = FALSE)
{
  line = noquote("------------------------------------------------------------------------------")
  print(line)
  print(noquote(paste("Running back test for:",title)))
  
  if(type == "vector")
  {
    rlist <- c(capital); crlist <- c(0)
    
    for(i in 2:(length(data)+1))  { r = rlist[i-1]*(1+data[i-1]); rlist <- c(rlist, r) }
    for(i in 2:length(rlist)) { cr = round(((rlist[i]-rlist[1])/rlist[1])*100,2); crlist <- c(crlist,cr) }
    
    trade.return = round(((rlist[length(rlist)] - rlist[1])/rlist[1])*100,2)
    
    out <- list(trade.return, crlist, rlist)
    names(out) <- c("return","cumretlist","perretlist")
    
    if(print.sum == TRUE) { print(noquote(paste("Back Test over",length(data),"periods yielded a",trade.return,"% Return"))) }
    return(out)
  }
  else if(type == "list" && !(is.null(list.loc)))
  {
    pr.list <- list(); r.df <- NULL; cr.df <- NULL
    capdiv = capital*(1/length(data))
    for(i in 1:length(data))
    {
      chunk <- data[[i]]
      test  <- chunk[,list.loc]
      rlist <- c(capdiv); crlist <- c(0)
      
      for(j in 2:(length(test)+1)) { r = rlist[j-1]*(1+test[j-1]); rlist <- c(rlist, r) }
      for(j in 2:length(rlist))    { cr = round(((rlist[j]-rlist[1])/rlist[1])*100,2); crlist <- c(crlist,cr) }
      
      trade.return = round(((rlist[length(rlist)] - rlist[1])/rlist[1])*100,2)
      
      if(print.all == TRUE) { print(noquote(paste("Back Test (",names(data)[i],") over ",length(test)," periods yielded a ",
                                              trade.return," % Return",sep=""))) }
      
      r.df <- cbind(r.df, rlist); cr.df <- cbind(cr.df, crlist); pr.list[[i]] <- trade.return
    }
    
    colnames(r.df) <- names(data); colnames(cr.df) <- names(data); names(pr.list) <- names(data)
    
    combine.r <- NULL; cum.cr <- c(0)
    
    if(add.cum == TRUE)
    {
      combine.r <- rowSums(r.df)
      for(j in 2:length(combine.r)) { cr = round(((combine.r[j]-combine.r[1])/combine.r[j])*100,2); cum.cr <- c(cum.cr,cr) }
      
      trade.return = round(((combine.r[length(combine.r)] - combine.r[1])/combine.r[1])*100,2)
      out <- list(pr.list, cr.df, r.df, trade.return, cum.cr, combine.r)
      names(out) <- c("retlist","cumretdf","perretdf","combret","combcumretlist","combperretlist")
      
      if(print.sum == TRUE) { print(noquote(paste("Cumulative Results over",(length(combine.r)-1),"periods yielded a",trade.return,"% Return"))) }
      
      return(out)
    }
    else
    {
      print(noquote("User did not analyse Cumulative Returns for this data set..."))
      out <- list(pr.list, cr.df, r.df)
      names(out) <- c("retlist","cumretdf","perretdf")
      return(out)
    }
  }
  else { print(noquote("Error: Type is invalid or list.loc (data index) is NULL...")); return(NULL) }
}

pred.sim        <- function(preds, tests, title, capital = 10000000, print.sum = TRUE, print.all = FALSE)
{
  line = noquote("------------------------------------------------------------------------------")
  print(line)
  print(noquote(paste("Running back test for:",title)))
  print(line)
  
  results <- list()
  etfn    <- names(preds)
  cn      <- names(preds[[1]])
  capdiv  <- capital*(1/(length(preds)))
  
  hold <- list()
  for(i in 1:length(preds))
  {
    etfp <- preds[[i]] 
    test <- tests[[i]]; test <- test[,2]
    
    p.df <- NULL; c.df <- NULL; rets <- list()
    
    for(j in 1:length(etfp))
    {
      cp    <- etfp[[j]]
      
      pred  <- cp$predclass
      acc   <- round(cp$classacc,2)
      plist <- c(capdiv); clist <- c(0)
      
      for(k in 2:(length(pred)+1))
      {
        if(pred[k-1] == "up"   && test[k-1] >= 0) { pr = plist[k-1]*(1+test[k-1])      ; plist <- c(plist,pr) } #W
        if(pred[k-1] == "up"   && test[k-1] <= 0) { pr = plist[k-1]*(1-abs(test[k-1])) ; plist <- c(plist,pr) } #L
        if(pred[k-1] == "down" && test[k-1] <= 0) { pr = plist[k-1]*(1+abs(test[k-1])) ; plist <- c(plist,pr) } #W
        if(pred[k-1] == "down" && test[k-1] >= 0) { pr = plist[k-1]*(1-abs(test[k-1])) ; plist <- c(plist,pr) } #L
      }
      
      for(k in 2:length(plist)) { cr = round(((plist[k]-plist[1])/plist[1])*100,2); clist <- c(clist,cr) }
      
      trade.return = round(((plist[length(plist)] - plist[1])/plist[1])*100,2)
      
      p.df <- cbind(p.df, plist); c.df <- cbind(c.df, clist) ; rets[[j]] <- trade.return
      
      if(print.all == TRUE) 
      { print(noquote(paste("Back Test (",etfn[i]," - ",cn[j]," -- Accuracy: ",acc," %) over ",length(pred)," periods yielded a ",
                          trade.return," % Return",sep=""))) }
      
    }
    if(print.all == TRUE) { print(line) }
    colnames(p.df) <- cn; colnames(c.df) <- cn; names(rets) <- cn
    hold[[i]] <- list(rets, c.df, p.df); names(hold[[i]]) <- c("retlist","cumretdf","perretdf")
  }
  names(hold) <- etfn
  
  # Aggregate Return data sets by cluster for cumulative analysis
  agg.df.list <- list()
  
  for(i in 1:length(cn))
  {
    agg.r <- NULL
    for(j in 1:length(hold)) 
    { 
      h <- hold[[j]]
      d <- h[[3]]
      agg.r  <- cbind(agg.r, d[,i]) 
    }
    colnames(agg.r) <- etfn
    agg.df.list[[i]] <- agg.r
  }
  names(agg.df.list) <- cn
  
  # Create Cumulative Return data
  comb.r.df <- NULL; cum.r.df <- NULL; rets <- list()
  for(i in 1:length(agg.df.list))
  {
    sub       <- agg.df.list[[i]]
    combine.r <- rowSums(sub)
    cum.cr    <- c(0)
    
    for(j in 2:length(combine.r)) { cr = round(((combine.r[j]-combine.r[1])/combine.r[1])*100,2); cum.cr <- c(cum.cr,cr) }
    
    trade.return = round(((combine.r[length(combine.r)] - combine.r[1])/combine.r[1])*100,2)
    
    if(print.sum == TRUE) { print(noquote(paste("Cumulative Results using (",cn[i],") yielded a ",trade.return," %",sep=""))) }
    
    comb.r.df <- cbind(comb.r.df, combine.r)
    cum.r.df  <- cbind(cum.r.df, cum.cr)
    rets[[i]] <- trade.return
  }
  colnames(comb.r.df) <- cn
  colnames(cum.r.df)  <- cn
  names(rets)         <- cn
  
  # Create output list
  for(i in 1:length(preds)) { results[[i]] <- hold[[i]] }
  results[[(length(results)+1)]] <- list(cumrets=rets, cumretdf=cum.r.df, combretdf=comb.r.df)
  names(results) <- c(etfn,"cumstats")
  
  return(results)
}

# Functions used to create data sets (Data Frames/tables/lists/vectors/Numericals) for comparable analysis

confusion.matrix <- function(pred, act)
{
  confm    <- table(pred, act)
  
  error <- c()
  for(i in 1:nrow(confm))
  {
    err = 100 - round(((confm[i,i]/sum(confm[,i]))*100),2)
    error <- c(error, err)
  }
  
  confm <- cbind(confm, error)
  
  return(confm)
}

extract.comp     <- function(acc.t, pred.t, sim.t, t, capital, print) # Called by cum.dat (Can also be used as standalone Function)
{
  line = noquote("-----------------------------------------------------------------------------------")
  capdiv = capital*(1/ncol(acc.t))
  out <- list()
  
  t.temp = p.temp = NULL
  
  mat.r = mat.a = mat.d = NULL; t.stat <- c(); pers = 0
  
  for(i in 1:ncol(acc.t))
  {
    if(print == TRUE) { print(noquote(paste("Analyzing:",names(acc.t)[i],"  - Calculating performance Data Frame..."))) }
    perf <- NULL; cm.l <- list(); cm.df <- NULL
    p.collect <- NULL
    
    st.all   <- sim.t[[i]]
    pred.all <- pred.t[[i]]
    test.all <- t[[i]] ; test <- test.all[,3]
    
    sum.d <- length(which(test == "down")) # Sum of down for current ETF
    sum.u <- length(which(test == "up"))   # Sum of up for current ETF
    
    pers = sum(sum.d, sum.u)
    
    #if(print == TRUE) { print(noquote(".")) }
    
    # Performance DF
    at   <- round(acc.t[,i],2)
    st   <- as.numeric(st.all[[1]])
    dr   <- lapply(st, function(x) return(capdiv*(1+(x/100))))
    perf <- cbind(perf, at, st, dr)
    rownames(perf) <- rownames(acc.t)
    colnames(perf) <- c("Accuracy","Return (%)","Return ($)")
    
    # Comparison Matrices
    mat.a <- cbind(mat.a, as.numeric(at))
    mat.r <- cbind(mat.r, as.numeric(st))
    mat.d <- cbind(mat.d, as.numeric(dr))
    
    #if(print == TRUE) { print(noquote("..")) }
    
    # Confusion Matrix
    for(j in 1:length(pred.all))
    {
      pred.sub  <- pred.all[[j]]
      class     <- pred.sub$predclass
      cm.l[[j]] <- confusion.matrix(class, test)
    }
    
    cm.df  <- rbind(cm.df,c(as.numeric(sum.d), as.numeric(sum.u), 0,0))
    
    for(j in 1:length(cm.l))
    {
      c     <- cm.l[[j]]
      dp    <- c[1,1] ; de <- c[1,3]
      up    <- c[2,2] ; ue <- c[2,3]
      cm.df <- rbind(cm.df,c(dp, up, de, ue))
      p.collect <- rbind(p.collect, c(dp,up))
    }
    
    colnames(cm.df) <- c("Down","Up","Error-Down","Error-Up")
    rownames(cm.df) <- c("Actual",rownames(acc.t))
    
    #if(print == TRUE) { print(noquote("...")) }
    
    # Collection of data for cumulative analysis
    t.stat      <- c(t.stat, as.numeric(test.all[,2])) # Test Data Collector (Actual)
    t.temp      <- rbind(t.temp, c(sum.d, sum.u))      # Test Data Collector (Class)
    p.temp[[i]] <- p.collect                           # Pred Data Collector (Class)
    
    out[[i]] <- list(performance=perf,confusion=cm.df) # Performance/Confusion for ETFs in current Model
  }
  print(line)
  if(print == TRUE) { print(noquote(paste("(Ind - 1) Calculating Cumulative Performance/Confusion Matrices..."))) }
  
  names(out) <- colnames(acc.t)
  
  # Clustered Cumulative Data - Performance + Confusion Matrix + Matrices ****
  
  mat.a <- as.matrix(mat.a)
  mat.r <- as.matrix(mat.r)
  mat.d <- as.matrix(mat.d)
  
  colnames(mat.a) <- colnames(acc.t); rownames(mat.a) <- rownames(acc.t)
  colnames(mat.r) <- colnames(acc.t); rownames(mat.r) <- rownames(acc.t)
  colnames(mat.d) <- colnames(acc.t); rownames(mat.d) <- rownames(acc.t)
  
  cum.df <- NULL; cumcf <- NULL
  
  cumret <- sim.t$cumstats$cumrets # Cumulative Returns for current modelling type
  actp   <- colSums(t.temp) 
  cumcf  <- rbind(cumcf, c(actp[1],actp[2],0,0))
  
  for(i in 1:length(cumret)) # Get down.up preds for each etf (per cluster 1-5)
  {
    mp  <- NULL            
    for(j in 1:length(p.temp)) { curr.p <- p.temp[[j]] ; mp <- rbind(mp, curr.p[i,]) } 
    mp  <- colSums(mp)
    
    da = round((mp[1]/actp[1])*100,2)
    ua = round((mp[2]/actp[2])*100,2)
    de = 100 - da
    ue = 100 - ua
    
    ta = round(((mp[1]+mp[2])/(actp[1]+actp[2]))*100,2)
    
    tr = capital*(1+as.numeric(cumret[[i]]/100))
    
    # Cumulative Confusion Matrix and Performance DF for the current modelling technique
    cumcf  <- rbind(cumcf, c(mp[1],mp[2],de,ue))
    cum.df <- rbind(cum.df, c(as.numeric(ta), as.numeric(cumret[i]), as.numeric(tr)))
  }
  
  if(print == TRUE) 
  { print(noquote(paste("(Ind - 2) Ordering performance by % Return + Adding Actual Data Return Statistics to Confusion Matrix..."))) }
  
  cum.df <- as.matrix(cum.df)
  cum.df <- cum.df[order(cum.df[,2],decreasing=TRUE),]
  
  # Add analysis of up and down price movements (actual data - proportions + mean/sd)
  t.down <- t.stat[which(t.stat < 0 )]
  t.up   <- t.stat[which(t.stat > 0 )]
  
  d.mu = round(mean(t.down),2); d.sd = round(sd(t.down),2); d.prp = round((length(t.down)/length(t.stat)),2)
  u.mu = round(mean(t.up),2)  ; u.sd = round(sd(t.up),2)  ; u.prp = round((length(t.up)/length(t.stat)),2)
  
  eu.mu = round(mean(as.numeric(cumcf[-1,3])),2); eu.sd = round(sd(as.numeric(cumcf[-1,3])),2)
  ed.mu = round(mean(as.numeric(cumcf[-1,4])),2); ed.sd = round(sd(as.numeric(cumcf[-1,4])),2)
  
  cumcf <- rbind(cumcf, c(d.prp,u.prp,0,0))
  cumcf <- rbind(cumcf, c(d.mu,u.mu,ed.mu,eu.mu))
  cumcf <- rbind(cumcf, c(d.sd,u.sd,ed.sd,eu.sd))
  
  colnames(cumcf) <- c("Down","Up","Error-Down","Error-Up")
  rownames(cumcf) <- c("Actual",rownames(acc.t),"% total","Mean","Std.")
  
  colnames(cum.df) <- c("Accuracy","Return (%)","Return ($)")
  rownames(cum.df) <- rownames(acc.t)
  
  if(print == TRUE) { print(noquote(paste("(Ind - 3) Ranking ETFs based on performance..."))) }
  
  # Rank by ETF
  rank.etf = NULL
  
  rank.etf <- data.frame(colnames(mat.r),colMeans(mat.r),colMeans(mat.a))
  rank.etf <- rank.etf[order(rank.etf[,2],decreasing=TRUE),]
  
  colnames(rank.etf) <- c("ETF","Avg Return","Avg Accuracy")
  rownames(rank.etf) <- seq(1,nrow(rank.etf),1)
  
  if(print == TRUE) { print(noquote(paste("(Ind - 4) Calculating Difference Matrix (% Return of cluster-based models vs. Full Data)..."))) }

  # Difference from full data with row means and colmeans
  diff.f5 <- NULL
  for(i in 1:ncol(mat.d))
  {
    temp <- c()
    for(j in 1:(nrow(mat.d)-1))
    {
      d5 = mat.d[5,i]; dc = mat.d[j,i]
      diff = round(((dc-d5)/d5)*100,2) # Return Difference
      temp <- c(temp, diff)
    }
    diff.f5 <- cbind(diff.f5, c(temp, 0)) # Base at 0
  }
  
  f5.rm <- RowMeans(diff.f5[1:4,]); diff.f5 <- cbind(diff.f5, c(f5.rm,0))

  colnames(diff.f5) <- c(colnames(acc.t), "Avg")
  rownames(diff.f5) <- c(rownames(acc.t))
  
  if(print == TRUE) { print(noquote(paste("(Ind - 5) Select Best clustering technique (including Full Data) per ETF (optimized backtest)..."))) }
  
  # Identify Best models from this model group for optimized backtest (10)
  best.models <- c()
  
  for(i in 1:ncol(mat.r))
  {
    loc <- which.max(mat.r[,i])
    best.models <- c(best.models,rownames(mat.r)[loc])
  }
  
  opt.df <- NULL; rn <- c()
  for(i in 1:length(best.models))
  {
    a = as.numeric(mat.a[best.models[i],i])
    r = as.numeric(mat.r[best.models[i],i])
    d = as.numeric(mat.d[best.models[i],i])
    opt.df <- rbind(opt.df, c(a, r, d))
    
    rn <- c(rn, paste(colnames(mat.d)[i],"~",best.models[i],sep=""))
  }
  colnames(opt.df) <- c("Accuracy","Return (%)","Return ($)")
  rownames(opt.df) <- rn
  
  accs <- c()
  for(i in 1:nrow(opt.df)) {accs <- c(accs, as.numeric((opt.df[i,1]*(1/100))*pers)) }
  
  a = round((sum(accs)/(nrow(opt.df)*pers))*100,2)
  d = round((sum(as.numeric(opt.df[,3]))-capital),2)
  r = round((d/capital)*100,2)
  
  opt.perf <- list(optsum=opt.df,acc=a,pret=r,dret=d)
  
  
  out[[(length(out)+1)]] <- list(performance=cum.df,confusion=cumcf,
                                  acctable=mat.a,rettable=mat.r,proftable=mat.d,
                                  rankedetfs=rank.etf,clustervsall=diff.f5,best=opt.perf)
  
  names(out) <- c(colnames(acc.t),"cumbycluster"); print(line)
  return(out)
}

cum.dat          <- function(mp, mc, ms, etf.t, capital, print)
{
  line = noquote("-----------------------------------------------------------------------------------")
  print(line)
  ## 1 - Aggregate Testing Data (1 DF of 20 columns (price + class per etf))
  if(print == TRUE) { print(noquote(paste("(1) Collecting All Testing data (Actual and Class)..."))) }
  
  t.df <- NULL; cn <- c()
  for(i in 1:length(etf.t))
  {
    etf  <- etf.t[[i]]
    t.df <- cbind(t.df,etf[,2],etf[,3])
    cn   <- c(cn,names(etf)[2:3])
  }
  colnames(t.df) <- cn
  
  ## 2 - Aggregate Accuracy Data (by model, by cluster type, by etf) --> 3 df (All running together)
  ## * Also extract accuracy matrices (as whole for ind. tests)
  if(print == TRUE) { print(noquote(paste("(2) Collecting Accuracy Data By: Model / Cluster Analysis / ETF..."))) }
  
  a.m = a.k = a.etf = NULL; ind.acc <- list()
  
  for(i in 1:length(mc)) 
  { 
    a.m <- cbind(a.m, as.vector(t(mc[[i]]))) 
    ind.acc[[i]] <- mc[[i]]
    
    for(j in 1:nrow(mc[[i]])) 
    { 
      a.etf <- rbind(a.etf, as.numeric(mc[[i]][j,])) 
      for(k in 1:ncol(mc[[i]])) { a.k   <- rbind(a.k, as.numeric(mc[[i]][,k])) }
    }
  }
  
  colnames(a.m)   <- names(mc)
  colnames(a.k)   <- rownames(mc[[1]])
  colnames(a.etf) <- names(mc[[1]])
  
  acc.list <- list(bymodel=a.m,bycluster=a.k,byetf=a.etf)
  
  ## 3 - Aggregate Return Data (by model, by cluster type, by etf) --> 3 df (All running together)
  ## 4 - (Include) Get strategy returns and lists of cum ret and cum profit dfs
  if(print == TRUE) 
  { print(noquote(paste("(3) Collecting Return Data By: Model / Cluster Analysis / ETF + Extracting Cumulative Results (per model type)..."))) }
  
  r.m = r.k = r.etf = p.cum = NULL; cr.list <- list(); cp.list <- list()
  
  for(i in 1:length(ms))
  {
    m.temp <- c(); etf.temp <- list(); etf.temp2 <- NULL
    etf <- ms[[i]]
    for(j in 1:length(etf))
    {
      if(j == length(etf))
      {
        p.cum <- cbind(p.cum, etf[[j]][[1]])
        cr.list[[i]] <- etf[[j]][[2]]
        cp.list[[i]] <- etf[[j]][[3]]
        next
      }
      m.temp <- c(m.temp,  as.numeric(etf[[j]][[1]]))
      r.k    <- rbind(r.k, as.numeric(etf[[j]][[1]]))
      if(i == 1) { r.etf <- cbind(r.etf, as.numeric(etf[[j]][[1]])) }
      else { etf.temp[[j]] <- as.numeric(etf[[j]][[1]]) }
    }
    r.m <- cbind(r.m, m.temp)
    if(i != 1) { for(j in 1:length(etf.temp)) { etf.temp2 <- cbind(etf.temp2, as.numeric(etf.temp[[j]])) }}
    if(i != 1) { for(j in 1:nrow(etf.temp2))  { r.etf <- rbind(r.etf, etf.temp2[j,])                     }}
  }
  colnames(r.m)   <- names(ms)
  colnames(r.k)   <- names(ms[[1]][[1]][[1]])
  colnames(r.etf) <- names(ms[[1]])[1:(length(ms[[1]])-1)]
  
  colnames(p.cum) <- names(ms)
  
  ret.list <- list(bymodel=r.m,bycluster=r.k,byetf=r.etf)
  cum.list <- list(performance=p.cum,cumrets=cr.list,cumprofs=cp.list)
  
  ## 5 - Calculate Individual Testing Mechanisms (Across Models with respect to Cluster Models)
  if(print == TRUE) { print(noquote(paste("(4) Calculating In-depth Comparision Data Types for Each Model..."))) }
  
  ind.list <- list()
  for(i in 1:length(mc)) 
  { 
    if(print == TRUE) { print(noquote(paste("Current Model:",names(mc)[i],"- (External Function Call)..."))); print(line) }
    ind.list[[i]] <- extract.comp(mc[[i]], mp[[i]], ms[[i]], etf.t, capital, print) 
  }
  
  names(ind.list) <- names(mc)
  
  out <- list(test=t.df,accuracy=acc.list,return=ret.list,cumperf=cum.list,indcomp=ind.list)
  return(out)
  
}

















