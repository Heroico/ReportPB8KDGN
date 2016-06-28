load_file <- function(path) {
  dt <- read.table(path, stringsAsFactors=FALSE)
  
  c <- length(dt[1,])
  cols <- dt[1,2:c]
  
  r <- length(dt[,1])
  rows <- dt[2:r, 1]
  rows <-  gsub("\\..*","",rows) #remove the "version" postfix
  
  d <- dt[2:r, 2:c]
  colnames(d) <- cols
  rownames(d) <- rows
  
  d
}

build_results <- function(d1, d2) {
  genes <- rownames(d2)
  OUT <- data.frame(gene=character(), beta=numeric(), zstat=numeric(), pval=numeric(), se_beta=numeric(), R2=numeric(), R=numeric())
  for (gene in genes) {
    x <- as.numeric(t(d1[gene,]))
    y <- as.numeric(t(d2[gene,]))
    s <- summary(lm(y ~ x))
    results = coef(s)[c(2,6,8,4)]
    c <- cor(x, y)
    l = data.frame(gene=gene, beta=results[1], zstat=results[2], pval=results[3], se_beta=results[4], R2=s$r.squared, R=c)
    OUT<-rbind(OUT,l)
  }
  OUT <- OUT[with(OUT, order(-R2)),]
  #colnames(OUT)<-c("gene","beta","z-stat","pval", "se(beta)", "R2")
  return(OUT)
}

save_stuff <- function(data, n, file_prefix) {
  print("Saving")
  file <- paste(arguments$result_prefix, ".txt", sep="")
  write.table(data,file, col.names=T,row.names=F,quote=F)
  
  print("plotting")
  
  qqR2data <- qqR2(data$R2, n)
  meanr2vec <- round(mean(qqR2data$observed,na.rm=TRUE),4)
  plot_path <- paste(arguments$result_prefix, "_qq.png", sep="", collapse="")
  plot_qqR2(qqR2data, meanr2vec, plot_path)
}

qqR2 <- function(R2,nn)
{
  #deprecated randomness.
  #set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
  set.seed(100)
  ## nn is the sample size, number of individuals used to compute correlation.
  ## needs correlation vector as input.
  ## nullcorvec generates a random sample from correlation distributions, under the null hypothesis of 0 correlation using Fisher's approximation.
  mm <- length(R2)
  nullcorvec <- tanh(rnorm(mm)/sqrt(nn-3)) ## null correlation vector
  data <- data.frame(observed = sort(R2), expected = sort(nullcorvec^2))
  return(data)
}

plot_qqR2 <- function(qqR2data, meanr2vec, plot_path)
{
  p<-ggplot(qqR2data, aes(x=expected,y=observed))+
    geom_point(pch=1,cex=1.5)
  
  p2<- p +
    geom_abline(intercept=0, slope=1) +
    xlab(expression("Expected R"^2)) +
    ylab(expression("Observed Predictive R"^2))+
    theme_bw(20)
  
  ann_text <- data.frame(observed=0.8,expected=0,r2=meanr2vec)
  p3<-p2+
    geom_text(data=ann_text,aes(label=paste("mean_R^2 ==",r2,sep="")),parse=T,hjust=0,size=5)
  
  png(file=plot_path,height=480,width=640)
  print(p3)
  dev.off()
}