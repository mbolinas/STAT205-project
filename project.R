
# Test for checking if outliers exist in the data
outliers = function(v,length){
  vq1 = quantile(v,0.25); 
  vq3 = quantile(v,0.75);
  outliers = FALSE; #Start off with assumption
  for(i in 1:length){
    if(v[i] < (vq1 - 1.5*IQR(v)) | 
       v[i] > (vq3 + 1.5*IQR(v))){
      outliers = TRUE;
      return(outliers);
    }
  }
  return(outliers);
}

# Test for checking if data is normally distributed
swt = function(v){
  r = shapiro.test(v);
  bool = FALSE;
  if(r$p.value > .05){
    # passed the test
    bool = TRUE;
  }
  return(bool);
}

acceptnull <- function(p){
   if(p < .05){
     return(FALSE)
   }else{
     return(TRUE)
   }
}

vectordiff <- function(v1, v2){
  if(length(v1) != length(v2)){
    return(NULL);
  }else{
    tmp <- numeric(length(v1))
    for(i in 1:length(v1)){
      tmp[i] = v1[i] - v2[i]
    }
    return(tmp);
  }
}


matchedpairst <- function(v1,v2,size1,size2){
  
}

sign <- function(v1,v2,size1,size2){
  
}

pooled <- function(v1, v2, size1, size2){
  
}

twosample <- function(v1, v2, size1, size2){
  
}

ztest <- function(v1, v2, size1, size2){
  ztestp = prop.test(c(sum(v1), sum(v2)), n=c(size1, size2))$p.value
  cat(sprintf("p-value (z-test): %s\n", ztestp))
  if(acceptnull(ztestp) == TRUE)
    cat(sprintf("We accept the null hypothesis\n"))
  else
    cat(sprintf("We reject the null hypothesis\n"))
}

ftest <- function(v1, v2, size1, size2){
  #f-test requires data to be normal: shapiro-wilk test of normality
  if(shapiro.test(v1)$p.value > .05 && shapiro.test(v2)$p.value > .05){
    ftestp = var.test(v1, v2)$p.value
    cat(sprintf("p-value (f-test): %s\n", ftestp))
    if(acceptnull(ftestp) == TRUE)
      cat(sprintf("We accept the null hypothesis\n"))
    else
      cat(sprintf("We reject the null hypothesis\n"))
  } else{
    print("Cannot run f-test for equal variance: data not normal")
  }
}

main <- function(){
  print("Import data (.csv) to test...")
  x <- read.csv(file=file.choose(), header=TRUE,sep=",");
  x1 <- x[,1];
  y1 <- x[,2];
  x1 <- x1[!is.na(x1)];
  y1 <- y1[!is.na(y1)];
  x1length <- length(x1);
  y1length <- length(y1);
  CSK <- as.character(x[1,3]);
  DI <- as.character(x[2,3]);
  x1y1 <- c(x1,y1);
 
  
  if(CSK == "K"){
    #run z test equal proportions
    print("Running z-test of equal proportions on data...")
    ztest(x1, y1, x1length, y1length)
  }
  else if(CSK == "S"){
    #run f test equal variance
    print("Running f-test of equal variance on data...")
    ftest(x1, y1, x1length, y1length)
  }
  else{
    if(DI =="I"){
      #either two sample t or pooled two sample t
      #check data size, outliers, distribution, to pick
    }
    else{
      #either matched pairs t or sign
      #check data size, outliers, distribution, to pick
    }
  }
  print("[End of script]")
}

main()

