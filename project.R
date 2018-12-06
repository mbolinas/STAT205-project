
# Test for checking if outliers exist in the data
outliers = function(v,length){
  vq1 = quantile(v,0.25, type=6); 
  vq3 = quantile(v,0.75, type=6);
  outliers = FALSE; #Start off with assumption
  for(i in 1:length){
    if(v[i] < (vq1 - 1.5*IQR(v, type=6)) | 
       v[i] > (vq3 + 1.5*IQR(v, type=6))){
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
  cat(sprintf("Running matched pairs test on data...\n"))
  valid = TRUE
  #both vectors v1 and v2 must satisfy ALL:
  #no outliers OR >39 entries
  #normally distributed
  #^^if either are false, test cannot be run
  if(swt(v1) == FALSE | swt(v2) == FALSE){
    cat(sprintf("Cannot run matched pairs test: data is not normal\n"))
    valid = FALSE
  }
  if(outliers(v1, size1) == TRUE & size1 < 40){
    cat(sprintf("Cannot run matched pairs test: data contains outliers with small sample size\n"))
    valid = FALSE
  }
  if(outliers(v2, size2) == TRUE & size2 < 40){
    cat(sprintf("Cannot run matched pairs test: data contains outliers with small sample size\n"))
    valid = FALSE
  }
  
  if(valid == TRUE){
    dif = vectordiff(v1, v2)
    if(sd(dif) == 0){
      cat(sprintf("Both columns of data are identical\n"))
    } else{
      ttestp = t.test(dif)$p.value
      cat(sprintf("p-value (matched-pairs): %s\n", ttestp))
      if(acceptnull(ttestp) == TRUE)
        cat(sprintf("We accept the null hypothesis\n"))
      else
        cat(sprintf("We reject the null hypothesis\n"))
    }
  }
  else{
    cat(sprintf("(Matched pairs test failed. Trying sign test...)\n"))
    signt(v1, v2, size1, size2)
  }
}

signt <- function(v1,v2,size1,size2){
  cat(sprintf("Running sign test on data...\n"))
  signtp = SIGN.test(v1, v2)$p.value
  cat(sprintf("p-value (sign-test): %s\n", signtp))
  if(acceptnull(signtp) == TRUE)
    cat(sprintf("We accept the null hypothesis\n"))
  else
    cat(sprintf("We reject the null hypothesis\n"))
}

pooled <- function(v1, v2, size1, size2){
  
}

twosample <- function(v1, v2, size1, size2){
  
}

ztest <- function(v1, v2, size1, size2){
  cat(sprintf("Running z-test of equal proportions on data...\n"))
  ztestp = prop.test(c(sum(v1), sum(v2)), n=c(size1, size2))$p.value
  cat(sprintf("p-value (z-test): %s\n", ztestp))
  if(acceptnull(ztestp) == TRUE)
    cat(sprintf("We accept the null hypothesis\n"))
  else
    cat(sprintf("We reject the null hypothesis\n"))
}

ftest <- function(v1, v2, size1, size2){
  cat(sprintf("Running f-test of equal variance on data...\n"))
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
  cat(sprintf("[Import data (.csv file) to test...]\n\n"))
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
    ztest(x1, y1, x1length, y1length)
  }
  else if(CSK == "S"){
    #run f test equal variance
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
      
      matchedpairst(x1, y1, x1length, y1length)
      #signt(x1, y1, x1length, y1length)
    }
  }
  cat(sprintf("\n[End of script]\n"))
}

main()

