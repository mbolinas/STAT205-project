
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

matchedtest <- function(v, length){
  if(swt(v) & (!outliers(v,length) | length >= 40))
    mtest = TRUE
  else
    mtest = FALSE
  return(mtest)
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
        cat(sprintf("We fail to reject the null hypothesis\n"))
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
    cat(sprintf("We fail to reject the null hypothesis\n"))
  else
    cat(sprintf("We reject the null hypothesis\n"))
}

pooled <- function(v1, v2, size1, size2){
  cat(sprintf("Running pooled test on data...\n"))
  combined = c(v1,v2);
  ttest = t.test(combined);
  pttestp = ttest$p.value;
  cat(sprintf("p-value (pooled two sample t): %s\n", pttestp))
  if(acceptnull(pttestp) == TRUE){
    cat(sprintf("We fail to reject the null hypothesis\n"));
  }
  else{
    cat(sprintf("We reject the null hypothesis\n"));
  }
}

twosample <- function(v1, v2, size1, size2){
  cat(sprintf("Running two sample test on data...\n"))
   if(matchedtest(v1, size1) == TRUE & matchedtest(v2, size2) == TRUE){
    diffs = vectordiff(v1,v2);
    if(sd(diffs) != 0){
      ttest = t.test(v1,v2);
      tttest = ttest$p.value;
      cat(sprintf("p-value (two sample t): %s\n", tttest))
      if(acceptnull(tttest) == TRUE){
        cat(sprintf("We fail to reject the null hypothesis\n")); 
      }
      else{
        cat(sprintf("We reject the null hypothesis\n")); 
      }
    }
    else{
      cat(sprintf("Both columns of data are identical\n"))
    }
  }
  else{
    print("Data fails matchtest");
  }
}

ztest <- function(v1, v2, size1, size2){
  cat(sprintf("Running z-test of equal proportions on data...\n"))
  ztestp = prop.test(c(sum(v1), sum(v2)), n=c(size1, size2))$p.value
  cat(sprintf("p-value (z-test): %s\n", ztestp))
  if(acceptnull(ztestp) == TRUE)
    cat(sprintf("We fail to reject the null hypothesis\n"))
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
      cat(sprintf("We fail to reject the null hypothesis\n"))
    else
      cat(sprintf("We reject the null hypothesis\n"))
  } else{
    cat(sprintf("Cannot run f-test for equal variance: data not normal\n"))
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
  
  sd1 <- sd(x1);
  sd2 <- sd(y1);
  #print(x1)
  #print(y1)
  
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
      if(swt(x1) == TRUE && swt(y1) == TRUE){
        if(sd1 > sd2){
          checkRatio = sd1/sd2;
        }
        else{
          checkRatio = sd2/sd1;
        }
        if(checkRatio <= 2){
          # The data passed the test
          pooled(x1,y1,x1length,y1length);
        }
        else{
          # The data did not pass the test
          cat(sprintf("The variances did not pass the test for pooled; running two sample instead\n"))
          twosample(x1,y1,x1length,y1length);
        }
      }
      else{
        print("Data is not normally distributed");
      }
    }
    else{
      #if matched pairs fails, it will run sign test
      matchedpairst(x1, y1, x1length, y1length)
    }
  }
  cat(sprintf("\n[End of script]\n"))
}

main()

