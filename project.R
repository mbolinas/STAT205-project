
outliers <- function(vector, size){
  q1 <- quartile(vector, .25);
  q3 <- quartile(vector, .75);
  hasoutliers = FALSE;
  
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
  
}

ftest <- function(v1, v2, size1, size2){
  
}

main <- function(){
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
 
  if(DI == "I"){
    if(CSK == "C"){
      print("center, independent")
    }else if(CSK == "S"){
      print("spread, independent")
    }else{
      print("binary, independent")
    }
  }else{
    if(CSK == "C"){
      print("center, dependent")
    }else if(CSK == "S"){
      print("spread, dependent")
    }else{
      print("binary, dependent")
    }
  }
   
}

main()

