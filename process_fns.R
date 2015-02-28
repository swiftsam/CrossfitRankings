DeUnitWeight <- function(str.val){
  if(str.val %in% c("--","", NA)) {return(NA)}
  unit  <- substr(str.val,
                  nchar(str.val)-1,
                  nchar(str.val))
  value <- as.numeric(substr(str.val,
                             1, nchar(str.val)-3))
  value <- switch(unit,
                  kg = value * 2.20462,
                  lb = value,
                  NA)
  return(value)
}

DeUnitHeight <- function(str.val){
  if(str.val %in% c("--","", NA)) {return(NA)}
  
  n.char <- nchar(str.val)
  if(substr(str.val,2,2) == "'"){
    feet <- as.numeric(substr(str.val,1,1))
    inch <- as.numeric(substr(str.val,3,n.char-1))
    return(feet*12 + inch)
  } else if(substr(str.val, n.char-1, n.char) == "cm"){
    cm <- substr(str.val, 1, n.char-3)
    return(as.numeric(cm) * 0.393701)
  } else {
    return(NA)
  }

}

MinSecToSec <- function(str.val){
  if(str.val %in% c("--","","0:00",NA)) {return(NA)}
  vec.val <- strsplit(str.val,split=":")[[1]]
  if(length(vec.val) == 2){
    return(as.numeric(vec.val[1])*60 + as.numeric(vec.val[2]))
  } else {
    return(NA)
  }
}
