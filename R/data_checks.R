

  check_augbinrheum_data  <- function(data,cts,bin,dichot) {
 
    # Check number of components 
    
    if (cts!=1 && cts!=2) {
      stop("Number of continuous components 'cts' must be equal to 1 or 2")
    }
    
    if (bin!=1 && bin!=0 ){
      stop("Number of binary components 'bin' must be equal to 0 or 1")
    }
    
    
    
  # Check data has correct number of columns 
    
  if (cts==1 && bin==0 && ncol(data) != 4) {
    stop("data must have 4 columns")
  }
  if (cts==2 && bin==0 && ncol(data) != 6) {
    stop("data must have 6 columns")
  }
  if (cts==1 && bin==1 && ncol(data) != 5) {
    stop("data must have 5 columns")
  }
  if (cts==2 && bin==1 && ncol(data) != 7) {
    stop("data must have 7 columns")
  }
  
    
  # Check continuous components
    
  if (cts==1 && bin==0 && !is.numeric(data[,3])) {
    stop("Column 3 of data must be numeric")
  } else {
      if (any(is.na(data[,3]), is.infinite(data[,3]))) {
        stop("Continuous component values in column 3 must be finite")
      }
  }
  if (cts==2 && bin==0 && !is.numeric(data[,3]) && !is.numeric(data[,4])) {
    stop("Columns 3 and 4 of data must be numeric")
  } else {
    if (any(is.na(data[,3]), is.infinite(data[,3])) && any(is.na(data[,4]), is.infinite(data[,4]))) {
      stop("Continuous component values in columns 3 and 4 must be finite")
    }
  }
    
    
  # Check binary component
    
    if (cts==1 && bin==1 && !is.numeric(data[,4])) {
      stop("Column 4 of data must be numeric")
    } else {
      if (cts==1 && bin==1 && any(!(data[,4] %in% 0:1))) {
        stop("Elements of binary component must be equal to 0 or 1")
      }
    }
    
    
  
  # Check treatment
    
    if (!is.numeric(data[,2])) {
      stop("treatment arm must be numeric")
    } 
    if (any(!(data[,2] %in% 0:1))) {
      stop("Elements of treatment must be equal to 0 or 1")
    }
    
  # Check dichotomisation threshold(s) 
    
  if (is.null(dichot)) {
    stop("continuous components must have dichotomisation thresholds")
  } else {
    if (!is.numeric(dichot)) {
      stop("Dichotomisation threshold for continuous components must be numeric")
    } else if (cts==1 && length(dichot)!=1) {
      stop("dichot must have length 1")
    } else if (cts==2 && length(dichot)!=2){
      stop("dichot must have length 2")
    }
  }
    
}

  

check_logical                <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_real                   <- function(value, name) {
  if (any(length(value) != 1, !is.numeric(value), is.infinite(value))) {
    stop(name, " must be a single finite numeric")
  }
}
