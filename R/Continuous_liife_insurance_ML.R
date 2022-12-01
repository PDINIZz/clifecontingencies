#' @export
cAxyzn <- function(tableslist,x,i,m,n, status ="joint"){

  #conditions

  if(missing(tableslist)){
    stop('REQUIRED TO DECLARE THE tableslist')
  }

  classlist <- sapply(tableslist, class)
  if(any(!classlist %in% c("lifetable","actuarialtable"))){
    stop("Error! A list of lifetableslist objects is required")
  }

  if(status== "j"){
    status = "joint"
  }

  if(status== "l"){
    status = "last"
  }

  if(status != "joint" && status !="last"){
    stop('Parameters other than "joint" and "last"')
  }

  #conditions for function
  b=1
  k=0
  for(ll in 1:length(x)){
    if(k<=x[ll]){
      k=x[ll]
      j=ll
    }
  }
  k1=100000
  for(ll1 in 1:length(x)){
    if(k1>=x[ll1]){
      k1=x[ll1]
      j1=ll1
    }
  }

  w=length(tableslist[[j]]@lx)

  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE "x"')
  }
  if(missing(i)){
    i=0.03
  }
  if(missing(m)){
    m=0
  }
  if(missing(n)){
    n=0
    max=w-x[j]
    max1=w1-x[j1]
  }else {
    n=n
    max= n+m
    max1=n+m

  }

  if(any(is.infinite(x), is.infinite(n), is.infinite(m))){
    stop("infinite values provided in x, n or m")
  }

  if(any(x< 0, n < 0, m < 0)){
    stop("(strictly) negative values provided in x, n or m")
  }
  if(any(n+x>w,n+m+x>w)){
    if(any(x<w,x+m<w)){
      n1=w-(x+m)
      max=n
    }
    if(any(x>=w,x+m>=w)){
      b=0}
  }
  rAxyc=0
  if(status =="joint"){

    if(n==0){
      min=m
      d=log(1+i)
      ft <- function(s) {
        s1=s[[1]]
        exp(-d*s)*pxyzt(tableslist,x=x,t=s1)
      }
      a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE)
      a=a$value
      rAxyc=1-(d*a)

    }else{
      min=m
      d=log(1+i)
      ft <- function(s) {
        s1=s[[1]]
        exp(-d*s)*pxyzt(tableslist,x=x,t=s1)*((-1/2)*(log(pxyzt(tableslist,x=x+s1-1,t=1))+log(pxyzt(tableslist,x=x+s1,t=1))))
      }
      a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE)
      a=a$value
      rAxyc=a
    }
    return(rAxyc)
  }

  if(status=="last"){
    if(n==0){
      min=m
      d=log(1+i)
      ft <- function(s) {
        s1=s[[1]]
        exp(-d*s)*(pxyzt(tableslist,x=x,t=s1,status="last"))
      }
      a=integrate(ft,min,max1,subdivisions= 10000,stop.on.error = FALSE)
      a=a$value
      rAxyc=1-(d*a)

    }else{
      min=m
      d=log(1+i)
      ft <- function(s) {
        s1=s[[1]]
        exp(-d*s)*pxyzt(tableslist,x=x,t=s1,status="last")*((-1/2)*(log(pxyzt(tableslist,x=x+s1-1,t=1,status="last"))+log(pxyzt(tableslist,x=x+s1,t=1,status="last"))))
      }
      a=integrate(ft,min,max1,subdivisions= 10000,stop.on.error = FALSE)
      a=a$value
      rAxyc=a
    }
    return(rAxyc)
  }
}






