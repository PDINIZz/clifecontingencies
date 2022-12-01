#' @export
caxyzn <- function(tableslist,x,i,m,n, status ="joint"){

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
  k=0
  for(ll in 1:length(x)){
    if(k<=x[ll]){
      k=x[ll]
      j=ll
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
  }else {
    n=n
    max= n+m

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
      raxc=0
      return(raxc)}
  }
    if(n==0){
      if(status =="joint"){
        d=log(1+i)

        caxyc=(axyzn(tableslist,x=x,m=m,i=i,payment = "arrears"))+0.5+((1/12)*(d*(log(pxyzt(tableslist,x=x+m-1,t=1))+log(pxyzt(tableslist,x=x+m,t=1)))))
        return(caxyc)
      }

      if(status=="last"){
        d=log(1+i)
        caxyc=(axyzn(tableslist,x=x,i=i,m=m, status="last",payment = "arrears"))+0.5+((1/12)*(d*(log(pxyzt(tableslist,x=x+m-1,t=1,status = "last"))+log(pxyzt(tableslist,x=x+m,t=1,status = "last")))))
        return(caxyc)
      }

    }else{

      if(status =="joint"){
        d=log(1+i)

        caxyc=(axyzn(tableslist,x=x,m=m,n=n,i=i,payment = "arrears"))+0.5+((1/12)*(d*(log(pxyzt(tableslist,x=x+m-1,t=1))+log(pxyzt(tableslist,x=x+m,t=1)))))
        return(caxyc)
      }

      if(status=="last"){
        d=log(1+i)
        caxyc=axyzn(tableslist,x=x,i=i,n=n,m=m,status="last",payment = "arrears")+0.5+((1/12)*(d*(log(pxyzt(tableslist,x=x+m-1,t=1,status = "last"))+log(pxyzt(tableslist,x=x+m,t=1,status = "last")))))
        return(caxyc)

      }
    }}
