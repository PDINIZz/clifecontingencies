#' @export
cAxyn <- function(tableslist,x,n,i,m, status ="joint"){

  #conditions

  if(missing(tableslist)){
    stop('REQUIRED TO DECLARE THE tableslist')
  }

  classlist <- sapply(tableslist, class)
  if(any(!classlist %in% c("lifetable","actuarialtable"))){
    stop("Error! A list of lifetableslist objects is required")
  }
  for(j in 1:2) {
    if(length(tableslist[[j]]@x) != length(tableslist[[j]]@lx)) {
      stop("length of x and lx must be equal")
    }
  }


  tablex=tableslist[1]
  tabley=tableslist[2]

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

    w=length(tablex[[1]]@lx)-1

    if(missing(x)){
      stop('REQUIRED TO DECLARE THE AGE "x"')
    }else{
      x1=max(x[1],x[2])
      y=min(x[1],x[2])
    }
    if(missing(i)){
      i=0.03
    }

    if(missing(m)){
      m=0
    }

    if(missing(n)){
      n1=w-x1
      n=0
      max=w-x1
    }else {
      n1=n
      max= n+m

    }

    if(any(is.infinite(x1),is.infinite(y), is.infinite(n), is.infinite(m))){
      stop("infinite values provided in x, n or m")
    }

    if(any(x1 < 0,y < 0, n < 0, m < 0)){
      stop("(strictly) negative values provided in x, n or m")
    }
    if(any(x>w,y>w,n+x>w,n+y>w,n+m+x>w,n+m+y>w)){
      raxyc=0
      return(raxc)
    }

    if(status =="joint"){

      min=m
      d=log(1+i)

      ft <- function(t) {
        (exp(-d*t))*(tablex[[1]]@lx[x1+t+1]/tablex[[1]]@lx[x1+1])*(tabley[[1]]@lx[y+t+1]/tabley[[1]]@lx[y+1])*(((tablex[[1]]@lx[x1+t]-tablex[[1]]@lx[x1+t+1+1])/(2*tablex[[1]]@lx[x1+t+1]))+((tabley[[1]]@lx[y+t]-tabley[[1]]@lx[y+t+1+1])/(2*tabley[[1]]@lx[y+t+1])))
       }
      a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE,abs.tol = TRUE)
      cAxyc=a$value
      return(cAxyc)
    }

    if(status=="last"){

      Ax= cAxn(tablex,x=x1,m=m,n=n1,i=i)
      Ay= cAxn(tabley,x=y,m=m,n=n1,i=i)
      Axy=cAxyn(tableslist = tableslist,x=x,i=i,m=m,n=n1)
      cAxyc=Ax+Ay-Axy

      return(cAxyc)

    }
}








