#'@import lifecontingencies
#' @export
caxn <- function(tableslist,x,n,i,m){

  #conditions

  if(missing(tableslist)){
    stop('REQUIRED TO DECLARE THE tableslist')
  }

  if(length(tableslist)==1){
    tableslist=c(tableslist)
  }

  classlist <- sapply(tableslist, class)
   if(any(!classlist %in% c("lifetable","actuarialtable"))){
    stop("Error! A list of lifetableslist objects is required")
    }




  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE "x"')
  }


  if(missing(i)){
    i=NaN
  }

  if(missing(m)){
    m=NaN
  }

  if(missing(n)){
    n=NaN
  }

  raxc=0

  #conditions for function
  for (j in 1:length(x)) {
    tableslist0=tableslist[j]
    if(is.null(tableslist0[[1]])){
      tableslist1=tableslist[[1]]
    }else{tableslist1=tableslist[[j]]}

    x1=x[j]
    m1=m[j]
    n1=n[j]
    i1=i[j]
    w=length(tableslist1@lx)




    if(length(tableslist1@x) != length(tableslist1@lx)) {
      stop("length of x and lx must be equal")
    }

    if(is.na(x1)){stop("check x has NaN")}

    if(is.na(m1)){
      m1=0
    }


    if(is.na(n1)){
      n1=0
      max=w-x1

    }else {
      max= n1+m1

    }

    if(is.na(i1)){
      i1=0.03
    }

    if(any(x1 < 0, n1 < 0, m1 < 0)){
      stop("( negative values provided in x, n or m")
    }
    if(any(x1>w,n1+x1>w,n1+m1+x1>w)){
      raxc=0
      return(raxc)
    }


    if(any(is.infinite(x1), is.infinite(n1), is.infinite(m1))){
      stop("infinite values provided in x, n or m")
    }



    #function calculation

    min=m1
    d=log(1+i1)
    ft <- function(t) {
      exp(-d*t)*(tableslist1@lx[x1+t+1]/tableslist1@lx[x1+1])
    }
    a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE,abs.tol = 0.0000000001)
    a=a$value
    raxc[j]=a
  }
  raxc
}


