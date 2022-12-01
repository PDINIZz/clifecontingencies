#' @export
cAxn <- function(tableslist,x,i,m,n){


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
    i=0.03
  }

  if(missing(m)){
    m=0
  }
  if(missing(n)){
    n=NULL
  }


  raxc=0
  b=1

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
      m1=m[1]
    }

    if(is.null(n1)){
      n1=0
      max= w-x1
    }else{
      if(is.na(n1)){
        n1=n[1]
      }
      max= n1+m1
    }


    if(is.na(i1)){
      i1=i[1]
    }

    if(any(x1 < 0, n1 < 0, m1 < 0)){
      stop("( negative values provided in x, n or m")
    }
    if(any(n1+x1>w,n1+m1+x1>w)){
      if(any(x1<w,x1+m1<w)){
        n1=w-(x1+m1)
        max=n1
      }
      if(any(x1>=w,x1+m1>=w)){
        b=0
        }
    }


    if(any(is.infinite(x1), is.infinite(n1), is.infinite(m1))){
      stop("infinite values provided in x, n or m")
    }


        min=m1
        d=log(1+i1)
    #function calculation
      if(n1==0){
        if(b==0){raxc[j]=0}else{
        ft <- function(s) {
          s1=s
          exp(-d*s)*pxt(tableslist1,x=x1,t=s1)
        }
        a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE)

        raxc[j]=(1-(d*a$value))*b
        }}else{

        if(b==0){raxc[j]=0}else{
          ft <- function(s) {
            s1=s
            exp(-d*s)*pxt(tableslist1,x=x1,t=s1)*((-1/2)*(log(pxt(tableslist1,x=x1+s1-1,t=1))+log(pxt(tableslist1,x=x1+s1,t=1))))
          }
          a=integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE)
          raxc[j]=a$value
        }

      }
    b=1
    }
    raxc


}

