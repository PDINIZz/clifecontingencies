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

  #definition of transition variables
  vars=1
  vmax=0
  raxyzc=0

  #conditions max min
  for(nmax in 1:length(x)){
    if(vmax<=x[nmax]){
      vmax=x[nmax]
      j=nmax
    }
  }
  vmin=100000
  for(nmin in 1:length(x)){
    if(vmin>=x[nmin]){
      vmin=x[nmin]
      j1=nmin
    }
  }

  #Conditions for function

  w=length(tableslist[[j]]@lx)
  w1=length(tableslist[[j1]]@lx)
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
      n=w-(x+m)
      max=n
    }
    if(any(x>=w,x+m>=w)){
      vars=0
      }
  }

  #function
      min=m
      d=log(1+i)


      if(status =="joint"){



        ft <- function(s) {
          s1=s
          allpxt1 <-prod(sapply(1:length(tableslist), function(h){pxt(tableslist[[h]], x[h], t=s1[[1]])}))
          allpxt2 <-prod(sapply(1:length(tableslist), function(h){pxt(tableslist[[h]], x[h], t=s1[[2]])}))
          out=c(allpxt1, allpxt2)

          exp(-d*s)*out
        }

        a=suppressWarnings(integrate(ft,min,max,subdivisions= 10000,stop.on.error = FALSE))

        raxyzc=a$value*vars
      }

      if(status=="last"){
        ft <- function(s) {
          s1=s
          allpxt1 <-prod(sapply(1:length(tableslist), function(h){qxt(tableslist[[h]], x[h], t=s1[[1]])}))
          allpxt2 <-prod(sapply(1:length(tableslist), function(h){qxt(tableslist[[h]], x[h], t=s1[[2]])}))

          allpxt11=1-allpxt1
          allpxt21=1-allpxt2
          out=c(allpxt11, allpxt21)

          exp(-d*s)*out
        }

        a=suppressWarnings(integrate(ft,min,max1,subdivisions= 10000,stop.on.error = FALSE))

        raxyzc=a$value*vars
      }


      raxyzc

    }
