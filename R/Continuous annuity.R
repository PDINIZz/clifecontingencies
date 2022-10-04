#'@import lifecontingencies
#' @export
caxn <- function(table,x,n,i,m){

  #conditions

  if(missing(table)){
    stop('REQUIRED TO DECLARE THE TABLE')
  }

  if (!(class(table) %in% c("lifetable","actuarialtable","mdt")))
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")


  if(length(table@x) != length(table@lx)) stop("length of x and lx must be equal")


  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE "x"')
  }

  if(missing(n)){
    w=length(table@lx)-1
    max=w-x
  }else {
    max= n
  }

  if(missing(m)){
    m=0
  }

  if(missing(i)){
    i=0.03
  }

  #function calculation

  min=m
  d=log(1+i)
  ft <- function(t) {
    exp(-d*t)*(table@lx[x+t+1]/table@lx[x+1])
  }
  a=integrate(ft,min,max,subdivisions= 1000)
  raxc=a
  raxc$value

}





