#' @export
cAxyn <- function(tablex,tabley,x,y,i,m,n, status = "joint"){

  #conditions for tablex
  if(missing(tablex)){
    stop('REQUIRED TO DECLARE THE TABLE')
  }

  if(missing(tabley)){
    stop('REQUIRED TO DECLARE THE TABLE')
  }

  if (!(class(tablex) %in% c("lifetable","actuarialtable","mdt")))
    stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")


  if(length(tablex@x) != length(tablex@lx)) stop("length of x and lx must be equal")

  #conditions for function
  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE "x"')
  }
  if(missing(y)){
    stop('REQUIRED TO DECLARE THE AGE "y"')
  }

  if(missing(n)){
    w=length(tablex@lx)-1
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

  if(status== "j"){
    status = "joint"
  }

  if(status== "l"){
    status = "last"
  }

  #function calculation
  if(missing(tabley)){
                  if(status=="joint"){

                    min=m
                    d=log(1+i)
                    ft <- function(t) {
                      (exp(-d*t))*(tablex@lx[x+t+1]/tablex@lx[x+1])*(tablex@lx[y+t+1]/tablex@lx[y+1])*(((tablex@lx[x+t-1+1]-tablex@lx[x+t+1+1])/(2*tablex@lx[x+t+1]))+((tablex@lx[y+t-1+1]-tablex@lx[y+t+1+1])/(2*tablex@lx[y+t+1])))}
                    a=integrate(ft,min,max,subdivisions= 1000)
                    A1xynjc=a$value
                    A1xynjc
                  }else if(status=="last"){

                    min=m
                    d=log(1+i)
                    ft <- function(t) {
                      (exp(-d*t))*(((tablex@lx[x+t-1+1]-tablex@lx[x+t+1+1])/2*tablex@lx[x+1])*((tablex@lx[y+1]-tablex@lx[y+t+1])/tablex@lx[y+1])+((tablex@lx[y+t-1+1]-tablex@lx[y+t+1+1])/2*tablex@lx[y+1])*((tablex@lx[x+1]-tablex@lx[x+t+1])/tablex@lx[x+1]))
                    }
                    a=integrate(ft,min,max,subdivisions= 1000)
                    A1xynlc=a$value
                    A1xynlc
                  } else {
                    stop('Parameters other than "joint" and "last"')
                  }
  }else{


    #conditions for tabley



    if (!(class(tabley) %in% c("lifetable","actuarialtable","mdt")))
      stop("Error! Only lifetable, actuarialtable or mdt classes are accepted")


    if(length(tabley@x) != length(tabley@lx)) stop("length of x and lx must be equal")


                  if(status=="joint"){

                    min=m
                    d=log(1+i)
                    ft <- function(t) {
                      (exp(-d*t))*(tablex@lx[x+t+1]/tablex@lx[x+1])*(tabley@lx[y+t+1]/tabley@lx[y+1])*(((tablex@lx[x+t-1+1]-tablex@lx[x+t+1+1])/(2*tablex@lx[x+t+1]))+((tabley@lx[y+t-1+1]-tabley@lx[y+t+1+1])/(2*tabley@lx[y+t+1])))}
                    a=integrate(ft,min,max,subdivisions= 1000)
                    A1xynjc=a$value
                    A1xynjc
                  }else if(status=="last"){

                    min=m
                    d=log(1+i)
                    ft <- function(t) {
                      (exp(-d*t))*(((tablex@lx[x+t-1+1]-tablex@lx[x+t+1+1])/2*tablex@lx[x+1])*((tabley@lx[y+1]-tabley@lx[y+t+1])/tabley@lx[y+1])+((tabley@lx[y+t-1+1]-tabley@lx[y+t+1+1])/2*tabley@lx[y+1])*((tablex@lx[x+1]-tablex@lx[x+t+1])/tablex@lx[x+1]))
                    }
                    a=integrate(ft,min,max,subdivisions= 1000)
                    A1xynlc=a$value
                    A1xynlc
                  } else {
                    stop('Parameters other than "joint" and "last"')
                  }
                }

}



