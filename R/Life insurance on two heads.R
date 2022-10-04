#' @export


Axync <- function(actuarialtablex,actuarialtabley,x,y,x,i,m,n, status = "joint"){

  if(missing(actuarialtabley)){



  }else {
                            if(status =="joint") {
                              if(missing(n)){

                                if(missing(m)){

                                  w=length(actuarialtablex@lx)-1
                                  min=0
                                  max=w-x
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(actuarialtablex@lx[x+t+1]/actuarialtablex@lx[x+1])*(actuarialtabley@ly[y+t+1]/actuarialtabley@ly[y+1])*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/(2*actuarialtablex@lx[x+t+1]))+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/(2*actuarialtabley@ly[y+t+1])))}
                                  a=integrate(ft,min,max)
                                  A1xynjc=a$value
                                  A1xynjc
                                }else {

                                  w= length(actuarialtablex@lx)-1
                                  min=m
                                  max= (w-x)
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(actuarialtablex@lx[x+t+1]/actuarialtablex@lx[x+1])*(actuarialtabley@ly[y+t+1]/actuarialtabley@ly[y+1])*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/(2*actuarialtablex@lx[x+t+1]))+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/(2*actuarialtabley@ly[y+t+1])))}
                                  a=integrate(ft,min,max)
                                  A1xynjc=a$value
                                  A1xynjc
                                }


                              }else {
                                if(missing(m)){

                                  w=length(actuarialtablex@lx)-1
                                  min=0
                                  max=n
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(actuarialtablex@lx[x+t+1]/actuarialtablex@lx[x+1])*(actuarialtabley@ly[y+t+1]/actuarialtabley@ly[y+1])*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/(2*actuarialtablex@lx[x+t+1]))+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/(2*actuarialtabley@ly[y+t+1])))}
                                  a=integrate(ft,min,max)
                                  A1xynjc=a$value
                                  A1xynjc

                                }else {

                                  w= length(actuarialtablex@lx)-1
                                  min=m
                                  max= (m+n)
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(actuarialtablex@lx[x+t+1]/actuarialtablex@lx[x+1])*(actuarialtabley@ly[y+t+1]/actuarialtabley@ly[y+1])*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/(2*actuarialtablex@lx[x+t+1]))+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/(2*actuarialtabley@ly[y+t+1])))}
                                  a=integrate(ft,min,max)
                                  A1xynjc=a$value
                                  A1xynjc
                                }
                              }




                            }else if(status =="last"){
                              if(missing(n)){

                                if(missing(m)){

                                  w=length(actuarialtablex@lx)-1
                                  min=0
                                  max=w-x
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/2*actuarialtablex@lx[x+1])*((actuarialtabley@ly[y+1]-actuarialtabley@ly[y+t+1])/actuarialtabley@ly[y+1])+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/2*actuarialtabley@ly[y+1])*((actuarialtablex@lx[x+1]-actuarialtablex@lx[x+t+1])/actuarialtablex@lx[x+1]))
                                  }
                                  a=integrate(ft,min,max)
                                  A1xynlc=a$value
                                  A1xynlc

                                }else {

                                  w= length(actuarialtablex@lx)-1
                                  min=m
                                  max= (w-x)
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/2*actuarialtablex@lx[x+1])*((actuarialtabley@ly[y+1]-actuarialtabley@ly[y+t+1])/actuarialtabley@ly[y+1])+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/2*actuarialtabley@ly[y+1])*((actuarialtablex@lx[x+1]-actuarialtablex@lx[x+t+1])/actuarialtablex@lx[x+1]))
                                  }
                                  a=integrate(ft,min,max)
                                  A1xynlc=a$value
                                  A1xynlc
                                }


                              }else {
                                if(missing(m)){

                                  w=length(actuarialtablex@lx)-1
                                  min=0
                                  max=n
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/2*actuarialtablex@lx[x+1])*((actuarialtabley@ly[y+1]-actuarialtabley@ly[y+t+1])/actuarialtabley@ly[y+1])+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/2*actuarialtabley@ly[y+1])*((actuarialtablex@lx[x+1]-actuarialtablex@lx[x+t+1])/actuarialtablex@lx[x+1]))
                                  }
                                  a=integrate(ft,min,max)
                                  A1xynlc=a$value
                                  A1xynlc

                                }else {

                                  w= length(actuarialtablex@lx)-1
                                  min=m
                                  max= (m+n)
                                  d=log(1+i)
                                  ft <- function(t) {
                                    (exp(-d*t))*(((actuarialtablex@lx[x+t-1+1]-actuarialtablex@lx[x+t+1+1])/2*actuarialtablex@lx[x+1])*((actuarialtabley@ly[y+1]-actuarialtabley@ly[y+t+1])/actuarialtabley@ly[y+1])+((actuarialtabley@ly[y+t-1+1]-actuarialtabley@ly[y+t+1+1])/2*actuarialtabley@ly[y+1])*((actuarialtablex@lx[x+1]-actuarialtablex@lx[x+t+1])/actuarialtablex@lx[x+1]))
                                  }
                                  a=integrate(ft,min,max)
                                  A1xynlc=a$value
                                  A1xynlc
                                }
                              }


                            }else {
                              stop('Parameters other than "joint" and "last"')
                            }
                        }
}



