#' Functions to evaluate continuous annuities on two heads
#'
#'@description These functions evaluates continuous life annuities on two heads.
#'
#'@param actuarialtablex	An actuarial table object.
#'@param actuarialtabley	An actuarial table object.
#'@param x Age of the annuitant.
#'@param y Age of the annuitant.
#'@param n Number of terms of the annuity, if missing annuity is intended to be paid until death.
#'@param i Interest rate (default value the interest of the life table). (should be a scalar).
#'@param m 	Deferring period. Assumed to be 1 whether missing.
#'
#'
#'
#'


#' @export




caxync <- function(actuarialtablex,actuarialtabley,x,y,i,m,n, status = "joint"){
  if(status =="joint") {
    if(missing(n)){

      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=w-x
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+t+1]/actuarialtable@lx[y+1]))
        }
        a=integrate(ft,min,max,subdivisions= 1000)$value
        maxjc=a$value
      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (w-x)
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+t+1]/actuarialtable@lx[y+1]))
        }
        a=integrate(ft,min,max,subdivisions= 1000)$value
        maxjc=a$value
      }


    }else {
      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=n
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+t+1]/actuarialtable@lx[y+1]))
        }
        a=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=a$value
        rAxc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (m+n)
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+t+1]/actuarialtable@lx[y+1]))
        }
        a=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=a$value
        rAxc
      }
    }




  }else if(status =="last"){
    if(missing(n)){

      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=w-x
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])+(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])-((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])))
        }
        A=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=A$value
        rAxc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (w-x)
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])+(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])-((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])))
        }
        A=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=A$value
        rAxc
      }


    }else {
      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=n
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])+(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])-((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])))
        }
        A=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=A$value
        rAxc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (m+n)
        d=log(1+i)
        ft <- function(t) {
          exp(-d*t)*((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])+(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])-((actuarialtable@lx[x+1+t]/actuarialtable@lx[x+1])*(actuarialtable@lx[y+1+t]/actuarialtable@lx[y+1])))
        }

        A=integrate(ft,min,max,subdivisions= 1000)$value
        rAxc=A$value
        rAxc
      }
    }


  }else {
    stop('Parameters other than "joint" and "last"')
  }


}




