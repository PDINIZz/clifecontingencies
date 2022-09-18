#' Functions to evaluate life insurance on two heads.
#'
#'@description These functions evaluates life insurances on two heads.
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




Axync <- function(actuarialtablex,actuarialtabley,x,y,x,i,m,n, status = "joint"){
  if(status =="joint") {
    if(missing(n)){

      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=w-x
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(tvbrf@lx[x+t+1]/tvbrf@lx[x+1])*(tvbrf@lx[y+t+1]/tvbrf@lx[y+1])*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+t+1]))+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/(2*tvbrf@lx[y+t+1])))}
        a=integrate(ft,min,max)
        A1xynjc=a$value
        A1xynjc
      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (w-x)
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(tvbrf@lx[x+t+1]/tvbrf@lx[x+1])*(tvbrf@lx[y+t+1]/tvbrf@lx[y+1])*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+t+1]))+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/(2*tvbrf@lx[y+t+1])))}
        a=integrate(ft,min,max)
        A1xynjc=a$value
        A1xynjc
      }


    }else {
      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=n
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(tvbrf@lx[x+t+1]/tvbrf@lx[x+1])*(tvbrf@lx[y+t+1]/tvbrf@lx[y+1])*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+t+1]))+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/(2*tvbrf@lx[y+t+1])))}
        a=integrate(ft,min,max)
        A1xynjc=a$value
        A1xynjc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (m+n)
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(tvbrf@lx[x+t+1]/tvbrf@lx[x+1])*(tvbrf@lx[y+t+1]/tvbrf@lx[y+1])*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+t+1]))+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/(2*tvbrf@lx[y+t+1])))}
        a=integrate(ft,min,max)
        A1xynjc=a$value
        A1xynjc
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
          (exp(-d*t))*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/2*tvbrf@lx[x+1])*((tvbrf@lx[y+1]-tvbrf@lx[y+t+1])/tvbrf@lx[y+1])+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/2*tvbrf@lx[y+1])*((tvbrf@lx[x+1]-tvbrf@lx[x+t+1])/tvbrf@lx[x+1]))
        }
        a=integrate(ft,min,max)
        A1xynlc=a$value
        A1xynlc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (w-x)
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/2*tvbrf@lx[x+1])*((tvbrf@lx[y+1]-tvbrf@lx[y+t+1])/tvbrf@lx[y+1])+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/2*tvbrf@lx[y+1])*((tvbrf@lx[x+1]-tvbrf@lx[x+t+1])/tvbrf@lx[x+1]))
        }
        a=integrate(ft,min,max)
        A1xynlc=a$value
        A1xynlc
      }


    }else {
      if(missing(m)){

        w=length(actuarialtable@lx)-1
        min=0
        max=n
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/2*tvbrf@lx[x+1])*((tvbrf@lx[y+1]-tvbrf@lx[y+t+1])/tvbrf@lx[y+1])+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/2*tvbrf@lx[y+1])*((tvbrf@lx[x+1]-tvbrf@lx[x+t+1])/tvbrf@lx[x+1]))
        }
        a=integrate(ft,min,max)
        A1xynlc=a$value
        A1xynlc

      }else {

        w= length(actuarialtable@lx)-1
        min=m
        max= (m+n)
        d=log(1+i)
        ft <- function(t) {
          (exp(-d*t))*(((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/2*tvbrf@lx[x+1])*((tvbrf@lx[y+1]-tvbrf@lx[y+t+1])/tvbrf@lx[y+1])+((tvbrf@lx[y+t-1+1]-tvbrf@lx[y+t+1+1])/2*tvbrf@lx[y+1])*((tvbrf@lx[x+1]-tvbrf@lx[x+t+1])/tvbrf@lx[x+1]))
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




