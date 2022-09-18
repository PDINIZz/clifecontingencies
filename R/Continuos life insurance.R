#' Function to evaluate continuos life insurance.
#'
#'@description This function calculates the actuarial value of continuous annuities, given an actuarial table. Temporary and deferred annuities can be evaluated.
#'
#'
#'@param actuarialtable	An actuarial table object.
#'@param x Age of the annuitant.
#'@param n Number of terms of the annuity, if missing annuity is intended to be paid until death.
#'@param i Interest rate (default value the interest of the life table). (should be a scalar).
#'@param m 	Deferring period. Assumed to be 1 whether missing.
#'
#'
#'
#'



#' @export
cAxn <- function(actuarialtable,x,n,i,m){

  if(missing(table)){
    stop('REQUIRED TO DECLARE THE TABLE')

  }

  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE')

  }
  .


  if(missing(i)){
    stop('REQUIRED TO DECLARE THE AGE')

  }

  if(missing(n)){

    if(missing(m)){

      w=length(tvbrf@lx)-1
      min=0
      max=w-x
      d=log(1+i)
      ft <- function(t) {

        (exp(-d*t))*((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+1]))
      }
      A=integrate(ft,min,max,subdivisions= 1000)$value
      rAxc=A$value
      rAxc

    }else {

      w= length(tvbrf@lx)-1
      min=m
      max= (w-x)
      d=log(1+i)
      ft <- function(t) {

        (exp(-d*t))*((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+1]))
      }
      A=integrate(ft,min,max,subdivisions= 1000)$value
      rAxc=A$value
      rAxc
    }


  }else {
    if(missing(m)){

      w=length(tvbrf@lx)-1
      min=0
      max=n
      d=log(1+i)
      ft <- function(t) {

        (exp(-d*t))*((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+1]))
      }
      A=integrate(ft,min,max,subdivisions= 1000)$value
      rAxc=A$value
      rAxc

    }else {

      w= length(tvbrf@lx)-1
      min=m
      max= (m+n)
      d=log(1+i)
      ft <- function(t) {

        (exp(-d*t))*((tvbrf@lx[x+t-1+1]-tvbrf@lx[x+t+1+1])/(2*tvbrf@lx[x+1]))
      }
      A=integrate(ft,min,max,subdivisions= 1000)$value
      rAxc=A$value
      rAxc
    }
  }
}





