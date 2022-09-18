#' Function to evaluate Continuous annuities.
#'
#'@description This function calculates the actuarial value of continuous life insurance, given an actuarial table.
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
#'


#' @export
caxnc <- function(actuarialtable,x,n,i,m){


  if(missing(actuarialtable)){
    stop('REQUIRED TO DECLARE THE TABLE')
  }

  if(missing(x)){
    stop('REQUIRED TO DECLARE THE AGE')
  }

  if(missing(i)){
    stop('REQUIRED TO DECLARE THE INTEREST AGE')

  }
  if(missing(n)){

    if(missing(m)){

      w=length(actuarialtable@lx)-1
      min=0
      max=w-x
      d=log(1+i)
      ft <- function(t) {
        exp(-d*t)*(actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])
      }
      a=integrate(ft,min,max,subdivisions= 1000)$value
      raxc=a$value
      raxc

    }else {

      w= length(actuarialtable@lx)-1
      min=m
      max= (w-x)
      d=log(1+i)
      ft <- function(t) {exp(-d*t)*(actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])}
      a=integrate(ft,min,max,subdivisions= 1000)$value
      raxc=a$value
      raxc
    }


  }else {
    if(missing(m)){

      w=length(actuarialtable@lx)-1
      min=0
      max=n
      d=log(1+i)
      ft <- function(t) {exp(-d*t)*(actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])}
      a=integrate(ft,min,max,subdivisions= 1000)$value
      raxc=a$value
      raxc
    }else {

      w= length(actuarialtable@lx)-1
      min=m
      max= (m+n)
      d=log(1+i)
      ft <- function(t) {exp(-d*t)*(actuarialtable@lx[x+t+1]/actuarialtable@lx[x+1])}
      a=integrate(ft,min,max,subdivisions= 1000)$value
      raxc=a$value
      raxc
    }
  }

}





