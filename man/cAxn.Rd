\name{cAxn & caxn}
\alias{cAxn}
\alias{caxn}

\title{
Functions to evaluate continuos life insurance and continuos annuities.
}
\description{
These functions evaluates continuos life insurance and continuos annuities.
}
\usage{
cAxn(table, x, i, m, n)

caxyn(table, x i, m, n)
}

\arguments{
  \item{tablex}{
An actuarial table object}

  \item{x}{
Age of life X.
}

  \item{i}{
Interest rate. Assumed to be 0.03 whether missing..
}
  \item{m}{
Deferring period. Default value is zero.
}
  \item{n}{
Fractional payments or periods where insurance is payable.
}}

\details{
Actuarial mathematics book formulas has been implemented.
}
\value{
A numeric value.
}
\references{
            DICKSON, D. C.; HARDY, M. R.; WATERS, H. R. Actuarial mathematics for life
            contingent risks. 2. ed. United Kingdom: Cambridge University Press, 2016.
                      }

\author{
        Pedro Henricky S. Diniz
}


\section{Warning }{
The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual losses arising from direct or indirect use of this software.
}


\seealso{
{\code{\link{cAxyn}}} or {\code{\link{caxyn}}}
}

\examples{
#example for cAxn
          #create acturial table using the "lifecontingencies" pack
          at2000m= read.table("at2000m.txt",h = T)
          actuarialtable = probs2lifetable(at2000m$qx,radix = 1000,type = "qx")
          #evaluate the value of continuos life insurance.
          cAxn(actuarialtable,x=40,y=10, n=20,m=10, i=0.04)}
#example for caxn

          #evaluate the value of continuous annuities.
          caxn(actuarialtable,x=40, n=20,m=10, i=0.04)}

}
