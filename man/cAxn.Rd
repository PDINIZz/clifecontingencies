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
cAxn(tableslist, x, i, m, n)

caxn(tableslist, x, i, m, n)
}

\arguments{
  \item{tableslist}{
An actuarial table object.If the information in x is greater than that of "tableslist", the calculation base will be the first value of "tableslist". (can be a vector).
}

  \item{x}{
Age of life X.(can be a vector).
}
 \item{n}{
Fractional payments or periods where insurance is payable.(can be a vector).
}
  \item{i}{
Interest rate. Assumed to be 0.03 whether missing.(can be a vector).
}
  \item{m}{
Deferring period. Default value is zero.Assumed to be 0 whether missing.(can be a vector).
}}


\details{
Actuarial mathematics book formulas has been implemented.
}
\value{
A numeric value.(can be a vector).
}
\references{
            DICKSON, D. C.; HARDY, M. R.; WATERS, H. R. Actuarial mathematics for life
            contingent risks. 2. ed. United Kingdom: Cambridge University Press, 2016.

          FERREIRA, Paulo Pereira. Matemática atuarial: risco de pessoas. Rio de Janeiro: ENS, 2019.

          PROMISLOW, S. D. Fundamentals of actuarial mathematics. [S.l.]: John Wiley & Sons, 2014.}

\author{
        Pedro Henricky S. Diniz
}


\section{Warning }{
The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual losses arising from direct or indirect use of this software.
}


\seealso{
{\code{\link{cAxyzn}}} or {\code{\link{caxyzn}}}
}

\examples{
#assume at2000f and at2000m are examples of life table
#example for cAxn 1 by 1
          tables = at2000f
          tables1= at2000m
          #evaluate the values of continuos life insurance.
          cAxn(tableslist=tables,x=30,n=20,m=10,i=0.04)
          cAxn(tableslist=tables1,x=40,n=10,m=10,i=0.04)
          cAxn(tableslist=tables,x=20,n=30,m=10,i=0.04)
#or together
          tables= c(at2000f,at2000m)
          ages =c(30,40,20)
          ns=c(20,10,30)
          ms=c(10,10,10)
          is=c(0.04,0.04,0.04)
          #evaluate the values of continuos life insurance.
          cAxn(tableslist=tables,x=ages,n=ns,m=ms,i=is)


#example for caxn 1 by 1
          tables = at2000f
          tables1= at2000m
          #evaluate the values of continuos life insurance.
          caxn(tableslist=tables,x=30,n=20,m=10,i=0.04)
          caxn(tableslist=tables1,x=40,n=10,m=10,i=0.04)
          caxn(tableslist=tables,x=20,n=30,m=10,i=0.04)
#or together
          tables= c(at2000f,at2000m)
          ages =c(30,40,20)
          ns=c(20,10,30)
          ms=c(10,10,10)
          is=c(0.04,0.04,0.04)
          #evaluate the values of continuos life insurance.
          caxn(tableslist=tables,x=ages,n=ns,m=ms,i=is)
}
