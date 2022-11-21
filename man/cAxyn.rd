\name{cAxyn & caxyn}
\alias{cAxyn}
\alias{caxyn}

\title{
On two heads continuos life insurance and continuos annuities .
}
\description{
These functions evaluates life insurances on two heads.
}
\usage{
cAxyn(tableslist, x, n, i, m, status = "joint")

caxyn(tableslist, x, n, i, m, status = "joint")
}

\arguments{
  \item{tableslist}{
A list whose elements are either lifetable or actuarialtable class objects.
}

  \item{x}{
A vector of the same size of tableList that contains the initial ages.
}

  \item{i}{
Interest rate. Assumed to be 0.03 whether missing..
}
  \item{m}{
Deferring period. Default value is zero.
}
  \item{n}{
Lenght of the insurance.
}
  \item{status}{

Either "joint" for the joint-life status model or "last" for the last-survivor status model (can be abbreviated).
}}



\details{
In theory, these functions apply the same concept of life insurances on two head.
}
\value{
A numeric value.
}
\references{
            DICKSON, D. C.; HARDY, M. R.; WATERS, H. R. Actuarial mathematics for life
            contingent risks. 2. ed. United Kingdom: Cambridge University Press, 2016.


          FERREIRA, Paulo Pereira. Matemática atuarial: risco de pessoas. Rio de Janeiro: ENS, 2019.

                     PROMISLOW, S. D. Fundamentals of actuarial mathematics. [S.l.]: John Wiley & Sons, 2014. }

\author{
        Pedro Henricky S. Diniz
}

\section{Warning }{
The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual losses arising from direct or indirect use of this software.
}


\seealso{
{\code{\link{cAxn}}} or {\code{\link{caxn}}}
}

\examples{
#example for cAxyn
assume at2000f and at2000m are examples of life table
          tables = c(at2000f,at2000m)
          ages=c(20,30)
          #evaluate the value of continuos life insurance on two heads.
          cAxyn(tables,x=ages, n=20,m=10, i=0.03, status = "last")
#example for caxyn

          #evaluate the value of continuous annuities on two heads.
          caxyn(tables,x=ages, n=20,m=10, i=0.03, status = "joint")

}
