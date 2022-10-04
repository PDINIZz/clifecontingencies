\name{cAxyn & caxyn}
\alias{cAxyn}
\alias{caxyn}

\title{
Functions to evaluate continuos life insurance and continuos annuities on two heads.
}
\description{
These functions evaluates life insurances on two heads.
}
\usage{
cAxyn(tablex, tabley, x, y, i, m, n, status = "joint")

caxyn(tablex, tabley, x, y, i, m, n, status = "joint")
}

\arguments{
  \item{tablex}{
An actuarial table object or a table class as "data.frame" with an lx column.
}
  \item{tabley}{
An actuarial table object or a table class as "data.frame" with an lx column.Tablex is assumed to be used if tabley is absent.
}
  \item{x}{
Age of life X.
}
  \item{y}{
Age of life Y.
}
  \item{i}{
Interest rate. Assumed to be 0.03 whether missing..
}
  \item{m}{
Deferring period. Default value is zero.
}
  \item{n}{
Fractional payments or periods where insurance is payable.
}
  \item{status}{

Either "joint" for the joint-life status model or "last" for the last-survivor status model (can be abbreviated).
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

\note {

}
\section{Warning }{
The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual losses arising from direct or indirect use of this software.
}



\seealso{
{\code{\link{cAxn}}} or {\code{\link{caxn}}}
}

\examples{
#example for cAxyn
          #create acturial table using the "lifecontingencies" pack
          at2000m= read.table("at2000m.txt",h = T)
          actuarialtable = probs2lifetable(at2000m$qx,radix = 1000,type = "qx")
          #or a table class as "data.frame" with an lx column
          #evaluate the value of continuos life insurance on two heads.
          cAxyn(actuarialtable,x=40,y=10, n=20,m=10, i=0.03, status = "last")}
#example for caxyn

          #evaluate the value of continuous annuities on two heads.
          caxyn(actuarialtable,x=40,y=10, n=20,m=10, i=0.03, status = "joint")}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
