\name{at2000f and at2000m}
\alias{at2000f}
\alias{at2000m}
\docType{data}
\title{
AT 2000 female and male life table object.
}
\description{
They are actuarial tables, calculated by "life contingencies" based on the AT2000 probabilities of death. They are just tables for calculation reference in the package.
}
\usage{
data(at2000f)

data(at2000m)
}
\format{
  Formal class 'lifetable' [package "lifecontingencies"] with 3 slots
  ..@ x   : int [1:116] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ lx  : num [1:116] 100000 99821 99745 99706 99677 ...
  ..@ name: chr "at2000f"


  Formal class 'lifetable' [package "lifecontingencies"] with 3 slots
  ..@ x   : int [1:116] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ lx  : num [1:116] 10000 9982 9975 9971 9968 ...
  ..@ name: chr "at2000m""
}


\note{
These tables are calculated by probs2lifetable lifecontingencies.
}

\examples{
data(at2000f)
data(at2000m)
## maybe str(at2000f) ; plot(at2000f) ...
## maybe str(at2000m) ; plot(at2000m) ...
}
