library(clifecontingencies)



#(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", power =1, payment = "advance", ...)


data(at2000f)
data(at2000m)
table=c(at2000f)

test_that("annuities_v_at2000f", {
  for(j in 1:115){

  daxn=axn(at2000f,x=j,i=0.05,k=4,payment = 'arrears' )
  uaxn=axn(at2000f,x=j,i=0.05,k=4,payment = "advance")

  limit<-function(x){
    if(any(daxn<x,x<uaxn)){
      x=1
      }
  }

  expect_equal(limit(caxn(at2000f,x=j,i=0.05)),  1) #BOWERS P 111
}
})

test_that("annuities_v_at2000m", {
  for(j in 1:115){

    daxn=axn(at2000m,x=j,i=0.05,k=4,payment = 'arrears' )
    uaxn=axn(at2000m,x=j,i=0.05,k=4,payment = "advance")

    limit<-function(x){
      if(any(daxn<x,x<uaxn)){
        x=1
      }
    }

    expect_equal(limit(caxn(at2000m,x=j,i=0.05)),  1) #BOWERS P 111
  }
})

test_that("annuities_t_at2000f", {
  for(j in 1:115){
    p=116-j
    daxn=axn(at2000f,x=j,i=0.05,n=p,k=4,payment = 'arrears' )
    uaxn=axn(at2000f,x=j,i=0.05,n=p,k=4,payment = "advance")

    limit<-function(x){
      if(any(daxn<x,x<uaxn)){
        x=1
      }
    }

    expect_equal(limit(caxn(at2000f,x=j,n=p,i=0.05)),  1) #BOWERS P 111
  }
})

test_that("annuities_t_at2000m", {
  for(j in 1:115){
    p=116-j
    daxn=axn(at2000m,x=j,i=0.05,n=p,k=4,payment = 'arrears' )
    uaxn=axn(at2000m,x=j,i=0.05,n=p,k=4,payment = "advance")

    limit<-function(x){
      if(any(daxn<x,x<uaxn)){
        x=1
      }
    }

    expect_equal(limit(caxn(at2000m,x=j,n=p,i=0.05)),  1) #BOWERS P 111
  }
})

