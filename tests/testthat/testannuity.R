library(clifecontingencies)

data(at2000f)
data(at2000m)
table=c(at2000f)



test_that("annuities_v_at2000f", {
  for(j in 1:115){

    daxn=axn(at2000f,x=j,i=0.05,k=12,payment = 'arrears' )
    uaxn=axn(at2000f,x=j,i=0.05,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000f,x=j,i=0.05)),  1)
  }
})

test_that("annuities_v_at2000m", {
  for(j in 1:115){

    daxn=axn(at2000m,x=j,i=0.05,k=12,payment = 'arrears' )
    uaxn=axn(at2000m,x=j,i=0.05,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000m,x=j,i=0.05)),  1)
  }
})

test_that("annuities_t_at2000f", {
  for(j in 1:115){
    p=116-j
    daxn=axn(at2000f,x=j,i=0.05,n=p,k=12,payment = 'arrears' )
    uaxn=axn(at2000f,x=j,i=0.05,n=p,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000f,x=j,n=p,i=0.05)),  1)
  }
})

test_that("annuities_t_at2000m", {
  for(j in 1:115){
    p=116-j
    daxn=axn(at2000m,x=j,i=0.05,n=p,k=12,payment = 'arrears' )
    uaxn=axn(at2000m,x=j,i=0.05,n=p,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000m,x=j,n=p,i=0.05)),  1)
  }
})

test_that("annuities_vd_at2000m", {
  for(j in 1:115){
    p=116-j

    daxn=axn(at2000m,x=0,i=0.05,m=j,k=12,payment = 'arrears' )
    uaxn=axn(at2000m,x=0,i=0.05,m=j,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000m,x=0,i=0.05,m=j)),  1)
  }
})

test_that("annuities_td_at2000m", {
  for(j in 1:115){
    p=116-j

    daxn=axn(at2000m,x=0,i=0.05,n=p,m=j,k=12,payment = 'arrears' )
    uaxn=axn(at2000m,x=0,i=0.05,n=p,m=j,k=12,payment = "advance")

    limit<-function(x){
      if(daxn<x&&x<uaxn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxn(at2000m,x=0,i=0.05,n=p,m=j)),  1) #BOWERS P 111
  }
})

