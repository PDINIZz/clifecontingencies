library(clifecontingencies)

data(at2000f)
data(at2000m)
table=c(at2000f)



test_that("LI_v_at2000f", {
  for(j in 1:115){

    dAxn=Axn(at2000f,x=j,i=0.05)
    uAxn=Axn(at2000f,x=j,i=0.05,k=12)

    limit<-function(x){
      if(dAxn<x){
        x=1
      }else{x=0}
    }

    expect_equal(limit(cAxn(at2000f,x=j,i=0.05)),  1)
  }
})

test_that("LI_v_at2000m", {
  for(j in 1:115){

    dAxn=Axn(at2000m,x=j,i=0.05 )
    uAxn=Axn(at2000m,x=j,i=0.05,k=12)

    limit<-function(x){
      if(dAxn<x){
        x=1
      }else{x=0}
    }

    expect_equal(limit(cAxn(at2000m,x=j,i=0.05)),  1)
  }
})

test_that("LI_v_at2000f", {   #DICKSON PAG. 131
  for(j in 1:115){

    daxn=caxn(at2000f,x=j,i=0.05 )

    d=log(1.05)
    limit<-function(x){
      z=d*daxn+x
      z
    }

    expect_equal(round(limit(cAxn(at2000f,x=j,i=0.05)),  0),1)
  }
})

test_that("LI_v_at2000m", {#DICKSON PAG. 131
  for(j in 1:115){

    daxn=caxn(at2000m,x=j,i=0.05 )

    d=log(1.05)
    limit<-function(x){
      z=d*daxn+x
      z
    }

    expect_equal(round(limit(cAxn(at2000m,x=j,i=0.05)),  0),1)
  }
})
test_that("LI_t_at2000f", {    #DICKSON PAG. 117
  for(j in 1:115){
    p=115-j
    daxn=caxn(at2000f,x=j,n=p,i=0.05 )

    d=log(1.05)
    limit<-function(x){
      z=d*daxn+x
      z
    }

    expect_equal(round(limit(cAxn(at2000f,x=j,n=p,i=0.05)),  0),1)
  }
})

test_that("LI_t_at2000m", { #DICKSON PAG. 117
  for(j in 1:115){
    p=115-j
    daxn=caxn(at2000f,x=j,n=p,i=0.05 )

    d=log(1.05)
    limit<-function(x){
      z=d*daxn+x
      z
    }

    expect_equal(round(limit(cAxn(at2000m,x=j,n=p,i=0.05)),  0),1)
  }
})
