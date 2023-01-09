library(clifecontingencies)

data(at2000f)
data(at2000m)
table=c(at2000f,at2000m)



test_that("annuities_v_table", {
  for(j in 0:115){
    l=115-j
    p=c(j,l)

    daxyzn=axyzn(table,x=p,i=0.05,payment = "arrears",status="joint")
    uaxyzn=axyzn(table,x=p,i=0.05,payment = "advance",status="joint")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=p,i=0.05,status="joint")),  1)
  }
})

test_that("annuities_v_table", {
  for(j in 0:115){
    l=115-j
    p=c(j,l)

    daxyzn=axyzn(table,x=p,i=0.05,payment = "arrears",status="last")
    uaxyzn=axyzn(table,x=p,i=0.05,payment = "advance",status="last")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=p,i=0.05,status="last")),  1)
  }
})

test_that("annuities_t_table", {
  for(j in 0:115){
    l=115-j
    p=c(j,j)
    z=116-j
    daxyzn=axyzn(table,x=p,i=0.05,n=z,payment = 'arrears' )
    uaxyzn=axyzn(table,x=p,i=0.05,n=z,payment = "advance")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=p,n=z,i=0.05)),  1)
  }
})

test_that("annuities_t_table", {
  for(j in 0:115){
    l=115-j
    p=c(j,j)
    z=116-j
    daxyzn=axyzn(table,x=p,i=0.05,n=z,payment = 'arrears',status="last" )
    uaxyzn=axyzn(table,x=p,i=0.05,n=z,payment = "advance",status="last")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=p,n=z,i=0.05,status="last")),  1)
  }
})




test_that("annuities_vd_table", {
  for(j in 0:115){

    p=c(0,0)

    daxyzn=axyzn(table,x=p,i=0.05,m=j,payment = 'arrears',status="last" )
    uaxyzn=axyzn(table,x=p,i=0.05,m=j,payment = "advance",status="last")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=p,i=0.05,m=j,status="last")),  1)
  }
})

test_that("annuities_td_table", {
  for(j in 0:115){
    p=115-j
    z=c(0,0)
    daxyzn=axyzn(table,x=z,i=0.05,n=p,m=j,payment = 'arrears' )
    uaxyzn=axyzn(table,x=z,i=0.05,n=p,m=j,payment = "advance")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=z,i=0.05,n=p,m=j)),  1) #BOWERS P 111
  }
})

test_that("annuities_td_table", {
  for(j in 0:115){
    p=115-j
    z=c(0,0)
    daxyzn=axyzn(table,x=z,i=0.05,n=p,m=j,payment = 'arrears',status="last" )
    uaxyzn=axyzn(table,x=z,i=0.05,n=p,m=j,payment = "advance",status="last")

    limit<-function(x){
      if(daxyzn<x&&x<uaxyzn){
        x=1
      }else{x=0}
    }

    expect_equal(limit(caxyzn(table,x=z,i=0.05,n=p,m=j,status="last")),  1) #BOWERS P 111
  }
})

