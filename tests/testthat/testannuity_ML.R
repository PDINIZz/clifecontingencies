library(clifecontingencies)

data(at2000f)
data(at2000m)
table=c(at2000f,at2000m)



test_that("annuities_v_table", {
  for(j in 1:113){
    l=114-j
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
  for(j in 1:113){
    l=114-j
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
  for(j in 1:113){
    l=114-j
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

test_that("annuities_v_table", {
  for(j in 1:113){
    l=114-j
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

