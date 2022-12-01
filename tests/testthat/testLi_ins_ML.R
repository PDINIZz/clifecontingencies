library(clifecontingencies)

data(at2000f)
data(at2000m)
table=c(at2000f,at2000m)



test_that("annuities_v_table", {
  for(j in 1:113){
    l=114-j
    p=c(j,l)

    dAxyzn=Axyzn(table,x=p,i=0.05,status="joint")


    limit<-function(x){
      if(dAxyzn<x){
        x=1
      }else{x=0}
    }

    expect_equal(limit(cAxyzn(table,x=p,i=0.05,status="joint")),  1)
  }
})


test_that("annuities_v_table", {
  for(j in 1:113){
    l=114-j
    p=c(l,j)

    dAxyzn=Axyzn(table,x=p,i=0.05,status="last")


    limit<-function(x){
      if(dAxyzn<x){
        x=1
      }else{x=0}
    }

    expect_equal(limit(cAxyzn(table,x=p,i=0.05,status="last")),  1)
  }
})



