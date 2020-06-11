#progeam to illustrate bootrap property
set.seed(1, sample.kind="Rounding")
#sample creation
y <- rnorm(100, 0, 1)
quantile(y,0.75)
#montecorlo simulation
N<-10000
result<-replicate(N,{
  y <- rnorm(100, 0, 1)
  quantile(y,0.75)
  })
 mean(result)
 sd(result)
 

 
 #bootstrap sampling
 set.seed(1, sample.kind="Rounding")
 c<-seq(1,10,1)
 
 indexes<-createResample(y,10000)

 r<-sapply(indexes,function(x){
   y_boot<-y[x]
   quantile(y_boot,0.75)
 })
 
 mean(r)
 
 sd(r)
 
 


