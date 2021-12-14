DT <- data.table( x = letters[1:5], y = 1:5, z = 6:10)
DT_lkup <- data.table(x=c('a','b','c',NA,'e'), y=c(1,NA,3,4,NA), z=c(NA,7,8,9,NA), check=c(1,2,3,4,5))
DT_lkup <- data.table(x=c('a','b','c',NA), y=c(1,NA,3,4), z=c(NA,7,8,9), check=c(1,2,3,4))
for (i in 1:nrow(DT_lkup)){
  DT[(if(is.na(DT_lkup[i,x])) TRUE else x==DT_lkup[i,x]) &
       (if(is.na(DT_lkup[i,y])) TRUE else y==DT_lkup[i,y]) &
       (if(is.na(DT_lkup[i,z])) TRUE else z==DT_lkup[i,z]) 
     ,check:=DT_lkup[i,check]]
  
}