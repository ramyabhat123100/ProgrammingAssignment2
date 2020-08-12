
#There are two functions makecachematrix,cachesolve
#library MASS is used to calcuate the inverse for square and non squared matrix
#makecachematrix it contains set,get setinverse,getinverse
library(MASS)
makecachematrix=function(k = matrix()) {
  inv=NULL                                      #initializing inverse as null
  set=function(z){           
    k=z
    inv=NULL
  }
  get = function()k                              
  setinverse=function(inverse)inv=inverse
  getinverse= function(){inv}                         # function to get a inverse of a matrix 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}



#cachesolve this function computes the inverse of the special matrix returned by `makecachematrix`
cachesolve=function(k, ...)      # to get cache data
{
  inv=k$getinverse()             
  if(!is.null(inv))                 # checking whether inverse is null
  {
    message("will get cached data")
    return(inv)                       #it returns inverse value
  }
  mat= k$get()
  inv=solve(mat,...)             # it calculate the inverse value
  k$setinverse(inv)
  inv
}                                   # Return a matrix that is the inverse of 'k'

