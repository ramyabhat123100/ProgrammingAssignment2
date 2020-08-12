
#There are two functions makeCacheMatrix,cacheSolve
#library MASS is used t cacuate the inverse for square and nn squared matrix
#makeCacheMatrix it contains set,get setInverse,getInverse
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL                                      #initializing inverse as null
  set <- function(y){           
    x <<- y
    inv <<- NULL
  }
  get <- function()x                              
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function(){                         # function to get a inverse of a matrix
    inver<-ginv(x)              
    inver%*%x
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
#set function store x as the value  "set"
#get function returs the value of x 
#The setinverse function returns the inverse  of the matrix  and store it as "inv"
#The getinverse function returns the inverse

#cacheSolve this function computes the inverse of the special matrix returned by `makeCacheMatrix`
cacheSolve <- function(x, ...)      # to get cache data
{
  inv <- x$getInverse()             
  if(!is.null(inv))                 # checking whether inverse is null
  {
    message("getting cached data")
    return(inv)                       #it returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)             # it calculate the inverse value
  x$setInverse(inv)
  inv
}                                   # Return a matrix that is the inverse of 'x'

#the return of the getinv function is stored in inv
#if inv is not null then will get message as "getting cached data"