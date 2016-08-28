## Caching the Inverse of a Matrix
## Below two functions are used to create a special object that
## stores a matrix and caches its inverse.

## First function creates special matrix object caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinverse<-function(inverse)inv<<-inverse
getinverse<-function()inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
}
