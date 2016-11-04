## The first function caches the inverse of a matrix in a list
## The second functions returns the inverse of the matrix that was cached by the makeCacheMatrix() 

## Write a short comment describing this function
## The function makeCacheMatrix() takes a matrix as an argument 
## and returns a list with the following functions:
## set() sets new data (matrix)
## get() accesses the data
## setinv() sets the inverse once it becomes available
## getinv() gets the inverse


makeCacheMatrix <- function(x = matrix()) {
  
     inv <-NULL
     ## Changes the data
     set <- function(y){
           x<<-y
           inv<<-NULL
      }
        
      get<-function () x
      setinv <-function(inverse) inv<<-inverse
      getinv <- function() inv
      list(set=set,get=get,
           setinv=setinv,
          getinv=getinv)

}


## This function gets the list created by the akeCacheMatrix() and returns the 
## inverse of the matrix in the list. If the inverse is cached then 
## it reads the value and returns it. If the inverse is not chached the function 
## computes the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
     inv<-x$getinv()
     if(!is.null(inv)){
        message("getting cached data")
     return(inv)
     }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
