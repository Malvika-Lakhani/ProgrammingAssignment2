## Put comments here that give an overall description of what your functions do 

## Write a short comment describing this function.

## We will first create a function called makeCacheMatrix which  

## will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv<<-inverse
  getInverse <- function() inv
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

## Next, we will creat a function called 'cacheSolve' which will 

## compute the inverse of the special "matrix" returned by makeCacheMatrix above.

## If the inverse is already calculated , then the cacheSolve should retrieve 

## the inverse from the cache.

## if X is a square invertible matrix, then solve(x) returns the inverse.

## We will consider the matrix supplied is always invertible.


cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}

## We will get the inverse of the matrix we put in, given it is invertible
