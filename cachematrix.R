## Put comments here that give an overall description of what your
## functions do

## The 1st function caches the inverse of a matrix. The 2nd looks
## if the inverse has already been calculated, and returns it / calculates
## it if it has not been calculated.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This object is actually a list containing a function to set then get the value
## of the matrix, and then set then get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ##Function with a variable x
  m <- NULL ##assigns the object NULL to m
  set <- function(y) { ##Function with a var y
    x <<- y ##x takes the value y (globalizes)
    m <<- NULL ##assigns the object NULL to m
  }
  get <- function() x ##returns x
  setsolve <- function(solve) { m<<-solve } ##m takes the value solve
  getsolve <- function() m ##returns m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##Function with var x
  m <- x$getsolve() ##m takes the value m from the previous list
  if(!is.null(m)) { ##if m is not null (which means already computed)
    message("getting cached data") ##display message
    return(m) ##returns m (already calculated)
  }
  data <- x$get() ##else data gets the "get" element ie the value x
  m <- solve(data, ...) ##m takes the value of the inverse of data
  x$setsolve(m) ##returns the element setsolve(m) 
  m ##m
}
## Personal note :
##For the fct to get cached data :
##  abc<-makeCacheMatrix(Matrice)
## cacheSolve(abc)
## cacheSolve(abc)
## Else, makeCacheMatrix reinitializes m so we need to store makeCacheMatrix(Matrice) into a variable abc.
