setwd("E:/Text mining/Data Scientist Coursera/literate-waffle")

##
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"

#omments here that give an overall description of what your
#functions do
#Our aim in this experiment is to write a pair of functions, namely,
#"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
#Write a short comment describing this function
#makeCacheMatrix is a function which creates a special "matrix" object that can
#cache its inverse for the input (which is an invertible square matrix)


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##
## Same here, changed "mean" to "solve" and "m" to "s"

#Write a short comment describing this function
#cacheSolve is a function which computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#Caching the Inverse of a Matrix:
#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than compute it repeatedly.
#Below are a pair of functions that are used to create a special object that
#stores a matrix and caches its inverse.
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" created by
#makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


