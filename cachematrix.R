## makeCacheMatrix
## This function caches a matrix and its inverse matrix.

## cacheSolve
## If the inverse matrix of the matrix cached by makeCacheMatrix has
## not been computed, this function computes the inverse matrix and
## caches it via makeCacheMatrix for subsequent retrieval. Otherwise,
## this function retrieves the inverse matrix cached by makeCacheMatrix.



## makeCacheMatrix returns a list containing functions to get
## and set the values of matrix mtx and its inverse in the cache.


makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  
  set <- function(m){
    mtx <<- m
    inv <<- NULL
  }
  get <- function() mtx
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## cacheSolve returns the inverse matrix of the matrix cached in cmtx.
## cmtx is a list created by makeCacheMatrix.

cacheSolve <- function(cmtx, ...) {
  inv <- cmtx$getInv()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  inv <- solve(cmtx$get(), ...)
  cmtx$setInv(inv)
  inv
}
