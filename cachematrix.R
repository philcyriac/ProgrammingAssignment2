## The solution is to get the inverse of the provided input matrix. If the 
## input matrix is the same then the inverse value should be redenered 
## from the cache, else the inverse is executed and stored in the cache and 
## also rendered as a result. There are 2 methods defined here 
## (1) makeCacheMatrix & (2) cacheSolve

## makeCacheMatrix takes a matrix as an input and creates a 
## special "vector", a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "vector" 
## created by the makeCacheMatrix function. The function first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache using 
## the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#TEST 1
#nums <- c(1,2,3,4)
#mat1 <- matrix(nums, nrow=2)
#cMatix <- makeCacheMatrix(mat1)
#result <- cacheSolve(cMatix)
#print(result)
#Executing cacheSolve(cMatix) again after the initial run with the 
#same input matrix will return a text "Getting cached data" along with the 
#result of the inverse of the matrix. 

#TEST 2
#nums <- c(1,0,4,1,3,4,4,1,0)
#mat2 <- matrix(nums, nrow=3, ncol=3, byrow = TRUE)
#cMatix <- makeCacheMatrix(mat2)
#result <- cacheSolve(cMatix)
#print(result)
