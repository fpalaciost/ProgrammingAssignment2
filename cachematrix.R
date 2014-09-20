### makeCacheMatrix and cacheSolve are two functions that can be used to compute
### the inverse matrix of a given matrix, with a resource-saving approach.
### when the inverse is computed for the first time, it is stored, so that it is not necessary to compute it again if the inverse is needed later

## makeCacheMatrix is a function which returns a list with 4 functions or "methods"
# 1 - set which sets the matrix
# 2 - get which returns the matrix
# 3 - setinverse which assigns the inverse that is received as a parameter
# 4 - getinverse which returns the inverse that is stored
makeCacheMatrix <- function(x = matrix()) {
  #We start by creating the inverse matrix filled with NA. inv will have the same dimensions as x
  inv <- x*NA
  #function "set" below is used to assign values, and also to create the inverse matrix filled with NA
  set <- function(y) {
    x <<- y
    inv <<- x*NA
  }
  #funcion get returns the value of the matrix
  get <- function() x
  #function setinverse is used to assign as the inverse, the matrix that is passed as a parameter
  #take into account that setinverse WON'T compute the inverse, only assigns the value that is passed as the first parameter
  setinverse <- function(inverse) inv <<- inverse
  #function getinverse will return the value that is stored as the inverse.
  #take into account that getinverse WON'T check the inverse, only returns the value that is stored
  getinverse <- function() inv
  
  #return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of a matrix
# Parameters: a list created with makeCacheMatrix
# The function will check if the inverse has already been calculated (using getinverse method)
# If the inverse has been already calculated, it return the stored value
# Otherwise, it will compute the inverse, store the inverse and finally return the computed inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #First of all, get the stored value for the inverse 
  inv <- x$getinverse()
  #if the first element of the stored inverse is not NA, the inverse has been already computed and store
  if(!is.na(inv[[1]])) {
    message("getting cached inverse")
    return(inv)
  }
  #If the first element of the stored inverse is NA, the inverse hasn't been computed yet, so it needs to be calculated
  data <- x$get() #get the matrix
  inv <- solve(data, ...) #compute the inverse
  x$setinverse(inv) #store the inverse using the setinverse function
  inv #return the inverse
}
