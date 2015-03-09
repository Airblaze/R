## Put comments here that give an overall description of what your
## functions do

###Description of makeCacheMatrix function

#Returns a list of 4 functions . Address of the parent environment can be seen
#when this function is invoked
#Following function takes a matrix as an input and returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y                ##Set variables  x and y in parent environment
    m <<- NULL   
  }
  get <- function() x
  setinverse <- function(inverseValue) m <<- inverseValue
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


### Description of cacheSolve function 
#Following functions takes the returned object of the makeCacheMatrix function and calculates 
#the inverse of the matrix using "solve". 

#First time the inverse is calculated and for every subsequent calls the inverse of the matrix is displayed
#along with the string "getting cached data"


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)          #If the value is already cached then return from this function right away.
   }
  
  data <- x$get()      #This portion will be evaluated every first time when the inverse of the matrix will be calculated
  m <- solve((data), ...)
  x$setinverse(m)
  m
}
