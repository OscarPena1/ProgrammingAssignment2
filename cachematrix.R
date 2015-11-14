## This set of two functions is designed to improve the efficiency of loops when it 
## comes to calculating the inverse of matrices. Calculating the inverse of a matrix
## is a costly calculation, so these functions are designed to cache the inverse of
## a matrix that has already been calculated in order to avoid calculating it again.

## This function creates a list containing a function to do the following
## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the inverse of the Matrix.
## 4. Get the value of the inverse of the Matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #function that sets the original matrix
  set <-function(y){
    x<<-y
    m<<-NULL
  }
  #function that gets the original matrix
  get <-function() x
  #function that sets the inverse of the original matrix. Used in the next function.
  setinverse <-function(inverse) m <<-inverse
  #function that gets the inverse of the original matrix. Used in the next function.
  getinverse <-function() m
  #returns a list with the functions above.
  list(set=set, get= get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of the matrix given, but first
## checks to see if the inverse has been calculated before. If it has,
## it returns the value without computing it again. Otherwise, it calculates
## it and sets the value of the inverse in the cache with the setInverse function.

cacheSolve <- function(x, ...) {
       
  m <-x$getinverse()
  #Checks to see if the inverse has been calculated. If it has, displays message and returns inverse.
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  #If the inverse had not been calculated, it is calculated and put into the cache.
  data <- x$get()
  m<-solve(X, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
