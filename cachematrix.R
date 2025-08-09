## The following functions are meant for scenarios where
## one has to use the inverse of a given matrix multiple.
## times. Since calculating those can be a costly computation,
## the combination of those functions can get rid of the need
## to make the same computation multiple times.

## NOTE: I did not see that I should assume the supplied
## matrix is inversible, therefore I added filters in the function

## the first function takes a matrix that is inversible
## and gives it the option to call useful functions.

makeCacheMatrix <- function(x = matrix()) {
  # Matrix?
  if (!is.matrix(x)){
    stop("Must enter a matrix.")
  }
  
  # Square matrix?
  else if (nrow(x) != ncol(x)){
    stop("Must enter a square matrix.")
  } 
  
  ## No NA?
  c <- tryCatch(det(x), error = function(e) NA)
  if (is.na(c)){
    stop("An error occured; check for NA's in the matrix!")
  }
  
  ## Determinant not zero?
  else if (c ==0) {
    stop("Must enter a matrix with a determinant other than 0.")
  }
  
  ## Variable to store the inverse; initially empty
  inv <- NULL
  
  ## Used to change the initially passed matrix; resets inv
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  
  ## Tells the current matrix that is supplied
  get <- function() x
  
  ## Calculates the inverse and assigns it to inv
  setinv <- function(solve) inv <<- solve
  
  ## Shows the current inv
  getinv <- function() inv
  
  ## Lets the matrix call the functions through the $ operator
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function takes the matrix created by makeCacheMatrix
## and either computes the inverse OR recalls it, if it already got
## computed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## Checks if inv is already computed, if yes, prints it
  ## and stop the function; saving resources if possible.
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  ## Computes the inverse if not already done, and returns it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}