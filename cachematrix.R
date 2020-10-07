## Those functions will return an inverted matrix from the cache once it is saved in there

## Here we create the function that will carry our data as somewhat an "instance", so the data can
## be carried between scopes.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Here we deal with the "instance" that comes as parameter to either, fetch from scope or calculate
## the inverse of the matrix stored inside of it and them returning it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() ## Fetching matrix from "Object"
  if(!is.null(m)) {  ##Returning from cache if it was there
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## Thereafter if it's not on cache, then, cache it and return
  m <- solve(data, ...)
  x$setinverse(m)
  
  ##Returning recently calculated and cached Matrix
  m
}

##Testing out algorithm
## For loop to test if it is properly checking if the matrix isn't a new one.
for(i in 1:4){
  print("-----------------New run---")
  solving <- makeCacheMatrix(matrix(sample(1:64), nrow=8, ncol=8))
  print(solving$get())
  
  for(j in 1:i)
    print(cacheSolve(solving))
}