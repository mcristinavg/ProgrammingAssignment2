
makeCacheMatrix <- function(x = Matrix()) {
                     m <- NULL
                     set <- function(y) {
                                  x <<- y
                                 m <<- NULL
                                }
                   get <- function() x
                   setinverse <- function(solve) m <<- solve(x)
                   getinverse <- function() m
                   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
              }
cacheSolve <- function(x, ...) {
                           m <- x$getinverse()
                           if(!is.null(m)) {
                                       message("getting cached data")
                                      return(m)
                                             }
                     data <- as.matrix(x) 
                           data <- x$get()
                           m <- solve(data, ...)
                           x$setinverse(m)
                           m
                     }

