
#function makeCacheMatrix creates a special "Matrix"
#with set,get,setinverse,getinverse functions

  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    set <- function(y) { # set value of matrix
      x <<- y;
      m <<- NULL;
    }
    get <- function() x; # set value of matrix
    setinverse <- function(solve) m <<- solve; # set inverse of matrix
    getinverse <- function() m; # get inverse of matrix
    # the following will be returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse);
  }




#function cacheSolve computes and caches the inverse of the special "matrix"
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinverse();
  if(!is.null(invert)) { # if invert is not null 
    message("cached data...");
    return(invert); # return cache 
  }
  data <- x$get();
  invert <- solve(data);
  x$setinverse(invert);
  invert;
  
  
}
