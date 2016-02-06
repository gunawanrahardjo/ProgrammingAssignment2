
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse){
    m <<- inverse
  }
  
  getinverse <- function(){
    m
  }
  
  ##Create accessor
  list(set=set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message ("Getting cached data")
    return (inv)
  }
  
  ##No cache data, get matrix
  mat <- x$get()
  
  ##No cache data, compute inverse matrix
  inv <- solve(mat, ...)
  
  ##No cache data, stored computation result to cache
  x$setinverse(inv)
  
  inv
}

testMatrix <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(testMatrix)
cacheSolve(testMatrix)
