## makeCacheMatrix Object
##
## 2 variables (r_matrix and c_matrix)
## 4 functions (get,set,getmatrix,setmatrix)

makeCacheMatrix <- function(r_matrix) {
  
  #Initalize the cache
  c_matrix <- NULL
  
  #set the regular matrix and reset the cached version
  set <- function(y) {
    r_matrix <<- y
    c_matrix <<- NULL 
  }
  
  #Return the regular matrix
  get <- function() {
       r_matrix
  }
  ##Put the input matrix into the cache
  setmatrix <- function(matrix_tmp) {
    c_matrix <<- matrix_tmp
  }
  
  ##Get the matrix out of cache and return it to the caller
  getmatrix <- function() 
  {
        c_matrix
  }
  
  ##List of functions in this object
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##cacheSolve -   Standalone function used to find the inverse of an NxN matrix
##It expects and utilizes an instance of the cacheMatrix object
cacheSolve <- function(x, ...) {
  ##Check for the existance of a cached version of the inverted matrix
  tmp_matrix <- x$getmatrix()
  
  ##If it exists in cache, return the matrix 
  if(!is.null(tmp_matrix)) {
    message("getting cached data")
    return(tmp_matrix)
  }
  ##otherwise get the non-cached, non-inverted matrix and use R's solve function to get the inverse
  tmp_matrix <- x$get()
  x_inverse <- solve(tmp_matrix, ...)
  x$setmatrix(x_inverse)
  #return the inverse matrix
  return(x_inverse)
}
