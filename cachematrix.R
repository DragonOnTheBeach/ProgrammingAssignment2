# cachematrix.R

## function-1 ----
### Purpose: provide setter, getter functionalities. The `set` & `get` functions are used if the `inverted-matrix` is not in cache and therefore needs to be set and get. After that, the `setMatrix` function will allow to use it later directly by calling `getMatrix` function. The `getMatrix` function is used if the matrix is already in cache and therefore, can be used directly by calling the function.
### Input: an object of class matrix and equal row and column dimension (square matrix). Provided assumption is that matrix is always invertible
### Output: a list of functions (setter, getter) 
makeCacheMatrix <- function(x = matrix()){
  mat <- NULL
  set <- function(z){
    x <<- z
    mat <<- NULL
  }
  
  get <- function() x
  
  setMatrix <- function(a) mat <<- a
  
  getMatrix <- function() mat
  
  list(set = set, get = get
       , setMatrix = setMatrix
       , getMatrix = getMatrix)
}

## function-2 ----
### Purpose: to get the inverse of a square invertible matrix, if exists in cache or calculating it using `solve` function
### Input: a list of functions which already has a matrix inside it's environment. Other parameters can be provided to the function which will be passed on to the `shape` function
### Output: inverse matrix of the square matrix provided
cacheSolve <- function(mat, ...){
  inverted_mat <- mat$getMatrix()
  if(!is.null(inverted_mat)){
    message("getting cached data")
    return(inverted_mat)
  }
  
  base_matrix <- mat$get()
  
  inverted_mat <- solve(base_matrix, ...)
  mat$setMatrix(inverted_mat)
  
  return(inverted_mat)
}