## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing function



makeCacheMatrix <- function(x = matrix()) {
     ## function takes matrix as argument
     ## This function returns list 
     ## This list  is used to set the matrix,
     ## get the matrix, get matrix inverse 
     ##and set matrix inverse
     
     
  
     
     invert = NULL
     
     set = function(y) {
          # `<<-` operator is used  to assign a value
          ##to an object which is different 
          ##from the current environment
          
          x <<- y
          
          invert <<- NULL
     }
     get = function() x
     
     setinvert = function(inverse) invert <<- inverse 
     
     getinvert = function() invert
     
     list(set=set, get=get, setinvert=setinvert, 

          getinvert=getinvert)
     
}



cacheSolve <- function(x, ...) {
     
     ## get inverse of the original matrix 
     ## input to makeCacheMatrix()
     
     invert = x$getinvert()
     
     # if the inverse of matrix been created
     
     if (!is.null(invert)){
          
          # retreive from cache
          
          message("getting cached data")
          return(invert)
     }
     
     # if not then calculates the inverse 
     
     new.data = x$get()
     
     invert = solve(new.data, ...)
     
     # settings inverse in
     ## the cache via the setinv function.
     
     x$setinvert(invert)
     
     return(invert)
}
B<- matrix(c(2,3,1,4),nrow=2, ncol=2)
temp = makeCacheMatrix(B)
catched <- cacheSolve(temp)
print(catched)
