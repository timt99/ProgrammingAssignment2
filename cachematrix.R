#Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. The following
# two functions are used to cache the inverse of a matrix.
#makeCacheMatrix creates a list containing a function to 
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) 

   {
       inv = NULL
     set = function(y) 
       {
          x <<- y
         inv <<- NULL
       }
  
    
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
     list( set= set, get=get, setinv = setinv, getinv=getinv)


  }

#The following function returns the inverse of the matrix.It first checks if the
#inverse has already been computed. If so, it gets the result and skips the computation.
#If not, it computes the inverse, sets the value in the cache via setinv function.


cacheSolve <- function(x=matrix(), ...) 
{

       inverse = x$getinv()
         if (!is.null(inverse))
         {
           message("getting cached data")
           return (inverse)
         }
         
         mat.data = x$get()
         inverse = solve(mat.data, ...)
         
         x$setinv(inverse)
           
         return(inverse)
      }


        ## Return a matrix that is the inverse of 'x'
}
