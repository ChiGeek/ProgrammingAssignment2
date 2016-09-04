## Creates a special object to store a matrix and caches its inverse

##This function creates a matrix and caches its value
makeCacheMatrix <- function(my_matrix = numeric())
{
  global_inverse = NULL
  set_data <- function(set_matrix)
  {
    my_matrix <<- set_matrix
    globlal_inverse = NULL ## since we replace the input data, we must recompute inverse
  }
  get_data <- function() my_matrix
  set_inverse <- function(inverse) global_inverse <<- inverse
  get_inverse <- function() global_inverse
  list(set_data = set_data, get_data = get_data, set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function calculates the inverse and caches the value

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$get_inverse()
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
  data <- x$get_data()
  m <- solve(data, ...)
  x$set_inverse(m)
  
}

my_mat <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
x <- makeCacheMatrix(my_mat)
cacheSolve(x)
print (x$get_inverse())
