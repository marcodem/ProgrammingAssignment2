## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# I conclude from the makeVector() example that this function acts a bit like a C++ object
# constructor for a vector object. (Quote: "everything that exists in R is an object")
# So I see the function definition like a C++ class definition that includes its
# constructor method and some private variables encapsulated within the object,
# including getter/setter functions to read/write these private variables.
# The return value of the "constructor" is a vector object reference, no: rather a list with
# the getter/setter functions that are accessible in the global environment.
# Once a mean was calculated it is stored within the vector object itself in the
# private variable m.
#  (I am still unsure about the '<<-' operator: does it assign a value to a variable
#  in the parent environment or the global environment. Looks like both are correct,
#  x is in the global env, whereas m is in the parent env, which is the private var
#  of x. Maybe I am just overstretching the C++ comparison here ...)
#  
# The solution for the matrix is analogous to the vector example.
#
makeCacheMatrix <- function(x = matrix()) {
  # we need a private matrix variable in the original matrix object
  # to hold the inverted matrix
  # and we need the according setters and getters (as private methods, to be exposed
  # to the global environment through the list return value)
  m_inverted = NULL
  set = function(y) { # initialize the matrix with new values, reset m_inverted
    x <<- y
    m_inverted <<- NULL
  }
  get = function() x
  set_m_inverted = function(someinvertedmatrix) {
    m_inverted <<- someinvertedmatrix 
  }
  get_m_inverted = function() m_inverted
  list(set=set, get=get, setim=set_m_inverted, getim=get_m_inverted)
}



## Write a short comment describing this function
# In analogy to cachemean in the vector example:
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  local_inverted_matrix <- x$getim()
  if (!is.null(local_inverted_matrix)) {
    message("getting cached inverted matrix data")
    return(local_inverted_matrix)
  }
  local_original_matrix <- x$get()
  # do the matrix inversion here (assuming that the input is a matrix of 
  # a form that solve can invert)
  local_inverted_matrix <- solve(local_original_matrix)

  # store the inverted matrix back into the matrix object x
  x$setim(local_inverted_matrix)
  mesage("matrix inversion executed and cached")
  local_inverted_matrix
}
