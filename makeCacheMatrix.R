@@ -0,0 +1,10 @@
  ## makeCacheMatrix assigns a matrix to the variable x in the parent env
  ## i is for the inverse of the matrix (it is set a NULL before the matrix 
  ##is solved)
  ##set puts the matrix into the parent environment
  ##setInverse puts the solved matrix into the parent environment
  ##get returns the objectx (assumed to be a matrix)
  ##getInverse returns the solution of the matrix
  ##all four are bundled into a list so that they can be accessed
  ##get x$Inv returns the inverse
  ##
  ## cache solve finds the inverse of a matrix and saves it to the list
  ## from makeCacheMAtrix UNLESS that has already done
  ## in which case it returns the saved value
  ## that has already been done in which case it retru
  
  
  makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function()x
    setInverse<-function(solve)inv<<-solve
    getInverse<-function()inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

  
## cacheSolve should be called on the object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    myMatrix <- x$get()
    i <- solve(myMatrix, ...)
    x$setInverse(i)
    i
  }
