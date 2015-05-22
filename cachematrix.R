## Set of two functions that calculates the inverse of a square matrix or directly returns the result if it was previously calculated


###THE FUNCTION makeCacheMatrix will set the functions "set","get","setinversion", "getinversion" for a squeare matrix(matrix)

makeCacheMatrix <- function(matrix= matrix()){
        inverse<- NULL
        set <- function(y){             # this function "sets" the matrix (y) we are going to work with, even though we don't use this function in the example we can use it to call it later...
                matrix<<-y                      # The argument "y" is set to "matrix" and is assigned to the parent enviorment of the function at a higher level
                inverse<<- NULL
        }
        get <- function() matrix        #HERE "matrix" is called from the parent function and is assigned to the function "get"
        setinversion <- function(inv) inverse <<- inv   # The argument "inv" of the function is set to "inverse" and is cached to the parent enviorment of the function
        getinversion <- function() inverse              #Call the argument "inverse" from the parent enviorment.
        list(set = set, get = get,... =         #Lists the functions "set", "get", "setinversion" and "getinversion" that are going to be used in the next function
                     setinversion = setinversion,
             getinversion = getinversion)
}


### THIS FUNCTION returns a matrix that is the inverse of "matrix" (previously used in the function makeCacheMatrix(matrix))
cacheSolve <- function(x, ...) {
        inverse <- x$getinversion()                             #Assign the "getinversion" function from the function x ("makeCacheMatrix" in this case) to the object "inverse"
        if(!is.null(inverse)) {                                 # If "Inverse" was previously calculated get the value of it and send the message "getting cached inversed matrix"
                message("getting cached inversed matrix")
                return(inverse)
        }                                                       
        data <- x$get()                 #If the inverse of the matrix was not calculated get the matrix data from the function "get" in x (makeCacheMatrix) and solve it with the function "Solve"
        inverse <- solve(data, ...)
        x$setinversion(inverse)         # Once "inverse" is calculeted call the function "setinversion" from x (makeCacheMatrix) that is going to assing this value to the enviroment of the function
        inverse
}
