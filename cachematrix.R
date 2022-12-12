
##Create a function that saves a matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()){
  
  ## We will save the inverse en A_inverse
  A_inverse <- NULL
        
  ## We create a function to call our matrix.      
  get <- function(){
    
    x
    
  }
    
  ## We create a function to save our inverse in cache. We will apply this function later to save our inverse in cache.
  setInverse <- function(Inverse){
    
    A_inverse <<- Inverse
    
  }
  
  ## Function to obtain the inverse. 
  getInverse <- function(){
    
    A_inverse
    
  }
  #The function makeCacheMatrix will return a list of the functions created.
  list(get = get, setInverse = setInverse,getInverse = getInverse)
  
}

#Function that calculate the invers (using solve) of a matrix and saves on cache. 
#If the inverse of our matrix is already calculated, then it will return the matrix in cache
#without applying the function solve.
#We use as a parameter a list (obtained in the previous call ).
cacheSolve <- function(x,...){
  #We obtain the inverse in the list x. This value is NULL in the first call.
  Solve_Matrix <- x$getInverse()

  #With this lines we can know if the inverse already exists in x. If exists, then it will search this inverse (saved previously in Solve_Matrix) and returns it.
  if( !is.null(Solve_Matrix) ) {
    message("getting cached data")
    return(Solve_Matrix)
  }
  
  # If the invers is not calculated yet, then we get the matrix saved in x.
  Original_matrix <- x$get()
  
  #Then we calculate the inverse and save the inverse in cache.
  Solve_Matrix <- solve(Original_matrix)
  x$setInverse(Solve_Matrix)

  #Finally we print the invers.
  Solve_Matrix
  
  
}

# Example:
# If we consider the matrix: matrix(c(2,2,3,4,1,2,1,9,10),3,3)

#     [,1] [,2] [,3]
#[1,]    2    4    1
#[2,]    2    1    9
#[3,]    3    2   10

#Then the following code will calculate its inverse:
# x <- makeCacheMatrix(matrix(c(2,2,3,4,1,2,1,9,10),3,3))
#       [,1]        [,2]        [,3]
# [1,] -0.20689655  0.34482759  0.24137931
# [2,]  0.29885057 -0.60919540  0.20689655
# [3,]  0.01149425  0.09195402 -0.06896552
# If we try to calculate again the invers, the following message appears:

# cacheSolve(x)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -0.61538462 -2.9230769  2.6923077
# [2,]  0.53846154  1.3076923 -1.2307692
# [3,]  0.07692308  0.6153846 -0.4615385
