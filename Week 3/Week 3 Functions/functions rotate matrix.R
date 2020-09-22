#install.packages("pracma", dependencies=T); #for dot product 
library(pracma)

##### 1. ROTATE MATRIX FUNCTIONS #####

transposeMatrix = function(mat) #transpose a matrix
{
  t(mat);
}

rotateMatrix90 = function(mat) #rotate a matrix 90 degrees clockwise
{
  #source: Matthew Lundberg - https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
  revMat = apply(mat, 2, rev); #reverse the columns (1=rows, 2=col) of the matrix
  t(revMat); #transpose the matrix
}

rotateMatrix180 = function(mat) #rotate a matrix 180 degrees clockwise
{
  mat90 = rotateMatrix90(mat)
  rotateMatrix90(mat90)
}

rotateMatrix270 = function(mat) #rotate a matrix 270 degrees clockwise
{
  mat180 = rotateMatrix180(mat)
  rotateMatrix90(mat180)
}

multiply3x3Matrix = function(mat1, mat2) #matrix multiplication without doing mat1 %*% mat2
{
  result = matrix(nrow = 3, ncol = 3); #create empty 3x3 matrix
  
  for(i in seq_along(mat1[1,])) #iterate through each row of mat1
  {
    for(j in seq_along(mat2[,1])) #iterate through each row of mat2
    {
      result[i, j] = dot(mat1[i,], mat2[,j])
    }
  }
  
  result;
}