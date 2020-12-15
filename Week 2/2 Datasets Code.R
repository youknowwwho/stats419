#install.packages("pracma", dependencies=T); #for dot() product in 1. ROTATE MATRIX FUNCTIONS
library(pracma)
#install.packages("tibble", dependencies=T); #for add_column() under 4. PERSONALITY DATA
library(tibble)
#install.packages("lubridate", dependencies=T); #for year() and week() under 4. PERSONALITY DATA
library(lubridate)
#install.packages("dplyr", dependencies=T); #for 
library(dplyr)

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

##### 1. ROTATE MATRIX CODE #####

myMatrix = matrix( c(
  1, 0, 2,
  0, 3, 0,
  4, 0, 5
), nrow=3, byrow=T);

yourMatrix = matrix( c(
  1, 2, 3,
  4, 5, 6,
  7, 8, 9
), nrow=3, byrow=T);

myMatrix
transposeMatrix(myMatrix);
rotateMatrix90(myMatrix);
rotateMatrix180(myMatrix);
rotateMatrix270(myMatrix);
multiply3x3Matrix(myMatrix, yourMatrix);
#############################

##### 2. IRIS GRAPHIC CODE #####
#source: https://commons.wikimedia.org/wiki/File:Iris_dataset_scatterplot.svg
pairs(iris[1:4], #use IRIS list data to create a matrix of scatterplots
      main = "Iris Data (red=setosa,green=versicolor,blue=virginica)", #title
      pch = 21, #set plot characters to be filled circles
      bg = c("red", "green3", "blue")[unclass(iris$Species)]); #set the background/fill color of the plot characters for the unclassed list of species (1=red, 2=green3, 3=blue)
#############################

##### 4. PERSONALITY DATA #####
data4 = read.delim("C:\\Users\\Connor\\Documents\\1) WSU 2018-\\Fall 2020\\Stat 419\\Week 2\\personality-raw.txt", header = T, sep = "|"); #import data
data4 = data4[c(1:2,4:63)]; #remove "V00" column
data4 = add_column(data4, year = NA, .after = 2); #add a year column
data4 = add_column(data4, week = NA, .after = 3); #add a week column

for(i in 1:nrow(data4))
{
  date = data4[i,2];
  date = strptime(date, format = "%m/%d/%Y %H:%M"); #convert date string to POSIXlt/POSIXt
  data4[i,3] = year(date); #get the year and put it in the year column
  data4[i,4] = week(date); #get the week and put it in the week column
}

data4 = data4[order(-data4$year, -data4$week),]; #sort the data descending by year first, then week
#dab = unique(data4, incomparables = F, MARGIN = 1, fromLast = F) #793 & 794 are duplicates (ece0c3bd125984b3528887c1b17a83b3)
#dab = data4 %>% distinct(data4$md5_email, .keep_all = T); 
data4 = data4[!duplicated(data4$md5_email),]; #remove duplicates
write.table(data4, "personality-clean.txt", sep = "|", row.names = T); #save a cleaned version of the data with the headers

#head(data4); #debug
#############################

##### 5. DO, VARIANCE, & Z-SCORES FUNCTIONS #####

doSummary = function(x) #length, number of NAs, mean, median, mode (custom), variance (custom), stan dev
{
  length = ncol(x); #length (64)
  nas = rowSums(is.na(x)); #number of NAs (0). Source: Sven Hohenstein - https://stackoverflow.com/questions/37801338/count-nas-per-row-in-dataframe
  
  xValues = as.numeric(x[5:64]); #get the list of "Vn"s for computation
  n = length(xValues);
  
  # sum = 0;
  # for(i in 5:length) #get the sum of the "Vn" values
  # {
  #   sum = sum + x[1, i]; #get 1 "Vn" value
  # }
  # mean = sum / length; #custom mean (3.48)
  avg = mean(xValues); #mean (3.48)
  
  # middle = (length + 4) / 2; #get index for middle of "Vn"s
  # median = x[,middle]; #custom median (3.4)
  median = median(xValues); #median (3.4)
  
  mode = doMode(x); #mode (4.2)
  
  # sum = 0;
  # for(i in xValues)
  # {
  #   sum = sum + ( (i - avg)^2 );
  # }
  # variance = sum / (length(xValues) - 1);
  variance = sum( (xValues - avg)^2 ) / ( n - 1 ); #variance (.75)
  
  stanDev = sd(xValues); #standard deviation (.86)
  
  cat("Length = ", length,
          "\nNumber of NAs = ", nas,
          "\nMean = ", avg,
          "\nMedian = ", median,
          "\nMode = ", mode,
          "\nVariance = ", variance,
          "\nStandard Deviation = ", stanDev);
}

doSampleVariance = function(x, method) #get the variance of a record using either naive or regular method
{
  xValues = as.numeric(x[5:64]); #get the list of "Vn"s for computation
  n = length(xValues);
  avg = mean(xValues)
  
  if(method == "naive") #naive variance
  {
    sum = sum(xValues^2) / n;
    sumSquared = ( sum(xValues) / n )^2;
    variance = (sum - sumSquared) * ( n / (n - 1) ); 
  }
  else #regular two-pass variance
  {
    sum = xValues - avg;
    sumSquared = sum(sum^2);
    variance = sum( (xValues - avg)^2 ) / ( n - 1 ); 
  }
  
  return( list(sum, sumSquared, variance) ); #return sum, sumSquared, and variance for each method
}

doMode = function(x) #create a freq table from the record and return the most frequent value
{
  recordValues = as.numeric(x[5:64]); #get the list of "Vn"s for computation
  recordInt = as.double(x[5:64]); #get the list of "Vn"s for computation
  
  freqTab = table(recordInt); #create a frequency table
  maxIndex = which.max(freqTab); #get the index of the max value in the freq table
  maxVal = recordValues[maxIndex]; #get the value under the maxIndex
  maxVal; 
}

##### 5. DO CODE #####

record = data4[1,]; #get the monte.shaffer@gmail.com record (b62c73cdaf59e0a13de495b84030734e 4/6/2020 12:57 2020  14 3.4 ...)
#recordValues = as.numeric(record[5:64]); #debug

doSummary(record);
doSampleVariance(record, 1);
doSampleVariance(record, "naive");
doMode(record);

#############################





#local-cache file ... will-vs-denzel.txt
#data.frame
#will$movies ...
new.will = will$movies.50;
new.will$nmid = will$nmid;
new.will$name = will$name;
new.will$countfilms = will$countfilms$totalcount;

new.will = new.will[, c(12:14,1:11)];

new.denzel = denzel$movies.50;
new.denzel$nmid = denzel$nmid;
new.denzel$name = denzel$name;
new.denzel$countfilms = denzel$countfilms$totalcount;

new.denzel = new.denzel[, c(12:14,1:11)];

df.will.denzel = rbind(new.will, new.denzel);

write.table(df.will.denzel, file = "Week 3/Output/df Will vs Denzel.txt", sep = ",", col.names = T, row.names = F);

df.will.denzel = read.csv("Week 3/Output/df Will vs Denzel.txt", header = T);

#if (cache file) ... grab df    (the cahce file is the "df Will vs Denzel.txt")
#else ... do above
#will = grabFilmsForPerson(nmid);
#my.source = 'local'; ... grab and store that file









print("") #removes "unexpected end of document error"