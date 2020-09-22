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