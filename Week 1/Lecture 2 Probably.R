print("hello friend"); #test

# Pr(H) = 1/2;  #probability of flipping a coin

h1 = 0.5;
t1 = 0.5;

# OR means add, AND means multiply 
cor = h1 + t1; #coin OR probability
cor; #1

cand = h1 * h1; #coin AND probability
cand; #.25

#permutations (order not important) and combinations (order is important) 

#CRAPS ... 2 dice, each 1-6
die1 = c(1,2,3,4,5,6);
die2 = seq(1,6,by=1);
freq = numeric(12);

#cat is sort of like print
for (d1 in die1)
{
  print( paste("D1", die1) );
  for (d2 in die2)
  {
    print( paste("D2", die2) );
    score = d1 + d2;	
    print(paste("SCORE",score));
    freq[score] = freq[score] + 1;
  }
}
freq;
sum(freq); #36
pfreq = freq/sum(freq);
pfreq;

table = cbind( seq(1,12,by=1), freq, pfreq);

#random generation
sample(1:6, 1); #generate a random number between and including 1-6, and put it back into the list of possibilities
sample(1:6, 1) + sample(1:6, 1); #generate 2 random numbers between and including 1-6 and add them together

#simulation of 1000 dice rolls
nfreq = numeric(12);
nsim = 1000;
for(i in 1:nsim)
{
  score = sample(1:6, 1) + sample(1:6, 1);
  nfreq[score] = nfreq[score] + 1;
}
nfreq;
table = cbind(table, nfreq/sum(nfreq) );

#set a seed. Allows for repeatable data
set.seed(1234); sample(1:6, 1);

#find a seed that allows 52/100 heads ...
set.seed(123); rbinom(1, 100, 0.5);


##wrap this in a for loop, change the seed until you get 52 exactly. What is that seed? +5 pts EASTER EGG. Post under lecture 02 probably "set.seed" EASTER EGG
seed_list = 111:999;
for(i in seed_list)
{
  set.seed(i);
  res = rbinom(1, 100, 0.5);
  if(res == 52) #if a seed with 52 heads is found, print the seed and number of heads and stop looping
  {
    print(i);
    print(res);
    break;
  }
}


#pdf or cdf ... probability distribution ... 
#Pr(1) = 0; Pr(2) = 1/36, Pr(3) = 2/36, ...

nfreq.cdf = ecdf(nfreq);
plot(nfreq.cdf);


x = cbind(
  runif(1000, 0, 1), #gen uniform distribution
  rnorm(1000, 0, 1), #gen normal distribution
  rbinom(1000, 1, 0.5) #gen binomal distribution
);
head(x);
plot(as.data.frame(x));