
x = read.csv("3.1-data-sheet-udemy-courses-business-courses.csv") #reading in csv
y = read.csv("3.1-data-sheet-udemy-courses-design-courses.csv")
z = read.csv("3.1-data-sheet-udemy-courses-music-courses.csv")
w = read.csv("3.1-data-sheet-udemy-courses-web-development.csv")

price = w$price
his = hist(price)
k = length(price)/5
if(k > 30){
  k = 25
}
lambda = 1/mean(price)
k
n = length(price) # sample size
m = n/k 
interval = seq(0,0,length = k + 1)
for( i in 1:(k+1)){
  interval[i] = qexp((i-1)/k,lambda)
  #count how many fall into each small interval
}
count = seq(0,0,length = k)
for( i in 1:k){
  #if count is greater than lower bound and less than upper bound
  count[i]  = length(price[price<interval[i+1]]) - length(price[price<= interval[i]])
}
count
chi2test = sum(count^2/m) - n
# value of test statistic
# find the critical value
# r is 3 since triangular dist has 3
r = 1
alpha = .05
q = qchisq(1-alpha, k - r - 1) #critical value of X^2 goodness of fit
if (chi2test > q){
  print("reject Service Process Two")
  
}else {
  print("fail to reject web dev Course")
}