  # FITTING A DISTRIBUTION

# Equations: 
# 1st equation is obtained by euqating derivative of log(L) wrt p = 0
# 2nd equation is obtained by equating derivative of log(L) wrt a = 0

f = function(val) {
  
  p = val[1]
  a = val[2]
  
  c( sum(p/a - mean(x)),  sum(log(a*x/(p - 1))) )
}

#Creating the Jacobian matrix

D = function(val) {
  
  p = val[1]
  a = val[2]
  
  matrix(c( (1/a), ((-a*x)/(p-1)^2), (-p/(a^2)), (x/p-1) ), 2, 2)
}

#To find inverse of D, we use solve() function

val = c(0.3, 0.5)
( val = val - solve(D(val), f(val)) )