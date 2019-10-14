get_q=function(rate, capital=0){
  (rate-capital)/(1+rate)
}
get_r=function(alpha, beta, capital=0){
  ((alpha+beta)*capital+alpha)/beta
}
get_profit_differentiated=function(rates, alpha, beta, capital=0){
  q=get_q(rates, capital)
  (1+rates)*(beta/(alpha+beta))*pbeta(q, alpha, beta+1)/pbeta(q, alpha, beta)-1
}
get_profit_undifferentiated=function(rates, alpha, beta, capital=0){
  q=get_q(rates, capital)
  (1+rates)*(beta/(alpha+beta))*(1-pbeta(q, alpha, beta+1))/(1-pbeta(q, alpha, beta))-1
}
get_percent_differentiated=function(rates, alpha, beta, capital=0){
  pbeta(get_q(rates, capital), alpha, beta)
}