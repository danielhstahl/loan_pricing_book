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

get_demand_adjusted_profit_differentiated=function(rates, alpha, beta, a, b){
  ((1+rates)*(1-a*rates)*beta)/(alpha+beta)+a*rates-1-((b*alpha)/(alpha+beta))*((1+alpha)/(alpha+beta+1))
}

get_base_return=function(rates, x){
  (1+rates)*(1-x)-1
}

demand_function=function(rates, x, a, b){
  1-a*rates+b*x
}

get_optimal_r=function(alpha, beta, a, b){
  optimize(
    f=function(r){
      -get_demand_adjusted_profit_differentiated(r, alpha, beta, a, b)
    },
    interval = c(0.0, 1.0)
    #lower=0.0,
    #upper=1.0
  )
}




