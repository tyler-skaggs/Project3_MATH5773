myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

rsq = function(xk,data){ # data=spruce.df
  df=within(data, X2<-(X-xk)*(X>xk))
  lmp=lm(Y ~ X + X2, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

AICc = function(xk,data){ # data=spruce.df
  df=within(data, X2<-(X-xk)*(X>xk))
  lmp=lm(Y ~ X + X2, data=df)
  AIC(lmp)
}

rsqdash = function(xk,h,data) {
  (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}

AICdash = function(xk, h, data){
  (AICc((xk+h/2),data)-AICc((xk-h/2),data))/h
}
