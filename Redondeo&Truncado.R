Truncado<-function(x,n){
  if(x>0){
    p<-x
    k<-0
    if(x<0.1){
      while(p<=1){
        p<-p*10
        k<-k+1
      }
      x<-x*(10^(k+n-1))
      x<-trunc(x)
      x<-x/(10^(k+n-1))
    }else{
      while(p>1){
        p<-p/10
        k<-k+1
      }
      x<-x/(10^k)
      x<-x*(10^n)
      x<-trunc(x)
      x<-x/(10^n)
      x<-x*(10^k)
    }
  }else{
    x<-(-1)*x
    p<-x
    k<-0
    if(x<0.1){
      if (x==0) {
        x<-0
      }else{
        while(p<=1){
          p<-p*10
          k<-k+1
        }
        x<-x*(10^(k+n-1))
        x<-trunc(x)
        x<-x/(10^(k+n-1))
      }
    }else{
      while(p>1){
        p<-p/10
        k<-k+1
      }
      x<-x/(10^k)
      x<-x*(10^n)
      x<-trunc(x)
      x<-x/(10^n)
      x<-x*(10^k)
    }
    x<-(-1)*x
  }
  return(x)
}


Redondeo<-function(x,n){
  if(x>0){
    k<-0
    p<-x
    if(x<0.1){
      while (p<=1) {
        p<-p*10
        k<-k+1
      }
      x<-x*(10^(k-1))
      x<-round(x,digits = n)
      x<-x/(10^(k-1))
    }else{
      while(p>1){
        p<-p/10
        k<-k+1
      }
      x<-x/(10^k)
      x<-round(x,digits = n)
      x<-x*(10^k)
    }
  }else{
    x<-(-1)*x
    k<-0
    p<-x
    if(x<0.1){
      if (x==0) {
        x<-0
      }else{
        while (p<=1) {
          p<-p*10
          k<-k+1
        }
        x<-x*(10^(k-1))
        x<-round(x,digits = n)
        x<-x/(10^(k-1))
      }
    }else{
      while(p>1){
        p<-p/10
        k<-k+1
      }
      x<-x/(10^k)
      x<-round(x,digits = n)
      x<-x*(10^k)
    }
    x<-(-1)*x
  }
  return(x)
}

Truncado(0.000586,2)

Redondeo(0,2)
