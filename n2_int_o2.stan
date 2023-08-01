
    
    data {
    int<lower = 1> Nobs;
    int<lower = 1> Nwell;
     int <lower=0> well[Nobs]; //factor for stream
    vector[Nobs]  n2anomaly;
    vector<lower=-21, upper=1> [Nobs]  o2anomaly;

    
    }
    parameters {
    vector[Nwell] wellmean;
    vector[Nwell] O2mean;
    real a;                // intercept
    real b;                // slope
    real<lower = 0> sigma; //  standard deviation within wells
        real<lower = 0> sigma_o; //  standard deviation O2 within wells
      real<lower = 0> sigma_reg; //  standard deviation among wells
    }
    model {
    for ( i in 1:Nobs){
    n2anomaly[i] ~ normal(wellmean[well[i]], sigma); // likelihood
    o2anomaly[i] ~ normal(O2mean[well[i]], sigma_o);
    }
    
    wellmean~normal (a + b*O2mean, sigma_reg);
 
    a~normal(0,10);
    b~normal(0.02,0.05);
    sigma ~normal (0,1);
    sigma_o ~normal (0,5);
    sigma_reg~normal (0,1);
    
    }

generated quantities{
  
}

    
