
    
    data {
    int<lower = 1> Nobs;
    int<lower = 1> Nwell;
     int <lower=0> well[Nobs]; //factor for stream
    vector[Nobs] n2anomaly;
    vector[Nwell] o2anomaly;

    
    }
    parameters {
    vector[Nwell] wellmean;
    real a;                // intercept
    real b;                // slope
    real<lower = 0> sigma; //  standard deviation within wells
      real<lower = 0> sigma_reg; //  standard deviation among wells
    }
    model {
    for ( i in 1:Nobs){
    n2anomaly[i] ~ normal(wellmean[well[i]], sigma); // likelihood
    }
    wellmean~normal (a + b*o2anomaly, sigma_reg);
    a~normal(0,10);
    b~normal(0.02,0.05);
    sigma ~normal (0,1);
    sigma_reg~normal (0,1);
    
    }

generated quantities{
  
}

    
 [1] "HA025/28/2020" "HA025/28/2020" "HA025/28/2020" "HA025/28/2020"
 [5] "HA025/28/2020" "HA025/28/2020" "HA125/28/2020" "HA125/28/2020"
 [9] "HA125/28/2020" "HA125/28/2020"
[1] 4
