
    
    data {
    int<lower = 1> Nobs;
    
    int<lower = 1> Ngroups;
    int <lower=0> group_id[Nobs];
     
    vector[Nobs]  Nar_noF;
    vector [Nobs]  Nar_F;

    
    }
    
    parameters {
     vector[Ngroups] noF_mean;
    vector[Ngroups] F_mean;
    
    real a;                // intercept
    real b;                // slope
    
    real<lower = 0> sigma_noF_meas;
    real<lower = 0> sigma_F_meas;
  
        
      real<lower = 0> sigma_reg; //  standard deviation among groups
    }
    
    
    model {
    
    //measurement model
    for ( i in 1:Nobs){
    Nar_noF[i] ~ normal(noF_mean[group_id[i]], sigma_noF_meas); // likelihood
     Nar_F[i] ~ normal(F_mean[group_id[i]], sigma_F_meas); ;
    }

   
    noF_mean~normal (b + F_mean, sigma_reg);
 
   // a~normal(0,10);
    b~normal(0, 1);
    sigma_noF_meas~ normal (0,1);
    sigma_F_meas ~ normal (0,1);
    sigma_reg~normal (0,1);
    
    }

generated quantities{
  
}

    
