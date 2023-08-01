
    
    data {
    int<lower = 1> NobsN;
    int<lower = 1> NobsO;
    int<lower = 1> Nwell_date;
    int<lower = 1> Nwell;
    
    int <lower=0> well_date_id_N[NobsN];
    int <lower=0> well_date_id_O[NobsO];
    int <lower=0> well_id[Nwell_date]; 
     
    vector[NobsN]  n2anomaly;
    vector<lower=-21, upper=1> [NobsO]  o2anomaly;

    
    }
    
    parameters {
     vector[Nwell_date] N2_well_date_mean;
    vector[Nwell_date] O2_well_date_mean;
    
    vector[Nwell] wellmean;
    vector[Nwell] O2mean;
    real a;                // intercept
    real b;                // slope
    
    real<lower = 0> sigma_N2_meas;
    real<lower = 0> sigma_O2_meas;
    
    real<lower = 0> sigma; //  standard deviation within wells
    real<lower = 0> sigma_o; //  standard deviation O2 within wells
        
      real<lower = 0> sigma_reg; //  standard deviation among wells
    }
    
    
    model {
    
    //measurement model
    for ( i in 1:NobsN){
    n2anomaly[i] ~ normal(N2_well_date_mean[well_date_id_N[i]], sigma_N2_meas); // likelihood
    
    }
    
    for ( i in 1:NobsO){
    o2anomaly[i] ~ normal(O2_well_date_mean[well_date_id_O[i]], sigma_O2_meas);
    }
    
    // within well model
     for ( j in 1:Nwell_date){
    N2_well_date_mean[j] ~ normal(wellmean[well_id[j]], sigma); 
    O2_well_date_mean[j] ~ normal(O2mean[well_id[j]], sigma_o);
    }
    
    //among well model
    wellmean~normal (a + b*O2mean, sigma_reg);
 
    a~normal(0,1);
    b~normal(0.008,0.05);
    sigma_N2_meas ~ normal (0,1);
    sigma_O2_meas ~ normal (0,1);
    sigma ~normal (0,1);
    sigma_o ~normal (0,5);
    sigma_reg~normal (0,1);
    
    }

generated quantities{
  vector[NobsN] log_lik;
  for (i in 1:NobsN) {
    log_lik[i] = normal_lpdf(n2anomaly[i] | N2_well_date_mean[well_date_id_N[i]], sigma_N2_meas);
  }
}

    
