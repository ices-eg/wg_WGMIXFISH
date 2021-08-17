#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List condition_fleet_effort(List fl, IntegerVector dim, IntegerVector sim_yrs, IntegerVector mean_yrs)
{
  
  // Loop over fleets
  for(int f=0; f<fl.length();f++){
  
  S4 x = fl[f]; // a fleet
  
  S4 eff = x.slot("effort");              // effort slot
  NumericVector dat = eff.slot(".Data");  
  S4 cap = x.slot("capacity");              // capacity slot
  NumericVector cap_dat = cap.slot(".Data");  

// Loop through updating the variable //

  int na = dim[0];  // number ages

// loop over the years
  for(int y=sim_yrs[0]; y<sim_yrs[sim_yrs.size()-1]+1; y++) {
    
    
  for(int a=0;a<na; a++) { // loop over ages

    // Calculate the mean for the year/age combination
    NumericVector hist_yrs(mean_yrs.size());
    NumericVector cap_hist_yrs(mean_yrs.size());

    for(int i=mean_yrs[0];i<mean_yrs[mean_yrs.size()-1]+1; i++) { // loop over the years to fill the matrix
      hist_yrs[i-mean_yrs[0]] = dat[a*mean_yrs.size() + i];
      if(R_IsNA(hist_yrs[i-mean_yrs[0]])) hist_yrs[i-mean_yrs[0]] = 0;
      cap_hist_yrs[i-mean_yrs[0]] = cap_dat[a*mean_yrs.size() + i];
      if(R_IsNA(cap_hist_yrs[i-mean_yrs[0]])) cap_hist_yrs[i-mean_yrs[0]] = 0;
                  }
   
           double meanval = mean(na_omit((hist_yrs))); // calculate the mean
           double cap_meanval = mean(na_omit((cap_hist_yrs)));
           if(R_IsNA(meanval)) meanval = 0; // zeros if no value
           if(R_IsNA(cap_meanval)) cap_meanval = 0; 
           
    dat[a*sim_yrs.size() + y] = meanval;
    cap_dat[a*sim_yrs.size() + y] = cap_meanval;
                           }
                    }
   
// Return the dimensions to the object    
dat.attr("dim") = dim;
cap_dat.attr("dim") = dim;
  
// Return to FLFleet
eff.slot(".Data") = dat;
x.slot("effort") = eff;
cap.slot(".Data") = cap_dat;
x.slot("capacity")= cap;


// Metier loop

List met = x.slot("metiers");             //  metiers

for(int m=0; m<met.length();m++){
 
 S4 mt = met[m];
  
  S4 effsh = mt.slot("effshare");
  NumericVector effsh_dat = effsh.slot(".Data");  
  
  // Loop through updating the variable //

  // loop over the years
  for(int y=sim_yrs[0]; y<sim_yrs[sim_yrs.size()-1]+1; y++) {
    
    
    for(int a=0;a<na; a++) { // loop over ages
      
      // Calculate the mean for the year/age combination
      NumericVector effsh_hist_yrs(mean_yrs.size());
      
      for(int i=mean_yrs[0];i<mean_yrs[mean_yrs.size()-1]+1; i++) { // loop over the years to fill the matrix
        effsh_hist_yrs[i-mean_yrs[0]] = effsh_dat[a*mean_yrs.size() + i];
        if(R_IsNA(effsh_hist_yrs[i-mean_yrs[0]])) effsh_hist_yrs[i-mean_yrs[0]] = 0;// If it#s NA, it's really a zero - important for calculating the mean correctly
      }
      
      double effsh_meanval = mean(na_omit(effsh_hist_yrs)); // calculate the mean
      if(R_IsNA(effsh_meanval)) effsh_meanval = 0;   // zeros if no value
      
      
      effsh_dat[a*sim_yrs.size() + y] = effsh_meanval;
    }
  }
  
  // Return the dimensions to the object    
  effsh_dat.attr("dim") = dim;
  
  // Return to FLFleet
  effsh.slot(".Data") = effsh_dat;
  mt.slot("effshare") = effsh;
  
  met[m] = mt;
  
}

x.slot("metiers") = met;

fl[f] = x; // return fleet to the list

  }
  return(fl);
}