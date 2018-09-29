  
  //////////////////////////////////////////////////////////////// 
  // Author: Nafis Sadat
  // Purpose: Forecasting model base across countries, incorporating ARIMA components
  // Created: Sometime in 2017
  ////////////////////////////////////////////////////////////////
  
  #include <TMB.hpp>
  #include <Eigen/Sparse>
  #include <vector>
  using namespace density;
  using namespace Eigen;
  
  
  
  
  // NAME: diff_y: 
  // DESC: first diff the Y matrix on columns
  // IN:   matrix<Type>, int
  // OUT:  matrix<Type>
  template<class Type>
  matrix<Type> diff_y(matrix<Type> mat_input, int fdiff) {
    
    int L = mat_input.rows();        // number of locations
    int T = mat_input.cols();        // number of years
   
   // Initialize our first-differenced matrix
   matrix<Type> y_out(L,T-1);
   
   
    if(fdiff == 1) {

      for (int l = 0; l < L; l++) {
        for (int t = 1; t < T; t++) {
           y_out(l,t-1) = mat_input(l,t) - mat_input(l,t-1);
          }
      }
      
      return y_out;
    }
    
    if(fdiff != 1) {
      
      return mat_input;
    }
     
  }
  
  
  
  // NAME: diff_covariates: 
  // DESC: Take an array of covariates (must have 3 dimensions) and creates first differences
  // IN:   array<Type>, int
  // OUT:  array<Type>
  template<class Type>
  array<Type> diff_covariates(array<Type> arr, int fdiff) {
    
    int C = arr.dim(0);        // number of covariates
    int L = arr.dim(1);        // number of locations
    int T = arr.dim(2);        // number of years
  
    // Initialize our first-differenced array
    array<Type> cov_out(C,L,T-1);
    
    
      if(fdiff == 1) {
  
      for (int c = 0; c < C; c++) {
        for (int l = 0; l < L; l++) {
            for (int t = 1; t < T; t++) {
              cov_out(c,l,t-1) = arr(c,l,t) - arr(c,l,t-1);
            }
          }
        }
        return cov_out;
      }
      if(fdiff != 1) {
        return arr;
      }
      
      
  }
  
  // NAME: lag_y
  // DESC: offset the Y matrix by one column to get lag
  // IN:   matrix<Type>
  // OUT:  matrix<Type>
  template<class Type>
  matrix<Type> lag_y(matrix<Type> mat_input) {
    
    size_t L = mat_input.rows();        // number of locations
    size_t T = mat_input.cols();        // number of years
    
    matrix<Type> y_out(L,T-1);
    matrix<Type> y_out_out;
    
      for (int l = 0; l < L; l++) {
        for (int t = 1; t < T; t++) {
          y_out(l,t-1) = mat_input(l,t-1);
        }
      }
      
    // Copy the output
    y_out_out = y_out;
    return y_out_out;
  }
  
  
  // NAME: isNA
  // DESC: Detect if a data point supplied from R is NA or not
  template<class Type>
  bool isNA(Type x){
    return R_IsNA(asDouble(x));
  }
  
  
  // NAME: Objective function
  // DESC: YOU KNOW WHAT IT DO
  template<class Type>
  Type objective_function<Type>::operator() ()
  {
    
    // Dependent variable
    DATA_MATRIX(Y_input);
    
    // Model variance
    PARAMETER(logSigma);
    
    // First difference all data?
    DATA_INTEGER(fd);
    
    // Include convergence term?
    DATA_INTEGER(convergence_term);
    
    // First diff Y
    matrix<Type> Y = diff_y(Y_input, fd);
    
    // Include Fixed Effects?
    DATA_INTEGER(fe);

    
    // Global intercept included?
    DATA_INTEGER(global_int);
  
  
    // Weight decay factor
    DATA_SCALAR(weight_decay);
    
    
      
    /////// PARAMETER ESTIMATION AND PREDICTION ///////
    
    // Initialize NLL fn
    Type nll = 0.;
    
    
    // Fill in in-sample data predictions
    matrix<Type> Y_hat(Y.rows(), Y.cols());
    Y_hat.setZero();
    

    
    // Add FEs
    if(fe == 1) {
      
      // matrix of covariates (in-sample and forecasts)
      DATA_ARRAY(x_input);
      
      // vector of fixed coefficients
      PARAMETER_VECTOR(b);
      
      // First diff x
      array<Type> x = diff_covariates(x_input, fd);
      
      for(int dim0=0; dim0 < x.dim(0); dim0++) {
        for(int dim1=0; dim1 < x.dim(1); dim1++) {
          for(int dim2=0; dim2 < x.dim(2); dim2++) {
            
            // Add to data
             Y_hat(dim1,dim2) += x(dim0,dim1,dim2)*b[dim0];
            
          }
        }
      }
      
      // Report the vectors of fixed effects
      REPORT(b);
    }
    
    // Add global intercept
    if(global_int == 1) {
      
      // Declare parameter
      PARAMETER(a);
      
      // Add to Y_hat
      for(int dim1=0; dim1 < Y.rows(); dim1++) {
        for(int dim2=0; dim2 < Y.cols(); dim2++) {
          Y_hat(dim1, dim2) += a;
        }
      }
      
      
      REPORT(a);
    }
  

  
  
    ////// IID Random Effects //////
    
    // Add random intercepts
    DATA_INTEGER(country_int);
    
    
    if(country_int > 0) {
      
      // printf("%s\n Adding random intercepts \n");
      
      // Parameter vector of country intercepts
      PARAMETER_VECTOR(z);
  
      // Do we want an iid or AR random effect distribution on the intercepts?
      DATA_INTEGER(country_int_dist);
      
      // IID random effect
      if(country_int_dist == 1) {
        
        // Std. error of parameter vector of country intercepts
        PARAMETER(loggroup);
        
        // Add the SE of country intercepts
        for(int m=0; m<z.size(); m++){
          nll -= dnorm(z[m], Type(0.), exp(loggroup), true);
        }
        
        // Set prior on RE SEs
        nll -= dgamma(exp(loggroup), Type(.1), Type(10.), true);
        
        REPORT(loggroup);
        
      }
      
      // AR(1) on countries
      if(country_int_dist == 2) {
        
        // Std. error of AR(1)
        // PARAMETER(loggroup);
        
        // AR(1) correlation value
        PARAMETER(z_ar1);
        
        // Add the SE of country intercepts
        // nll += SCALE(AR1(z_ar1),exp(loggroup))(z);
        nll += AR1(z_ar1)(z);
        
        REPORT(z_ar1);
        
      }
      
      // If neither: fixed effect (as dummy vars)
      
      // Add to prediction
      for(int i=0; i< Y_hat.rows(); i++){
        for(int j=0; j< Y_hat.cols(); j++){
          Y_hat(i,j) += z[i]  ;
        }
      }
      
      REPORT(z);

      
    }
    
  
    // Add random coefficients
    DATA_INTEGER(re_coef);
    
    
    if(re_coef == 1) {
      
      // Get random coefficient array
      DATA_ARRAY(re_coef_input);
      
      // Convert to first differences if fd == 1
      array<Type> re_coef = diff_covariates(re_coef_input, fd);
      
      // Parameter matrix of country coefficients [Covars X Loc]
      PARAMETER_MATRIX(z_coef);
      // Std. error by covars
      PARAMETER_VECTOR(loggrcoef);
  
      // Loop over and add the country-covar coefficient values to Y_hat
        for(int dim0=0; dim0 < re_coef.dim(0); dim0++) {
          for(int dim1=0; dim1 < re_coef.dim(1); dim1++) {
            
            // Add the SE of country coeffs (covar specific)
            nll -= dnorm(z_coef(dim1,dim0), Type(0.), exp(loggrcoef[dim0]), true);
            
            // Add to prediction
            for(int dim2=0; dim2 < re_coef.dim(2); dim2++) {
              Y_hat(dim1,dim2) += z_coef(dim1,dim0) * re_coef(dim0,dim1,dim2);
            }
          }
          
          // Set prior on RE SEs
          nll -= dgamma(exp(loggrcoef[dim0]), Type(.1), Type(10.), true);
        }
        

        
          
     
     REPORT(z_coef);
     REPORT(loggrcoef);
     
    }
    
    
    
    
    ////// Convergence term (under a first-difference model, it's the lagged level value of Y) //////
    
    if(fd == 1 & convergence_term == 1) {
      

      PARAMETER(c);
      
      // Add convergence term for parameter estimation //

      // Create lagged convergence term matrix from original Y_input
      matrix<Type> Y_conv = lag_y(Y_input);
      Y_hat  += Y_conv  * c;
      
      REPORT(c);
      
    }
    
    
    
    ////// Autoregressive processes //////
    
    
    // AR(p) order
    DATA_INTEGER(ar);
    
    if(ar > 0) {
      
      
      // Will we have a global or country specific AR?
      DATA_INTEGER(ar_mod);
      
      // ar_mod == 1: only Global AR as fixed effects
      // ar_mod == 2: only country AR as fixed effects
      // ar_mod == 3: global AR plus country random AR with zero mean
      
      
      
      if(ar_mod == 1) {
        
        // Vector of global ARs
        PARAMETER_VECTOR(rho_global);
        
        // Add to nll and Y_hat
        for(int arr=0; arr< ar; arr++) {
            
            // Add a prior distribution on the rho to try and restrict to (-1,1)
            nll -= dnorm(rho_global[arr], Type(0.0), Type(1.0), true); 
          
          for(int i=0; i< Y.rows(); i++) {
            // Add to fit
            for(int t=0; t < Y.cols(); t++) {
              if(t == 0) {
              }
              if(t > arr ) {
                if(!isNA(Y(i,t-(arr+1)))) {
                  Y_hat(i,t) += Y(i,t-(arr+1)) * rho_global[arr];   
                }
              }
            }
          }
        }
        
        REPORT(rho_global);
        
      }
      
      if(ar_mod == 2) {
        
        // Matrix of AR parameters by country
        PARAMETER_MATRIX(rho_country);
        
        // Add to nll and Y_hat
          for(int arr=0; arr< ar; arr++) {
            for(int i=0; i< Y.rows(); i++) {
    
              // Add a prior distribution on the rho to try and restrict to (-1,1)
              nll -= dnorm(rho_country(i,arr), Type(0.0), Type(1.0), true); 
              
              // Add both the AR terms to our fits
              for(int t=0; t < Y.cols(); t++) {
                if(t == 0) {
                }
                if(t > arr ) {
                  if(!isNA(Y(i,t-(arr+1)))) {
                    Y_hat(i,t) += (Y(i,t-(arr+1)) * rho_country(i,arr)) ; 
                  }
                }
              }
            }
          }
          
        REPORT(rho_country);
      
      }
        
      if(ar_mod == 3) {
        
        // Vector of global ARs
        PARAMETER_VECTOR(rho_global);
        
        // Matrix of AR parameters by country
        PARAMETER_MATRIX(rho_country);
        
        // SD of country rhos
        PARAMETER_VECTOR(logSigma_rho_country);
        
        // Add to nll and Y_hat
        for(int arr=0; arr< ar; arr++) {
          
          // Add a prior distribution on the global rho to try and restrict to (-1,1)
          nll -= dnorm(rho_global[arr], Type(0.0), Type(1.0), true); 
          
          // Add the SE of country ARs
          for(int m=0; m<rho_country.rows(); m++){
            nll -= dnorm(rho_country(m, arr), Type(0.), exp(logSigma_rho_country[arr]), true);
          }
          
          
          for(int i=0; i< Y.rows(); i++) {
            
            // Add to fit
            for(int t=0; t < Y.cols(); t++) {
              if(t == 0) {
              }
              if(t > arr ) {
                if(!isNA(Y(i,t-(arr+1)))) {
                  Y_hat(i,t) += (Y(i,t-(arr+1)) * rho_country(i,arr)) + (Y(i,t-(arr+1)) * rho_global[arr]) ; 
                }
              }
            }
          }
          
          // Set prior on AR RE SEs
          nll -= dgamma(exp(logSigma_rho_country[arr]), Type(.1), Type(10.), true);
          
        }
        
        REPORT(rho_global);
        REPORT(rho_country);
        
      } 

    }
    
    
    
    // Create a matrix of residuals by looping over Y and Y_hat (of size Y)
    matrix<Type> resid = Y - Y_hat;
    
    // Report the residuals
    REPORT(resid);
    
    
    
    ////// Moving average processes //////
    
    // MA(q) order
    DATA_INTEGER(ma);
    
    // NOTE: We only need to pass this to the NLL, since the errors are 
    //       zero mean and doesn't contribute to the data
    
    if(ma > 0) {
      
      
      // Will we have a global or country specific MA?
      DATA_INTEGER(ma_mod);
      
      // ma_mod == 1: only Global MA as fixed effects
      // ma_mod == 2: only country MA as fixed effects
      // ma_mod == 3: global MA plus country random MA with zero mean
      
      
      if(ma_mod == 1) {
        
        // Vector of global MAs
        PARAMETER_VECTOR(theta_global);
        
        // Variance of MAs
        PARAMETER_VECTOR(var_theta_global);
        
        
        // Loop over and add MA contributions to NLL
        for(int maa=0; maa< ma; maa++) {
          
          // Add a prior distribution on the theta to try and restrict to (-1,1)
          nll -= dnorm(theta_global[maa], Type(0.0), Type(1.0), true); 
          
          for(int i=0; i< resid.rows(); i++) {
            for(int t=0; t < resid.cols(); t++) {
              if(t == 0) {
              }
              if(t > maa) {
                nll -= dnorm(resid(i,t), theta_global[maa]*resid(i,t-(maa+1)), exp(var_theta_global[maa]), true);
              }
            }
          }
          
          
          // Set prior on MA RE SEs
          nll -= dgamma(exp(var_theta_global[maa]), Type(.1), Type(10.), true);
        }
        
        REPORT(theta_global);
        REPORT(var_theta_global);
        
      }
      
      
      if(ma_mod == 2) {
        
        // Vector of country MAs
        PARAMETER_MATRIX(theta_country);
        
        // Variance of MAs
        PARAMETER_VECTOR(var_theta_country);
        
        
        // Loop over and add MA contributions to NLL
        for(int maa=0; maa< ma; maa++) {
          for(int i=0; i< resid.rows(); i++) {
            
            // Add a prior distribution on the theta to try and restrict to (-1,1)
            nll -= dnorm(theta_country(i,maa), Type(0.0), Type(1.0), true); 
            
            for(int t=0; t < resid.cols(); t++) {
              if(t == 0) {
              }
              if(t > maa) {
                nll -= dnorm(resid(i,t), theta_country(i,maa)*resid(i,t-(maa+1)), exp(var_theta_country[maa]), true);
              }
            }
          }
          
          // Set prior on MA RE SEs
          nll -= dgamma(exp(var_theta_country[maa]), Type(.1), Type(10.), true);
          
        }
        REPORT(theta_country);
        REPORT(var_theta_country);
        
        
      }
      
      if(ma_mod == 3) {
        
        
        // Vector of global MAs
        PARAMETER_VECTOR(theta_global);
        
        // Variance of MAs
        PARAMETER_VECTOR(var_theta_global);
        
        
        // Vector of global MAs
        PARAMETER_MATRIX(theta_country);
        
        // Variance of MAs
        PARAMETER_VECTOR(var_theta_country);
        
        
        
        // Loop over and add MA contributions to NLL
        for(int maa=0; maa< ma; maa++) {
          
          // Add a prior distribution on the theta to try and restrict to (-1,1)
          nll -= dnorm(theta_global[maa], Type(0.0), Type(1.0), true); 
          
          for(int i=0; i< resid.rows(); i++) {
            
            // Add SE of country MAs as IID REs
            nll -= dnorm(theta_country(i, maa), Type(0.), exp(var_theta_country[maa]), true);
            
            for(int t=0; t < resid.cols(); t++) {
              if(t == 0) {
              }
              if(t > maa) {
                nll -= dnorm(resid(i,t), theta_country(i,maa)*resid(i,t-(maa+1)) + theta_global[maa]*resid(i,t-(maa+1)) , exp(var_theta_global[maa]), true);
              }
            }
          }
          
          
          // Set prior on MA RE SEs
          nll -= dgamma(exp(var_theta_country[maa]), Type(.1), Type(10.), true);
          nll -= dgamma(exp(var_theta_global[maa]), Type(.1), Type(10.), true);
        }
        
        
        REPORT(theta_global);
        REPORT(var_theta_global);
        
        REPORT(theta_country);
        REPORT(var_theta_country);
      }
    }
    
    
    
    
    
    
    ////// Add data to likelihood //////
    
    // Add the data to the NLL IFF Y is not missing with inverse variance weighting
    // We start from t+ar time period, since having AR terms will have blanks in the prediction, ya know
    for(int i=0; i< Y.rows(); i++) {
      for(int t=ar; t < Y.cols(); t++) {
        if(!isNA(Y(i,t)) & !isNA(Y_hat(i,t))) {
         nll -= dnorm(Y(i,t), Y_hat(i,t), exp(logSigma) * Type(pow(Type(Y.cols() - t + 1), weight_decay)) , true);
        }
      }
    }
    
    REPORT(logSigma);

    
    
    
    // We return the in-sample fitted data
    REPORT(Y_hat);
    
    
    return nll ;
    
    
  }
