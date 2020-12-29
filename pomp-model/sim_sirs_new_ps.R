stopifnot(packageVersion("pomp")>="2.0.9.1")


rproc_euler_multinomial <- Csnippet("
  
  double betaA, betaB, foiA, foiB, seas_A, seas_B, N; //dw foiB
  double rate[16], trans[16];

  // white noise (extra-demographic stochasticity, 
  // currently asssumed the same for both types(?))
  // dw = rgammawn(sigmaSE,dt);
    
  // sinusoidal seasonality (for both types)
  seas_A = 1 + amplitude_A*cos(2.*3.1415*(t-tpeak_A)/1.); // in units of years
  seas_B = 1 + amplitude_B*cos(2.*3.1415*(t-tpeak_B)/1.); // in units of years

  betaA = R0_A*seas_A*gamma_A;
  betaB = R0_B*seas_B*gamma_B; 


  // forces of infection
  N = S + I_A + I_B + C_A + C_B + R_A + R_B + I_AB + I_BA + R_AB;
  
  foiA = betaA*(I_A + I_AB + eta_A)/N; 
  foiB = betaB*(I_B + I_BA + eta_B)/N;
  
  // Out of S
  rate[0] = foiA; // *dw/dt;        // infection of S with A
  rate[1] = foiB; // *dw/dt;        // infection of S with B
  
  reulermultinom(2,S,&rate[0],dt,&trans[0]);
  
  // Out of I_A
  rate[2] = gamma_A;                // recovery of I_A
  
  reulermultinom(1,I_A,&rate[2],dt,&trans[2]);

  // Out of I_B
  rate[3] = gamma_B;                // recovery of I_B
  
  reulermultinom(1,I_B,&rate[3],dt,&trans[3]);

  
  // Out of R_A
  rate[4] = foiB; //*dw/dt;        // infection of R_A with B
  rate[5] = w_A;                   // loss of immunity of R_A 
  
  reulermultinom(2,R_A,&rate[4],dt,&trans[4]);

  
  // Out of R_B
  rate[6] = foiA; //*dw/dt;       // infection of R_B with A
  rate[7] = w_B;                  // loss of immunity of R_B
  
  reulermultinom(2,R_B,&rate[6],dt,&trans[6]);

  
  // Out of I_BA
  rate[8] = gamma_B;              // recovery of I_BA (recovery from B infection)
  
  reulermultinom(1,I_BA,&rate[8],dt,&trans[8]);

  // Out of I_AB
  rate[9] = gamma_A;              // recovery of I_AB (recovery from A infection)
  
  reulermultinom(1,I_AB, &rate[9],dt,&trans[9]);

  // Out of R_AB
  rate[10] = w_A;                 // loss of immunity of R_AB to A
  rate[11] = w_B;                 // loss of immunity of R_AB to B
  
  reulermultinom(2,R_AB,&rate[10],dt,&trans[10]);
  
  // Out of C_A
  rate[12] = phi_A;                      // loss of crossimmunity of C_A to R_A
  rate[13] = (1-chi_BA)*foiB; //*dw/dt;  // infection of C_A with B
  
  reulermultinom(2,C_A,&rate[12],dt,&trans[12]);


  // Out of C_B
  rate[14] = phi_B;                      // loss of crossimmunity of C_B to R_B
  rate[15] = (1-chi_AB)*foiA; //*dw/dt;  // infection of C_B with A
  
  reulermultinom(2,C_B,&rate[14],dt,&trans[14]);


  S   += trans[5]  + trans[7] - trans[0] - trans[1];
  I_A += trans[0]  - trans[2];
  I_B += trans[1]  - trans[3];
  C_A += trans[2]  - trans[12] - trans[13];
  C_B += trans[3]  - trans[14] - trans[15]; 
  R_A += trans[12] + trans[11] - trans[4] -  trans[5];
  R_B += trans[14] + trans[10] - trans[6] -  trans[7];
  I_BA += trans[4] + trans[13] - trans[8];
  I_AB += trans[6] + trans[15] - trans[9];
  R_AB += trans[8] + trans[9]  - trans[10] - trans[11];

  //W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise

  K_A += trans[2] + trans[9];           // true incidence of A
  K_B += trans[3] + trans[8];           // true incidence
  
")


det_skel <- Csnippet("

  double betaA, betaB, foiA, foiB, seas_A, seas_B, N; //dw foiB
  double rate[16];

  // white noise (extra-demographic stochasticity, 
  // currently asssumed the same for both types(?))
  //dw = rgammawn(sigmaSE,dt);
    
  // sinusoidal seasonality (for both types)
  seas_A = 1 + amplitude_A*cos(2.*3.1415*(t-tpeak_A)/1.); // in units of years
  seas_B = 1 + amplitude_B*cos(2.*3.1415*(t-tpeak_B)/1.); // in units of years

  betaA = R0_A*seas_A*gamma_A;
  betaB = R0_B*seas_B*gamma_B; 


  // forces of infection
  N = S + I_A + I_B + C_A + C_B + R_A + R_B + I_AB + I_BA + R_AB;
  
  foiA = betaA*(I_A + I_AB + eta_A)/N; 
  foiB = betaB*(I_B + I_BA + eta_B)/N;
  
  // Out of S
  rate[0] = S*foiA; // *dw/dt;               // infection of S with A
  rate[1] = S*foiB; // *dw/dt;               // infection of S with B
  
  // Out 
  rate[2] = I_A*gamma_A;                     // recovery of I_A
  rate[3] = I_B*gamma_B;                     // recovery of I_B
  rate[4] = R_A*foiB; //*dw/dt;              // infection of R_A with B
  rate[5] = R_A*w_A;                         // loss of immunity of R_A 
  rate[6] = R_B*foiA; //*dw/dt;              // infection of R_B with A
  rate[7] = R_B*w_B;                         // loss of immunity of R_B
  rate[8] = I_BA*gamma_B;                    // recovery of I_BA (recovery from B infection)
  rate[9] = I_AB*gamma_A;                    // recovery of I_AB (recovery from A infection)
  rate[10] = R_AB*w_A;                       // loss of immunity of R_AB to A
  rate[11] = R_AB*w_B;                       // loss of immunity of R_AB to B

  rate[12] = C_A*phi_A;                      // loss of crossimmunity of C_A to R_A
  rate[13] = C_A*(1-chi_BA)*foiB; //*dw/dt;  // infection of C_A with B
  
  rate[14] = C_B*phi_B;                      // loss of crossimmunity of C_B to R_B
  rate[15] = C_B*(1-chi_AB)*foiA; //*dw/dt;  // infection of C_B with A
  
  
  // transitions 
  DS    = rate[5]  +  rate[7] - rate[0] - rate[1];
  DI_A  = rate[0]  -  rate[2];
  DI_B  = rate[1]  -  rate[3];
  DC_A  = rate[2]  -  rate[12]  -  rate[13];
  DC_B  = rate[3]  -  rate[14]  -  rate[15];
  DR_A  = rate[12] +  rate[11]  -  rate[4]   - rate[5];
  DR_B  = rate[14] +  rate[10]  -  rate[6]   - rate[7];
  DI_BA = rate[4]  +  rate[13]  -  rate[8];
  DI_AB = rate[6]  +  rate[15]  -  rate[9];
  DR_AB = rate[8]  +  rate[9]   -  rate[10]  - rate[11];

  //W = (dw - dt)/sigmaSE;  // standardized i.i.d. white noise

  DK_A = rate[2] + rate[9];           // true incidence of A
  DK_B = rate[3] + rate[8];           // true incidence of B
  
")



rinit_ee <- Csnippet("
  // how best to initialise population to ensure population size is correct
  // what is this baryometric thing
  // variables iS_A etc refer to endemic equilibrium of single type model
  double iS_A = 1/R0_A;
  double iS_B = 1/R0_B;

  
  double wA_prime = w_A + phi_A;
  double wB_prime = w_B + phi_B;

  double k_A = gamma_A/(wA_prime);
  double k_B = gamma_B/wB_prime;

  double r_A = phi_A/w_A;
  double r_B = phi_B/w_B;


  double iI_A = (1-iS_A)/(1 + k_A + k_A*r_A);
  double iI_B = (1-iS_B)/(1 + k_B + k_B*r_B);

  double iC_A = k_A*iI_A;
  double iC_B = k_B*iI_B;
  double iR_A = r_A*iC_A;
  double iR_B = r_B*iC_B;

  
  S = nearbyint(pop*iS_A*iS_B);
  I_A = nearbyint(pop*iI_A*iS_B);
  I_B = nearbyint(pop*iS_A*iI_B);
  C_A = nearbyint(pop*iC_A*iS_B);
  C_B = nearbyint(pop*iS_A*iC_B);
  R_A = nearbyint(pop*iR_A*iS_B);
  R_B = nearbyint(pop*iS_A*iR_B);
  I_AB = nearbyint(pop*iI_A*iR_B);
  I_BA = nearbyint(pop*iR_A*iI_B);
  R_AB = nearbyint(pop*iR_A*iR_B);

  W = 0;
  K_A = 0;
  K_B = 0;

")

rinit_2019 <- Csnippet("
  
  S = S_0;
  I_A = I_A_0;
  I_B = I_B_0;
  C_A = C_A_0;
  C_B = C_B_0;
  R_A = R_A_0;
  R_B = R_B_0;
  I_AB = I_AB_0;
  I_BA = I_BA_0;
  R_AB = R_AB_0;

  W = 0;
  K_A = 0;
  K_B = 0;                     
")



# Assumes reporting probability the same (is this an issue?)
# in comparison to the previous model, currently negative binomial ditribution
# is being implemented for the observation process

dmeas_negbin <- Csnippet("
  double lik_A, lik_B;
  
  if(ISNA(total_a)) {
    lik_A = (give_log) ? 0 : 1;
  } else {
      if (psi > 0.0) { 
        lik_A = dnbinom_mu(nearbyint(total_a), 1.0/psi, rho_A*K_A, give_log);
      } else { 
          lik_A = dpois(nearbyint(total_a), rho_A*K_A, give_log); 
        }
    }
  
  if(ISNA(total_b)) {
    lik_B = (give_log) ? 0 : 1;
  } else {
      if (psi > 0.0) { 
        lik_B = dnbinom_mu(nearbyint(total_b), 1.0/psi, rho_B*K_B, give_log);
      } else { 
          lik_B = dpois(nearbyint(total_b), rho_B*K_B, give_log); 
        }
    }
  
  lik = lik_A + lik_B;

")


# quick test: is_data_missing - the switch works
# is_data_missing(is_it = F)


rmeas_negbin <- Csnippet("
  if(psi > 0.0) {
    total_a = rnbinom_mu(1.0/psi, rho_A*K_A);
    total_b = rnbinom_mu(1.0/psi, rho_B*K_B);
  } else {
    total_a = rpois(rho_A*K_A);
    total_b = rpois(rho_B*K_B);
  }

")


# Only poisson model for observation process

dmeas_poisson <- Csnippet("
  double lik_A, lik_B;
  
  if(ISNA(total_a)) {
    lik_A = (give_log) ? 0:1;
  } else {
      lik_A = dpois(nearbyint(total_a), rho_A*K_A, give_log); 
  }
  
  if(ISNA(total_a)) {
    lik_B = (give_log) ? 0:1;
  } else {
      lik_B = dpois(nearbyint(total_b), rho_B*K_B, give_log); 
  }
  
  lik = lik_A + lik_B;

")


rmeas_poisson <- Csnippet("

  total_a = rpois(rho_A*K_A);
  total_b = rpois(rho_B*K_B);
  
")



if(FALSE) {
# Old

#todo: need to fit to both serotypes
# assume reporting probability the same (is this an issue?)
dmeas_gauss <- Csnippet("
  double lik_A, lik_B;
  double tol = 1.0e-18;
  
  double m = rho*K_A;
  double v = m*(1.0-rho+psi*psi*m);
  
  if(ISNA(total_a)) {
    lik_A = (give_log) ? 0 : 1;
  } else {
      if (total_a > 0.0) {
        lik_A = pnorm(total_a+0.5,m,sqrt(v)+tol,1,0)-pnorm(total_a-0.5,m,sqrt(v)+tol,1,0)+tol;
      } else {
          lik_A = pnorm(total_a+0.5,m,sqrt(v)+tol,1,0)+tol;
        }
  }
  
  
  // Hacky duplication, assumes same reporting probability
  m = rho*K_B;
  v = m*(1.0-rho+psi*psi*m);
  
  if(ISNA(total_b)) {
    lik_B = (give_log) ? 0 : 1;
  } else {
      if (total_b > 0.0) {
        lik_B = pnorm(total_b+0.5,m,sqrt(v)+tol,1,0)-pnorm(total_b-0.5,m,sqrt(v)+tol,1,0)+tol;
      } else {
          lik_B = pnorm(total_b+0.5,m,sqrt(v)+tol,1,0)+tol;
      }
  }      
  
  lik = lik_A + lik_B;
")

#todo: need to fit to both serotypes
# assume reporting probability the same (is this an issue?)
rmeas_gauss <- Csnippet("
  double m = rho*K_A;
  double v = m*(1.0-rho+psi*psi*m);
  double tol = 1.0e-18;
  total_a = rnorm(m,sqrt(v)+tol);
  
  if (total_a > 0.0) {
    total_a = nearbyint(total_a);
  } else {
    total_a = 0.0;
  }
  
  // hacky duplication
  m = rho*K_B;
  v = m*(1.0-rho+psi*psi*m);
  total_b = rnorm(m,sqrt(v)+tol);
  
  if (total_b > 0.0) {
    total_b = nearbyint(total_b);
  } else {
    total_b = 0.0;
  }
  
")


}
