library(pomp)



stopifnot(packageVersion("pomp")>="2.0.9.1")


rproc_euler_multinomial <- Csnippet("
  //double tol = 1.0e-18;
  
  //int n_comp = 12; // number of compartments

  double betaA, betaB, foiA, foiB, seas, N; //dw foiB
  double rate[12], trans[12];

  // white noise (extra-demographic stochasticity, 
  // currently asssumed the same for both types(?))
  //dw = rgammawn(sigmaSE,dt);
    
  // sinusoidal seasonality (same for both serotypes currently!)
  seas = 1 + amplitude*cos(2.*3.1415*(t-tpeak)/1.); // in units of years

  betaA = R0_A*seas*gamma_A;
  betaB = R0_B*seas*gamma_B; //currently sharing seasonality


  // forces of infection
  N = S + I_A + I_B + R_A + R_B + I_AB + I_BA + R_AB;
  foiA = betaA*(I_A + I_AB + eta_A)/N; 
  foiB = betaB*(I_B + I_BA + eta_B)/N;
  
  // Out of S
  rate[0] = foiA; // *dw/dt;  //infection of S with A
  rate[1] = foiB; // *dw/dt;  //infection of S with B
  reulermultinom(2,S,&rate[0],dt,&trans[0]);
  
  // Out of I_A
  rate[2] = gamma_A;  //Recovery of I_A
  reulermultinom(1,I_A,&rate[2],dt,&trans[2]);

  // Out of I_B
  rate[3] = gamma_B;  //Recovery of I_B
  reulermultinom(1,I_B,&rate[3],dt,&trans[3]);

  
  // Out of R_A
  rate[4] = (1-chi_BA)*foiB; //*dw/dt;  //infection of R_A with B
  rate[5] = w_A;  //Loss of immunity of R_A 
  reulermultinom(2,R_A,&rate[4],dt,&trans[4]);

  
  // Out of R_B
  rate[6] = (1-chi_AB)*foiA; //*dw/dt;  //infection of R_B with A
  rate[7] = w_B;  //Loss of immunity of R_B
  reulermultinom(2,R_B,&rate[6],dt,&trans[6]);

  
  // Out of I_BA
  rate[8] = gamma_B;  //Recovery of I_BA (recovery from B infection)
  reulermultinom(1,I_BA,&rate[8],dt,&trans[8]);

  // Out of I_AB
  rate[9] = gamma_A;  //Recovery of I_AB (recovery from A infection)
  reulermultinom(1,I_AB, &rate[9],dt,&trans[9]);

  // Out of R_AB
  rate[10] = w_A;  //Loss of immunity of R_AB to A
  rate[11] = w_B;  //Loss of immunity of R_AB to B
  reulermultinom(2,R_AB,&rate[10],dt,&trans[10]);


  //for(int i=0; i<12; i++){
  //  trans[i] = dt*rate[i]; // deterministic
  
    //if(rate[i] > 0){
    //  trans[i] = rnorm(dt*rate[i],sqrt(dt*rate[i])+tol); // gaussian approx.
    //}
    //else{
    //  trans[i] = 0;
    //}
  //}

  S += trans[5] + trans[7] - trans[0] - trans[1];
  I_A += trans[0] - trans[2];
  I_B += trans[1] - trans[3];
  R_A += trans[2] + trans[11] - trans[4] - trans[5];
  R_B += trans[3] + trans[10] - trans[6] - trans[7];
  I_BA += trans[4] - trans[8];
  I_AB += trans[6] - trans[9];
  R_AB += trans[8] + trans[9] - trans[10] - trans[11];

  //W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise

  K_A += trans[2] + trans[9];           // true incidence of A
  K_B += trans[3] + trans[8];           // true incidence
  
")


rproc_gauss <- Csnippet("
  //double tol = 1.0e-18;
  
  //int n_comp = 12; // number of compartments

  double betaA, betaB, foiA, foiB, seas, N; //dw foiB
  double rate[12], trans[12];

  // white noise (extra-demographic stochasticity, 
  // currently asssumed the same for both types(?))
  //dw = rgammawn(sigmaSE,dt);
    
  // sinusoidal seasonality (same for both serotypes currently!)
  seas = 1 + amplitude*cos(2.*3.1415*(t-tpeak)/1.); // in units of years

  betaA = R0_A*seas*gamma_A;
  betaB = R0_B*seas*gamma_B; //currently sharing seasonality


  // forces of infection
  N = S + I_A + I_B + R_A + R_B + I_AB + I_BA + R_AB;
  foiA = betaA*(I_A + I_AB + eta_A)/N; 
  foiB = betaB*(I_B + I_BA + eta_B)/N;
  
  // Out of S
  rate[0] = S*foiA; // *dw/dt;  //infection of S with A
  rate[1] = S*foiB; // *dw/dt;  //infection of S with B
  
  // Out 
  rate[2] = I_A*gamma_A;  //Recovery of I_A
  rate[3] = I_B*gamma_B;  //Recovery of I_B
  rate[4] = R_A*(1-chi_BA)*foiB; //*dw/dt;  //infection of R_A with B
  rate[5] = R_A*w_A;  //Loss of immunity of R_A 
  rate[6] = R_B*(1-chi_AB)*foiA; //*dw/dt;  //infection of R_B with A
  rate[7] = R_B*w_B;  //Loss of immunity of R_B
  rate[8] = I_BA*gamma_B;  //Recovery of I_BA (recovery from B infection)
  rate[9] = I_AB*gamma_A;  //Recovery of I_AB (recovery from A infection)
  rate[10] = R_AB*w_A;  //Loss of immunity of R_AB to A
  rate[11] = R_AB*w_B;  //Loss of immunity of R_AB to B



  for(int i=0; i<12; i++){
    trans[i] = dt*rate[i]; // deterministic
  
    //if(rate[i] > 0){
    //  trans[i] = rnorm(dt*rate[i],sqrt(dt*rate[i])+tol); // gaussian approx.
    //}
    //else{
    //  trans[i] = 0;
    //}
  }

  S += trans[5] + trans[7] - trans[0] - trans[1];
  I_A += trans[0] - trans[2];
  I_B += trans[1] - trans[3];
  R_A += trans[2] + trans[11] - trans[4] - trans[5];
  R_B += trans[3] + trans[10] - trans[6] - trans[7];
  I_BA += trans[4] - trans[8];
  I_AB += trans[6] - trans[9];
  R_AB += trans[8] + trans[9] - trans[10] - trans[11];

  //W += (dw - dt)/sigmaSE;  // standardized i.i.d. white noise

  K_A += trans[2] + trans[9];           // true incidence of A
  K_B += trans[3] + trans[8];           // true incidence
  
")


rinit_ee <- Csnippet("
  // how best to initialise population to ensure population size is correct
  // what is this baryometric thing
  // variables iS_A etc refer to endemic equilibrium of single type model
  double iS_A = 1/R0_A;
  double iS_B = 1/R0_B;
  double k_A = gamma_A/w_A;
  double k_B = gamma_A/w_A;
  
  double iI_A = (1-iS_A)/(1+k_A);
  double iR_A = k_A*(1-iS_A)/(1+k_A);
  double iI_B = (1-iS_B)/(1+k_B);
  double iR_B = k_B*(1-iS_B)/(1+k_B);
  
  S = nearbyint(pop*iS_A*iS_B);
  I_A = nearbyint(pop*iI_A*iS_B);
  I_B = nearbyint(pop*iS_A*iI_B);
  R_A = nearbyint(pop*iR_A*iS_B);
  R_B = nearbyint(pop*iS_A*iR_B);
  I_AB = nearbyint(pop*iI_A*iR_B);
  I_BA = nearbyint(pop*iR_A*iI_B);
  R_AB = nearbyint(pop*iR_A*iR_B);

  W = 0;
  K_A = 0;
  K_B = 0;

")





rinit <- Csnippet("
  // how best to initialise population to ensure population size is correct
  // what is this baryometric thing
  S = nearbyint(pop*S_0);
  I_A = nearbyint(pop*I_A_0);
  I_B = nearbyint(pop*I_B_0);
  R_A = nearbyint(pop*R_A_0);
  R_B = nearbyint(pop*R_B_0);
  I_AB = nearbyint(pop*I_AB_0);
  I_BA = nearbyint(pop*I_BA_0);
  R_AB = nearbyint(pop*R_AB_0);

  W = 0;
  K_A = 0;
  K_B = 0;

")


#todo: need to fit to both serotypes
# assume reporting probability the same (is this an issue?)
dmeas <- Csnippet("
  double lik_A, lik_B;
  
  double m = rho*K_A;
  double v = m*(1.0-rho+psi*psi*m);
  double tol = 1.0e-18;
  if (total_a > 0.0) {
      lik_A = pnorm(total_a+0.5,m,sqrt(v)+tol,1,0)-pnorm(total_a-0.5,m,sqrt(v)+tol,1,0)+tol;
  } else {
    lik_A = pnorm(total_a+0.5,m,sqrt(v)+tol,1,0)+tol;
  }
  
  // Hacky duplication, assumes same reporting probability
  m = rho*K_B;
  v = m*(1.0-rho+psi*psi*m);
  if (total_b > 0.0) {
      lik_B = pnorm(total_b+0.5,m,sqrt(v)+tol,1,0)-pnorm(total_b-0.5,m,sqrt(v)+tol,1,0)+tol;
  } else {
    lik_B = pnorm(total_b+0.5,m,sqrt(v)+tol,1,0)+tol;
  }
  
  lik = lik_A + lik_B;
")

#todo: need to fit to both serotypes
# assume reporting probability the same (is this an issue?)
rmeas <- Csnippet("
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





