data{
  int K;   // the number of occupancy parameters
  int L;   // the number of detection parameters
  int N;   // the number of sites
  int NJ;  // the total number of observations
  int S;   // the number of unique combinations of 1s and 0s

  int obs[N];         // the number of replicate surveys at each site
  int start[N];       // index of the first replicate survey at each site
  matrix[S, K] x[N];  // design matrix

  // detection model covariates
  vector[NJ] trl;
  vector[NJ] dd;

  // indicators of whether each species was detected at least once at a site
  int I1[N];
  int I2[N];
  int I3[N];
  int I4[N];

  // detection / non-detection data over all sites and replicate surveys
  vector[NJ] Y1;
  vector[NJ] Y2;
  vector[NJ] Y3;
  vector[NJ] Y4;
}

parameters{
  // detection model covariates
  vector[L] a1;
  vector[L] a2;
  vector[L] a3;
  vector[L] a4;

  vector[K] beta;  // occupancy model covariates
}

model{

  vector[S] psi[N];   // probability of each combination of 1s and 0s
  vector[S] prob[N];  // psi * probability of detection history
  vector[S] z[N];     // log contribution of each site to the likelihood

  // probability of observing the detection history at each site
  vector[N] cd1;
  vector[N] cd2;
  vector[N] cd3;
  vector[N] cd4;

  beta ~ logistic(0, 1);
  a1 ~ logistic(0, 1);
  a2 ~ logistic(0, 1);
  a3 ~ logistic(0, 1);
  a4 ~ logistic(0, 1);
  
  for(i in 1:N){

    // Stan does not support ragged indexing.  This section accommodates 
    // different numbers of replicate surveys at each site

    // elements of the detection model design matrix
    vector[obs[i]] one;
    matrix[obs[i], 2] w2;
    matrix[obs[i], L] w;
 
    // detection probability at each replicate survey
    vector[obs[i]] lp1;
    vector[obs[i]] lp2;
    vector[obs[i]] lp3;
    vector[obs[i]] lp4;

    // detection history of each species at each replicate survey
    vector[obs[i]] y1;
    vector[obs[i]] y2;
    vector[obs[i]] y3;
    vector[obs[i]] y4;

    y1 <- segment(Y1, start[i], obs[i]);
    y2 <- segment(Y2, start[i], obs[i]);
    y3 <- segment(Y3, start[i], obs[i]);
    y4 <- segment(Y4, start[i], obs[i]);

    one <- rep_vector(1, obs[i]);
    w2 <- append_col(one, segment(trl, start[i], obs[i]));
    w <- append_col(w2, segment(dd, start[i], obs[i]));
  
    lp1 <- exp(w * a1) ./ (1 + exp(w * a1));
    lp2 <- exp(w * a2) ./ (1 + exp(w * a2));
    lp3 <- exp(w * a3) ./ (1 + exp(w * a3));
    lp4 <- exp(w * a4) ./ (1 + exp(w * a4));

    cd1[i] <- exp(sum(y1 .* log(lp1) + (1 - y1) .* log(1 - lp1)));
    cd2[i] <- exp(sum(y2 .* log(lp2) + (1 - y2) .* log(1 - lp2)));
    cd3[i] <- exp(sum(y3 .* log(lp3) + (1 - y3) .* log(1 - lp3)));
    cd4[i] <- exp(sum(y4 .* log(lp4) + (1 - y4) .* log(1 - lp4)));

    psi[i] <- softmax(x[i] * beta);

    prob[i][1] <- psi[i][1] * cd1[i] * cd2[i] * cd3[i] * cd4[i];
    prob[i][2] <- psi[i][2] * cd1[i] * cd2[i] * cd3[i];
    prob[i][3] <- psi[i][3] * cd1[i] * cd2[i] * cd4[i];
    prob[i][4] <- psi[i][4] * cd1[i] * cd2[i];
    prob[i][5] <- psi[i][5] * cd1[i] * cd3[i] * cd4[i];
    prob[i][6] <- psi[i][6] * cd1[i] * cd3[i];
    prob[i][7] <- psi[i][7] * cd1[i] * cd4[i];
    prob[i][8] <- psi[i][8] * cd1[i];
    prob[i][9] <- psi[i][9] * cd2[i] * cd3[i] * cd4[i];
    prob[i][10] <- psi[i][10] * cd2[i] * cd3[i];
    prob[i][11] <- psi[i][11] * cd2[i] * cd4[i];
    prob[i][12] <- psi[i][12] * cd2[i];
    prob[i][13] <- psi[i][13] * cd3[i] * cd4[i];
    prob[i][14] <- psi[i][14] * cd3[i];
    prob[i][15] <- psi[i][15] * cd4[i];
    prob[i][16] <- psi[i][16];

    z[i][1] <- I1[i] * I2[i] * I3[i] * I4[i] * log(prob[i][1]);
    z[i][2] <- I1[i] * I2[i] * I3[i] * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2]);
    z[i][3] <- I1[i] * I2[i] * (1 - I3[i]) * I4[i] *
                log(prob[i][1] + prob[i][3]);
    z[i][4] <- I1[i] * I2[i] * (1 - I3[i]) * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4]);
    z[i][5] <- I1[i] * (1 - I2[i]) * I3[i] * I4[i] *
                log(prob[i][1] + prob[i][5]);
    z[i][6] <- I1[i] * (1 - I2[i]) * I3[i] * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][5] + prob[i][6]);
    z[i][7] <- I1[i] * (1 - I2[i]) * (1 - I3[i]) * I4[i] *
                log(prob[i][1] + prob[i][3] + prob[i][5] + prob[i][7]);
    z[i][8] <- I1[i] * (1 - I2[i]) * (1 - I3[i]) * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4] +
                    prob[i][5] + prob[i][6] + prob[i][7] + prob[i][8]);
    z[i][9] <- (1 - I1[i]) * I2[i] * I3[i] * I4[i] *
                log(prob[i][1] + prob[i][9]);
    z[i][10] <- (1 - I1[i]) * I2[i] * I3[i] * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][9] + prob[i][10]);
    z[i][11] <- (1 - I1[i]) * I2[i] * (1 - I3[i]) * I4[i] *
                  log(prob[i][1] + prob[i][3] + prob[i][9] + prob[i][11]);
    z[i][12] <- (1 - I1[i]) * I2[i] * (1 - I3[i]) * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4] +
                      prob[i][9] + prob[i][10] + prob[i][11] + prob[i][12]);
    z[i][13] <- (1 - I1[i]) * (1 - I2[i]) * I3[i] * I4[i] *
                  log(prob[i][1] + prob[i][5] + prob[i][9] + prob[i][13]);
    z[i][14] <- (1 - I1[i]) * (1 - I2[i]) * I3[i] * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][5] + prob[i][6] +
                      prob[i][9] + prob[i][10] + prob[i][13] + prob[i][14]);
    z[i][15] <- (1 - I1[i]) * (1 - I2[i]) * (1 - I3[i]) * I4[i] *
                  log(prob[i][1] + prob[i][3] + prob[i][5] + prob[i][7] +
                      prob[i][9] + prob[i][11] + prob[i][13] + prob[i][15]);
    z[i][16] <- (1 - I1[i]) * (1 - I2[i]) * (1 - I3[i]) * (1 - I4[i]) *
                  log(sum(prob[i]));

    // likelihood not included in Stan; this allows specification of
    // non-standard likelihoods
    increment_log_prob(sum(z[i]));
  }
}
generated quantities{
  vector[S] psi[N];   // probability of each combination of 1s and 0s
  vector[S] prob[N];  // psi * probability of detection history
  vector[S] z[N];     // log contribution of each site to the likelihood

  // probability of observing the detection history at each site
  vector[N] cd1;
  vector[N] cd2;
  vector[N] cd3;
  vector[N] cd4;
  
  vector[N] ll;  // log likelihood
  
  for(i in 1:N){

    // Stan does not support ragged indexing.  This section accommodates 
    // different numbers of replicate surveys at each site

    // elements of the detection model design matrix
    vector[obs[i]] one;
    matrix[obs[i], 2] w2;
    matrix[obs[i], L] w;
 
    // detection probability at each replicate survey
    vector[obs[i]] lp1;
    vector[obs[i]] lp2;
    vector[obs[i]] lp3;
    vector[obs[i]] lp4;

    // detection history of each species at each replicate survey
    vector[obs[i]] y1;
    vector[obs[i]] y2;
    vector[obs[i]] y3;
    vector[obs[i]] y4;

    y1 <- segment(Y1, start[i], obs[i]);
    y2 <- segment(Y2, start[i], obs[i]);
    y3 <- segment(Y3, start[i], obs[i]);
    y4 <- segment(Y4, start[i], obs[i]);

    one <- rep_vector(1, obs[i]);
    w2 <- append_col(one, segment(trl, start[i], obs[i]));
    w <- append_col(w2, segment(dd, start[i], obs[i]));
  
    lp1 <- exp(w * a1) ./ (1 + exp(w * a1));
    lp2 <- exp(w * a2) ./ (1 + exp(w * a2));
    lp3 <- exp(w * a3) ./ (1 + exp(w * a3));
    lp4 <- exp(w * a4) ./ (1 + exp(w * a4));

    cd1[i] <- exp(sum(y1 .* log(lp1) + (1 - y1) .* log(1 - lp1)));
    cd2[i] <- exp(sum(y2 .* log(lp2) + (1 - y2) .* log(1 - lp2)));
    cd3[i] <- exp(sum(y3 .* log(lp3) + (1 - y3) .* log(1 - lp3)));
    cd4[i] <- exp(sum(y4 .* log(lp4) + (1 - y4) .* log(1 - lp4)));

    psi[i] <- softmax(x[i] * beta);

    prob[i][1] <- psi[i][1] * cd1[i] * cd2[i] * cd3[i] * cd4[i];
    prob[i][2] <- psi[i][2] * cd1[i] * cd2[i] * cd3[i];
    prob[i][3] <- psi[i][3] * cd1[i] * cd2[i] * cd4[i];
    prob[i][4] <- psi[i][4] * cd1[i] * cd2[i];
    prob[i][5] <- psi[i][5] * cd1[i] * cd3[i] * cd4[i];
    prob[i][6] <- psi[i][6] * cd1[i] * cd3[i];
    prob[i][7] <- psi[i][7] * cd1[i] * cd4[i];
    prob[i][8] <- psi[i][8] * cd1[i];
    prob[i][9] <- psi[i][9] * cd2[i] * cd3[i] * cd4[i];
    prob[i][10] <- psi[i][10] * cd2[i] * cd3[i];
    prob[i][11] <- psi[i][11] * cd2[i] * cd4[i];
    prob[i][12] <- psi[i][12] * cd2[i];
    prob[i][13] <- psi[i][13] * cd3[i] * cd4[i];
    prob[i][14] <- psi[i][14] * cd3[i];
    prob[i][15] <- psi[i][15] * cd4[i];
    prob[i][16] <- psi[i][16];

    z[i][1] <- I1[i] * I2[i] * I3[i] * I4[i] * log(prob[i][1]);
    z[i][2] <- I1[i] * I2[i] * I3[i] * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2]);
    z[i][3] <- I1[i] * I2[i] * (1 - I3[i]) * I4[i] *
                log(prob[i][1] + prob[i][3]);
    z[i][4] <- I1[i] * I2[i] * (1 - I3[i]) * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4]);
    z[i][5] <- I1[i] * (1 - I2[i]) * I3[i] * I4[i] *
                log(prob[i][1] + prob[i][5]);
    z[i][6] <- I1[i] * (1 - I2[i]) * I3[i] * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][5] + prob[i][6]);
    z[i][7] <- I1[i] * (1 - I2[i]) * (1 - I3[i]) * I4[i] *
                log(prob[i][1] + prob[i][3] + prob[i][5] + prob[i][7]);
    z[i][8] <- I1[i] * (1 - I2[i]) * (1 - I3[i]) * (1 - I4[i]) *
                log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4] +
                    prob[i][5] + prob[i][6] + prob[i][7] + prob[i][8]);
    z[i][9] <- (1 - I1[i]) * I2[i] * I3[i] * I4[i] *
                log(prob[i][1] + prob[i][9]);
    z[i][10] <- (1 - I1[i]) * I2[i] * I3[i] * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][9] + prob[i][10]);
    z[i][11] <- (1 - I1[i]) * I2[i] * (1 - I3[i]) * I4[i] *
                  log(prob[i][1] + prob[i][3] + prob[i][9] + prob[i][11]);
    z[i][12] <- (1 - I1[i]) * I2[i] * (1 - I3[i]) * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][3] + prob[i][4] +
                      prob[i][9] + prob[i][10] + prob[i][11] + prob[i][12]);
    z[i][13] <- (1 - I1[i]) * (1 - I2[i]) * I3[i] * I4[i] *
                  log(prob[i][1] + prob[i][5] + prob[i][9] + prob[i][13]);
    z[i][14] <- (1 - I1[i]) * (1 - I2[i]) * I3[i] * (1 - I4[i]) *
                  log(prob[i][1] + prob[i][2] + prob[i][5] + prob[i][6] +
                      prob[i][9] + prob[i][10] + prob[i][13] + prob[i][14]);
    z[i][15] <- (1 - I1[i]) * (1 - I2[i]) * (1 - I3[i]) * I4[i] *
                  log(prob[i][1] + prob[i][3] + prob[i][5] + prob[i][7] +
                      prob[i][9] + prob[i][11] + prob[i][13] + prob[i][15]);
    z[i][16] <- (1 - I1[i]) * (1 - I2[i]) * (1 - I3[i]) * (1 - I4[i]) *
                  log(sum(prob[i]));
                  
    ll[i] <- sum(z[i]);
  }
}