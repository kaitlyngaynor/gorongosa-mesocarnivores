// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#ifdef _OPENMP
  #include <omp.h>
#endif

using namespace Rcpp;
using namespace arma;

vec get_theta(arma::vec S, double U){
  vec out(2);
  out(0) = (sum(S) + U) / 2;
  out(1) = (sum(S) - U) / 2;
  return out;
}

mat get_m(arma::vec S, double theta, arma::vec mu){
  mat out(2,2); //2x2 matrix
  out(0,0) = S(1) - theta; //element in first row, first column
  out(1,0) = mu(1); //second row, first column
  out(0,1) = mu(0); //first row, second column
  out(1,1) = S(0) - theta; //second row, second column
  return out;
}

mat diffU(double Uinv, arma::vec theta, double yd, arma::mat m1, arma::mat m2){
  return Uinv * (exp(-theta[1] * yd) * m1 - exp(-theta[0] * yd) * m2);
}

double splt_lik(vec yd, double lambda, rowvec zet, vec mu, vec e){

  vec lamvec = zeros(2);
  lamvec(1) = lambda;

  vec S = lamvec + mu;
  //I think this can be simplified since lambda(0) is always 0
  double K = prod(lamvec) + lamvec(0) * mu(1) + lamvec(1) * mu(0);
  double U = sqrt(pow(sum(S), 2) - 4 * K);
  double Uinv = 1 / U;
  vec theta = get_theta(S, U);

  mat m1 = get_m(S, theta(1), mu);
  mat m2 = get_m(S, theta(0), mu);

  unsigned nobs = yd.n_elem;
  mat lik;

  mat diaglam = diagmat(lamvec);

  if(nobs == 1){
    lik = (zet * diffU(Uinv, theta, yd[0], m1, m2) * e);

  } else if(nobs == 2){

    lik = (zet * diffU(Uinv, theta, yd[0], m1, m2) * diaglam *
                diffU(Uinv, theta, yd[1], m1, m2) * e);

  } else {

    rowvec ll_m = zet * diffU(Uinv, theta, yd[0], m1, m2) * diaglam;

    for (unsigned i=1; i<(nobs-1); i++){
      ll_m = ll_m * diffU(Uinv, theta, yd[i], m1, m2) * diaglam;
    }

    lik = (ll_m * diffU(Uinv, theta, yd[(nobs-1)], m1, m2) * e);

  }
  return log(lik(0,0));
}

// [[Rcpp::export(rng=false)]]
double mmpp_covs(arma::vec params, arma::mat pind, arma::mat X_f1, arma::mat X_f2,
    arma::mat X_f12, arma::mat X_lam1, arma::mat X_lam2, arma::mat X_lam3,
    arma::vec yd1, arma::vec yd2,
    arma::mat lidx_i, arma::mat yd1_st_idx, arma::mat yd1_en_idx,
    arma::mat yd2_st_idx, arma::mat yd2_en_idx,
    arma::uvec y1_i, arma::uvec y2_i, int threads){

  #ifdef _OPENMP
    omp_set_num_threads(threads);
  #endif

  vec f1 = X_f1 * params.subvec(pind(0,0), pind(0,1));
  vec f2 = X_f2 * params.subvec(pind(1,0), pind(1,1));
  vec f12 = X_f12 * params.subvec(pind(2,0), pind(2,1));

  vec mu1 = exp(params.subvec(pind(3,0), pind(3,1)));
  vec mu2 = exp(params.subvec(pind(4,0), pind(4,1)));

  vec lambda1 = exp(X_lam1 * params.subvec(pind(5,0), pind(5,1)));
  vec lambda2 = exp(X_lam2 * params.subvec(pind(6,0), pind(6,1)));
  vec lambda3 = exp(X_lam3 * params.subvec(pind(7,0), pind(7,1)));

  unsigned N = y1_i.n_elem;

  rowvec zet1(2);
  zet1(0) = mu1(1) / sum(mu1);
  zet1(1) = mu1(0) / sum(mu1);

  rowvec zet2(2);
  zet2(0) = mu2(1) / sum(mu2);
  zet2(1) = mu2(0) / sum(mu2);

  vec e = ones(2);

  double loglik = 0.0;

  #pragma omp parallel for reduction(+: loglik) if(threads > 1)
  for (unsigned i=0; i<N; i++){

    vec psi(4);
    psi(0) = exp(f1(i) + f2(i) + f12(i));
    psi(1) = exp(f1(i));
    psi(2) = exp(f2(i));
    psi(3) = 1;
    psi = psi / sum(psi);

    //lambda idx for site i (same for both species)
    int li_st = lidx_i(i,0);
    int li_en = lidx_i(i,1);
    //subset lambda to values for site i
    vec lambda1_i = lambda1.subvec(li_st, li_en);
    vec lambda2_i = lambda2.subvec(li_st, li_en);
    vec lambda3_i = lambda3.subvec(li_st, li_en);

    double ll1 = 0.0;
    double ll2 = 0.0;
    double ll3 = 0.0;

    for (unsigned j=0; j<lambda1_i.size(); j++){
      vec yd1_j = yd1.subvec(yd1_st_idx(i,j), yd1_en_idx(i,j));
      vec yd2_j = yd2.subvec(yd2_st_idx(i,j), yd2_en_idx(i,j));

      ll1 += splt_lik(yd1_j, lambda1_i(j), zet1, mu1, e); //KLG: x+=y is equivalent to x = x+y
      ll2 += splt_lik(yd1_j, lambda2_i(j), zet1, mu1, e);
      ll3 += splt_lik(yd2_j, lambda3_i(j), zet2, mu2, e);
    }

    double l1 = exp(ll1);
    double l2 = exp(ll2);
    double l3 = exp(ll3);

    loglik += y1_i(i) * y2_i(i) * log(psi(0) * l1 * l3) +
            y1_i(i) * (1-y2_i(i)) * log(psi(0) * l1 * l3 + psi(1) * l2) +
            (1-y1_i(i)) * y2_i(i) * log(psi(0) * l1 * l3 + psi(2) * l3) +
            (1-y1_i(i)) * (1-y2_i(i)) *
            log(psi(0) * l1 * l3 + psi(1) * l2 + psi(2) * l3 + psi(3));

  }

  return -loglik;

}

