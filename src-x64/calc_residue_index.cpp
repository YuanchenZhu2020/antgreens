#include <Rcpp.h>
using namespace Rcpp;

//' Mapping MRL data frame by line
//'
//' @description this Rcpp function `mapping_mrl()` construct a map object
//' <String, NumericVector> using prduct name as key and numbers of drugs' MRL values
//' vector of this product as value. In this case, users can easily and efficiently query
//' MRL numeric vector with product's name.
//'
//' @param mrl MRL numeric matrix of each product (row) in each drug (column)
//' @param products character vector composed of product names, which correspond to each
//' row in param `mrl`
//'
//' @return
//' std::map<String, NumericVector> in Rcpp and List in R.
// [[Rcpp::export]]
std::map<String, NumericVector> mapping_mrl(NumericMatrix mrl, CharacterVector products) {
  std::map<String, NumericVector> mrl_map;

  int n = products.size();
  for (int i = 0; i < n; i++) {
    mrl_map[products[i]] = mrl(i,_);
  }

  return mrl_map;
}

//' Calculate residue index using MRL list
//'
//' @description this Rcpp function `calc_use_mrl()` receives test values matrix and
//' returns a numeric matrix that each value have to be divide by correspoding MRL value.
//'
//' @param vm test values matrix of each product (row) in each drug (column)
//' @param products character vector composed of product names, which correspond to each
//' row in param `vm`
//' @param mrl_map a R object List that the product name is used as a key and the MRL
//' standards vector on pesticides is used as a value.
//'
//' @return
//' numeric matrix where each value have to be divided by corresponding MRL standard.
// [[Rcpp::export]]
NumericMatrix calc_residue_index(NumericMatrix vm, CharacterVector products, List mrl_map) {
  int rows = vm.nrow();
  int cols = vm.ncol();

  NumericMatrix nvm(rows, cols);

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      String product_name = products[i];
      nvm(i,j) = vm(i,j) / as<NumericVector>(mrl_map[product_name])[j];
    }
  }

  return nvm;
}
