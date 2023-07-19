#include <Rcpp.h>
using namespace Rcpp;

// function to be called from R
// [[Rcpp::export]]
NumericMatrix raster_data(int iter, 
                          int layers, 
                          int pixels_wide, 
                          int pixels_high, 
                          double zoom, 
                          double alpha) {

  int n_square = 10;
  
  NumericMatrix image(pixels_wide, pixels_high); // initially zero
  NumericMatrix coeffs(9, layers);
  
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i, j) = R::runif(-.2, .2);
    }
  }
  
  // set image matrix to zeros
  for(int r = 0; r < pixels_high; r++) {
    for(int c = 0; c < pixels_wide; c++) {
      image(c, r) = 0;
    }
  }
  
  // iterate
  int layer;
  int variant;
  int square;

  // convenience variables
  double s = 0;
  double u = .3;
  
  // offsets
  double x_sh = 0;
  double y_sh = 0;

  // indices for storing coordinates
  int x_ind;
  int y_ind;
  
  // sigh
  double x_plot, y_plot, z_plot;

  // values for current state
  double x = 0;
  double y = 0;
  double z = 0;
  
  // values for previous state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  // iterate...
  for(int it = 1; it < iter; it++) {
    
    layer = rand() % layers;   // which affine transform to use?
    variant = rand() % 3;      // which variant function to use?
    square = rand() % n_square;       // which square to use?
    
    // coordinates after random transform
    x = x_old + coeffs(0, layer) * x_old + coeffs(1, layer) * y_old + coeffs(2, layer);
    y = y_old + coeffs(3, layer) * x_old + coeffs(4, layer) * y_old + coeffs(5, layer);
    z = z_old + coeffs(6, layer) * x_old + coeffs(7, layer) * y_old + coeffs(8, layer);

    // apply function to the transformed coordinates
    if(variant == 0) {
      s = x*x + y*y + z*z;
      if(s < .02) {
        s = .02;
      }
      x = x + 2 * x/s;
      y = y + 2 * y/s;
      z = .5*z + .5*z/s + square/2;
      y_plot = y + sin(square);
    } else if(variant == 1){
      x = 1.2 * cos(x) ;
      y = 1.2 * cos(y) - 2;
      z = 4 + sin(z) + square/2;
      y_plot = y + sin(square);
    } else {
      x = 3.5 * cos(x);
      y = 2.5 * cos(y) - 2;
      z = 3 + sin(z) + square/2;
      y_plot = y + 2 * cos(square);
    }
    x_plot = x + square * 3;

    // compute indices to be updated (yes it's meant to be
    // pixels_high on both, and there's a hack here duh)
    x_ind = int (x_plot * pixels_high * zoom) + pixels_high * .5;
    y_ind = int (y_plot * pixels_high * zoom) + pixels_high * .66;

    // store results if they fall within the range
    if(x_ind >= 0 & x_ind < pixels_wide) {
      if(y_ind >= 0 & y_ind < pixels_high) {
        image(x_ind, y_ind) = alpha * z + (1- alpha) * image(x_ind, y_ind);
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = z * .5 + z_old * .5; 
  }
  
  return image;
}


