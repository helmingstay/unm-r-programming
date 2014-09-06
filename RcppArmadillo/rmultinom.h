// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */
//
// rmultinom.h: Rcpp/Armadillo equivalent to R's stats::rmultinom().  
// This is intended for use in C++ functions, and should *not* be called from R.
// It should yield identical results to R.
//
// Copyright (C)  2014  Christian Gunning
//
// This file is part of RcppArmadillo.
//
// RcppArmadillo is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RcppArmadillo is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

#ifndef RCPPARMADILLO__EXTENSIONS__MULTINOM_H
#define RCPPARMADILLO__EXTENSIONS__MULTINOM_H

#include <RcppArmadilloExtensions/fixprob.h>

namespace Rcpp{
    namespace RcppArmadillo{

        IntegerMatrix rmultinom(const int n, const int size,  NumericVector prob_  ) {
            // meaning of n, size, prob as in ?rmultinom
            // opposite of sample() - n=number of draws
            
            double pp;            
            int ii, jj, kk;
            int probsize = prob_.size();
            // Create return object 
            IntegerMatrix ret(probsize, n);
            IntegerMatrix::iterator retIt;
            
            if ( n < 0 || NA_INTEGER(n))  throw std::range_error( "Invalid n" ) ;
            if ( size < 0 || NA_INTEGER(size))  throw std::range_error( "Invalid size" ) ;
            // copy probs once, pass-by-ref hereafter
            NumericVector fixprob = clone(prob_);
            // normalize, error-check probability vector
            // fixprob will be modified in-place
            // replace=true, we don't care about size
            FixProb(fixprob, 1, true);
            // should be 1?
            double p_tot = sum(fixprob);
            // for each column
            for(ii= 0; ii < n; ii++) {
                // column counter
                tmpn = n;
                retIt = ret.begin() + ii * probsize;
                //rmultinom(size, REAL(prob), k, &INTEGER(ans)[ik]);
                // for each row in this column
                for(kk = 0; kk < probsize; kk++) { /* (p_tot, n) are for "remaining binomial" */
                    if(fixprob[kk] >0) {
                        pp = fixprob[kk] / p_tot;
                        *(retIt+kk) = ((pp < 1.) ? (int) Rf_rbinom((double) tmpn,  pp):
                             /*>= 1; > 1 happens because of rounding */
                             tmpn);
                        tmpn -= *(retIt+kk);
                    }; // else { *(retIt+kk) = 0; }
                    if(tmpn <= 0) /* we have all*/ continue;
                    p_tot -= fixprob[k]; /* i.e. = sum(prob[(k+1):K]) */
                }
                // the rest go here
                *(retIt + probsize-1) = tmpn;
            }
            return ret;
        }

    }
}

#endif
