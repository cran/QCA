/*
Copyright (c) 2016 - 2026, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote
      products derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "qca_r.h"
#include <math.h>
#include "consistency.h"
#include "find_models.h"
#include "find_consistent_models.h"
#include "utils.h"

void CCubes(
    const int p_tt[],       // the truth table
    const int ttrows,       // number of rows in the truth table
    const int nconds,       // number of conditions in the data
    const double p_data[],  // calibrated dataset
    const int nrdata,       // number of rows in the raw data
    const Rboolean allsol,      // all solution
    const Rboolean rowdom,      // row dominance
    const double picons,    // PI consistency
    const int pidepth,      // depth in number of conditions for each PI
    const int p_fsconds[],  // are conditions fuzzy?
    const int soldepth,     // depth in number of PIs for each solution
    const double solcons,
    const double solcov,
    const double maxcomb,
    const Rboolean keeptrying,

    // pointers to save the results
    int **pichart,           // final PI chart
    int **implicants,        // final implicants in matrix form
    int **models,            // final solution models
    unsigned int *foundPI_,  // final number of PIs found
    int *solrows,            // number of rows for the solution matrix
    int *solcols,            // number of columns for the solution matrix
    Rboolean *complex,       // signal if the returned solution is incomplete due to a too complex PI chart
    // int *solmin_,         // solution minima

    const Rboolean firstmin,   // IEEE switch
    const Rboolean hybrid,     // hybrid exact backend, default TRUE
    const Rboolean gurobi,     // whether to use Gurobi (if installed), default is TRUE
    const Rboolean solind      // IF using Gurobi, findmin() returns the indexes of the solutions
);
