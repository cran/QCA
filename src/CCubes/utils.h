/*
Copyright (c) 2020, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdbool.h>
#include "row_dominance.h"
#include "sort_cols.h"
void resize(
    int **array,
    const int rows,
    const int newcols,
    const int oldcols
);
bool too_complex(
    const int foundPI,
    const int solmin,
    const double maxcomb
);
void over_transpose(
    int matrix[],
    const int nr,
    const int nc,
    const int type 
);
bool altb(
    double a,
    double b
);
bool agteb(
    double a,
    double b
);
bool nonredundant(
    const int p_implicants[],
    const int p_indx[],
    const int p_ck[],
    const int tempk[],
    const int tempc[],
    const int nconds,
    const int k,
    const int prevfoundPI
);
void push_PI(
    int p_implicants[],
    int p_indx[],
    int p_ck[],
    int p_pichart[],
    const int tempk[],
    const int tempc[],
    const int nconds,
    const int k,
    const int f,
    const int decpos[],
    const int frows[],
    const int posrows,
    const int foundPI
);
void increment(
    int k,
    int *e,
    int *h,
    int nconds,
    int *tempk,
    int minval
);
void populate_posneg(
    int *rowpos,
    int *rowneg,
    int nconds,
    int ttrows,
    int posrows,
    const int p_tt[],
    int posmat[],
    int negmat[]
);
void get_noflevels(
    int noflevels[],
    const int p_tt[],
    int nconds,
    int ttrows
);
void fill_mbase(
    int mbase[],
    const int tempk[],
    const int noflevels[],
    const int k
);
void get_decimals(
    const int posrows,
    const int negrows,
    const int k,
    int decpos[],
    int decneg[],
    const int posmat[],
    const int negmat[],
    const int tempk[],
    const int mbase[]
);
void get_uniques(
    const int posrows,
    int *found,
    int decpos[],
    bool possiblePI[],
    int possiblePIrows[]
);
void verify_possible_PI(
    const int compare,
    const int negrows,
    int *found,
    bool possiblePI[],
    const int possiblePIrows[],
    const int decpos[],
    const int decneg[]
);
void get_frows(
    int frows[],
    const bool possiblePI[],
    const int possiblePIrows[],
    const int compare
);
void fill_matrix(
    int nrows,
    int ncols,
    int nofl[],
    int *matrix,
    int startrow,
    int cols[],
    int plus1
);
void calculate_rows(
    int *nrows,
    int ncols,
    int nofl[],
    int arrange,
    int maxprod
);
int fillCombinationTasks(
    const int n,
    int k,
    int (*comb)[3],
    const int arrayMaxSize,
    int* outNumItemsFilled
);
int GetNextComb(
    int *arr, 
    int k, 
    int n, 
    int indexLimit
);
bool all_covered(
    const int p_pichart[],
    const int pirows,
    const int picols
);
