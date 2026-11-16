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
#include "row_dominance.h"
#include "sort_cols.h"

// void printfarray3(int* arr, int nr, int nc);
// void printfarray4(double* arr, int nr, int nc);

int compute_value_bit_width(
    int max_value
);

void resize(
    void **array,
    int type, // 1 = int, 2 = unsigned int
    int increase,
    int size,
    int nrows
);

Rboolean too_complex(
    const unsigned int foundPI,
    const int solmin,
    const double maxcomb
);

void over_transpose(
    int matrix[],
    const int nr,
    const int nc,
    const int type // 0 boolean, 1 int, 2 double
);

Rboolean altb(
    double a,
    double b
);

Rboolean agteb(
    double a,
    double b
);

Rboolean redundant(
    unsigned int p_implicants_pos[],
    unsigned int p_implicants_val[],
    int implicant_words,
    unsigned int fixed_bits[],
    unsigned int value_bits[],
    unsigned int prevfoundPI,
    const int covered[],
    unsigned int covered_limit,
    bool debug,
    const unsigned int p_pichart_pos[],
    int pichart_words,
    const unsigned int candidate_coverage[],
    const unsigned char solver_active[],
    Rboolean *chart_dominated
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
    int negmat[],
    int *max_value
);

void get_noflevels(
    int noflevels[],
    const int p_tt[],
    int nconds,
    int ttrows
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
    Rboolean possiblePI[],
    int possiblePIrows[]
);

void verify_possible_PI(
    const int compare,
    const int negrows,
    int *found,
    Rboolean possiblePI[],
    const int possiblePIrows[],
    const int decpos[],
    const int decneg[]
);

void get_frows(
    int frows[],
    const Rboolean possiblePI[],
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

Rboolean all_covered(
    const int p_pichart[],
    const int pirows,
    const unsigned int picols
);

long long unsigned int nchoosek(
    int n,
    int k
);
