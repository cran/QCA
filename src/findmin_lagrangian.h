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

#ifndef QCA_FINDMIN_LAGRANGIAN_H
#define QCA_FINDMIN_LAGRANGIAN_H

#include "qca_rinternals.h"

void solvePIchart_lagrangian(
    int pichart[],
    const int foundPI,
    const int ON_minterms,
    const double weights[],
    int *solution,
    int *solmin,
    double *best_lb_out,
    double *lagr_score_out
);

void solvePIchart_lagrangian_prepare(
    int pichart[],
    const int foundPI,
    const int ON_minterms,
    const double weights[],
    int *solution,
    int *solmin,
    double *best_lb_out,
    double *lagr_score_out,
    unsigned char *improving_core_out,
    int *improving_core_size_out
);
void solvePIchart_lagrangian_prepare_with_incumbent(
    int pichart[],
    const int foundPI,
    const int ON_minterms,
    const double weights[],
    const int *initial_solution,
    int initial_solmin,
    int *solution,
    int *solmin,
    double *best_lb_out,
    double *lagr_score_out,
    unsigned char *improving_core_out,
    int *improving_core_size_out
);

void solvePIchart_lagrangian_prepare_native(
    int pichart[], int foundPI, int ON_minterms, const double weights[],
    const int *initial_solution, int initial_solmin, int *solution, int *solmin,
    double *best_lb_out, double *lagr_score_out,
    unsigned char *improving_core_out, int *improving_core_size_out,
    double *row_dual_out
);

SEXP C_findminLagrangian(SEXP chart);
SEXP C_findminLagrangianInfo(SEXP chart);

#endif
