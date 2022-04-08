/*
Copyright (c) 2016 - 2022, Adrian Dusa
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

#include <R_ext/RS.h> 
#include "utils.h"
#include "lp_min.h"
#include "find_min.h"
void find_min(
    const int p_pichart[],
    const int pirows,
    const unsigned int picols,
    int *solmin,
    int p_indices[]
) {
    double *p_objective = R_Calloc(picols + 1, double);
    p_objective[0] = 0;
    for (unsigned int i = 0; i < picols; i++) {
        p_objective[i + 1] = 1;
    }
    double *p_constraints = R_Calloc(pirows * (picols + 2) + 1, double);
    p_constraints[0] = 0;
    int pos = 1;
    for (int r = 0; r < pirows; r++) { 
        for (unsigned int c = 0; c < picols; c++) {
            p_constraints[pos] = p_pichart[c * pirows + r] * 1;
            pos++;
        }
        p_constraints[pos] = 2;
        p_constraints[pos + 1] = 1;
        pos = pos + 2;
    }
    int *p_bin_vec = R_Calloc(picols, int);
    for (unsigned int i = 0; i < picols; i++) {
        p_bin_vec[i] = i + 1;
    }
    double *p_obj_val = R_Calloc(1, double);
    double *p_solution = R_Calloc(picols + 1, double);
    int *p_status = R_Calloc(1, int);
    lp_min(p_objective,
            pirows, 
            p_constraints,
            picols, 
            p_bin_vec,
            p_obj_val,
            p_solution,
            p_status);
    *solmin = 0;
    for (unsigned int c = 0; c < picols; c++) {
        if (p_solution[c] > 0) { 
            p_indices[*solmin] = c;
            *solmin += 1;
        }
    }
    R_Free(p_objective);
    R_Free(p_constraints);
    R_Free(p_bin_vec);
    R_Free(p_obj_val);
    R_Free(p_solution);
    R_Free(p_status);
    return;
}
