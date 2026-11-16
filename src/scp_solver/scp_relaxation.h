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

#ifndef QCA_SCP_RELAXATION_H
#define QCA_SCP_RELAXATION_H

#include "scp_solver.h"
#include <stdint.h>

#define QCA_SCP_DUAL_SCALE INT64_C(65536)

/* Fixed-point arithmetic makes every returned bound and fixing exact. */
int qca_scp_lagrangian_lb(
    const qca_scp_problem *problem, qca_scp_state *state,
    int64_t *dual, int target, int iterations,
    int64_t *costs, int *gradient, int64_t *best_dual,
    int *fixed_columns, int *iterations_used
);

int qca_scp_relaxation_dual_lb(
    const qca_scp_problem *problem,
    const qca_scp_state *state
);

double qca_scp_relaxation_dual_info(
    const qca_scp_problem *problem,
    const qca_scp_state *state,
    double *slack_out
);

#endif
