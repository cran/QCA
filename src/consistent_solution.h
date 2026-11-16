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

#ifndef QCA_CONSISTENT_SOLUTION_H
#define QCA_CONSISTENT_SOLUTION_H

#include <stddef.h>
#include "qca_r.h"

typedef struct {
    const double *data;
    const int *implicants, *depths, *indices, *fuzzy;
    int conditions, cases;
    double consistency, coverage, outcome_sum;
    double *memberships;
} QCAConsistentFit;

typedef struct {
    double *suffix_memberships, *suffix_overlap;
    size_t overlap_stride;
    double slack;
} QCAConsistentBounds;

void qca_cache_pi_memberships(QCAConsistentFit *fit, unsigned int count);
Rboolean consistent_solution(const QCAConsistentFit *fit, int terms, const int selected[]);
void qca_prepare_consistent_bounds(const QCAConsistentFit *fit, unsigned int count,
                                  int max_terms, QCAConsistentBounds *bounds);
Rboolean qca_consistent_prefix_possible(const QCAConsistentFit *fit,
                                        const QCAConsistentBounds *bounds,
                                        const int selected[], int depth, int slots);

#endif
