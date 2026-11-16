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

#ifndef CCUBES_COVER_ENCODING_H
#define CCUBES_COVER_ENCODING_H

#include "native_sat.h"

#define NSAT_COVER_ENCODING_VERSION 1
#define NSAT_WORK_VERSION 2

typedef struct {
    const int *on, *off;
    size_t inputs, positives, negatives, target;
    const size_t *row_order;
    size_t order_count;
} NsatCoverInput;

typedef struct {
    size_t pairs, staircase, anchors;
    uint64_t prep_work;
    bool discovery_capped;
} NsatCoverDomainStats;

typedef struct {
    size_t clauses;
    uint64_t comparisons, work;
    bool capped;
    NsatStopReason stop;
} NsatCoverDistanceStats;

/* Add RUP-checked distance-one shortcuts to the distinguished-slot encoding.
 * Points use row-major 1/2 values. Call after sealing the originals. */
NsatStatus nsat_cover_distance_one(const NsatCoverInput *input,
                                    NsatSolver *solver, NsatVerifier *checker,
                                    NsatLimits limits, NsatCoverDistanceStats *stats);

/* Build and solve the checked query with anchor exclusions, active-slot pairs,
 * fewest eligible anchor slots first, positive priority phases, shortest
 * unsatisfied row branching, and persistent checked root units. The caller
 * destroys both handles, including partially initialized handles on failure. */
NsatStatus nsat_cover_solve_default(const NsatCoverInput *input, NsatLimits limits,
                                    NsatSolver **solver, NsatVerifier **checker,
                                    NsatStopReason *stop, NsatCoverDomainStats *stats);

/* Load solver originals from the care rows. Compare each emitted clause
 * against the independent checker reconstruction in emission order. */
NsatStatus nsat_cover_load_solver(NsatSolver **out, const NsatCoverInput *input,
                                  const NsatVerifier *checker, NsatLimits limits,
                                  NsatStopReason *stop);
int nsat_cover_selector(int inputs, int term, int input, int value);

/* Equality literals for categorical conditions. Points are row-major, with
 * values 1..levels[c]; no selected value means a wildcard. This separate
 * layout does not use the binary pricing or pair-discovery policies. */
typedef struct {
    const int *on, *off, *levels;
    size_t inputs, positives, negatives, target;
} NsatMultivalueInput;

NsatStatus nsat_multivalue_load_solver(NsatSolver **out, const NsatMultivalueInput *input,
                                      const NsatVerifier *checker, NsatLimits limits,
                                      NsatStopReason *stop);
NsatStatus nsat_multivalue_solve(const NsatMultivalueInput *input, NsatLimits limits,
                                NsatSolver **solver, NsatVerifier **checker,
                                NsatStopReason *stop);

#endif
