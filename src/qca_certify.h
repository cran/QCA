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

#ifndef QCA_CERTIFY_H
#define QCA_CERTIFY_H

#include "native_sat/native_sat_verify.h"

typedef struct {
    const int *on, *off; /* QCA's column-major truth-table matrices. */
    int conditions, positives, negatives;
    NsatResources resources;
    int verified_minimum; /* Checked bound with a witness for this table. */
} QcaCertification;

/* For binary or categorical equality terms, NSAT_UNSAT rules out every
 * smaller cover. The caller supplies a feasible incumbent of this size.
 * UNKNOWN and ERROR provide no minimum certificate. */
NsatStatus qca_check_smaller_model(QcaCertification *table, int incumbent);

/* Prove that every legal term covering an ON row contains a chart PI. */
bool qca_check_complete_chart(QcaCertification *table, const int *chart,
                              const int *implicants, int columns);

/* Prove that every binary minimum model uses only chart PIs.
 * minimum must already be a global minimum for this table. */
bool qca_check_minimum_models(QcaCertification *table, const int *implicants,
                              int columns, int minimum);

#endif
