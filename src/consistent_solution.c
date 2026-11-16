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

#include "consistent_solution.h"
#include "utils.h"

#include <R_ext/Utils.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>

static Rboolean pi_memberships(const QCAConsistentFit *fit, size_t pi, double *memberships)
{
    int depth = fit->depths[pi];
    if (depth <= 0 || depth > fit->conditions) return FALSE;

    for (int r = 0; r < fit->cases; ++r) memberships[r] = 1.0;
    size_t offset = pi * fit->conditions;
    for (int c = 0; c < depth; ++c) {
        int condition = fit->indices[offset + c] - 1;
        if (condition < 0 || condition >= fit->conditions) return FALSE;

        int level = fit->implicants[offset + condition] - 1;
        const double *values = fit->data + (size_t)condition * fit->cases;
        if (fit->fuzzy[condition]) {
            for (int r = 0; r < fit->cases; ++r) {
                double value = level == 0 ? 1.0 - values[r] : values[r];
                if (value < memberships[r]) memberships[r] = value;
            }
        } else {
            for (int r = 0; r < fit->cases; ++r) {
                if (values[r] != level && 0.0 < memberships[r]) memberships[r] = 0.0;
            }
        }
    }
    return TRUE;
}

static double *allocate_memberships(unsigned int terms, int cases)
{
    if (cases <= 0 || terms > SIZE_MAX / sizeof(double) / (size_t)cases) return NULL;
    return malloc((size_t)terms * cases * sizeof(double));
}

void qca_cache_pi_memberships(QCAConsistentFit *fit, unsigned int count)
{
    const double *outcome = fit->data + (size_t)fit->conditions * fit->cases;
    for (int r = 0; r < fit->cases; ++r) fit->outcome_sum += outcome[r];

    fit->memberships = allocate_memberships(count, fit->cases);
    if (!fit->memberships) return;

    for (unsigned int pi = 0; pi < count; ++pi) {
        if (pi % 256 == 0) R_CheckUserInterrupt();
        if (!pi_memberships(fit, pi, fit->memberships + (size_t)pi * fit->cases)) {
            free(fit->memberships);
            fit->memberships = NULL;
            return;
        }
    }
}

Rboolean consistent_solution(const QCAConsistentFit *fit, int terms, const int selected[])
{
    const double *columns[terms];
    double *scratch = NULL;
    if (fit->memberships) {
        for (int t = 0; t < terms; ++t)
            columns[t] = fit->memberships + (size_t)selected[t] * fit->cases;
    } else {
        // Score a single model at a time if the full cache cannot be allocated.
        scratch = allocate_memberships(terms, fit->cases);
        if (!scratch) return FALSE;
        for (int t = 0; t < terms; ++t) {
            double *membership = scratch + (size_t)t * fit->cases;
            if (!pi_memberships(fit, selected[t], membership)) {
                free(scratch);
                return FALSE;
            }
            columns[t] = membership;
        }
    }

    const double *outcome = fit->data + (size_t)fit->conditions * fit->cases;
    double total = 0.0, overlap = 0.0;
    for (int r = 0; r < fit->cases; ++r) {
        double membership = 0.0;
        for (int t = 0; t < terms; ++t) {
            if (columns[t][r] > membership) membership = columns[t][r];
        }
        total += membership;
        overlap += membership < outcome[r] ? membership : outcome[r];
    }

    if (scratch) free(scratch);
    return total > 0.0 && fit->outcome_sum > 0.0 &&
        agteb(overlap / total, fit->consistency) &&
        agteb(overlap / fit->outcome_sum, fit->coverage);
}

static Rboolean bounded_membership(double value)
{
    return value == 0.0 || (value >= DBL_MIN && value <= 1.0);
}

void qca_prepare_consistent_bounds(const QCAConsistentFit *fit, unsigned int count,
                                  int max_terms, QCAConsistentBounds *bounds)
{
    size_t stride = (size_t)max_terms + 1;
    if (!fit->memberships || count > INT_MAX || max_terms < 2 ||
        !(fit->outcome_sum > 0.0) || !isfinite(fit->outcome_sum) ||
        stride > SIZE_MAX / sizeof(double) / ((size_t)count + 1))
        return;

    const double *outcome = fit->data + (size_t)fit->conditions * fit->cases;
    for (int r = 0; r < fit->cases; ++r) {
        if (!bounded_membership(outcome[r])) return;
    }

    bounds->suffix_memberships = allocate_memberships(count + 1, fit->cases);
    bounds->suffix_overlap = malloc(((size_t)count + 1) * stride * sizeof(double));
    if (!bounds->suffix_memberships || !bounds->suffix_overlap) goto unavailable;
    bounds->overlap_stride = stride;
    /* Scores use ordinary double sums and agteb() at the leaves. Allow for
       rounding in the case sums, the slot bound and the final ratios. */
    bounds->slack = 16.0 * DBL_EPSILON * ((double)fit->cases + max_terms + 1.0);
    double *last_memberships = bounds->suffix_memberships + (size_t)count * fit->cases;
    double *last_overlap = bounds->suffix_overlap + (size_t)count * stride;
    for (int r = 0; r < fit->cases; ++r) last_memberships[r] = 0.0;
    for (size_t s = 0; s < stride; ++s) last_overlap[s] = 0.0;

    for (unsigned int pi = count; pi-- > 0;) {
        if (pi % 256 == 0) R_CheckUserInterrupt();
        const double *term = fit->memberships + (size_t)pi * fit->cases;
        double *suffix = bounds->suffix_memberships + (size_t)pi * fit->cases;
        const double *next_suffix = suffix + fit->cases;
        double mass = 0.0;
        for (int r = 0; r < fit->cases; ++r) {
            double value = term[r];
            if (!bounded_membership(value)) goto unavailable;
            suffix[r] = value > next_suffix[r] ? value : next_suffix[r];
            mass += value < outcome[r] ? value : outcome[r];
        }
        double *overlap = bounds->suffix_overlap + (size_t)pi * stride;
        const double *next_overlap = overlap + stride;
        overlap[0] = 0.0;
        for (size_t s = 1; s < stride; ++s) {
            double included = mass + next_overlap[s - 1];
            overlap[s] = included > next_overlap[s] ? included : next_overlap[s];
        }
    }
    return;

unavailable:
    free(bounds->suffix_memberships);
    free(bounds->suffix_overlap);
    *bounds = (QCAConsistentBounds){0};
}

Rboolean qca_consistent_prefix_possible(const QCAConsistentFit *fit,
                                        const QCAConsistentBounds *bounds,
                                        const int selected[], int depth, int slots)
{
    size_t next = depth ? (size_t)selected[depth - 1] + 1 : 0;
    const double *outcome = fit->data + (size_t)fit->conditions * fit->cases;
    const double *suffix = bounds->suffix_memberships + next * fit->cases;
    double overlap = 0.0, envelope = 0.0, excess = 0.0;
    for (int r = 0; r < fit->cases; ++r) {
        double current = 0.0;
        for (int t = 0; t < depth; ++t) {
            double value = fit->memberships[(size_t)selected[t] * fit->cases + r];
            if (value > current) current = value;
        }
        double attainable = current > suffix[r] ? current : suffix[r];
        double y = outcome[r];
        overlap += current < y ? current : y;
        envelope += attainable < y ? attainable : y;
        if (current > y) excess += current - y;
    }

    /* The union of the suffix bounds overlap case by case. The largest
       individual overlaps also bound how much the remaining slots can add. */
    double slot_limit = overlap + bounds->suffix_overlap[next * bounds->overlap_stride + slots];
    double upper = envelope < slot_limit ? envelope : slot_limit;
    if (!agteb(upper / fit->outcome_sum + bounds->slack, fit->coverage)) return FALSE;

    /* Disjunction can add overlap, but cannot remove existing excess.
       Consistency is overlap divided by overlap plus excess. */
    return excess == 0.0 || agteb(upper / (upper + excess) + bounds->slack, fit->consistency);
}
