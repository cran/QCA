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

#include "cover_validation.h"

#include <stddef.h>

Rboolean qca_cover_is_feasible(
    const int *chart,
    int nrows,
    int ncols,
    const int *indices,
    int solmin
) {
    if (!chart || nrows <= 0 || ncols <= 0 || !indices ||
        solmin <= 0 || solmin > ncols) {
        return FALSE;
    }

    for (int i = 0; i < solmin; ++i) {
        int col = indices[i];
        if (col < 0 || col >= ncols) return FALSE;
        for (int j = 0; j < i; ++j) {
            if (indices[j] == col) return FALSE;
        }
    }

    for (int row = 0; row < nrows; ++row) {
        Rboolean covered = FALSE;
        for (int i = 0; i < solmin && !covered; ++i) {
            covered = chart[(size_t)indices[i] * (size_t)nrows + (size_t)row] != 0;
        }
        if (!covered) return FALSE;
    }
    return TRUE;
}
