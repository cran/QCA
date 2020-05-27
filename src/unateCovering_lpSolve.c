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

#include "lpsolve/lp_lib.h"
#include <assert.h>
#define USE_AS_LIB
#include "unateCovering_lpSolve.h"
int solveUnateCovering(const double* dataInColumnMajor, const int numRows, const int numCols, const int verbose, int* outSol)
{
    for (int i = 0; i < numCols; i++)
        outSol[i] = 0;
    lprec *lp = make_lp(numRows, numCols);
    if (lp == NULL)
    {
        assert("Can't create the lp model");
        return -1;
    }
    {
        double cols[numCols];
        for (int i = 0; i < numCols; i++)
        {
            cols[i] = 1;
            set_binary(lp, i + 1, TRUE);
        }
        if (!set_obj_fn(lp, cols -1 ))
        {
            assert("Can't set the obj func");
            return -1;
        }
    }
    set_minim(lp);
    {
        for (int i = 1; i <= numRows; i++)
        {
            double* addrBeginDataRow = (double*)&dataInColumnMajor[(i-1) * numCols];
            addrBeginDataRow --; 
            set_row(lp, i, addrBeginDataRow);
        }
    }
    {
        double rhs[1 + numRows];
        for (int i = 1; i <= numRows; i++)
        {
            rhs[i] = 1;
        }
        set_rh_vec(lp, rhs);
        for (int i = 1; i <= numRows; i++)
        {
            set_constr_type(lp, i, GE);
        }
    }
    if (verbose)
    {
        print_objective(lp);
        print_solution(lp, 1);
        print_constraints(lp, 1);
        print_lp(lp);
        set_verbose(lp, IMPORTANT); 
    }
    else
    {
        set_verbose(lp, NEUTRAL); 
    }
    const int res = solve(lp);
    if (res == 0)
    {
        presolveundorec *psundo = lp->presolve_undo;
        for(int i = 1; i <= psundo->orig_columns; i++)
        {
            const int j = psundo->orig_rows + i;
            const int value = (int)get_var_primalresult(lp, j);
            outSol[i - 1] = value;
        }
        return 0;
    }
    return -1;
}
#ifndef USE_AS_LIB
#include <stdio.h>
#include <time.h>
void readInput(double** dataInColumnMajor, int* prows, int *pcols)
{
    FILE *f = fopen("/Users/ciprian/QCA/Tests/test.dat", "rb");
    *pcols = 150; 
    *prows = 150; 
    const int rows = *prows;
    const int cols = *pcols;
    int nextIter = 0;
    double * data = (double*)malloc(rows * cols * sizeof(double));
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            double item = -1;
            fread(&item, sizeof(double), 1, f);
            data[nextIter++] = item;
        }
    }
    *dataInColumnMajor = data;
}
int main()
{
    double * out_dataInColumnMajor = NULL;
    int out_rows = 0, out_cols = 0;
    readInput(&out_dataInColumnMajor, &out_rows, &out_cols);
    int *outRes = (int*)malloc(out_rows * sizeof(int));
    clock_t start = clock() ;
    int res = demo2(out_dataInColumnMajor, out_cols, out_rows, outRes);
    clock_t end = clock() ;
    double elapsed_time = (end-start)/(double)CLOCKS_PER_SEC ;
    printf("Elapsed time: %f\n", (float)elapsed_time);
    if (res < 0)
    {
        printf("No solution\n");
    }
    else
    {
        printf("Found solution: cost %d\n", res);
        for (int i = 0; i < out_rows; i++)
        {
            printf("%d ", outRes[i]);
        }
    }
    return 0;
}
#endif
