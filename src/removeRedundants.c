# include <R.h>
# include <Rinternals.h>
# include <R_ext/Rdynload.h>

SEXP removeRedundants(SEXP rowno, SEXP noflevels, SEXP mbase) {
    
    int *pfinal, *prowno, *pnoflevels, *pmbase, lmbase, ltemp2, lrowno, lmbasei, i, j, k, ck, rn, finalength, lungime, flag, templung, *ptemp1, *ptemp2;
    SEXP final, temp1, temp2;
    
    SEXP usage = PROTECT(allocVector(VECSXP, 7));
    
    SET_VECTOR_ELT(usage, 0, rowno = coerceVector(rowno, INTSXP));
    SET_VECTOR_ELT(usage, 1, noflevels = coerceVector(noflevels, INTSXP));
    SET_VECTOR_ELT(usage, 2, mbase = coerceVector(mbase, INTSXP));
    
    
    prowno = INTEGER(rowno);
    pnoflevels = INTEGER(noflevels);
    pmbase = INTEGER(mbase);
    lmbase = length(mbase);
    lrowno = length(rowno);
    
    
    for (rn = 0; rn < lrowno; rn++) {
        templung = 1;
        
        SET_VECTOR_ELT(usage, 3, temp1 = allocVector(INTSXP, 1));
        ptemp1 = INTEGER(temp1);
        ptemp1[0] = prowno[rn];
        
        
        flag = 0;
        
        if (prowno[rn] > 0) {
            
            for (i = 0; i < lmbase; i++) {
                lmbasei = lmbase - i - 1;
                if (div(div(prowno[rn] - 1, pmbase[lmbasei]).quot, pnoflevels[lmbasei] + 1).rem == 0) {
                    flag = 1;
                    lungime = templung * (pnoflevels[lmbasei] + 1);
                    
                    SET_VECTOR_ELT(usage, 4, temp2 = allocVector(INTSXP, lungime));
                    ptemp2 = INTEGER(temp2);
                    
                    for (j = 0; j < length(temp1); j++) {
                        ptemp2[j] = ptemp1[j];
                        for (k = 0; k < pnoflevels[lmbasei]; k++) {
                            ptemp2[j + length(temp1)*(k + 1)] = ptemp1[j] + (k + 1)*pmbase[lmbasei];
                        }
                    }
                    
                    if (i < length(mbase)) {
                        SET_VECTOR_ELT(usage, 3, temp1 = allocVector(INTSXP, lungime));
                        ptemp1 = INTEGER(temp1);
                        
                        for (j = 0; j < lungime; j++) {
                            ptemp1[j] = ptemp2[j];
                        }
                        templung = lungime;
                    }
                }
            }
            
        }
        
        if (flag == 1) {
            //Rprintf("exprnec:\n");
            //PrintValue(rowno);
            
            //Rprintf("\n\nsubs:\n");
            //PrintValue(temp2);
            //Rprintf("\n");
            ltemp2 = length(temp2);
            ck = 1;
            for (j = rn; j < lrowno; j++) {
                for (k = ck; k < ltemp2; k++) {
                    if (prowno[j] >= ptemp2[k]) {
                        //Rprintf("j: %d; k: %d; prowno[j]: %d; ptemp2[k]: %d\n\n", j, k, prowno[j], ptemp2[k]);
                        if (prowno[j] == ptemp2[k]) {
                            prowno[j] = 0;
                        }
                    }
                    else {
                        ck = k;
                        break;
                    }
                }
            }
        }
        
        //Rprintf("\n---------------------------------------\n");
    }
    
    
    finalength = 0;
    for (i = 0; i < lrowno; i++) {
        if (prowno[i] > 0) {
            finalength += 1;
        }
    }
     
    SET_VECTOR_ELT(usage, 5, final = allocVector(INTSXP, finalength));
    pfinal = INTEGER(final);
    
    j = 0;
    for (i = 0; i < lrowno; i++) {
        if (prowno[i] > 0) {
            pfinal[j] = prowno[i];
            j += 1;
        }
    }
    
    UNPROTECT(1);
    
    return(final);
    
}


