# include <R.h>
# include <Rinternals.h>
# include <R_ext/Rdynload.h>
SEXP m2(SEXP goodbad, SEXP valents, SEXP mvector, SEXP mbase, SEXP t1g, SEXP t1b, SEXP rnofl) {
    int *pmvector, *pmbase, *prnofl; 
    SEXP t1gc, t1bc, t2g, t2b, temp, final, tempfinal, cr, tobj, tl; 
    int i, j, k, ln, nofconditions, lt1c, lt2, newl, counter, tcounter; 
    int *pt1gc, *pt1bc, *pt2g, *pt2b, *ptemp, *pfinal, *ptempfinal, *pcr, *ptobj, *ptl;
    PROTECT(goodbad);
    PROTECT(valents);
    SEXP usage = PROTECT(allocVector(VECSXP, 15));
    SET_VECTOR_ELT(usage, 0, mvector = coerceVector(mvector, INTSXP));
    SET_VECTOR_ELT(usage, 1, mbase = coerceVector(mbase, INTSXP));
    SET_VECTOR_ELT(usage, 2, t1g = coerceVector(t1g, INTSXP));
    SET_VECTOR_ELT(usage, 3, t1b = coerceVector(t1b, INTSXP));
    SET_VECTOR_ELT(usage, 4, rnofl = coerceVector(rnofl, INTSXP));
    SET_VECTOR_ELT(usage, 5, tl = allocVector(INTSXP, 1)); 
    nofconditions = length(mbase);
    SET_VECTOR_ELT(usage, 6, temp = allocVector(INTSXP, nofconditions));
    prnofl = INTEGER(rnofl);
    ptl = INTEGER(tl);
    ptl[0] = 1;
    for (i = 0; i < nofconditions; i++) {
        ptl[0] = ptl[0]*prnofl[i];
    }
    SET_VECTOR_ELT(usage, 7, tempfinal = allocVector(INTSXP, ptl[0])); 
    SET_VECTOR_ELT(usage, 8, cr = allocVector(INTSXP, nofconditions));
    pmvector = INTEGER(mvector);
    pmbase = INTEGER(mbase);
    ptemp = INTEGER(temp);
    ptempfinal = INTEGER(tempfinal);
    pcr = INTEGER(cr);
    ln = 2;
    counter = 0; 
    while (ln < ptl[0]) {
        for (i = 0; i < nofconditions; i++) {
            ptemp[i] = 0;
        }
        for (i = 0; i < nofconditions; i++) {
            pcr[i] = div(div(ln - 1, pmvector[i]).quot, prnofl[i]).rem;
        }
        SET_VECTOR_ELT(usage, 9, t1gc = t1g);
        pt1gc = INTEGER(t1g);
        SET_VECTOR_ELT(usage, 10, t1bc = t1b);
        pt1bc = INTEGER(t1b);
        for (i = 0; i < nofconditions; i++) {
            if (pcr[i] > 0) {
                SET_VECTOR_ELT(usage, 11, t2g = VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(goodbad, i), pcr[i] - 1), 0));
                pt2g = INTEGER(t2g);
                SET_VECTOR_ELT(usage, 12, t2b = VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(goodbad, i), pcr[i] - 1), 1));
                pt2b = INTEGER(t2b);
                lt1c = length(t1gc);
                lt2 = length(t2g);
                newl = lt1c < lt2 ? lt1c : lt2;
                SET_VECTOR_ELT(usage, 13, tobj = allocVector(INTSXP, newl));
                ptobj = INTEGER(tobj);
                j = 0;
                k = 0;
                tcounter = 0;
                while (j < lt1c && k < lt2) {
                    if (pt1gc[j] < pt2g[k]) {
                        j++;
                    }
                    else if (pt1gc[j] > pt2g[k]) {
                        k++;
                    }
                    else { 
                        ptobj[tcounter] = pt1gc[j];
                        tcounter++;
                        j++;
                        k++;
                    }
                }
                if (tcounter > 0) {
                    SET_VECTOR_ELT(usage, 9, t1gc = allocVector(INTSXP, tcounter));
                    pt1gc = INTEGER(t1gc);
                    for (j = 0; j < tcounter; j++) {
                        pt1gc[j] = ptobj[j];
                    }
                    SET_VECTOR_ELT(usage, 13, tobj = coerceVector(VECTOR_ELT(valents, i), INTSXP));
                    ptobj = INTEGER(tobj);
                    ptemp[i] = ptobj[pcr[i] - 1];
                    lt1c = length(t1bc);
                    lt2 = length(t2b);
                    newl = lt1c < lt2 ? lt1c : lt2;
                    SET_VECTOR_ELT(usage, 13, tobj = allocVector(INTSXP, newl));
                    ptobj = INTEGER(tobj);
                    j = 0;
                    k = 0;
                    tcounter = 0;
                    while (j < lt1c && k < lt2) {
                        if (pt1bc[j] < pt2b[k]) {
                            j++;
                        }
                        else if (pt1bc[j] > pt2b[k]) {
                            k++;
                        }
                        else { 
                            ptobj[tcounter] = pt1bc[j];
                            tcounter++;
                            j++;
                            k++;
                        }
                    }
                    if (tcounter == 0) {
                        for (j = 0; j < nofconditions; j++) {
                            tcounter += ptemp[j]*pmbase[j];
                        }
                        ptempfinal[counter] = tcounter + 1;
                        counter += 1;
                        break;
                    }
                    else {
                        SET_VECTOR_ELT(usage, 10, t1bc = allocVector(INTSXP, tcounter));
                        pt1bc = INTEGER(t1bc);
                        for (j = 0; j < tcounter; j++) {
                            pt1bc[j] = ptobj[j];
                        }
                    }
                }
                else {
                    break;
                }
            }
        }
        ln += pmvector[i == nofconditions ? i - 1 : i];
    }
    SET_VECTOR_ELT(usage, 14, final = allocVector(INTSXP, counter));
    pfinal = INTEGER(final);
    for (i = 0; i < counter; i++) {
        pfinal[i] = ptempfinal[i];
    }
    UNPROTECT(3);
    return(final);
}
