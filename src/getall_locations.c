/**
 * An helping function to speed up the computation of expression values
 *
 * Laurent@cbs.dtu.dk (2002)
 */


#include <R.h>
#include <Rdefines.h>

/*****************EXPORT**********************/
SEXP getallLocations(SEXP namesR, SEXP dimR, SEXP atomsR, SEXP ispmR, SEXP nb_affyidR);
/* Takes as input a matrix of integers (the slot 'name'
   of a 'Cdf' object), and return a list of 'locations'.
   Each locations is a (n,2) array (n being the number of
   probes related to an affyid).
/*********************************************/


SEXP getallLocations(SEXP namesR, SEXP dimR, SEXP atomsR, SEXP selectR, SEXP nb_affyidR) {
  int nrows, ncols, nb_affyid;
  int ii, jj;
  int *names, *atoms, *select;
  int *nbElements;
  //int *iLastElement;
  int x;
  
  SEXP loc_list;
  SEXP tmp_dim;
  
  nrows = INTEGER_POINTER(dimR)[0];
  ncols = INTEGER_POINTER(dimR)[1];
  nb_affyid = INTEGER(nb_affyidR)[0];
  names = INTEGER_POINTER(namesR);
  atoms = INTEGER_POINTER(atomsR);
  select = INTEGER_POINTER(selectR);
  nbElements = (int *)R_alloc(nb_affyid, sizeof(int));
  
  PROTECT(loc_list = NEW_LIST(nb_affyid));
  PROTECT(tmp_dim = NEW_INTEGER(2));
  
  for (ii=0; ii<nb_affyid; ii++) {
    nbElements[ii] = 0;
    //iLastElement[ii] = 0;
  }
  
  /* count the number of elemets for each affyid */
  for (ii=0; ii<nrows; ii++) {
    for (jj=0; jj<ncols; jj++) {

      if (select[ii + nrows * jj] != 1) {
	continue;
      }
      
      x = names[ii + nrows * jj];
      
      if (x == NA_INTEGER) {
	x = nb_affyid;
      }
      
      nbElements[x-1]++;
      
    }
  }
  
  /* init the list accordingly */
  for (ii=0; ii<nb_affyid; ii++) {
    SET_VECTOR_ELT(loc_list, ii, NEW_INTEGER(nbElements[ii] * 2));
    INTEGER_POINTER(tmp_dim)[0] = nbElements[ii];
    if (nbElements[ii] == 0) {
      INTEGER_POINTER(tmp_dim)[1] = 0;
    } else {
      INTEGER_POINTER(tmp_dim)[1] = 2;
    }
    SET_DIM(VECTOR_ELT(loc_list, ii), tmp_dim);
    //printf("%i:%i  -- ",ii,nbElements[ii]);
    /* extra-paranoia set the locations to NA */
    for (jj = 0; jj<nbElements[ii]*2; jj++) {
      INTEGER_POINTER(VECTOR_ELT(loc_list, ii))[jj] = NA_INTEGER;
    }
  }
  
  /* fill it */
  for (ii=0; ii<nrows; ii++) {
    for (jj=0; jj<ncols; jj++) {
      
      if (select[ii + nrows * jj] != 1) {
	continue;
      }
      
      x = names[ii + nrows * jj];
      
      if (x == NA_INTEGER) {
	x = nb_affyid;
      }
      /* sanity check */
      /*DEBUG*/
      if (x == 29) {
	//printf("ii=%i, jj=%i, affyid=%i, atom=%i, maxatom=%i\n", ii+1, jj+1, x, atoms[ii + nrows * (jj)], nbElements[x-1]);
      }
      
      INTEGER_POINTER(VECTOR_ELT(loc_list, x-1))[atoms[ii + nrows * jj] + nbElements[x-1] * 0] = ii+1;
      INTEGER_POINTER(VECTOR_ELT(loc_list, x-1))[atoms[ii + nrows * jj] + nbElements[x-1] * 1] = jj+1;
      //iLastElement[x-1]++;
    }
  }
  UNPROTECT(2);
  return loc_list;
}



