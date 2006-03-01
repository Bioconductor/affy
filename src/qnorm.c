/**********************************************************
 **
 ** file: qnorm.c
 **
 ** aim: A c implementation of the quantile normalization method 
 **
 ** Copyright (C) 2002-2005    Ben Bolstad
 **
 ** written by: B. M. Bolstad  <bolstad@stat.berkeley.edu>
 **
 ** written: Feb 2, 2002
 ** last modified: Mar 3, 2005
 ** 
 ** This c code implements the quantile normalization method
 ** for normalizing high density oligonucleotide data as discussed
 ** in
 **
 ** Bolstad, B. M., Irizarry R. A., Astrand, M, and Speed, T. P. (2003)(2003) 
 ** A Comparison of Normalization Methods for High 
 ** Density Oligonucleotide Array Data Based on Bias and Variance.
 ** Bioinformatics 19,2,pp 185-193
 **
 ** History
 ** Feb 2, 2002 - Intial c code version from original R code
 ** Apr 19, 2002 - Update to deal more correctly with ties (equal rank)
 ** Jan 2, 2003 - Documentation/Commenting updates reformating
 ** Feb 17, 2003 - add in a free(datvec) to qnorm(). clean up freeing of dimat
 ** Feb 25, 2003 - try to reduce or eliminate compiler warnings (with gcc -Wall)
 ** Feb 28, 2003 - update reference to normalization paper in comments
 ** Mar 25, 2003 - ability to use median, rather than mean in so called "robust" method
 ** Aug 23, 2003 - add ability to do normalization on log scale in "robust" method.
 **                also have added .Call() interface c functions which may be called
 **                now from R as alterative to traditonal means.
 **                Fixed a bug where use_median was not being dereferenced in "robust method"
 ** Oct 7, 2003 - fix a bug with length is qnorm_robust
 ** Mar 6, 2004 - change malloc/free pairs to Calloc/Free
 ** Mar 3, 2005 - port across the low memory quantile normalization from RMAExpress (and make it the new qnorm_c (previous version made qnorm_c_old)
 **
 ***********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "rma_common.h"



#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <Rinternals.h>
 



/*************************************************************
 **
 ** the dataitem record is used to keep track of data indicies 
 ** along with data value when sorting and unsorting in the 
 ** quantile algorithm.
 **
 ************************************************************/

typedef struct{
  double data;
  int rank;
} dataitem;
  

/***********************************************************
 **  
 ** int min(int x1, int x2)							    
 **
 ** returns the minimum of x1 and x2
 **		    
 **********************************************************/

int min(int x1,int x2){
  if (x1 > x2)
    return x2;
  else
    return x1;
}

/**********************************************************
 **
 ** int sort_fn(const void *a1,const void *a2)
 **
 ** a comparison function for sorting objects of the dataitem type.
 **
 **
 **********************************************************/

int sort_fn(const void *a1,const void *a2){
  dataitem *s1, *s2;
  s1 = (dataitem *)a1;
  s2 = (dataitem *)a2;
  
  if (s1->data < s2->data)
    return (-1);
  if (s1 ->data > s2->data)
    return (1);
  return 0;
}


/************************************************************
 **
 ** dataitem **get_di_matrix(double *data, int rows, int cols)
 **
 ** given data  form a matrix of dataitems, each element of
 ** matrix holds datavalue and original index so that 
 ** normalized data values can be resorted to the original order
 **
 ***********************************************************/

dataitem **get_di_matrix(double *data, int rows, int cols){
  int i,j;
  dataitem **dimat;
  /* dataitem *xtmp; */
  
  dimat = (dataitem **)Calloc((cols),dataitem *);
  
  if (dimat == NULL){
    printf("\nERROR - Sorry the normalization routine could not allocate adequate memory\n       You probably need more memory to work with a dataset this large\n");
  }

  /* xtmp = malloc(cols*rows*sizeof(dataitem));
     for (j=0; j < cols; j++, xtmp +=rows) dimat[j] = xtmp; */
  
  for (j=0; j < cols; j++)
    dimat[j] = Calloc(rows,dataitem);



  for (j =0; j < cols; j++)
    for (i =0; i < rows; i++){
      dimat[j][i].data = data[j*rows + i];
      dimat[j][i].rank = i;
    }

  return(dimat); 
}

/************************************************************
 **
 ** double *get_ranks(dataitem *x,int n)
 **
 ** get ranks in the same manner as R does. Assume that *x is
 ** already sorted
 **
 *************************************************************/

void get_ranks(double *rank, dataitem *x,int n){
  int i,j,k;
   
  i = 0;

  while (i < n) {
    j = i;
    while ((j < n - 1) && (x[j].data  == x[j + 1].data))
      j++;
    if (i != j) {
      for (k = i; k <= j; k++)
	rank[k] = (i + j + 2) / 2.0;
    }
    else
      rank[i] = i + 1;
    i = j + 1;
  }
  /*return rank;*/
}

/*********************************************************
 **
 ** void qnorm_c_old(double *data, int *rows, int *cols)
 **
 **  this is the function that actually implements the 
 ** quantile normalization algorithm. It is called from R
 **
 ** Previous implementation, replaced with lower memory overhead version below
 ** 
 ********************************************************/

void qnorm_c_old(double *data, int *rows, int *cols){
  int i,j,ind;
  dataitem **dimat;
  double sum;
  double *row_mean = (double *)Calloc(*rows,double);
  double *datvec = (double *)Calloc(*cols,double);
  double *ranks = (double *)Calloc(*rows,double);

  /*# sort original columns */
  
  dimat = get_di_matrix(data, *rows, *cols);
  
  for (j=0; j < *cols; j++){
    qsort(dimat[j],*rows,sizeof(dataitem),sort_fn);
  }

  /*# calculate means */
  
  for (i =0; i < *rows; i++){
    sum = 0.0;
    for (j=0; j < *cols; j++)
      datvec[j] = dimat[j][i].data;
    /*qsort(datvec,*cols,sizeof(double),(int(*)(const void*, const void*))sort_double); */
    for (j=0; j < *cols; j++){
      sum +=datvec[j]/(double)*cols;;
    }
    row_mean[i] = sum; /*/(double)*cols;*/
  }
  
  /*# unsort mean columns */
  for (j =0; j < *cols; j++){
    get_ranks(ranks,dimat[j],*rows);
    for (i =0; i < *rows; i++){
      ind = dimat[j][i].rank;
      data[j*(*rows) + ind] = row_mean[(int)floor(ranks[i])-1];
    }
  }
  Free(ranks);
  Free(datvec);   

  for (j=0; j < *cols; j++){
    Free(dimat[j]);
  }

  Free(dimat);
  Free(row_mean); 
}


/*********************************************************
 **
 ** void qnorm_robust_c(double *data,double *weights, int *rows, int *cols, int *use_median)
 **
 ** double *data - datamatrix
 ** double *weights - weights to give each chip when computing normalization chip
 ** int *rows, *cols - matrix dimensions
 ** int *use_median - 0 if using weighted mean, otherwise use median.
 ** int *use_log2  - 0 if natural scale, 1 if we should log2 the data and normalize
 **                  on that scale
 **
 ** this is the function that actually implements the 
 ** quantile normalization algorithm. It is called from R. 
 ** this function allows the user to downweight particular
 ** chips, in the calculation of the mean or to use the median
 ** rather than the mean. If median is used weights are ignored.
 **
 ** note that log scale with mean is equivalent to geometric mean.
 **
 ********************************************************/

void qnorm_robust_c(double *data,double *weights, int *rows, int *cols, int *use_median, int *use_log2){
  int i,j,ind;
  int half,length;
  dataitem **dimat;
  double sum,sumweights;
  double *row_mean = Calloc(*rows,double);
  double *datvec = Calloc(*cols,double);
  double *ranks = Calloc(*rows,double);

  /* Log transform the data if needed */

  if ((*use_log2)){
    for (j =0; j < *cols; j++)
      for (i =0; i < *rows; i++){
	data[j *(*rows) + i] = log(data[j*(*rows) + i])/log(2.0);
      }
  }
  

  dimat = get_di_matrix(data, *rows, *cols);

  
  for (j=0; j < *cols; j++){
    qsort(dimat[j],*rows,sizeof(dataitem),sort_fn);
  }

  for (i =0; i < *rows; i++){
    sum = 0.0;
    for (j=0; j < *cols; j++)
      datvec[j] = dimat[j][i].data;
    /* qsort(datvec,*cols,sizeof(double),(int(*)(const void*, const void*))sort_double); */
    if (!(*use_median)){
      for (j=0; j < (*cols); j++){
	sum +=weights[j]*datvec[j];
      }
      sumweights = 0.0;
      for (j=0; j < (*cols); j++){
	sumweights = sumweights + weights[j];
      }
      row_mean[i] = sum/sumweights;
    } else {
       qsort(datvec,*cols,sizeof(double),(int(*)(const void*, const void*))sort_double);
       half = (*cols + 1)/2;
       length = *cols;
       if (length % 2 == 1){
	 row_mean[i] = datvec[half - 1];
       } else {
	 row_mean[i] = (datvec[half] + datvec[half-1])/2.0;
       }
    }

  }
  
  /*# unsort mean columns */
  for (j =0; j < *cols; j++){
    get_ranks(ranks,dimat[j],*rows);

    for (i =0; i < *rows; i++){
      ind = dimat[j][i].rank;
      data[j*(*rows) + ind] = row_mean[(int)floor(ranks[i])-1];
    }
  }

  /* antilog  return to the natural scale*/

   if ((*use_log2)){
    for (j =0; j < (*cols); j++)
      for (i =0; i < (*rows); i++){
	data[j *(*rows) + i] = pow(2.0,data[j*(*rows) + i]);
      }
   }

  Free(datvec);
  Free(ranks); 

  for (j=0; j < *cols; j++){
    Free(dimat[j]);
  }

  Free(dimat);
  Free(row_mean); 
}




/*********************************************************
 **
 ** void qnorm_c(double *data, int *rows, int *cols)
 **
 **  this is the function that actually implements the
 ** quantile normalization algorithm. It is called from R.
 **
 ** returns 1 if there is a problem, 0 otherwise
 **
 ********************************************************/

int qnorm_c(double *data, int *rows, int *cols){
  int i,j,ind;
  dataitem **dimat;
  /*  double sum; */
  double *row_mean = (double *)Calloc((*rows),double);
  double *datvec; /* = (double *)Calloc(*cols,double); */
  double *ranks = (double *)Calloc((*rows),double);
  
  datvec = (double *)Calloc(*rows,double);
  
  for (i =0; i < *rows; i++){
    row_mean[i] = 0.0;
  }
  
  /* first find the normalizing distribution */
  for (j = 0; j < *cols; j++){
    for (i =0; i < *rows; i++){
      datvec[i] = data[j*(*rows) + i];
    }
    qsort(datvec,*rows,sizeof(double),(int(*)(const void*, const void*))sort_double);
    for (i =0; i < *rows; i++){
      row_mean[i] += datvec[i]/((double)*cols);
    }
  }
  
  /* now assign back distribution */
  dimat = (dataitem **)Calloc(1,dataitem *);
  dimat[0] = (dataitem *)Calloc(*rows,dataitem);
  
  for (j = 0; j < *cols; j++){
    for (i =0; i < *rows; i++){
      dimat[0][i].data = data[j*(*rows) + i];
      dimat[0][i].rank = i;
    }
    qsort(dimat[0],*rows,sizeof(dataitem),sort_fn);
    get_ranks(ranks,dimat[0],*rows);
    for (i =0; i < *rows; i++){
      ind = dimat[0][i].rank;
      data[j*(*rows) +ind] = row_mean[(int)floor(ranks[i])-1];
      }
  }
  
  Free(ranks);
  Free(datvec);
  Free(dimat[0]);
  
  Free(dimat);
  Free(row_mean);
  return 0;
}












/*********************************************************
 **
 ** SEXP R_qnorm_c(SEXP X)
 **
 ** SEXP X      - a matrix
 ** SEXP copy   - a flag if TRUE then make copy
 **               before normalizing, if FALSE work in place
 **               note that this can be dangerous since
 **               it will change the original matrix.
 **
 ** returns a quantile normalized matrix.
 **
 ** This is a .Call() interface for quantile normalization
 **
 *********************************************************/

SEXP R_qnorm_c(SEXP X, SEXP copy){

  SEXP Xcopy,dim1;
  double *Xptr;
  int rows,cols;
  
  PROTECT(dim1 = getAttrib(X,R_DimSymbol));
  rows = INTEGER(dim1)[0];
  cols = INTEGER(dim1)[1];
  if (asInteger(copy)){
    PROTECT(Xcopy = allocMatrix(REALSXP,rows,cols));
    copyMatrix(Xcopy,X,0);
  } else {
    Xcopy = X;
  }
  Xptr = NUMERIC_POINTER(AS_NUMERIC(Xcopy));
  
  qnorm_c(Xptr, &rows, &cols);
  if (asInteger(copy)){
    UNPROTECT(2);
  } else {
    UNPROTECT(1);
  }
  return Xcopy;
}
