/** \file 
  Conversion from apop_data to R matrix/data frame
*/
#include "rapophenia.h"

/** \mainpage Intro

Apophenia is a library of C functions intended to facilitate statistical and scientific
computing in C, providing data structures and functions so that you don't need to work
with raw <tt>double **</tt> arrays or reinvent basic methods.

This package provides a bridge between Apophenia and R. Notably, you can write R data frames to \c apop_data sets, and vice versa.

Apophenia's SQL interface is often faster than R's, so we expose that as well.

 */

/** \page samples Sample code

Place sample code here.

 */

/** \page dbs Databases

   Apophenia maintains a single database handle.  You can open it with the name of a database (or for SQLite, a file), or NULL, indicating that the database should be in memory. If you do not explicitly open a database, then on the first operation, a database will be opened in memory.

   If you need two databases to be open at once, you can use SQL's \c attach to open the second.

   R's method to read from a text file to the database requires first reading the file into a data frame in memory, then writing the data frame to the database. For a large database, this is intractable.  Apophenia's \c apop_text_to_db reads each line of text directly into the database, and so may be preferable for large data sets.

 */


/*
Convert an R character vector into a C **char
*/
char **R_STRSXP_TO_C(SEXP s){
	assert(TYPEOF(s) == STRSXP);
	int n = LENGTH(s);
    char **cptr = (char**)R_alloc(n, sizeof(char*));
	for (int i = 0 ; i < n ; i++) 
        asprintf(cptr+i, "%s", translateChar(STRING_ELT(s, i)));
    return cptr;
}

/*Converts a gsl_matrix into an R matrix.
	Returns the R matrix.
*/
SEXP R_vector_from_gsl_vector(const gsl_vector *X){
	SEXP Y;
	int nrow = X->size;
	PROTECT(Y = allocVector(REALSXP,nrow));
	for (int rdx = 0; rdx < nrow; rdx++)
		REAL(Y)[rdx] = gsl_vector_get(X, rdx);
	UNPROTECT(1);
	return(Y);
}

/*Converts a gsl_matrix into an R matrix.
	Returns the R matrix.
*/
SEXP R_matrix_from_gsl_matrix(const gsl_matrix *X){
	SEXP Y;
	int rdx;
	int cdx;
	int nrow = X->size1;
	int ncol = X->size2;

	PROTECT(Y = allocMatrix(REALSXP,nrow,ncol));
	//Column major indices in R, evidently, so need to get gsl column
	//Still doesn't work cause C still gives addresses row major
	//will just fill in REAL(Y) with the corresponding element
	for (rdx = 0; rdx < nrow; rdx++){
		for (cdx = 0; cdx < ncol; cdx++){
			REAL(Y)[cdx*nrow + rdx] = gsl_matrix_get(X,rdx,cdx);
		}
	}
	UNPROTECT(1);
	return(Y);
}

/** Take in an R matrix, save it as an \c apop_data set.

 */
apop_data * apop_data_from_R_matrix(const SEXP in){ //by BK
    if (!in) return NULL;
	int rowct = nrows(in);
	int colct = ncols(in);
    apop_data *out = apop_data_alloc(rowct, colct);

	PROTECT(in);
    //as with R_matrix_from_gsl_matrix, we're switching
    //from Fortran/R's column-major to C's row-major.
	for (int rdx = 0; rdx < rowct; rdx++){
		for (int cdx = 0; cdx < colct; cdx++){
			apop_data_set(out,rdx,cdx, REAL(in)[cdx*rowct + rdx]);
		}
	}

    SEXP rl, cl;
    const char *rn, *cn;
    GetMatrixDimnames(in, &rl, &cl, &rn, &cn);
    if(rl !=R_NilValue)
        for (int ndx=0; ndx < LENGTH(rl); ndx++)
            apop_name_add(out->names, translateChar(STRING_ELT(rl, ndx)), 'r');
            //printf("row %i: %s\n", ndx, (char *) translateChar(STRING_ELT(rl, ndx)));
    if(cl !=R_NilValue)
        for (int ndx=0; ndx < LENGTH(cl); ndx++)
            apop_name_add(out->names, translateChar(STRING_ELT(cl, ndx)), 'c');
            //printf("col %i: %s\n", ndx, (char *) translateChar(STRING_ELT(cl, ndx)));

	UNPROTECT(1);
	return out;
}

SEXP gsl_matrix_test(SEXP nrow, SEXP ncol){
	SEXP Y;

	gsl_matrix *X = gsl_matrix_alloc(INTEGER(nrow)[0],INTEGER(ncol)[0]);
	gsl_matrix_set_all(X,3.14);

	PROTECT(Y = R_matrix_from_gsl_matrix(X));
	UNPROTECT(1);
	return(Y);
}

/* Deprecated because I never figured out how to apply names to a vector.

 Pulls the vector element, row and column names from an \c apop_data set.

  \li If there are fewer row names than the rows of the vector, then new names will be inserted.

  \param D An \c apop_data set. I ignore everything but the vector and names.
  \return An R \c SEXP, representing an \f$Nx1\f$ <b>matrix</b> with row and column names. The R side can do \c as.vector if desired.
*/
/*SEXP R_get_apop_data_vector(const apop_data *D){
    if (!D || !D->vector || !D->vector->size) return R_NilValue;
	SEXP Y, rowNames, colNames,dimNames;
	int rowct = D->vector->size;
	PROTECT(Y = R_vector_from_gsl_vector(D->vector));
    PROTECT(rowNames = allocVector(STRSXP,rowct));
    PROTECT(colNames = allocVector(STRSXP,1));
    PROTECT(dimNames = allocVector(VECSXP, 2));
	for (int rdx = 0; rdx < rowct; rdx++){
        if (D->names->rowct > rdx)
            SET_STRING_ELT(rowNames, rdx, mkChar(D->names->row[rdx]));
        else {
            char *rowno;
            asprintf(&rowno, "Row_%i", rdx);
            SET_STRING_ELT(rowNames, rdx, mkChar(rowno));
            free(rowno);
        }
	}
    SET_STRING_ELT(colNames, 0, mkChar(D->names->vector ? D->names->vector: "Vector"));
    SET_VECTOR_ELT(dimNames,0,rowNames);
    SET_VECTOR_ELT(dimNames,1,colNames);
    setAttrib(Y, R_DimNamesSymbol, dimNames);
    UNPROTECT(4);
	return Y;
}*/


/** Pulls the matrix element, row and column names from an \c apop_data set.

  \li If there are fewer column or row names than the dimensions of the matrix, then new names will be inserted.

  \param D An \c apop_data set. I ignore everything but the matrix and names.
  \return An R \c SEXP, representing a matrix with row and column names.
*/
/*
SEXP R_get_apop_data_matrix(const apop_data *D){
    if (!D || !D->matrix || !D->matrix->size2) return R_NilValue;

	SEXP Y;
    PROTECT(Y = R_matrix_from_gsl_matrix(D->matrix));
    //no more names.
    UNPROTECT(1);
    return Y;
}*/

void R_apop_query(char** query){
	apop_query("%s", query[0]);
}

void R_apop_db_open(char** path){
	apop_db_open(path[0]);
}

//Registration Kruft
void R_init_Rapophenia(DllInfo *info){
       /* Register routines, allocate resources. */
     /*R_CallMethodDef callMethods[]  = {
       {"R_get_apop_data_matrix", (DL_FUNC) &R_get_apop_data_matrix, 1},
	   {"R_STRSXP_TO_C", (DL_FUNC) &R_STRSXP_TO_C, 1},
       {NULL, NULL, 0}
     };*/
     init_registry();
	 //R_RegisterCCallable("Rapophenia", "R_get_apop_data_matrix", (DL_FUNC) &R_get_apop_data_matrix);
	 //R_RegisterCCallable("Rapophenia", "R_get_apop_data_vector", (DL_FUNC) &R_get_apop_data_vector);
	 R_RegisterCCallable("Rapophenia", "R_STRSXP_TO_C", (DL_FUNC) &R_STRSXP_TO_C);
    R_RegisterCCallable("Rapophenia", "data_frame_from_apop_data",(DL_FUNC) &data_frame_from_apop_data);
    R_RegisterCCallable("Rapophenia", "apop_data_from_frame",(DL_FUNC) &apop_data_from_frame);
    R_RegisterCCallable("Rapophenia", "get_am_from_registry",(DL_FUNC) &get_am_from_registry);
 }

//This is what the R tests call. Not really intended for use by users.
//Needs a rewrite!
void test_Rapophenia(){ //by BK
    for (int i=0; i< 1000; i++){ //see if we can get R's garbage collector active.
        int dim1=50, dim2=12;
        apop_data *m = apop_data_alloc(dim1, dim2);
        gsl_rng *r = apop_rng_alloc(1212121);
        for (int i=0; i< dim1; i++)
            for (int j=0; j< dim2; j++)
                apop_data_set(m, i, j, gsl_rng_uniform(r));
        apop_data_free(m);

        m = apop_data_alloc(3, 3);
        for (int i=0; i< 3; i++)
            for (int j=0; j< 3; j++)
                apop_data_set(m, i, j, gsl_rng_uniform(r));
        apop_data_add_names(m, 'r', "one", "two", "three");
        apop_data_add_names(m, 'c', "four", "five", "six");
        apop_data_free(m);

        m = apop_data_alloc(3);
        for (int i=0; i< 3; i++)
            apop_data_set(m, i, -1, gsl_rng_uniform(r));
        apop_data_add_names(m, 'r', "one", "two", "three");
        apop_data_free(m);
    }
}


/*

library(Rapophenia)
a <- matrix(c(1,0,0,1,2,0,0,2,3,0,0,3),ncol=4,byrow=TRUE)
b <- Rapop_wishart(a)

*/
/*let's try to wrap the apop_wishart model, just to get a draw*/
/*
SEXP R_draw_wishart(SEXP Rmatrix){
	apop_data *E = apop_data_from_R_matrix(Rmatrix);
	apop_model *M = apop_estimate(E, apop_wishart);
	apop_model_show(M);
	gsl_rng *r = apop_rng_alloc(12345);
	apop_draw(E->matrix->data, r, M);
	//matrix is stored as first row of E matrix
	Apop_row_v(E->matrix, 0, onerow);
	apop_data *D = apop_data_alloc((onerow->size)/2,(onerow->size)/2);
	apop_data_unpack(onerow, D);
	SEXP Return;
	PROTECT(Return = R_matrix_from_gsl_matrix(D->matrix));
	UNPROTECT(1);
	return(Return);
}
*/

SEXP kldiv(SEXP dftop, SEXP dfbottom){
	apop_data *top = apop_data_from_frame(dftop);
	apop_data *bottom = apop_data_from_frame(dfbottom);
	apop_model *mtop = apop_estimate(top,apop_pmf);
	apop_model *mbottom = apop_estimate(bottom,apop_pmf);
	double div = apop_kl_divergence(mtop,mbottom);
	printf("%g\n",div);
	return(R_NilValue);
}
