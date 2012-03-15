#include <apop.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
SEXP R_get_apop_data_matrix(const apop_data *D);
SEXP R_get_apop_data_vector(const apop_data *D);

apop_data *get_factors(SEXP ls, char const *varname){
    int nls = LENGTH(ls);
    if (isNull(ls)) return NULL;
    //else:
    apop_data *out = apop_text_alloc(NULL, nls, 1);
    for (int i = 0; i < nls; i++) 
        apop_text_add(out, i, 0, translateChar(STRING_ELT(ls, i)));
    snprintf(out->names->title, 100, "<categories for %s>", varname);
    apop_data_show(out);
    return out;
}

apop_data *apop_data_from_frame(SEXP in){
    apop_data *out;
    if (TYPEOF(in)==NILSXP) return NULL;

    PROTECT(in);
    assert(TYPEOF(in)==VECSXP); //I should write a check for this on the R side.
    int total_cols=LENGTH(in);
    int total_rows=LENGTH(VECTOR_ELT(in,0));
    int char_cols = 0;
    for (int i=0; i< total_cols; i++){
        SEXP this_col = VECTOR_ELT(in, i);
        char_cols += (TYPEOF(this_col)==STRSXP);
    }
    SEXP rl, cl;
    //const char *rn, *cn;
    //GetMatrixDimnames(in, &rl, &cl, &rn, &cn);
    PROTECT(cl = getAttrib(in, R_NamesSymbol));
    PROTECT(rl = getAttrib(in, R_RowNamesSymbol));

    int current_numeric_col=0, current_text_col=0, found_vector=0;

    if(cl !=R_NilValue && TYPEOF(cl)==STRSXP) //just check for now.
        for (int ndx=0; ndx < LENGTH(cl) && !found_vector; ndx++)
            if (apop_strcmp(translateChar(STRING_ELT(cl, ndx)), "Vector")) found_vector++;

    int matrix_cols= total_cols-char_cols-found_vector;
    out= apop_data_alloc((found_vector?total_rows:0), (matrix_cols?total_rows:0),  matrix_cols);
    if (char_cols) out=apop_text_alloc(out, total_rows, char_cols);

    if(rl !=R_NilValue)
        for (int ndx=0; ndx < LENGTH(rl); ndx++)
            if (TYPEOF(rl)==STRSXP)
                apop_name_add(out->names, translateChar(STRING_ELT(rl, ndx)), 'r');
            else //let us guess that it's a numeric list and hope the R Project one day documents this stuff.
                {char *ss; asprintf(&ss, "%i", ndx); apop_name_add(out->names, ss, 'r'); free(ss);}

    for (int i=0; i< total_cols; i++){
        const char *colname = NULL;
        if(cl !=R_NilValue)
            colname = translateChar(STRING_ELT(cl, i));
        SEXP this_col = VECTOR_ELT(in, i);
        if (TYPEOF(this_col) == STRSXP){
            //could this be via aliases instead of copying?
            //printf("col %i is chars\n", i);
            if(colname) apop_name_add(out->names, colname, 't');
            for (int j=0; j< total_rows; j++)
                apop_text_add(out, j, current_text_col,
				(STRING_ELT(this_col,j)==NA_STRING ? apop_opts.db_nan : translateChar(STRING_ELT(this_col, j))));
            current_text_col++;
            continue;
        } else {    //plain old matrix data.
            int col_in_question = current_numeric_col;
            if (apop_strcmp(colname, "Vector")) {
                out->vector = gsl_vector_alloc(total_rows);
                col_in_question = -1;
            } else {current_numeric_col++;}
            Apop_col(out, col_in_question, onecol);
            if (TYPEOF(this_col) == INTSXP){
                //printf("col %i is ints\n", i);
                int *vals = INTEGER(this_col);
                for (int j=0; j< onecol->size; j++){
					//printf("%i\n",vals[j]);
                    gsl_vector_set(onecol, j, (vals[j]==NA_INTEGER ? GSL_NAN : vals[j]));
					}
            } else {
                double *vals = REAL(this_col);
                for (int j=0; j< onecol->size; j++)
                    gsl_vector_set(onecol, j, (ISNAN(vals[j])||ISNA(vals[j]) ? GSL_NAN : vals[j]));
            }
            if(colname && col_in_question > -1) apop_name_add(out->names, colname, 'c');
            else apop_name_add(out->names, colname, 'v'); //which is "vector".
        }
        //Factors
        SEXP ls = getAttrib(this_col, R_LevelsSymbol);
        if (ls){
            apop_data *end;//find last page for adding factors.
            for(end=out; end->more!=NULL; end=end->more);
            end->more = get_factors(ls, colname);
        }
    }
    UNPROTECT(3);
    return out;
}

SEXP wrapped_apop_data_from_frame(SEXP in){
    apop_data *outdata = apop_data_from_frame(in);
    printf("Now wrapping:\n");
    apop_data_print(outdata);
    return R_MakeExternalPtr(outdata, NULL, NULL);
}


/* copy row/colnames from the apop_data set to the appropriate nest of SEXPs.
   Attach them to the outdata set.  */
void handle_names(apop_data *D, SEXP outdata){
    int msize = D->matrix ? D->matrix->size2 : 0;
	int colct =  msize + !!D->vector + !!D->weights + D->textsize[1];
	int rowct = D->names->rowct;
	SEXP colNames, rowNames;
    int name_position =0;
    PROTECT(rowNames = allocVector(STRSXP,rowct));
    PROTECT(colNames = allocVector(STRSXP,colct));
    //mkChar automagically makes a char * into a CHARSXP
    #define Add_name(namelist, text) \
            {SET_STRING_ELT(namelist, name_position++, mkChar(strdup(text)));}
    #define Printf_name(namelist, ...) {            \
            char *a_spare_string;                   \
            asprintf(&a_spare_string, __VA_ARGS__); \
            Add_name(colNames, a_spare_string);     \
            free(a_spare_string);    \
        }

    //vector, matrix, text, weights names
    if (D->vector) 
        Add_name(colNames, (D->names->vector ? D->names->vector: "Vector"));
    for (int cdx ; cdx < (D->matrix ? D->matrix->size2 : 0); cdx++)
        if (D->names->colct > cdx) Add_name(colNames, D->names->column[cdx])
        else Printf_name(colNames, "V%i", name_position);
    for (int cdx=0 ; cdx < D->textsize[1]; cdx++)
        if (D->names->textct > cdx) Add_name(colNames, D->names->column[cdx])
        else Printf_name(colNames, "V%i", name_position);
    if (D->weights) Add_name(colNames, "weights");
    //Now rownames
    name_position=0;
    for (int rdx = 0; rdx < rowct; rdx++)
        if (D->names->rowct > rdx)
            Add_name(rowNames, D->names->row[rdx])
        else Printf_name(rowNames, "Row_%i", rdx);
    setAttrib(outdata, R_NamesSymbol, colNames);
    setAttrib(outdata, R_RowNamesSymbol, rowNames);
    UNPROTECT(2);
}

apop_data* find_factor(SEXP vector, apop_data *in, int col){
    char n[101], 
         *thisname = 
               (col==-1 && in->names)                ? in->names->vector
             : (in->names && in->names->colct > col) ? in->names->column[col]
                                                     : NULL;
    if (thisname){
        snprintf(n, 100, "<categories for %s>", thisname);
        return apop_data_get_page(in, n);
    }
    return NULL;

}

void set_factor(SEXP vector, apop_data *fp){
    SEXP la;
    PROTECT(la = allocVector(STRSXP, fp->textsize[0]));
    for (int j = 0; j < fp->textsize[0]; j++) {
        SET_STRING_ELT(la, j, mkChar(*fp->text[j]));
    }
    setAttrib(vector, R_LevelsSymbol, la);
    setAttrib(vector, R_ClassSymbol, mkString("factor"));
    UNPROTECT(1);
}

SEXP data_frame_from_apop_data(apop_data *in){
    if (!in) return R_NilValue;
    int numeric_rows = !!(in->vector) 
                        + (in->matrix ? in->matrix->size2 : 0)
                        + !!(in->weights);
    int text_rows = in->textsize[1];
    SEXP out, onerow;
    PROTECT(out = allocVector(VECSXP, numeric_rows + text_rows));
    int col_ct = 0;
    int firstcol = in->vector ? -1 : 0;
    int lastcol = in->matrix ? in->matrix->size2 : 0;
    for (int i= firstcol; i < lastcol; i++){
        int len = (i == -1) ? in->vector->size : in->matrix->size1;
        apop_data *factorpage = find_factor(onerow, in, i);
        if (factorpage){
            SET_VECTOR_ELT(out, col_ct++, (onerow = allocVector(INTSXP, len)));
            for (int j=0; j< len; j++) INTEGER(onerow)[j] = apop_data_get(in, j, i);
            set_factor(onerow, factorpage);
        } else {
            SET_VECTOR_ELT(out, col_ct++, (onerow = allocVector(REALSXP, len)));
            for (int j=0; j< len; j++) REAL(onerow)[j] = apop_data_get(in, j, i);
        }
    }
    for (int i= 0; i < text_rows; i++){
        int len = in->textsize[0];
        SEXP onerow;
        SET_VECTOR_ELT(out, col_ct++, (onerow =  allocVector(STRSXP, len)));
        for (int j=0; j< len; j++) 
            SET_STRING_ELT(onerow, j, mkChar(in->text[j][i])); //Do I need strdup?
    }
    if (in->weights){
        int len =  in->weights->size;
        SET_VECTOR_ELT(out, col_ct++, (onerow = allocVector(REALSXP, len)));
        for (int j=0; j< len; j++) 
            REAL(onerow)[j] = gsl_vector_get(in->weights, j);
    }
    handle_names(in, out);
    UNPROTECT(1);
    return out;
}

SEXP data_frame_from_sexp_wrapped_apop_data(SEXP in){
    if (TYPEOF(in)==NILSXP || 
            (TYPEOF(in)==EXTPTRSXP && !R_ExternalPtrAddr(in))) 
        return R_NilValue;
    apop_data *d  =R_ExternalPtrAddr(in);
    return  data_frame_from_apop_data(d);
}
