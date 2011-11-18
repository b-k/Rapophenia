#include <apop.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
SEXP R_get_apop_data_matrix(const apop_data *D);
SEXP R_get_apop_data_vector(const apop_data *D);
apop_data * apop_data_from_R_vector(const SEXP in);

typedef struct {
   SEXP est_fn, draw_fn, ll_fn, constr_fn, env;
} R_model_settings;

Apop_settings_init(R_model, )
Apop_settings_copy(R_model, )
Apop_settings_free(R_model, )

apop_model Rapophenia_model;

#define Get_slot(m, val) GET_SLOT(m, install(#val))
#define sexp_to_string(s) (translateChar(STRING_ELT(s, 0)))

//The list version
#define get_sexp(m, elmt) SEXP elmt ## sexp;    \
    SEXP elmt;                                                  \
    PROTECT(elmt ## sexp = findVar(install(#elmt), m));        \
    elmt = (elmt ## sexp ==R_UnboundValue) ? NULL : elmt ## sexp;   \
    UNPROTECT(1);

#if 0
#define get_number(m, type, elmt) SEXP elmt ## sexp;        \
    type elmt;                                              \
    PROTECT(elmt ## sexp =  findVar(install(#elmt), m));    \
    elmt = (elmt ## sexp ==R_UnboundValue) ? NULL : REAL(elmt ## sexp);   \
    UNPROTECT(1);

#define get_string(m, elmt) SEXP elmt ## sexp;  \
    char* elmt; \
    PROTECT(elmt ## sexp =  findVar(install(#elmt), m));   \
    elmt = (elmt ## sexp ==R_UnboundValue) ? NULL : strdup(translateChar(STRING_ELT(elmt ## sexp, 0)));   \
    UNPROTECT(1);

#define settings_set_char(intype, s_name) /*intype is both the list and the settings group name.*/\
    get_string(intype, s_name); /*get a local variable with the given name. */\
    if (s_name) {    \
        intype ## _settings *apop_tmp_settings = apop_settings_get_grp(model, #intype, 'c');  \
        apop_tmp_settings->s_name = s_name[0];    \
    }

#define settings_set_realptr(intype, s_name) {/*intype is both the list and the settings group name.*/\
    get_number(intype, double *, s_name); /*get a local variable with the given name. */\
    if (s_name) {    \
        intype ## _settings *apop_tmp_settings = apop_settings_get_grp(model, #intype, 'c');  \
        apop_tmp_settings->s_name = s_name;    \
    } }
#endif

/* Had to rewrite the settings group setting macros.
 */


#define settings_set_real(sexp, intype, s_name) /*intype is both the list and the settings group name.*/\
    if (R_has_slot(sexp, install(#s_name))){    \
        double *s_name = REAL(GET_SLOT(sexp, install(#s_name)));   \
        if (s_name) {    \
            intype ## _settings *apop_tmp_settings = apop_settings_get_grp(model, #intype, 'c');  \
            apop_tmp_settings->s_name = *s_name;    \
        }   \
    } 

#define settings_logical_to_yesno(sexp, intype, s_name) /*intype is both the list and the settings group name.*/\
    if (R_has_slot(sexp, install(#s_name))){    \
        int *s_name = LOGICAL(GET_SLOT(sexp, install(#s_name)));   \
        if (s_name) {    \
            intype ## _settings *apop_tmp_settings = apop_settings_get_grp(model, #intype, 'c');  \
            apop_tmp_settings->s_name = (*s_name ?  'y' : 'n');    \
        }   \
    }

#define settings_set_logical(sexp, intype, s_name) /*intype is both the list and the settings group name.*/\
    if (R_has_slot(sexp, install(#s_name))){    \
        int *s_name = LOGICAL(GET_SLOT(sexp, install(#s_name)));   \
        if (s_name) {    \
            intype ## _settings *apop_tmp_settings = apop_settings_get_grp(model, #intype, 'c');  \
            apop_tmp_settings->s_name = *s_name;    \
        }   \
    }

void handle_settings(apop_model *model, SEXP list){
    if (!list) return;
    int len=LENGTH(list);
    if (!len) return;
    
    for (int i=0; i< len; i++){
        SEXP this = VECTOR_ELT(list, i); 
        if (apop_strcmp(sexp_to_string(Get_slot(this, class)), "apop_mle_settings")){
            SEXP apop_mle = this;
            if (!Apop_settings_get_group(model, apop_mle))
                Apop_model_add_group(model, apop_mle);
    //        settings_set_realptr(apop_mle, starting_pt)     //to do
            settings_set_logical(this, apop_mle, verbose)
            settings_set_real(this, apop_mle, tolerance)
            settings_set_real(this, apop_mle, max_iterations)
        }
        if (apop_strcmp(sexp_to_string(Get_slot(this, class)), "apop_parts_wanted_settings")){
            if (!Apop_settings_get_group(model, apop_parts_wanted))
                Apop_model_add_group(model, apop_parts_wanted);
            settings_logical_to_yesno(this, apop_parts_wanted, covariance);
            settings_logical_to_yesno(this, apop_parts_wanted, predicted);
            settings_logical_to_yesno(this, apop_parts_wanted, tests);
            settings_logical_to_yesno(this, apop_parts_wanted, info);
        }
    }
}


SEXP setup_R_model(SEXP m){
    apop_model *new_model = apop_model_copy(Rapophenia_model);
    SEXP settings = Get_slot(m, settings);
    //get_sexp(m, settings);
    char *name = strdup(translateChar(STRING_ELT(Get_slot(m, name),0)));
    if (name) snprintf(new_model->name, 100, "%s", name);
    new_model->vbase = *INTEGER(Get_slot(m, vbase));
    new_model->dsize = *INTEGER(Get_slot(m, dsize));
    new_model->m1base = *INTEGER(Get_slot(m, m1base));
    new_model->m2base = *INTEGER(Get_slot(m, m2base));
    Apop_model_add_group(new_model, R_model, 
            .est_fn = Get_slot(m, estimate_function), 
            .ll_fn = Get_slot(m, ll_function), 
            .constr_fn = Get_slot(m, constraint_function), 
            .draw_fn = Get_slot(m, draw_function));
    Apop_model_add_group(new_model, apop_parts_wanted);     //scaffolding.
#define Rnil_to_NULL(sexp, checkme) if (apop_settings_get(new_model, R_model, sexp)==R_NilValue) checkme = NULL;
    Rnil_to_NULL(est_fn, new_model->estimate);
    Rnil_to_NULL(ll_fn, new_model->log_likelihood);
    Rnil_to_NULL(constr_fn, new_model->constraint);
    Rnil_to_NULL(draw_fn, new_model->draw);
    handle_settings(new_model, settings);
    return R_MakeExternalPtr(new_model, NULL, NULL);
}

//put the data in the sexp, if needed
//put the parameters in the sexp if needed.
void check_data_and_params(apop_data *d, apop_model *m, SEXP env){
    //if (env == R_NilValue) return;
    if (d){
        if (findVar(install("data"), env)==R_UnboundValue)
            defineVar(install("data"), R_get_apop_data_matrix(d), env);
    }
    if (m->parameters)
        ((findVar(install("parameters"), env) !=R_UnboundValue) //overwrite every time.
           ? setVar : defineVar)(install("parameters"), R_get_apop_data_vector(m->parameters), env);
/*
        if (findVar(install("parameters"), env) ==R_UnboundValue) //overwrite every time.
            defineVar(install("parameters"), R_get_apop_data_vector(d), env);*/
}

/* //not sure what this means right now
apop_model *R_estimate(apop_data *d, apop_model *m){
    SEXP env = Apop_settings_get(m, R_model, env);
    SEXP f = Apop_settings_get(m, R_model, est_fn);
    check_data_and_params(d, m, env);
    eval(f, env);
}*/

double R_ll(apop_data *d, apop_model *m){
    static int i=0;
    SEXP env = Apop_settings_get(m, R_model, env); //protected at the Rapophenia_estimate or Rapophenia_ll level
    check_data_and_params(d, m, env);
    SEXP R_fcall; //, val;
    PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, ll_fn), env));
    SEXP evaluated;
    PROTECT(evaluated =eval(R_fcall, NULL));
    double outval = REAL(evaluated)[0];
    UNPROTECT(2);
    return outval;
}

double R_constraint(apop_data *d, apop_model *m){
    static int i=0;
    SEXP env = Apop_settings_get(m, R_model, env); //protected at the Rapophenia_estimate or Rapophenia_ll level
    check_data_and_params(d, m, env);
    SEXP R_fcall;
    PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, constr_fn), env));
    SEXP evaluated;
    PROTECT(evaluated =eval(R_fcall, NULL));
    double outval = REAL(evaluated)[0];
    if (outval){ //the parameters may have changed.
        SEXP psexp;
        PROTECT(psexp = findVar(install("parameters"), env)); 
        double *newparams = (psexp ==R_UnboundValue) ? NULL : REAL(psexp); 
        if (newparams) apop_data_fill_base(m->parameters, newparams);
        UNPROTECT(1);
    }
    UNPROTECT(2);
    return outval;
}

void R_draw(double *d, gsl_rng *r, apop_model *m){
    eval(Apop_settings_get(m, R_model, draw_fn), Apop_settings_get(m, R_model, env));
}

apop_model Rapophenia_model  = {"R model", 1, -1, -1, .dsize=-1, 
    /*.estimate=R_estimate,*/ .draw = R_draw, .log_likelihood=R_ll,
    .constraint=R_constraint 
   };

SEXP Rapophenia_estimate(SEXP model, SEXP env){
    PROTECT(model);
    PROTECT(env);
    apop_model *m =R_ExternalPtrAddr(model);
    Apop_settings_add(m, R_model, env, env);
    apop_model *est = apop_estimate(NULL, *m);
    UNPROTECT(2);
    return R_MakeExternalPtr(est, NULL, NULL);
}

char const *get_text(SEXP env, char *name){
    SEXP namesexp;
    PROTECT(namesexp =  findVar(install(name), env));
    if (namesexp == R_UnboundValue)
         return NULL;
    else return translateChar(STRING_ELT(namesexp, 0));
}

SEXP Rapophenia_ll(SEXP model, SEXP env){
    PROTECT(env);
    PROTECT(model);
    apop_model *m =R_ExternalPtrAddr(model);
    Apop_settings_add(m, R_model, env, env);
    SEXP out= ScalarReal(apop_log_likelihood(NULL, m));
    UNPROTECT(2);
    return out;
}

SEXP get_model_element(SEXP model, SEXP elmtin){
    apop_model *m =R_ExternalPtrAddr(model);
    char const *elmt = translateChar(STRING_ELT(elmtin, 0));
    if (apop_strcmp(elmt, "parameters"))
        return R_get_apop_data_vector(m->parameters);
}
