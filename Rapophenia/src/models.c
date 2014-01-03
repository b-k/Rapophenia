#include "rapophenia.h"

Apop_settings_init(R_model, )
Apop_settings_copy(R_model, )
Apop_settings_free(R_model, )

#define Get_slot(m, val) GET_SLOT(m, install(#val))
#define sexp_to_string(s) (translateChar(STRING_ELT(s, 0)))

/* Apophenia has a selling point of not requiring registration of models. You write a
 model on one line, and use it on the next. 
 //C doesn't do eval: we can't go from "apop_ols" to the apop_ols model.
 //I couldn't work out a way to send a list of opaque pointers to R. 

So: we estabish this registry of apop_models. If you write a new C-side model, register it here.
 */

apop_model **apop_model_registry;
int registry_length;

void add_to_registry(apop_model *m){
    apop_model_registry = realloc(apop_model_registry, sizeof(apop_model*) * ++registry_length);
    apop_model_registry[registry_length-1] = m;
}

int scaling=100;
long double ll(apop_data *ignore, apop_model *m){
    double *p = m->parameters->vector->data;
    return -gsl_pow_2(gsl_pow_2(1-p[0]) + scaling*(gsl_pow_2(p[1]-p[0])));
}

long double constr(apop_data *keep_ignoring, apop_model *m){
    gsl_vector *p = m->parameters->vector;
    double penalty = 2-apop_vector_distance(p);
    if (penalty <=0) return 0;
    //else
    gsl_vector_scale(p, (2/(2-penalty)));
    return penalty;
}

static int is_c(apop_model * m){
    return (!Apop_settings_get_group(m, R_model) 
                || Apop_settings_get(m, R_model, is_c_model)=='y');
}

apop_model banana={"banana", .vsize=2, .log_likelihood=ll};
apop_model bananac={"bananac", .vsize=2, .log_likelihood=ll, .constraint=constr};

void init_registry(){
    apop_model_registry = NULL;
    registry_length=0;
    for (apop_model **m=(apop_model*[]){
                apop_beta,
                apop_bernoulli,
                &banana,
                &bananac,
                apop_binomial,
                apop_dirichlet,
                apop_exponential,
                apop_gamma,
                apop_improper_uniform,
                apop_iv,
                apop_kernel_density,
                apop_loess,
                apop_logit,
                apop_lognormal,
                apop_multinomial,
                apop_multivariate_normal,
                apop_normal,
                apop_ols,
                apop_pmf,
                apop_poisson,
                apop_probit,
                apop_t_distribution,
                apop_uniform,
                apop_yule,
                apop_zipf, NULL}; *m; m++)
        add_to_registry(*m);
}

apop_model * get_am_from_registry(char const * findme){
    apop_model *new_model = NULL;
    for (apop_model **m=apop_model_registry; *m; m++){
        if (findme && !strcmp(findme, (*m)->name)){
             new_model= apop_model_copy(*m);
             break;
        }
    }
    Apop_assert(new_model, "Couldn't find a registered model named %s.", findme);
    return new_model;
}


SEXP get_from_registry(SEXP findmexp){
    const char *findme = sexp_to_string(findmexp);
    apop_model *new_model = NULL;
    for (apop_model **m=apop_model_registry; *m; m++){
        if (findme && !strcmp(findme, (*m)->name)){
             new_model= apop_model_copy(*m);
             break;
        }
    }
    Apop_assert(new_model, "Couldn't find a registered model named %s.", findme);
    if (!Apop_settings_get_group(new_model, R_model))
        Apop_settings_add_group(new_model, R_model, .is_c_model = 'y');
    return R_MakeExternalPtr(new_model, NULL, NULL);
}


//Bridge macros


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
        if (!strcmp(sexp_to_string(Get_slot(this, class)), "apop_mle_settings")){
            //SEXP apop_mle = this;
            if (!Apop_settings_get_group(model, apop_mle))
                Apop_settings_add_group(model, apop_mle);
    //        settings_set_realptr(apop_mle, starting_pt)     //to do
            settings_set_logical(this, apop_mle, verbose)
            settings_set_real(this, apop_mle, tolerance)
            settings_set_real(this, apop_mle, max_iterations)
        }
        if (!strcmp(sexp_to_string(Get_slot(this, class)), "apop_parts_wanted_settings")){
            if (!Apop_settings_get_group(model, apop_parts_wanted))
                Apop_settings_add_group(model, apop_parts_wanted);
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
    char *name = strdup(sexp_to_string(Get_slot(m, name)));
    if (name) snprintf(new_model->name, 100, "%s", name);
    new_model->vsize = *INTEGER(Get_slot(m, vsize));
    new_model->dsize = *INTEGER(Get_slot(m, dsize));
    new_model->msize1 = *INTEGER(Get_slot(m, msize1));
    new_model->msize2 = *INTEGER(Get_slot(m, msize2));
    Apop_settings_add_group(new_model, R_model, 
            .est_fn = Get_slot(m, estimate_function), 
            .ll_fn = Get_slot(m, ll_function), 
            .constr_fn = Get_slot(m, constraint_function), 
            .draw_fn = Get_slot(m, draw_function));
    Apop_settings_add_group(new_model, apop_parts_wanted);     //scaffolding.
#define Rnil_to_NULL(sexp, checkme) if (apop_settings_get(new_model, R_model, sexp)==R_NilValue) checkme = NULL;
    Rnil_to_NULL(est_fn, new_model->estimate);
    Rnil_to_NULL(ll_fn, new_model->log_likelihood);
    Rnil_to_NULL(constr_fn, new_model->constraint);
    Rnil_to_NULL(draw_fn, new_model->draw);
    handle_settings(new_model, settings);
    if (!apop_model_registry) init_registry(); //Safety. R_init_rapophenia should have done this.
    add_to_registry(new_model);
    return R_MakeExternalPtr(new_model, NULL, R_BaseEnv);
}



//put the data in the sexp, if needed
//put the parameters in the sexp if needed.
void check_data_and_params(apop_data *d, apop_model *m, SEXP env){
    //if (env == R_NilValue) return;
    if (d){
        if (findVar(install("data"), env)==R_UnboundValue)
            defineVar(install("data"), data_frame_from_apop_data(d), env);
    }
    if (m->parameters)
        ((findVar(install("parameters"), env) !=R_UnboundValue) //overwrite every time.
           ? setVar : defineVar)(install("parameters"), data_frame_from_apop_data(m->parameters), env);
}

void R_estimate(apop_data *d, apop_model *m){
    SEXP env = Apop_settings_get(m, R_model, env);
    check_data_and_params(d, m, env);
    SEXP R_fcall;
    PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, est_fn), env));
    eval(R_fcall, env);
    UNPROTECT(1);
}

long double R_ll(apop_data *d, apop_model *m){
    SEXP env = Apop_settings_get(m, R_model, env); //protected at the Rapophenia_estimate or Rapophenia_ll level
    check_data_and_params(d, m, env);
    SEXP R_fcall, evaluated;
    PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, ll_fn), env));
    //PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, ll_fn), R_NilValue));
    PROTECT(evaluated =eval(R_fcall, env));
    long double outval = REAL(evaluated)[0];
    UNPROTECT(2);
    return outval;
}

long double R_constraint(apop_data *d, apop_model *m){
    SEXP env = Apop_settings_get(m, R_model, env); //protected at the Rapophenia_estimate or Rapophenia_ll level
    check_data_and_params(d, m, env);
    SEXP R_fcall;
    PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, constr_fn), env));
    SEXP evaluated;
    PROTECT(evaluated =eval(R_fcall, env));
    long double outval = REAL(evaluated)[0];
    if (outval){ //the parameters may have changed.
        SEXP psexp = findVar(install("parameters"), env);  //env is potected ==> psexp is.
        if (psexp !=R_UnboundValue){ //replace 
            apop_data_free(m->parameters);
            m->parameters = apop_data_from_frame(psexp);
        }
    }
    UNPROTECT(2);
    return outval;
}

//currently unused:
void R_draw(double *d, gsl_rng *r, apop_model *m){
    eval(Apop_settings_get(m, R_model, draw_fn), Apop_settings_get(m, R_model, env));
}

SEXP Rapophenia_draw(SEXP model){
    static gsl_rng *r =NULL; if (!r) r = apop_rng_alloc(apop_opts.rng_seed);
    PROTECT(model);
    Apop_assert(TYPEOF(model)==EXTPTRSXP, 
            "The input to the draw routine sould be a pointer to an apop_model.");
    apop_model *m =R_ExternalPtrAddr(model);
    if (is_c(m)){
        Apop_assert(m->dsize > 0, "This model doesn't seem set up for random draws yet."
                " I check whether dsize (the size of one draw) > 0, and in this case it isn't.");
        double dd[m->dsize];
        apop_draw(dd, r, m);
        apop_data *out = apop_data_fill_base(apop_data_alloc(m->dsize), dd);
        UNPROTECT(1);
        return data_frame_from_apop_data(out);

        /* do something here. */;
    }else{
        SEXP env, R_fcall, out;
        PROTECT(env = Apop_settings_get(m, R_model, env));
        assert(TYPEOF(env) == ENVSXP);
        assert(TYPEOF(Apop_settings_get(m, R_model, draw_fn)) != NILSXP);
        assert(TYPEOF(Apop_settings_get(m, R_model, draw_fn)) == CLOSXP);
        PROTECT(R_fcall = lang2(Apop_settings_get(m, R_model, draw_fn), env));
        PROTECT(out = eval(R_fcall, env));
        UNPROTECT(4);
        return out;
    }
}

apop_model *Rapophenia_model = &(apop_model){"R model", 1, -1, -1, .dsize=-1, 
    .estimate=R_estimate, .draw = R_draw, .log_likelihood=R_ll,
    .constraint=R_constraint 
   };

SEXP Rapophenia_estimate(SEXP env, SEXP model){
    PROTECT(model);
    PROTECT(env);
    Apop_assert(TYPEOF(model)==EXTPTRSXP, "The second input to the estimate routine sould be a pointer to an apop_model.");
    apop_model *est;
    apop_model *m =R_ExternalPtrAddr(model);
    if (TYPEOF(env)==ENVSXP){
        Apop_settings_add(m, R_model, env, env);
        est = apop_estimate(NULL, m);
    } else if (TYPEOF(env)==EXTPTRSXP)
        est = apop_estimate(R_ExternalPtrAddr(env), m);
    else if (TYPEOF(env)==VECSXP)
        est = apop_estimate(apop_data_from_frame(env), m);
    else if (TYPEOF(env)==NILSXP)
        est = apop_estimate(NULL, m);
    else Apop_assert(0, "I don't know what to do with the first argument. "
                        "It's not NULL, an environment, or a vector|matrix.");
    UNPROTECT(2);
    return R_MakeExternalPtr(est, NULL, NULL);
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
    if (elmt && !strcmp(elmt, "parameters"))
        return data_frame_from_apop_data(m->parameters);
    if (elmt && !strcmp(elmt, "environment")){
        if (is_c(m)) return R_NilValue;
        else return Apop_settings_get(m, R_model, env);
    }
    if (elmt && !strcmp(elmt, "is_c_model")){
        SEXP a_single_character;
        PROTECT(a_single_character = allocVector(STRSXP,1));
        char *cc, 
        c = (!Apop_settings_get_group(m, R_model) ? 'y'
                : Apop_settings_get(m, R_model, is_c_model));
        asprintf(&cc, "%c", c);
        SET_STRING_ELT(a_single_character, 0, mkChar(strdup(cc)));
        free(cc);
        UNPROTECT(1);
        return a_single_character;
    }
}

SEXP update_wrapper(SEXP dataenv, SEXP prior, SEXP likelihood){
    PROTECT(prior); PROTECT(likelihood); PROTECT(dataenv);
    apop_model *pm = R_ExternalPtrAddr(prior);
    apop_model *lm = R_ExternalPtrAddr(likelihood);
    int c_like = is_c(lm);
    apop_data *d = c_like ? R_ExternalPtrAddr(dataenv): NULL;
    apop_model *updated=apop_update(.data=d, pm, lm);
    UNPROTECT(3);
    return R_MakeExternalPtr(updated, NULL, NULL);
}
