#include <apop.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/*When calling from another library, use these types. E.g.

data_frame_from_apop_data_type rapop_df_from_ad;
rapop_df_from_ad =  R_GetCCallable("Rapophenia", "data_frame_from_apop_data");

 */
typedef SEXP R_get_apop_data_matrix_type(apop_data const *D);
typedef apop_data *apop_data_from_frame_type(SEXP in);
typedef SEXP wrapped_apop_data_from_frame_type(SEXP in);
typedef SEXP data_frame_from_apop_data_type(apop_data *in);
typedef apop_model * get_am_from_registry_type(char const * findme);

R_get_apop_data_matrix_type R_get_apop_data_matrix;
apop_data_from_frame_type apop_data_from_frame;
wrapped_apop_data_from_frame_type wrapped_apop_data_from_frame;
data_frame_from_apop_data_type data_frame_from_apop_data;
get_am_from_registry_type get_am_from_registry;

typedef struct {
   SEXP est_fn, draw_fn, ll_fn, constr_fn, env;
   char is_c_model;
} R_model_settings;

apop_model Rapophenia_model;

Apop_settings_declarations(R_model);

void init_registry(); //for internal use. R_init_Rapophenia() calls this for you.
