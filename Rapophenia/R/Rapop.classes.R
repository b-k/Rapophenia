#make data.frame class useable in S4 definitions
setOldClass("data.frame")

setClass("apop_settings")

setClass("apop_mle_settings",
	representation(tolerance="numeric",
		starting_pt="numeric",
		method="numeric",
		max_iterations="numeric",
		verbose="logical"),
	prototype(tolerance=1e-5, starting_pt=0, verbose=FALSE),
	contains="apop_settings"
)

setClass("apop_parts_wanted",
	representation(parts="character"),
	prototype(parts="none"),
	contains="apop_settings"
)


validate_apop_model <- function(object){
	#is there something in the settings
	if(length(object@settings)==0) return(TRUE)
	#is everything an apop_settings instance?
	return(all(unlist(lapply(object@settings,is,"apop_settings"))))
}

setClassUnion("a_function",c("NULL","function"))


setClass("apop_model",
	representation(ll_function="a_function",
        estimate_function="a_function",
        constraint_function="a_function",
        draw_function="a_function",
        parameters="numeric",
		data="data.frame",
		dsize="integer",
		vsize="integer",
		msize1="integer",
		msize2="integer",
		name="character",
		settings="list"),
	prototype(data=data.frame(),
        ll_function=NULL,
        constraint_function=NULL,
        draw_function=NULL,
        estimate_function=NULL,
        dsize=0L,
		vsize=2L,
        msize1=0L,
        msize2=0L,
		name="An R model",
		settings=list()),
	validity=validate_apop_model
)

