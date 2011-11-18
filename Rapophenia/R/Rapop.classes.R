#make data.frame class useable in S4 definitions
setOldClass("data.frame")

setClass("apop_settings")

setClass("apop_mle_settings",
	representation(tolerance="numeric",
		starting_pt="numeric",
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
		vbase="integer",
		m1base="integer",
		m2base="integer",
		name="character",
		settings="list"),
	prototype(data=data.frame(),
        ll_function=NULL,
        constraint_function=NULL,
        draw_function=NULL,
        estimate_function=NULL,
        dsize=0L,
        m1base=0L,
        m2base=0L,
		vbase=2L,
		name="An R model",
		settings=list()),
	validity=validate_apop_model
)

