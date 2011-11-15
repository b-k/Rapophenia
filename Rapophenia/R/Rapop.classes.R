#make data.frame class useable in S4 definitions
setOldClass("data.frame")

setClass("apop_settings")

setClass("apop_mle_settings",
	representation(tolerance="numeric",
		starting_pt="numeric",
		verbose="logical"),
	prototype(tolerance=1e-5,starting_pt=0,verbose=FALSE),
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

setClass("apop_model",
	representation(ll_function="function",
		data="data.frame",
		vbase="integer",
		name="character",
		settings="list"),
	prototype(ll_function=function(x) return(x),
		data=data.frame(),
		vbase=2L,
		name="name",
		settings=list()),
	validity=validate_apop_model
)

