
#' Given a list of numeric, positive semi-definite matrices
#' assumed to be from a Wishart distribution,
#' return a new draw from the MLE-estimated Wishart.
#' @param Lm a list of matrices.  Each element must be numeric and positive semi-definite
#' @returns a matrix drawn from an estimated Wishart distribution
Rapop_wishart <- function(Lm){
    if(length(Lm)<2) stop("You need to give me at least 2 numeric matrices")
    if(!all(lapply(Lm,is.matrix))) stop("Every list element must be a numeric matrix")
    if(!all(lapply(Lm,is.numeric))) stop("Every list element must be a numeric matrix")
    #flatten matrices to send to R_draw_wishart
    Matrix <- do.call(rbind,lapply(Lm,as.numeric))
    return(.Call("R_draw_wishart", Matrix))
}

#' Write a text file to an SQLite database.
#' Wrapper function for apop_text_to_db().
#' Supports comma and pipe as delimiters,
#' otherwise column end points must be given.
#' @param db database to use
#' @param file file to read
#' @param tbl name to give to the table
#' @param row.names logical indicating presence of row names in file
#' @param col.names logical indicating presence of column names in file
#' @param field.names names to use for the columns
#' @param field.ends integer vector giving end columns for each variable
#' in the case of a non-delimited file
TextToDB <- function(file, db, tbl, row.names=FALSE, col.names=TRUE, field.names=NULL,
	field.ends=NULL){
	
	file <- as.character(file)
	db <- as.character(db)
	tbl <- as.character(tbl)
	row.names <- as.integer(row.names)
	col.names <- as.integer(col.names)
	field.names <- as.character(field.names)
	field.ends <- as.integer(field.ends)

	return(!inherits(try(.C("WriteTable",file,db,tbl,row.names,col.names,field.names,field.ends)),"try-error"))
}

getModelElement <- function(modelname, elementname){
    out<- .Call("get_model_element", modelname$model, elementname)
    return(out)
    
}

setupRapopModel <- function(input){ 
    M <-.Call("setup_R_model", input)
    return (list(model=M))
}

#' Take in an un-estimated model, and estimate parameters.
#' @param mod A model that you set up using \c setupRapopModel
#' @param data An environment, including anything needed by your models. The system will add a \c
#' parameters element as necessary.
#' @return a Rapophenia model. Interrogate it using \c getModelElement
RapopModelEstimate <- function(data, mod){
    #stopifnot(is.environment(data) || is.externalpointer(data)) #except there is no is.extpointer function that I could find.
    ctype <-getModelElement(mod, "is_c_model")
    if (ctype=="y"){
        M <-.Call("Rapophenia_estimate", data, mod$model)
    } else {
        if (!is.null(data)) env <- as.environment(data)
        else                env <- new.env()
        M <-.Call("Rapophenia_estimate", env, mod$model)
    }
    out <- list(model=M)
    out$env <-getModelElement(out, "environment")
    return (out)
}

#' Make one random draw from an estimated model.
#'
#' @param modelList A model that you set up using \c setupRapopModel and estimated using
#'        \ref RapopModelEstimate
#' @return a single draw from the model. If this is a C-side model, it will be a single vector; R-side models can return whatever form you like.
RapopModelDraw <- function(modelList){
    return(.Call("Rapophenia_draw", modelList$model))
}

#' Produce an apop_data set from an input data frame
#' @param data A data frame
#' @return An opaque pointer to an allocated and filled apop_data_set. As of this writing, data is copied, not pointed to.
apop_data_from_frame <- function(data){ 
    stopifnot(is.data.frame(data)) 
    return (.Call("wrapped_apop_data_from_frame", data))
}

#' Produce a data frame from an opaque pointer to an apop_data set.
#' @param data An opaque pointer to an apop_data_set, that you generated via \ref apop_data_from_frame.
#' @return A data frame. As of this writing, data is copied, not pointed to.
data_frame_from_apop_data <- function(data){ 
    out <- .Call("data_frame_from_sexp_wrapped_apop_data", data)
    if (is.null(out)) return (out)
    return (as.data.frame(out))
}

#' We keep a registry of C models, so you can pull them to the R side by name.
#' This will return a C-side model that should behave identically to your R-side models,
get_C_model <- function(name){
    return (list(model=.Call("get_from_registry", name)))
}

update <-function(data, prior, likelihood){
   return(.Call("update_wrapper", data, prior$model, likelihood$model))
}
