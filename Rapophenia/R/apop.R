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
    return(.Call("get_model_element", modelname, elementname))
    
}

setupRapopModel <- function(...){
    return( .Call("setup_R_model", 
        as.environment(list(...)))
    )
}

#' Take in an un-estimated model, and estimate parameters.
#' @param mod A model that you set up using \c setupRapopModel
#' @param data An environment, including anything needed by your models. The system will add a \c
#' parameters element as necessary.
#' @return a Rapophenia model. Interrogate it using \c getModelElement
estimateRapopModel <- function(mod, data){
    stopifnot(is.environment(data))
    return (.Call("Rapophenia_estimate", mod, data))
}
