\name{TextToDB}
\alias{TextToDB}
\title{Write a text file to an SQLite database.}
\usage{TextToDB(file, db, tbl, row.names=FALSE, col.names=TRUE, field.names,
    field.ends)}
\description{Write a text file to an SQLite database.
Wrapper function for apop_text_to_db().
Supports comma and pipe as delimiters,
otherwise column end points must be given.}
\arguments{\item{db}{database to use}
\item{file}{file to read}
\item{tbl}{name to give to the table}
\item{row.names}{logical indicating presence of row names in file}
\item{col.names}{logical indicating presence of column names in file}
\item{field.names}{names to use for the columns}
\item{field.ends}{integer vector giving end columns for each variable
in the case of a non-delimited file}}
