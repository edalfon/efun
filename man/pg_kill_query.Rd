% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postgres.R
\name{pg_kill_query}
\alias{pg_kill_query}
\title{Kill a running query in Postgres}
\usage{
pg_kill_query(con, pid)
}
\arguments{
\item{con}{pg connection}

\item{pid}{pid of the query to kill}
}
\value{
see pg_cancel_backend
}
\description{
Kill a running query in Postgres
}
