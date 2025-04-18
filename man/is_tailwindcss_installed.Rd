% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailwind_cli.R
\name{is_tailwindcss_installed}
\alias{is_tailwindcss_installed}
\title{Checks if 'TailwindCSS' CLI is installed}
\usage{
is_tailwindcss_installed(tailwindcss = NULL, verbose = FALSE)
}
\arguments{
\item{tailwindcss}{name and path to the executable}

\item{verbose}{report version number etc}
}
\value{
TRUE/FALSE if the CLI is installed
}
\description{
To install the CLI of 'TailwindCSS', please follow the instructions of
\href{https://github.com/tailwindlabs/tailwindcss/releases}{'TailwindCSS' releases}.
Make sure that you either provide the direction to the executable as the
first argument to this function or put it in a folder on your PATH variable.
}
\examples{
if (interactive()) {
  is_tailwindcss_installed()
}
}
