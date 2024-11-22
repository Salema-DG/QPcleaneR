% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosswalk_establishment.R
\name{crosswalk_establishment}
\alias{crosswalk_establishment}
\title{Crosswalk for Establishments in QP}
\usage{
crosswalk_establishment(data)
}
\arguments{
\item{data}{A tibble}
}
\value{
The end variable is \code{estab_id}. Where, after 2010, it's the one that comes in raw. before, it's filled
}
\description{
There are 2 establishment IDs:
\itemize{
\item estab_id, for 2010 and after. Each establishment as a unique ID;
\item nuest, for before 2010. Each establishment as a number inside a firm. Thus, the establishment is only identifiable with the firm ID.
}

Besides this, there is not an obvious way to connect them.
To solve it, I follow the people inside the establishments, from 2009 to 2010 and from 89 to 91 too.
These are the two years with discountinuities.

There are some NA's warnings iun the loops
}
