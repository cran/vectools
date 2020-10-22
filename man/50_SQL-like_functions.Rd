\name{50_SQL-like_functions}
\alias{select}
\alias{selectf}
\title{SQL-Like Functions}
\description{
SQL-like select functions, for grouping, sorting and grouped aggregation functions, with limited support for automatic formatting.

Note that currently, there are significant limitations, defining new fields within the select statement.}
\usage{
select (\dots)
selectf (\dots)

#select constructs:
# from (\dots)
# group.by (\dots)
# sort.by (\dots)
# where (\dots)

#additional selectf constructs:
# partition.by (\dots)
}
\arguments{
\item{\dots}{
	Existing fields (from the source data), new fields (used with a group.by construct), select constructs (which needs to include a from construct, with a single data.frame) or partition.by, if using selectf.\cr
	Refer to details section.}
}
\details{
The select function is an R (only) function with standard R syntax, but nonstandard evaluation.

Currently, it supports a subset of SQL select functionality, and is designed for convenience only and not for high performance or large datasets.

The function needs to be called with one or more unique field names and a from construct, and optional group.by, sort.by and where constructs. These constructs need to be included inside the select function call, and not called separately. Results (i.e. table rows) may not be unique.

Field names need to be from the data.frame in the from construct, or names of new fields for use with group.by.\cr
Currently, you can't define new fields unless they're the result of a group.by construct.\cr
If you need to create arbitrary fields, then you need to do it before or after calling select.

Alternatively, a dot (.) may be used rather than specifying each field name. Currently, dot is not allowed with other field names. And if there's a group.by construct, any fields not included in group.by (or for use with group.by) are discarded. However, this may be changed in the future.

Currently, the from construct needs to include the name of a single data.frame, in calling environment.

The group.by construct should include comma-separated field names only, giving field names from the source data. The partition.by construct (for selectf) needs a single field name from fields present after possible grouping. The sort.by construct should include comma-separated fields names from the fields present after possible grouping, optionally prefixed with a plus or a minus, for ascending and descending order, respectively. The where statement, should include simple comma-separated (in)equalities, such as x == 1 or y >= 1000, with field names from either the grouped data or source data.

Currently, new fields need to be defined using the "<-" operator and not "="

The operand to the left gives the new field name, and the operand to the right can be any call that maps fields from the grouped data to a scalar value. Currently, new fields are listed after old fields, regardless of their input order.

Note that the partition.by construct is primarily for situations where you only want to print the results, without further operations. Also, note that subsetting operations corresponding to the where construct, may be applied at two different points in the function's execution, before or after grouping, depending on which fields are involved.

Expanding on the previous point, the execution order is:\cr
(1) Apply the where construct, for fields in the source data.\cr
(2) Apply the group.by construct and select fields.\cr
(3) Apply the where construct, for fields in the grouped data.\cr
(4) Apply the sort.by construct.\cr
(5) Renumber the rows.\cr
(6) Apply the partition.by construct.

}
\value{
select returns a data.frame.

selectf returns a SectMatrix object, with row separators between each change in the partitioning field, and column separators around it. Also, repetitions in the partitioning field are replaced with spaces.}
\seealso{
\link{ghead.data.frame}

\link{Combined Head and Tail Functions}
}
\examples{
#all fields
select (., from (mtcars) )

#some fields
select (am, cyl, mpg, from (mtcars) )

#grouped by am and cyl
#with mean of mpg, by group
select (am, cyl,
    from (mtcars),
    group.by (am, cyl),
        count <- length (mpg),
        mean.mpg <- mean (mpg) )

#same as above
#but partitioned and sorted
selectf (am, cyl,
    from (mtcars),
    group.by (am, cyl), partition.by (am), sort.by (-am, -mean.mpg),
        count <- length (mpg),
        mean.mpg <- mean (mpg) )

#earlier example but with a where construct
select (am, cyl, mpg, from (mtcars), where (mpg >= 20) )
}
