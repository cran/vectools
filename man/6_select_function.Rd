\name{6_select_function}
\alias{select}
\alias{from}
\alias{group.by}
\alias{partition.by}
\alias{sort.by}
\alias{where}
\title{Select Function}
\description{SQL-like select function, for grouping, sorting and grouped aggregation functions, with limited support for automatic formatting.}
\usage{
select (\dots)

#select constructs:
#   from (\dots)
#   group.by (\dots)
#   partition.by (\dots)
#   sort.by (\dots)
#   where (\dots)
}
\details{
This function is a R (only) function with standard R syntax, but nonstandard evaluation.

Currently, it supports a subset of SQL select functionality, and is designed for convenience only and not for high performance or large datasets.

The function needs to be called with one or more unique variable names and a from construct, and optional group.by, partition by, sort.by and where constructs, along with optional new variables (currently, for use with group.by only). These constructs need to be included inside the select function call, and not called separately. Results (i.e. table rows) may not be unique. So, if you need unique results, then you should use the unique function after calling the select function.

Alternatively, a dot (.) may be used rather than specifying each variable name. Currently, dot is not allowed with other variable names. And if there's a group.by construct, any variables not included in group.by (or for use with group.by) are discarded. However, this may be changed in the future.

Currently, the from construct needs to include the name of a single data.frame.

The group.by construct should include comma-separated variable names only, giving variable names from the source data. The partition.by construct needs a single variable name from variables present after possible grouping. The sort.by construct should include comma-separated variables names from the variables present after possible grouping, optionally prefixed with a plus or a minus, for ascending and descending order, respectively. The where statement, should include simple comma-separated (in)equalities, such as x == 1 or y >= 1000, with variable names from either the grouped data or source data.

Re-iterating, new variables are for use with group.by only. If you need to create arbitrary variables, then you need to do it before or after calling select. Currently, new variables need to be defined using the "<-" operator and not "=". The operand to the left gives the new variable name, and the operand to the right can be any call that maps variables from the grouped data to a scalar value. Currently, new variables are listed after old variables, regardless of their input order.

Note that the partition.by construct is primarily for situations where you only want to print the results, without further operations. Also, note that subsetting operations corresponding to the where construct, may be applied at two different points in the function's execution, before or after grouping, depending on which variables are involved.

Expanding on the previous point, the execution order is:\cr
(1) Apply the where construct, for variables in the source data.\cr
(2) Apply the group.by construct and select variables.\cr
(3) Apply the where construct, for variables in the grouped data.\cr
(4) Apply the sort.by construct.\cr
(5) Renumber the rows.\cr
(6) Apply the partition.by construct.
}
\value{
A data.frame unless it includes a partition.by construct.

Partitioning returns a SectMatrix object, with row separators between each change in the partitioning variable, and column separators around it. Also, repetitions in the partitioning variable are replaced with spaces.
}
\arguments{
\item{\dots}{Refer to details section.}
}
\examples{
#all variables
select (., from (mtcars) )

#some variables
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
select (am, cyl,
    from (mtcars),
    group.by (am, cyl), partition.by (am), sort.by (-am, -mean.mpg),
        count <- length (mpg),
        mean.mpg <- mean (mpg) )

#earlier example but with a where construct
select (am, cyl, mpg, from (mtcars), where (mpg >= 20) )
}
