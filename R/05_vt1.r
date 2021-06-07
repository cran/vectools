#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.is.vector = function (x)
	(is.logical (x) || is.integer (x) || is.character (x) ||is.numeric (x) || is.complex (x) )

CLASS = function (v)
{	if (is.ObjectArray (v) )
		v@CLASS
	else
		stop ("CLASS can only be called on ObjectArray objects")
}

ndim = function (v)
{	dims = dim (v)
	if (is.null (dims) )
		NA
	else
		length (dims)
}

seqt = function (...)
	2 * pi * seq (...)

.arg.error = function (...)
{	expr = format ( (sys.call (-1)) )
	n = length (list (...) )
	if (n > 0)
	{	cat ("call with unsupported args:\n")
		print (expr)
		cat ("check for incorrect argument names\n")
		cat ("check for unnamed non-leading arguments\n")
		cat ("e.g. in ObjectArray (n, CLASS), CLASS\n")
		warning ("unsupported args, check arg names")
	}
}

plotv = function (...) UseMethod ("plotv")
plot.VecLike = function (x, ...) plotv (x, ...)
