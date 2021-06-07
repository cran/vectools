#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.val.names = function (x)
{	if (is.null (rownames (x) ) )
		rownames (x) = paste ("[", 1:nrow (x), ",]", sep="")
	if (is.null (colnames (x) ) )
		colnames (x) = paste ("[,", 1:ncol (x), "]", sep="")
	x
}

.data = function (x)
{	y = x@data

	if (is.null (x@rnames) )
		rownames (y) = paste ("[", 1:x@nr, ",]", sep="")
	else
		rownames (y) = x@rnames


	if (is.null (x@cnames) )
		colnames (y) = paste ("[,", 1:x@nc, "]", sep="")
	else
		colnames (y) = x@cnames
	y
}

headt.default = function (v, nh=3, nt=nh, ...)
{	if (is.vector (v) )
	{	n = length (v)
		if (n <= nh + nt)
			v
		else
			c (head (v, nh, ...), tail (v, nt, ...) )
	}
	else
		stop ("headt.default only for simple vectors")
}

headt.ObjectArray = function (v, nh=3, nt=nh, ...)
	headt (format (v, ...), nh, nt)

headt.MatrixLike = function (v, nh=3, nt=nh, ...)
	headt (format (v, ...), nh, nt)

headt.raster = function (v, nh=3, nt=nh, ...)
	headt (as.matrix (v), nh, nt, ...)

headt.matrix = function (v, nh=3, nt=nh, ...)
{	x = v
	if (length (nh) == 1)
		nh = c (nh, nh)
	if (length (nt) == 1)
		nt = c (nt, nt)
	onr = nh [1] + nt [1]
	onc = nh [2] + nt [2]
	nr = nrow (x)
	nc = ncol (x)
	x = .val.names (x)
	if (nr <= onr && nc <= onc)
		y = x
	else if (nr <= onr && nc > onc)
	{	x = x [, c (1:nh [2], (1 + nc - nt [2]):nc), drop=FALSE]
		y = as.PartMatrix (x,, nh [2])
	}
	else if (nr > onr && nc <= onc)
	{	x = x [c (1:nh [1], (1 + nr - nt [1]):nr),, drop=FALSE]
		y = as.PartMatrix (x, nh [1])
	}
	else
	{	x = x [c (1:nh [1], (1 + nr - nt [1]):nr), c (1:nh [2], (1 + nc - nt [2]):nc)]
		y = as.PartMatrix (x, nh [1], nh [2])
	}
	noquote (format (y, hsep=".", vsep=".", xsym=".") )
}


headt.data.frame = function (v, nh=3, nt=nh, ...)
{	v = .as.matrix (v)
	if (nrow (v) <= nh + nt)
		noquote (v)
	else
	{	v = .val.names (v)
		v = rbind (head (v, nh), tail (v, nt) )
		v = as.PartMatrix (v, nh)
		noquote (format (v, hsep=".") )
	}
}
