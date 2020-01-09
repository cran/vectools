#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

ObjectArray = function (dim, names, default.value=NA)
{	if (missing (dim) || length (dim) == 0)
		n = 0
	else
		n = prod (dim)
	v = vector ("list", n)
	for (i in seq_len (n) )
		v [[i]] = default.value
	as.ObjectArray (v, dim, names)
}

NestMatrix = function (nr, nc, rnames, cnames, default.value=NA)
{	x = matrix (default.value, nr, nc)
	as.NestMatrix.2 (x, rnames, cnames)
}

SectMatrix = function (nsect, nr, nc, rnames, cnames, default.value=NA)
{	x = matrix (default.value, nr, nc)
	as.SectMatrix (x, nsect, rnames, cnames)
}

PartMatrix = function (Rb, Cb, nr, nc, rnames, cnames, default.value=NA)
{	x = matrix (default.value, nr, nc)
	as.PartMatrix (x, Rb, Cb, rnames, cnames)
}

as.ObjectArray = function (x, dim, names)
{	if (missing (x) || length (x) == 0)
		new ("ObjectArray", nbd=0L, dim=0L, names = list (), data = list () )
	else
	{	if (missing (dim) )
			dim = dim (x)
		else
			dim = as.integer (dim)
		nbd = length (dim)
		if (missing (names) )
			names = as.list (rep (NA_character_, nbd) )
		else
		{	if (! is.list (names) )
				stop ("names needs to be list")
			if (nbd != length (names) )
				stop ("dimensions don't match names")
			for (i in seq (nbd) )
			{	if (is.na (names [[i]][1]) )
					NULL
				else if (dim [i] != length (names [[i]]) )
					stop ("dimensions don't match names")
			}
		}
		x = as.list (x)
		attributes (x) = NULL
		if (prod (dim) != length (x) )
			stop ("prod (dim) != length (x)")
		new ("ObjectArray", nbd=nbd, dim=dim, names=names, data=x)
	}
}

as.NestMatrix = function (x, Rb, Cb, rnames, cnames)
{	x = as.PartMatrix (x, Rb, Cb)
	n = x@nsect
	y = NestMatrix (n [1], n [2], rnames, cnames)
	for (i in 1:n [1])
	{	for (j in 1:n [2])
			y [[i, j]] = x [i, j]
	}
	y
}

as.NestMatrix.2 = function (x, rnames, cnames)
{	x = .as.matrix (x)
	nr = nrow (x)
	nc = ncol (x)
	v = as.ObjectArray (x, c (nr, nc) )
	new ("NestMatrix", nr=nr, nc=nc, data=v)
}

as.SectMatrix = function (x, nsect, rnames, cnames)
{	header = is.data.frame (x)
	x = .as.matrix (x)
	if (missing (nsect) )
	{	nfd = 0L
		nsect = integer ()
	}
	else
	{	nsect = as.integer (nsect)
		nfd = length (nsect)
	}
	vmap = ObjectArray (nsect)
	dims = dim (x)
	if (missing (rnames) )
	{	rnames = rownames (x)
		if (is.null (rnames) )
			rnames = NA_character_
	}
	else if (dims [1] != length (rnames) )
		stop ("nrows != length (rnames)")
	if (missing (cnames) )
	{	cnames = colnames (x)
		if (is.null (cnames) )
			cnames = NA_character_
	}
	else if (dims [2] != length (cnames) )
		stop ("ncols != length (cnames)")
	attributes (x) = NULL
	dim (x) = dims
	nr = nrow (x)
	nc = ncol (x)
	new ("SectMatrix", header=header, nfd=nfd, nsect=nsect, vmap=vmap, nr=nr, nc=nc, rnames=rnames, cnames=cnames, data=x)
}

as.PartMatrix = function (x, Rb, Cb, rnames, cnames)
{	header = is.data.frame (x)
	x = .as.matrix (x)
	nr = nrow (x)
	nc = ncol (x)
	R = .part.indices (Rb, nr)
	C = .part.indices (Cb, nc)
	nR = length (R)
	nC = length (C)
	nsect = c (nR, nC)
	vmap = ObjectArray (nsect)
	for (i in 1:nR)
	{	for (j in 1:nC)
			vmap [[i, j]] = matrix (c (R [[i]][1], C [[j]][1], R [[i]][2], C [[j]][2]), 2, 2)
	}
	y = as.SectMatrix (x, 0, rnames, cnames)
	new ("PartMatrix", header=FALSE, nfd=2L, nsect=nsect, vmap=vmap, nr=nr, nc=nc, rnames=y@rnames, cnames=y@cnames, data=x)
}

rnames = function (x)
{	if (is.na (x@rnames [1]) ) NA
	else x@rnames
}

cnames = function (x)
{	if (is.na (x@cnames [1]) ) NA
	else x@cnames
}

"rnames<-" = function (x, ..., value)
{	if (x@nr == length (value) )
	{	x@rnames = value
		x
	}
	else
		stop ("nrows != length (rnames)")
}

"cnames<-" = function (x, ..., value)
{	if (x@nc == length (value) )
	{	x@cnames = value
		x
	}
	else
		stop ("ncols != length (cnames)")
}

"setmap<-" = function (x, ..., value)
{	mode (value) = "integer"
	if (! is.matrix (value) )
	{	value = as.vector (value)
		value = matrix (value, 2, 2)
	}
	if (length (value) == 4)
	{	x@vmap [[...]] = value
		x
	}
	else
		stop ("value needs to be 2*2  matrix, or length-4 vector")
}

getmap = function (x, ...)
	x@vmap [[...]]

.part.indices = function (Ib, n)
{	if (missing (Ib) )
		list (c (1, n) )
	else
	{	Ib = unique (as.integer (Ib) )
		nI = length (Ib) + 1
		if (nI == 1)
			I = list (1:n)
		else
		{	Ib = sort (Ib)
			if (any (Ib < 1 | Ib >= n) )
				stop ("\nRb needs be in [1, nr - 1]; and\nCb needs be in [1, nc - 1]")
			I = vector ("list", nI)
			I [[1]] = c (1, Ib [1])
			if (nI > 2)
			{	for (i in 2:(nI - 1) )
					I [[i]] = c (Ib [i - 1] + 1, Ib [i])
			}
			I [[nI]] = c (Ib [nI - 1] + 1, n)
		}
		I
	}
}

.as.matrix = function (x)
{	if (is.matrix (x) )
		x
	else if (is.data.frame (x) )
		as.matrix (format (x) )
	else
		stop ("needs matrix or data.frame")
}
