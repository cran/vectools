#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.matrix2array = function (dims, m)
{	N = length (dims)
	if (N == 2)
		m
	else
	{	names = colnames (m)
		dim (m) = dims
		if (is.null (names) )
			dimnames (m) = NULL
		else
		{	vnames = vector ("list", N)
			vnames [[N]] = names
			dimnames (m) = vnames
		}
		m
	}
}

.array2matrix = function (v)
{	dims = dim (v)
	N = length (dims)
	dim (v) = c (prod (dims [-N]), dims [N])
	v
}

.array2list = function (u)
{	dims = dim (u)
	N = length (dims)
	n = dims [N]
	dims = dims [-N]
	names = dimnames (u)[[N]]
	u = .array2matrix (u)
	v = vector ("list", n)
	names (v) = names
	if (N == 2)
	{	for (k in 1:n)
			v [[k]] = as.vector (u [,k])
	}
	else
	{	for (k in 1:n)
			v [[k]] = array (u [,k], dims)
	}
	v
}

.list2array = function (u)
{	n = length (u)
	dims = vector ("list", n)
	for (k in 1:n)
	{	if (is.array (u [[k]]) )
			dims [[k]] = dim (u [[k]])
		else
		{	u [[k]] = as.vector (u [[k]])
				dims [[k]] = length (u [[k]])
		}
	}
	dim1 = dims [[1]]
	for (k in 2:n)
	{	if (length (dim1) == length (dims [[k]]) && all (dim1 == dims [[k]]) )
			NULL
		else
			stop ("array components need identical dims")
	}
	m = matrix (0, prod (dim1), n)
	for (k in 1:n)
		m [,k] = u [[k]]
	.matrix2array (c (dim1, n), m)
}

.check.colorspace.loaded = function ()
{	ns = loadedNamespaces ()
	if (! ("colorspace" %in% ns) )
		stop ("need to load colorspace package")
}

.map.col = function (colvs, from, to, correction)
{	colvs = rbind (colvs)
	nc = ncol (colvs)
	rownames (colvs) = NULL
	names = rep ("", nc)
	if (nc == 3) 0
	else if (nc == 4) names [4] = "a"
	else
		stop ("colvs needs 3 or 4 columns")
	color.spaces = c ("XYZ", "RGB", "sRGB", "LAB", "polarLAB", "HSV", "HLS", "LUV", "polarLUV", "HCL")
	if (! (from %in% color.spaces && to %in% color.spaces) )
		stop ("unsupported color space")
	if (from != to)
	{	if (from == "HCL")
		{	from = colorspace::polarLUV
			colvs [,1:3] = colvs [,3:1]
		}
		else
			from = eval (str2lang (from) )
		if (to == "HCL")
		{	v = colorspace::coords (as (from (colvs [,1], colvs [,2], colvs [,3]), "polarLUV") )
			colvs [,3:1] = v
			names [1:3] = c ("H", "C", "L")
		}
		else
		{	v = colorspace::coords (as (from (colvs [,1], colvs [,2], colvs [,3]), to) )
			colvs [,1:3] = v
			names [1:3] = colnames (v)
		}
		colnames (colvs) = names
		if (to == "sRGB" && correction)
		{	colvs [colvs < 0] = 0
			colvs [colvs > 1] = 1
		}
	}
	colvs
}

vt3.prep.cols = function (cols, nchannels, single.flag=TRUE, ..., as.list=FALSE, as.array=FALSE)
{	dims = NULL
	if (is.array (cols) )
		dims = vdim = dim (cols)
	else
		vdim = length (cols)
	cols = t (col2rgb (cols, TRUE) )
	opaque = (cols [,4] == 255)
	opaque1 = all (opaque)

	if (single.flag)
		opaque = opaque1
	if (missing (nchannels) )
	{	if (opaque1)
			nchannels = 3
		else
			nchannels = 4
	}
	else if (! (nchannels == 3 || nchannels == 4) )
		stop ("nchannels (if supplied) needs to be 3 or 4")

	cols = cols / 255L
	if (as.list || as.array)
	{	colnames (cols) = c ("r", "g", "b", "a")
		cols = cols [,1:nchannels, drop=FALSE]
		cols = .matrix2array (c (vdim, nchannels), cols)
		if (as.list)
			.array2list (cols)
		else
			cols
	}
	else
	{	if (nchannels == 3)
			cols = rgb (cols [,1], cols [,2], cols [,3])
		else
			cols = rgb (cols [,1], cols [,2], cols [,3], cols [,4])
		dim (cols) = dims
		if (! single.flag)
			dim (opaque) = dims
		list (opaque, cols)
	}
}

is.opaque = function (cols, ..., single.flag=TRUE)
	vt3.prep.cols (cols, single.flag=single.flag)[[1]]

standardize.cols = function (cols, ..., nchannels, as.list=FALSE, as.array=FALSE)
{	if (as.list || as.array)
		vt3.prep.cols (cols, nchannels, as.list=as.list, as.array=as.array)
	else
		vt3.prep.cols (cols, nchannels, single.flag=FALSE)[[2]]
}

mapcol = function (colv, ...,
	as.list = is.list (colv), as.string=FALSE,
	from="sRGB", to=from,
	correction=TRUE)
{	if (from != to)
		.check.colorspace.loaded ()

	if (is.array (colv) )
	{	dims = dim (colv)
		N = length (dims)
		n = dims [N]
		if (! (n == 3 || n == 4) )
			stop ("last array dim needs to be 3 or 4")
		if (prod (dims) == 0)
			stop ("empty arrays not allowed")
		
		if (is.logical (colv) )
			mode (colv) = "numeric"
		else if (! is.numeric (colv) )
			stop ("mapcol requires logical/numeric array, or list of such")
		if (as.string)
			to = "sRGB"

		names = names (colv)
		colv = .array2matrix (colv)
		colv = .map.col (colv, from, to, correction)

		if (as.string)
		{	if (n == 3)
				cols = rgb (colv [,1], colv [,2], colv [,3])
			else
				cols = rgb (colv [,1], colv [,2], colv [,3], colv [,4])
			if (N > 2)
				dim (cols) = dims [-N]
			cols
		}
		else
		{	colv = .matrix2array (dims, colv)
			names (colv) = names
			if (as.list)
				.array2list (colv)
			else
				colv
		}
	}
	else if (is.list (colv) )
	{	colv = .list2array (colv)
		mapcol (colv, as.list=as.list, as.string=as.string, from=from, to=to, correction=correction)
	}
	else
		stop ("colv needs to be array or list")
}

mapcol3 = function (x, y, z, alpha, ...,
	as.list=FALSE, as.string=FALSE,
	from="sRGB", to=from,
	correction=TRUE)
{	if (missing (alpha) )
		colv = list (x, y, z)
	else
		colv = list (x, y, z, alpha)
	mapcol (colv, as.list=as.list, as.string=as.string, from=from, to=to, correction=correction)
}
