#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.assert.class.conform = function (CLASS, x)
{	if (CLASS != "<OBJECT>" && ! is.null (x) )
	{	if (! is (x, CLASS) )
			stop ("input doesn't match class restrictions")
	}
}

.assert.nsub.conform = function (i, j, nrs, ncs, conform, recursive, value)
{	if (is.ZERO (value) )
	{	nrs [i] = ncs [j] = 0
		list (value, TRUE, TRUE, nrs, ncs)
	}
	else
	{	if (is.vector (value) )
		{	if (length (value) == 1)
				value = matrix (value, 1, 1)
			else
				stop ("standard (non-matrix) vectors need to length one")
		}
		else if (recursive)
		{	if (! (is.NestMatrix (value) || is.matrix (value) ) )
				stop ("recursive NestMatrix needs ZERO, NestMatrix, matrix or vector")
		}
		else
		{	if (! is.matrix (value) )
				stop ("non-recursive NestMatrix needs ZERO, matrix or vector")
		}
		update.r = update.c = FALSE
		if (conform)
		{	n = dim (value)
			if (any (n == 0) )
				stop ("value needs non-zero dims, use ZERO instead")
			if (all (nrs == 0) )
			{	update.r = TRUE
				nrs [i] = n [1]
			}
			else
			{	nrs2 = nrs [nrs != 0]
				if (any (n [1] != nrs2) )
					stop ("value non-conformable in rows")
			}
			if (all (ncs == 0) )
			{	update.c = TRUE
				ncs [j] = n [2]
			}
			else
			{	ncs2 = ncs [ncs != 0]
				if (any (n [2] != ncs2) )
					stop ("value non-conformable in columns")
			}
		}
		list (value, update.r, update.c, nrs, ncs)
	}
}

"[.ObjectArray" = function (v, ...)
{	if (v@N != 1)
		stop ("single bracket subsetting, supports 1-dim only")
	I = unlist (list (...) )
	y = v@data [I]
	as.ObjectArray (y, CLASS=v@CLASS, n = length (y) )
}

"[.ImageArray" = function (v, ...)
{	u = `[.ObjectArray` (v, ...)
	new ("ImageArray", CLASS="RImage", N=1L, n = length (u), names = list (), data=u@data)
}

"[.GeomArray" = function (v, ...)
{	u = `[.ObjectArray` (v, ...)
	new ("GeomArray", CLASS="GeomObject", D=v@D, N=1L, n = length (u), names = list (), data=u@data)
}

"[.MatrixArray" = function (v, ...)
{	u = `[.ObjectArray` (v, ...)
	new ("MatrixArray", CLASS="matrix", N=1L, n = length (u), names = list (), conform=v@conform, data=u@data)
}

"[[.ObjectArray" = function (v, ...)
{	I = unlist (list (...) )
	v@data [[.mi2si (v@n, I)]]
}

"[[<-.ObjectArray" = function (v, ..., value)
{	I = unlist (list (...) )
	.assert.class.conform (v@CLASS, value)
	v@data [.mi2si (v@n, I)] = list (value)
	v
}

"[[.NestMatrix" = function (v, i, j, ..., drop=TRUE, zero=TRUE)
{	y = v@data [[.mi2si (v@n, c (i, j) )]]
	if (is.ZERO (y) && zero)
	{	nrs = apply (v@nrs, 1, max)
		ncs = apply (v@ncs, 2, max)
		y = matrix (0, nrs [i], ncs [j])
	}
	if (drop && ! is.ZERO (y) )
	{	if (nrow (y) == 1 || ncol (y) == 1)
			as.vector (y)
		else
			y
	}
	else
		y
}

"[[<-.NestMatrix" = function (v, i, j, value)
{	r = .assert.nsub.conform (i, j, v@nrs [i,], v@ncs [,j], v@conform, v@recursive, value)
	if (r [[2]]) v@nrs [i,] = r [[4]]
	if (r [[3]]) v@ncs [,j] = r [[5]]
	v@data [[.mi2si (v@n, c (i, j) )]] = r [[1]]
	v
}

"[[.PartMatrix" = function (v, i, j, ..., drop=TRUE)
	`.sSectMatrix` (v, i, j, drop=drop)

"[[<-.PartMatrix" = function (v, i, j, value)
	`.sSectMatrix.assign` (v, i, j, value=value)

"[[<-.GeomArray" = function (v, ..., value)
{	if (v@D != value@D)
		stop ("GeomObject doesn't match GeomArray D (dimension) value")
	`[[<-.ObjectArray` (v, ..., value=value)
}

getSect = function (v, ..., drop=TRUE)
	`.sSectMatrix` (v, ..., drop=drop)

".sSectMatrix" = function (x, ..., drop=TRUE)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J, drop=drop]
}

".sSectMatrix.assign" = function (x, ..., value)
{	vmap = x@vmap [[...]]
	I = vmap [1]:vmap [3]
	J = vmap [2]:vmap [4]
	x@data [I, J] = value
	x
}

.prod = function (x)
{	if (length (x) == 0)
		0
	else
		prod (x)
}

.mi2si = function (dim, I)
{	ndim = length (dim)
	if (ndim <= 1)
		I
	else
		sum ( (I - 1) * cumprod (c (1, dim [-ndim]) ) ) + 1
}
