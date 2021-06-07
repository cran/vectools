#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.init.list = function (n, default.value=NULL)
{	if (length (n) == 0)
		list ()
	else
	{	nprod = prod (n)
		x = vector ("list", nprod)
		if (nprod != 0 && ! is.null (default.value) )
		{	for (i in 1:nprod)
				x [[i]] = default.value
		}
		x
	}
}

ObjectArray = function (n, ..., CLASS="<OBJECT>", names, default.value=NULL)
{	.arg.error (...)

	if (missing (n) ) stop ("n required")
	else n = as.integer (n)
	if (length (n) == 0) stop ("n zero length")
	v = .init.list (n, default.value)
	as.ObjectArray (v, n=n, CLASS=CLASS, names=names)
}

GeomArray = function (n, ..., D)
{	if (missing (D) )
		stop ("D required")
	as.GeomArray (.init.list (n), n=n, D=D)
}
GeomArray2 = function (n) GeomArray (n, D=2)
GeomArray3 = function (n) GeomArray (n, D=3)

ImageArray = function (n)
	as.ImageArray (.init.list (n), n=n)

MatrixArray = function (n, ..., NR, NC, conform=TRUE, default.value=ZERO)
{	.arg.error (...)
	as.MatrixArray (.init.list (n, default.value), n=n, NR=NR, NC=NC, conform=conform)
}

as.ObjectArray = function (v, ..., n, CLASS="<OBJECT>", names)
{	.arg.error (...)
	x = v
	if (missing (x) || length (x) == 0)
		stop ("x (non-zero length) required")

	if (missing (n) )
	{	n = dim (x)
		if (is.null (n) )
			n = length (x)
	}
	else
		n = as.integer (n)

	N = length (n)
	if (missing (names) )
		names = NULL
	else if (is.null (names) )
		0
	else if (is.list (names) )
	{	if (N != length (names) )
			stop ("dimensions don't match names")
		for (k in seq_len (N) )
		{	nk = names [[k]]
			if (! is.null (nk) && n [k] != length (nk) )
				stop ("dimensions don't match names")
		}
	}
	else
		stop ("names needs to be NULL or list")
	x = as.list (x)
	attributes (x) = NULL
	if (prod (n) != length (x) )
		stop ("prod (n) != length (x)")

	for (xi in x)
		.assert.class.conform (CLASS, xi)
	new ("ObjectArray", CLASS=CLASS, N=N, n=n, names=names, data=x)
}

as.ImageArray = function (im, ..., n)
{	.arg.error (...)

	v = as.ObjectArray (im, n=n)
	new ("ImageArray", CLASS="RImage", N=v@N, n=v@n, names = list (), data=v@data)
}

as.GeomArray = function (v, ..., n, D)
{	.arg.error (...)

	v = as.ObjectArray (v, n=n)
	if (v@N != 1)
		stop ("currently, only 1d GeomArray(s) supported")
	if (missing (D) )
		D = v [[1]]@D
	else
		D = as.integer (D)
	for (i in seq_len (length (v) ) )
	{	if (is.null (v [[i]]) )
			0
		else if (v [[i]]@D != D)
			stop ("\neither:\n    objects don't match the user-specified D arg, or\n    mixed 2D/3D objects")
	}
	new ("GeomArray", CLASS="GeomObject", D=D, N=v@N, n=v@n, names = list (), data=v@data)
}

as.MatrixArray = function (v, ..., n, NR, NC, conform=TRUE)
{	.arg.error (...)
	if (missing (NR) || missing (NC) || ! is.na (NR [1]) || ! is.na (NC [1]) )
		stop ("currently, NR and NC need to be set to NA")
	v = as.ObjectArray (v, n=n)
	if (v@N != 1)
		stop ("currently, only 1d MatrixArray(s) supported")
	if (conform)
	{	if (v@n >= 2)
		{	dims.1 = dim (v@data [[1]])
			for (i in 2:v@n)
			{	dims.i = dim (v@data [[i]])
				if (all (dims.1 == dims.i) )
					NULL
				else
					stop ("conform=TRUE, but different dims")
			}
		}
	}
	new ("MatrixArray", CLASS="matrix", N=v@N, n=v@n, names = list (), conform=conform, data=v@data)
}
