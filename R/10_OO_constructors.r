#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

ObjectArray = function (CLASS="<OBJECT>", n, ..., names, default.value=NULL)
{	if (missing (n) ) stop ("n required")
	else n = as.integer (n)
	if (length (n) == 0) stop ("n zero length")
	n1 = prod (n)

	v = vector ("list", n1)
	for (i in seq_len (n1) )
		v [i] = list (default.value)
	as.ObjectArray (v, CLASS, n, names=names)
}

NestMatrix = function (nr, nc, ..., rnames, cnames, conform=TRUE, recursive=FALSE, default.value=ZERO)
{	if (missing (nr) || missing (nc) )
		stop ("nr and nc required")
	nr = as.integer (nr)
	nc = as.integer (nc)

	default.value = .assert.nsub.conform (1, 1, 0, 0, FALSE, recursive, default.value)[[1]]
	if (is.ZERO (default.value) )
		nrs = ncs = matrix (0L, nr, nc)
	else
	{	nsub = dim (default.value)
		nrs = matrix (nsub [1], nr, nc)
		ncs = matrix (nsub [2], nr, nc)
	}

	n = c (nr, nc)
	if (missing (rnames) ) rnames = NULL
	if (missing (cnames) ) cnames = NULL
	v = ObjectArray (,n, names = list (rnames, cnames), default.value=default.value)

	new ("NestMatrix", CLASS = character (), N=2L, n=n, nrs=nrs, ncs=ncs,
		names=v@names, conform=conform, recursive=recursive, data=v@data)
}

PartMatrix = function (nr, nc, isep, jsep, ..., rnames, cnames, default.value=0)
{	if (missing (nr) || missing (nc) )
		stop ("nr and nc required")
	nr = as.integer (nr)
	nc = as.integer (nc)

	default.value = .smat.val (default.value)

	x = matrix (default.value, nr, nc)
	as.PartMatrix (x, isep, jsep, rnames=rnames, cnames=cnames)
}

SectMatrix = function (nr, nc, ..., vmap, rnames, cnames, default.value=0)
{	if (missing (nr) || missing (nc) || missing (vmap) )
		stop ("nr, nc and vmap required")

	nr = as.integer (nr)
	nc = as.integer (nc)
	default.value = .smat.val (default.value)
	
	x = matrix (default.value, nr, nc)
	as.SectMatrix (x, vmap=vmap, rnames=rnames, cnames=cnames)
}

VMap = function (n)
{	if (missing (n) )
		stop ("n requried")
	n = as.integer (n)
	N = length (n)
	if (any (n <= 0) )
		stop ("n values need to be positive")
	v = ObjectArray (,n)
	new ("VMap", CLASS="matrix", N=N, n=n, data=v@data)
}

MatrixArray = function (n, ..., conform=TRUE, default.value=ZERO)
{	if (missing (n) )
		stop ("n requried")
	n = as.integer (n)
	N = length (n)
	if (N != 1)
		stop ("currently, only 1d MatrixArray(s) supported")

	v = ObjectArray (,n, default.value=default.value)
	new ("MatrixArray", CLASS="matrix", N=N, n=n, names = list (), conform=conform, data=v@data)
}

as.ObjectArray = function (x, CLASS="<OBJECT>", n, ..., names)
{	if (missing (x) || length (x) == 0)
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

as.NestMatrix = function (x, ..., rnames, cnames, conform=TRUE, recursive=FALSE)
{	if (missing (x) )
		stop ("x required")
	if (is.PartMatrix (x) )
	{	n = x@ns
		nrs = ncs = matrix (1L, n [1], n [2])
		v = ObjectArray (,n, default.value=0)
		for (i in 1:n [1])
		{	for (j in 1:n [2])
			{	xsub = x [[i, j, drop=FALSE]]
				nrs [i, j] = nrow (xsub)
				ncs [i, j] = ncol (xsub)
				v [[i, j]] = xsub
			}
		}
	}
	else
	{	x = .as.matrix (x)
		n = dim (x)
		rnames = .resolve.names (n [1], rownames (x), rnames)
		cnames = .resolve.names (n [2], rownames (x), cnames)

		nrs = ncs = matrix (1L, n [1], n [2])
		v = as.ObjectArray (as.list (x),, n, names = list (rnames, cnames), default.value=0)
	}

	new ("NestMatrix", CLASS = character (), N=2L, n=n, nrs=nrs, ncs=ncs,
		names=v@names, conform=conform, recursive=recursive, data=v@data)
}

as.PartMatrix = function (x, isep, jsep, ..., rnames, cnames)
{	if (missing (x) )
		stop ("x required")
	if (is.NestMatrix (x) )
	{	if (x@conform && ! x@recursive)
		{	header = FALSE
			nrs = as.integer (apply (x@nrs, 1, max) )
			ncs = as.integer (apply (x@ncs, 2, max) )
			if (any (nrs == 0) || any (ncs == 0) )
				stop ("conversion needs non-zero dims")
			nr = sum (nrs)
			nc = sum (ncs)
			ns = x@n
			cumr = c (0, cumsum (nrs) )
			cumc = c (0, cumsum (ncs) )

			rnames = .resolve.names (nr, NULL, rnames)
			cnames = .resolve.names (nc, NULL, cnames)

			y = matrix (0, nr, nc)
			vmap = VMap (ns)
			for (i in 1:ns [1])
			{	for (j in 1:ns [2])
				{	m = c (cumr [i] + 1, cumc [j] + 1, cumr [i + 1], cumc [j + 1])
					m = matrix (m, 2, 2)
					vmap [[i, j]] = m
					I = m [1, 1]:m [1, 2]
					J = m [2, 1]:m [2, 2]

					sub = x [[i, j, drop=FALSE]]
					if (is.ZERO (sub) )
						y [I, J] = 0
					else
						y [I, J] = sub
				}
			}
		}
		else
			stop ("only comformable non-recursive NestMatrix(s) allowed")
	}
	else
	{	header = is.data.frame (x)
		y = .as.matrix (x)
		nr = nrow (y)
		nc = ncol (y)

		rnames = .resolve.names (nr, rownames (y), rnames)
		cnames = .resolve.names (nc, colnames (y), cnames)

		R = .part.indices (isep, nr)
		C = .part.indices (jsep, nc)
		nR = length (R)
		nC = length (C)
		ns = c (nR, nC)
		vmap = VMap (ns)

		for (i in 1:nR)
		{	for (j in 1:nC)
				vmap [[i, j]] = matrix (c (R [[i]][1], C [[j]][1], R [[i]][2], C [[j]][2]), 2, 2)
		}
		
	}
	new ("PartMatrix", header=header, NS=2L, ns=ns, vmap=vmap, nr=nr, nc=nc, rnames=rnames, cnames=cnames, data=y)
}

as.SectMatrix = function (x, ..., vmap, rnames, cnames)
{	if (missing (x) || missing (vmap) )
		stop ("x and vmap required")

	header = is.data.frame (x)
	x = .as.matrix (x)
	
	ns = dim (vmap)
	NS = length (ns)
	for (i in length (vmap) )
	{	smap = vmap@data [[i]]
		if (is.null (smap) )
			stop ("vmap contains null values")
	}
	nr = nrow (x)
	nc = ncol (x)

	rnames = .resolve.names (nr, rownames (x), rnames)
	cnames = .resolve.names (nc, colnames (x), cnames)

	new ("SectMatrix", header=header, NS=NS, ns=ns, vmap=vmap, nr=nr, nc=nc, rnames=rnames, cnames=cnames, data=x)
}

.vmap.val = function (vmap)
{	nv = dim (vmap)
	if (! (is.array (vmap) && length (nv) == 3) )
		stop ("vmap needs to be 3d array")
	if (! (nv [2] == 2 && nv [3] == 2) )
		stop ("the 2nd and 3rd vmap dims need to be two")
	nv [1]
}

.nmat.val = function (x)
{	if (is.null (x) || is.NestMatrix (x) || is.matrix (x) )
		x
	else
		as.vector (x)
}

.smat.val = function (x)
{	x = as.vector (x)
	if (length (x) != 1)
		stop ("PartMatrix/SectMatrix default value not scalar")
	x
}

.resolve.names = function (n, xnames, fnames)
{	if (missing (fnames) )
	{	if (is.null (xnames) )
		{	NULL
		}
		else
			xnames
	}
	else if (n == length (fnames) )
		fnames
	else
		stop ("length of row/col names doesn't match dims")
}

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
