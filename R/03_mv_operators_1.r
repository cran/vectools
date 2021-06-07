#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.val.M = function (x)
{	if (is.MatrixArray (x) )
		x
	else if (is.matrix (x) )
	{	mv = MatrixArray (1, NR=NA, NC=NA)
		mv [[1]] = x
		mv
	}
	else
		stop ("matrix or MatrixArray required")
}

.val.G = function (x)
{	if (is.GeomArray (x) )
		x
	else if (is.GeomObject (x) )
	{	gv = GeomArray (1, D=x@D)
		gv [[1]] = x
		gv
	}
	else
		stop ("GeomObject or GeomArray required")
}

.val.GM = function (x)
{	if (is.GeomArray (x) || is.MatrixArray (x) )
		x
	else if (is.GeomObject (x) )
	{	gv = GeomArray (1, D=x@D)
		gv [[1]] = x
		gv
	}
	else if (is.matrix (x) )
	{	mv = MatrixArray (1, NR=NA, NC=NA)
		mv [[1]] = x
		mv
	}
	else
		stop ("GeomObject, GeomArray, matrix or MatrixArray required")
}

.val.m1 = function (x, post)
{	if (is.vector (x) )
	{	if (post) x = rbind (x)
		else x = cbind (x)
		x
	}
	if (is.matrix (x) )
		x
	else if (is.MatrixArray (x) && length (x) == 1)
		x [[1]]
	else
		stop ("matrix or length-one MatrixArray required")
}

.mvpair = function (a, b)
{	na = length (a)
	nb = length (b)
	if (na == nb)
		list (a, b, na)
	if (na < nb)
	{	if (nb %% na != 0)
			stop ("length of longer object not multiple of shorter object")
		list (rep2 (a, nb), b, nb)
	}
	else
	{	if (na %% nb != 0)
			stop ("length of longer object not multiple of shorter object")
		list (a, rep2 (b, na), na)
	}
}

.mvmult.pw = function (post, a, b, pack=FALSE, simplify=TRUE)
{	ab = .mvpair (a, b)
	is.mv = (is.MatrixArray (ab [[1]]) && is.MatrixArray (ab [[2]]) )
	n = ab [[3]]
	if (is.mv)
		v = MatrixArray (n, NR=NA, NC=NA)
	else
		v = GeomArray (n, D=b@D)
	if (is.mv && ! pack)
	{	for (k in 1:n)
			v [[k]] = ab [[1]][[k]] %*% ab [[2]][[k]]
	}
	else if (post)
	{	for (k in 1:n)
			v [[k]] = .mvmult0.adat (ab [[2]][[k]], ab [[1]][[k]])
	}
	else
	{	for (k in 1:n)
			v [[k]] = .mvmult0.bdat (ab [[1]][[k]], ab [[2]][[k]])
	}
	if (simplify && length (v) == 1)
		v [[1]]
	else
		v
}

.mvmult.ext = function (a, b, type="std", block.by.right=FALSE)
{	a = .val.M (a)
	b = .val.M (b)
	if (type == "rollc")
	{	na = length (a)
		nb = length (b)
		if (block.by.right)
		{	a = rep (a, times=nb)
			b = rep (b, each=na)
		}
		else
		{	a = rep (a, each=nb)
			b = rep (b, times=na)
		}
	}
	.mvmult.pw (,a, b)
}

.mvmult.dat = function (post, tm, dat, type="std", block.by.subobjects=FALSE, pack=TRUE)
{	tm = .val.M (tm)
	if (type == "std")
	{	dat = .val.GM (dat)
		.mvmult.pw (post, tm, dat, pack)
	}
	else if (type == "nestc")
	{	dat = .val.G (dat)
		if (block.by.subobjects)
		{	ndat = length (dat)
			for (k in 1:ndat)
				dat [[k]] = .mvmult.pw (post, tm, dat [[k]])
			dat
		}
		else
		{	gv = GeomArray (1, D=dat@D)
			gv [[1]] = dat
			.mvmult.pw (post, tm, gv)
		}
	}
	else if (type == "rollc")
	{	dat = .val.GM (dat)
		ntm = length (tm)
		ndat = length (dat)
		if (block.by.subobjects)
		{	tm = rep (tm, times=ndat)
			dat = rep (dat, each=ntm)
		}
		else
		{	tm = rep (tm, each=ndat)
			dat = rep (dat, times=ntm)
		}
		.mvmult.pw (post, tm, dat, pack)
	}
	else if (type == "vec")
	{	dat = .val.m1 (dat, post)
		if (post)
			.join.rows (.mvmult.pw (post, tm, .split.into.rows (dat), pack, FALSE) )
		else
			.join.cols (.mvmult.pw (post, tm, .split.into.cols (dat), pack, FALSE) )
	}
	else stop ("invalid input")
}

.mvmult.pre = function (a, bdat, ...) .mvmult.dat (FALSE, a, bdat, ...)
.mvmult.post = function (adat, b, ...) .mvmult.dat (TRUE, b, adat, ...)

.join.rows = function (mv)
{	n = length (mv)
	x = matrix (0, n, ncol (mv [[1]]) )
	for (i in 1:n)
		x [i,] = mv [[i]]
	x
}

.join.cols = function (mv)
{	n = length (mv)
	x = matrix (0, nrow (mv [[1]]), n)
	for (j in 1:n)
		x [,j] = mv [[j]]
	x
}

.split.into.rows = function (x)
{	n = nrow (x)
	vm = MatrixArray (n, NR=NA, NC=NA)
	for (i in 1:n)
		vm [[i]] = x [i,, drop=FALSE]
	vm
}

.split.into.cols = function (x)
{	n = ncol (x)
	vm = MatrixArray (n, NR=NA, NC=NA)
	for (j in 1:n)
		vm [[j]] = x [,j, drop=FALSE]
	vm
}
