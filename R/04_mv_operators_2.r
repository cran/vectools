#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

########################################
#pre-mult versions
#(right-side data object)
########################################
"%*[%" = function (a, b) .mvmult.pre (a, b)
"%*{%" = function (a, b) .mvmult.pre (a, b, "rollc")
"%*|%" = function (a, b) .mvmult.pre (a, b, "vec")
"%*||%" = function (a, b) .mvmult.pre (a, b, "vec", pack=FALSE)

########################################
#post-mult versions
#(left-side data object)
########################################
"%]*%" = function (a, b) .mvmult.post (a, b)
"%}*%" = function (a, b) .mvmult.post (a, b, "rollc")
"%|*%" = function (a, b) .mvmult.post (a, b, "vec")
"%||*%" = function (a, b) .mvmult.post (a, b, "vec", pack=FALSE)

########################################
#standard functions
########################################
vt3.mmult = function (a, b)
	.mvmult.ext (a, b)

vt3.mmultc = function (a, b, ..., gby.rhs=FALSE)
	.mvmult.ext (a, b, "rollc", gby.rhs)

vt3.gmult.pre = function (a, b)
	.mvmult.pre (a, b)

vt3.gmultc.pre = function (a, b, ..., gby.trans=TRUE, rolling=TRUE)
{	type = "nestc"
	if (rolling)
		type = "rollc"
	.mvmult.pre (a, b, type, ! gby.trans)
}

vt3.cmult.pre = function (a, b, ..., pack=TRUE)
	.mvmult.pre (a, b, "vec", pack=pack)

vt3.gmult.post = function (a, b)
	.mvmult.post (a, b)

vt3.gmultc.post = function (a, b, ..., gby.trans=TRUE, rolling=TRUE)
{	type = "nestc"
	if (rolling)
		type = "rollc"
	.mvmult.post (a, b, type, ! gby.trans)
}

vt3.rmult.post  = function (a, b, pack=TRUE)
	.mvmult.post (a, b, "vec", pack=pack)

########################################
#object-level multiplication
########################################
.grid2matrix = function (g)
{	if (g@D == 2)
		matrix (cbind (g@x, g@y),, 2)
	else
		matrix (cbind (g@x, g@y, g@gv),, 3)
}

.matrix2grid = function (g, m)
{	g@x = matrix (m [,1], g@nr, g@nc)
	g@y = matrix (m [,2], g@nr, g@nc)
	if (ncol (m) == 3)
		g@gv = matrix (m [,3], g@nr, g@nc)
	g
}

.mvmult0.bdat = function (a, b)
{	if (is.matrix (b) )
	{	nr = nrow (b)
		(a %*% rbind (b, 1) )[1:nr,, drop=FALSE]
	}
	else if (is.Points (b) || is.Line (b) || is.Polygon (b) || is.Text (b) )
	{	b@data = t ( (a %*% rbind (t (b@data), 1) )[1:b@D,, drop=FALSE])
		b
	}
	else if (is.Grid (b) )
		.matrix2grid (b, t ( (a %*% rbind (t (.grid2matrix (b) ), 1) )[1:b@D,]) )
	else if (is.GeomArray (b) )
		a %*[% b
	else
		stop ("invalid input")
}

.mvmult0.adat = function (a, b)
{	if (is.matrix (a) )
	{	nc = ncol (a)
		(cbind (a, 1) %*% b)[,1:nc, drop=FALSE]
	}
	else if (is.Points (a) || is.Line (a) || is.Polygon (a) || is.Text (a) )
	{	a@data = (cbind (a@data, 1) %*% b)[,1:a@D, drop=FALSE]
		a
	}
	else if (is.Grid (a) )
		.matrix2grid (a, (cbind (.grid2matrix (a), 1) %*% b)[,1:a@D])
	else if (is.GeomArray (a) )
		a %]*% b
	else
		stop ("invalid input")
}
