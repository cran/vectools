#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.rep.ObjectArray = function (x, each, times, ret.type="OA", ..., D)
{	if (x@N == 1)
	{	if (each < 1 || times < 1)
			stop ("n needs to be positive")
		nx = length (x)
		I = rep (1:nx, each=each, times=times)
		nv = nx * each * times
		if (ret.type == "OA") v = ObjectArray (nv, CLASS=x@CLASS)
		else if (ret.type == "ImV") v = ImageArray (nv)
		else if (ret.type == "GV") v = GeomArray (nv, D=D)
		else if (ret.type == "MV") v = MatrixArray (nv, NR=NA, NC=NA)
		else stop ("invalid input")
		v@data = x@data [I]
		v
	}
	else
		stop ("rep method can only be called on 1-dim objects")
}

.rep2.ObjectArray = function (x, n.out, ret.type="OA", D)
{	if (x@N == 1)
	{	if (n.out < 1)
			stop ("n needs to be positive")
		if (ret.type == "OA") v = ObjectArray (n.out, CLASS=x@CLASS)
		else if (ret.type == "ImV") v = ImageArray (n.out)
		else if (ret.type == "GV") v = GeomArray (n.out, D=D)
		else if (ret.type == "MV") v = MatrixArray (n.out, NR=NA, NC=NA)
		else stop ("invalid input")
		for (i in 1:n.out)
			v [[i]] = x [[(i - 1) %% x@n + 1]]
		v
	}
	else
		stop ("rep2 method can only be called on 1-dim objects")
}

c.ObjectArray = function (...)
{	u = list (...)
	n = length (u)
	CLASSES = character (n)
	classes = character (n)
	v = vector ("list", n)
	for (i in seq_len (n) )
	{	N = u [[i]]@N
		if (N != 1)
			stop ("currently, only 1-dim objects supported")
		CLASSES [i] = CLASS (u [[i]])
		classes [i] = class (u [[i]])[1]
		v [[i]] = u [[i]]@data
	}
	v = unlist (v, FALSE)
	
	if (is.each.equal (classes) )
	{	class = classes [1]
		if (class == "ObjectArray")
		{	if (is.each.equal (CLASSES) )
				CLASS = CLASS [1]
			else
				CLASS = "<OBJECT>"
			as.ObjectArray (v, CLASS=CLASS)
		}
		else if (class == "ImageArray")
			as.ImageArray (v)
		else if (class == "GeomArray")
			as.GeomArray (v)
		else if (class == "MatrixArray")
			as.MatrixArray (v, NR=NA, NC=NA)
		else
		{	warning ("unknown ObjectArray subclasses")
			as.ObjectArray (v)
		}
	}
	else
	{	warning ("ObjectArray(s) of different classes")
		as.ObjectArray (v)
	}
}

rep.ObjectArray = function (x, n=1, ..., each=1, times=n) .rep.ObjectArray (x, each, times)
rep2.ObjectArray = function (v, n, ..., n.out=n) .rep2.ObjectArray (v, n.out)

rep.ImageArray = function (x, n=1, ..., each=1, times=n) .rep.ObjectArray (x, each, times, "ImV")
rep.GeomArray = function (x, n=1, ..., each=1, times=n) .rep.ObjectArray (x, each, times, "GV", D = x [[1]]@D)
rep.MatrixArray = function (x, n=1, ..., each=1, times=n) .rep.ObjectArray (x, each, times, "MV")

rep2.ImageArray = function (v, n, ..., n.out=n) .rep2.ObjectArray (v, n.out, "ImV")
rep2.GeomArray = function (v, n, ..., n.out=n) .rep2.ObjectArray (v, n.out, "GV", D = v [[1]]@D)
rep2.MatrixArray = function (v, n, ..., n.out=n) .rep2.ObjectArray (v, n.out, "MV")
