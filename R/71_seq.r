#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.int.ok = function (int.ok)
{	what="I"
	if (int.ok)
		what="NMP"
	what
}

.which.ms = function (mins, maxs, x, what, endpoints)
{	endpoints = rep_len (endpoints, 2)

	n = length (x)
	I = 1:(n - 1)
	sdx = sign (diff (x) )

	non0 = (sdx != 0)
	I = I [non0]
	sdx = sdx [non0]

	N = length (sdx)
	s = rep (FALSE, N - 1)
	a = sdx [-N]
	b = sdx [-1]

	if (mins)	s = (s | (a == -1 & b == 1) )
	if (maxs) s  = (s | (a == 1 & b == -1) )
	J = which (s)
	ints = cbind (a = I [J] + 1, b = I [J + 1])

	if (endpoints [1])
	{	if ( (mins && a [1] == 1) || (maxs && a [1] == -1) )
			ints = rbind (c (1, I [1]), ints)
	}
	if (endpoints [2])
	{	if ( (mins && b [N - 1] == -1) || (maxs && b [N - 1] == 1) )
			ints = rbind (ints, c (I [N] + 1L, n) )
	}

	.resolve.intervals (what, ints)
}

.which.intervals = function (int.type, x, what, endpoints=TRUE)
{	endpoints = rep_len (endpoints, 2)
	if (what == "I")
		stop ('what="I" not supported')

	n = length (x)
	I = 1:(n - 1)
	sdx = sign (diff (x) )
	if (int.type == "00")
		sdx = abs (sdx)

	K = c (TRUE, sdx [1:(n - 2)] != sdx [2:(n - 1)])
	I = c (I [K], n)
	sdx = sdx [K]

	nsub = length (I)
	if (! endpoints [1])
	{	nsub = nsub - 1
		I = I [-1]
		sdx = sdx [-1]
	}
	if (! endpoints [2])
	{	if (nsub > 0)
		{	nsub = nsub - 1
			sdx = sdx [-nsub]
		}
	}

	if (nsub == 0)
		ints = matrix (0, 0, 2)
	else
	{	if (int.type == "0")
			J = which (sdx == 0)
		else if (int.type == "00")
			J = which (sdx == 1)
		else if (int.type == "+")
			J = which (sdx == 1)
		else if (int.type == "-")
			J = which (sdx == -1)
		else if (int.type == "0+")
			J = which (sdx >= 0)
		else if (int.type == "0-")
			J = which (sdx <= 0)
		else
			stop ("unsupported int.type")
		a = I [J]
		b = I [J + 1]
		ints = cbind (a, b)
	}
	.resolve.intervals (what, ints)
}

.resolve.intervals = function (what, ints)
{	if (what == "I")
	{	if (all (ints [,1] == ints [,2]) )
			as.integer (ints [,1])
		else
			stop ('\nwhat is "I" (for single-index) sections\nbut there are multi-index optimal sections\n(consider setting what to "NMP" or "XMP")')
	}
	else if (what == "intervals")
	{	mode (ints) = "integer"
		colnames (ints) = c ("a", "b")
		ints
	}
	else
	{	mp = (ints [,1] + ints [,2]) / 2
		if (what == "NMP" || what == "NMP-")
			as.integer (floor (mp) )
		else if (what == "NMP+")
			as.integer (ceiling (mp) )
		else if (what == "XMP")
			as.numeric (mp)
		else if (what == "first")
			ints [,1]
		else if (what == "last")
			ints [,2]
		else
			stop ("unsupported what value")
	}
}

is.sttincreasing = function (v)
	all (diff (v) > 0)
is.sttdecreasing = function (v)
	all (diff (v) < 0)

is.nondecreasing = function (v)
	all (diff (v) >= 0)
is.nonincreasing = function (v)
	all (diff (v) <= 0)

is.each.unique = function (v)
	(n.unique (v) == length (v) )
is.each.equal = function (v)
{	n = length (v)
	if (n == 0 || n == 1) TRUE
	else all (v [1] == v [-1])
}

which.mins = function (v, ..., what="I", ends=FALSE)
	.which.ms (TRUE, FALSE, v, what, ends)
which.maxs = function (v, ..., what="I", ends=FALSE)
	.which.ms (FALSE, TRUE, v, what, ends)
which.opts = function (v, ..., what="I", ends=FALSE)
	.which.ms (TRUE, TRUE, v, what, ends)

which.level = function (v, ..., what="intervals")
	.which.intervals ("0", v, what)
which.nonlevel = function (v, ..., what="intervals")
	.which.intervals ("00", v, what)
which.increasing = function (v, ..., what="intervals")
	.which.intervals ("+", v, what)
which.decreasing = function (v, ..., what="intervals")
	.which.intervals ("-", v, what)
which.nondecreasing = function (v, ..., what="intervals")
	.which.intervals ("0+", v, what)
which.nonincreasing = function (v, ..., what="intervals")
	.which.intervals ("0-", v, what)

mins = function (v, ..., allow.intervals=FALSE, ends=FALSE)
	v [which.mins (v, what = .int.ok (allow.intervals), ends=ends)]
maxs = function (v, ..., allow.intervals=FALSE, ends=FALSE)
	v [which.maxs (v, what = .int.ok (allow.intervals), ends=ends)]
opts = function (v, ..., allow.intervals=FALSE, ends=FALSE)
	v [which.opts (v, what = .int.ok (allow.intervals), ends=ends)]

midpoints = function (v)
{	n = length (v)
	(v [-n] + v [-1]) / 2
}
endpoints = function (v)
{	n = length (v)
	c (v [1], v [n])
}
