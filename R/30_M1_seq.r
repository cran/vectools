#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.int.ok = function (int.ok)
{	ret.type="I"
	if (int.ok)
		ret.type="NMP"
	ret.type
}

.which.ms = function (mins, maxs, x, ret.type, endpoints)
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

	if (ret.type == "I")
	{	if (all (ints [,1] == ints [,2]) )
			as.integer (ints [,1])
		else
			stop ("\nret.type is I, but optimal (multi-index) regions\n(consider setting ret.type to NMP or MP)")
	}
	else if (ret.type == "intervals")
	{	mode (ints) = "integer"
		colnames (ints) = c ("a", "b")
		ints
	}
	else
	{	mp = (ints [,1] + ints [,2]) / 2
		if (ret.type == "NMP" || ret.type == "NMP-")
			as.integer (floor (mp) )
		else if (ret.type == "NMP+")
			as.integer (ceiling (mp) )
		else if (ret.type == "MP")
			as.numeric (mp)
		else
			stop ("unsupported ret.type value")
	}
}

n.unique = function (x)
	length (unique (x) )
n.duplicated = function (x)
	length (x) - n.unique (x)

which.unique = function (x)
	which (! duplicated (x) )

usv = function (x)
	sort (unique (x) )

is.increasing = function (x)
	all (diff (x) > 0)
is.decreasing = function (x)
	all (diff (x) < 0)

is.nondecreasing = function (x)
	all (diff (x) >= 0)
is.nonincreasing = function (x)
	all (diff (x) <= 0)

is.each.unique = function (x)
	(n.unique (x) == length (x) )
is.each.equal = function (x)
{	n = length (x)
	if (n == 0 || n == 1) TRUE
	else all (x [1] == x [-1])
}

which.mins = function (x, ..., ret.type="I", endpoints=FALSE)
	.which.ms (TRUE, FALSE, x, ret.type, endpoints)
which.maxs = function (x, ..., ret.type="I", endpoints=FALSE)
	.which.ms (FALSE, TRUE, x, ret.type, endpoints)
which.opts = function (x, ..., ret.type="I", endpoints=FALSE)
	.which.ms (TRUE, TRUE, x, ret.type, endpoints)

mins = function (x, ..., intervals.ok=FALSE, endpoints=FALSE)
	x [which.mins (x, ret.type = .int.ok (intervals.ok), endpoints=endpoints)]
maxs = function (x, ..., intervals.ok=FALSE, endpoints=FALSE)
	x [which.maxs (x, ret.type = .int.ok (intervals.ok), endpoints=endpoints)]
opts = function (x, ..., intervals.ok=FALSE, endpoints=FALSE)
	x [which.opts (x, ret.type = .int.ok (intervals.ok), endpoints=endpoints)]

midpoints = function (x)
{	n = length (x)
	(x [-n] + x [-1]) / 2
}
endpoints = function (x)
{	n = length (x)
	c (x [1], x [n])
}
