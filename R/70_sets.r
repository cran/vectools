#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

n.unique = function (v)
	length (unique (v) )
n.duplicated = function (v)
	length (v) - n.unique (v)

which.unique = function (v)
	which (! duplicated (v) )

usv = function (v)
	sort (unique (v) )

cprod = function (..., sets = list (...), gby.last=FALSE, as.list=FALSE)
{	N = length (sets)

	if (N > 1)
	{	n = integer (N)
		for (k in 1:N)
			n [k] = length (sets [[k]])

		neach = ntimes = rep (1, N)

		for (k in 1:(N - 1) )
			neach [k] = prod (n [(k + 1):N])
		for (k in 2:N)
			ntimes [k] = prod (n [1:(k - 1)])

		if (gby.last)
		{	for (k in 1:N)
				sets [[k]] = rep (sets [[k]], each = ntimes [k], times = neach [k])
		}
		else
		{	for (k in 1:N)
				sets [[k]] = rep (sets [[k]], each = neach [k], times = ntimes [k])
		}
	}

	if (as.list)
		sets
	else
	{	strs = names (sets)
		v = .list2array (sets)
		colnames (v) = strs
		v
	}
}
