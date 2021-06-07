#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

ghead.data.frame = function (v, nh=3, ..., gname)
{	if (missing (gname) )
	{	nv = length (v)
		classes = character (nv)
		for (j in 1:nv)
		{	classes [j] = class (v [[j]])[1]
			if (is.each.unique (v [[j]]) )
				classes [j] = ""
		}
		J = (1:nv)[classes == "character" | classes == "factor"]
		if (length (J) == 0)
			stop ("no non-unique character or factor types, group.by needed")
		g = v [[J [1] ]]
	}
	else
		g = v [,gname]
	u = unique (g)
	nu = length (u)
	v = .val.names (v)
	Rb = numeric ()
	nr = 0
	ns = y = list ()
	for (i in seq_len (nu) )
	{	sub = head (v [u [i] == g,], nh, ...)
		ns [[i]] = rownames (sub)
		y [[i]] = sub
		nr = nr + nrow (sub)
		if (i < nu)
			Rb = c (Rb, nr)
	}
	z = PartMatrix (nr, ncol (v), Rb, cnames=colnames (v), default.value="")
	z@header = TRUE
	for (i in seq_len (nu) )
		z [[i, 1]] = .as.matrix (y [[i]])
	noquote (format (z) )
}
