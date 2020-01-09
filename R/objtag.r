#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.dimstr = function (x)
	paste (dim (x), collapse="x")

.objtag = function (prefix, n = length (x), x)
	paste ("<", prefix, " ", n, ">", sep="")

objtag.default = function (x, ...)
{	if (is.null (x) )
		""
	else
	{	y = x
		attributes (y) = NULL
		if (is.vector (y) )
		{	if (length (y) == 1)
			{	if (is.character (y) )
				{	y = trimws (y)
					if (nchar (y) <= 6)
						y
					else
						paste (substring (y, 1, 4), "..", sep="")
				}
				else
					y
			}
			else
				.objtag ("v",, y)
		}
		else
			"*"
	}
}

objtag.ObjectArray = function (x, ...) .objtag ("OA", .dimstr (x) )
objtag.NestMatrix = function (x, ...) .objtag ("NM", .dimstr (x) )
objtag.SectMatrix = function (x, ...) .objtag ("SM", .dimstr (x) )
objtag.PartMatrix = function (x, ...) .objtag ("PM", .dimstr (x) )

objtag.function = function (x, ...) "<f>"
objtag.list = function (x, ...) .objtag ("l",, x)
objtag.matrix = function (x, ...) .objtag ("m", .dimstr (x) )
objtag.data.frame = function (x, ...) .objtag ("df", .dimstr (x) )
