#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

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

objtag.default = function (v, ...)
{	if (is.null (v) )
		"."
	else if (isS4 (v) )
		"<S4>"
	else
	{	attributes (v) = NULL
		if (.is.vector (v) )
		{	if (length (v) == 1)
			{	if (is.character (v) )
				{	v = trimws (v)
					if (nchar (v) <= 6)
						v
					else
						paste (substring (v, 1, 4), "..", sep="")
				}
				else
					as.character (v)
			}
			else
				.objtag ("v",, v)
		}
		else
			"!"
	}
}

objtag.Zero = function (v, ...) "<0>"
objtag.GeomObject = function (v, ...) "<G>"
objtag.RImage = function (im, ...) "<Im>"

objtag.ObjectArray = function (v, ...) .objtag ("OA", .dimstr (v) )
objtag.NestMatrix = function (v, ...) .objtag ("NM", .dimstr (v) )
objtag.SectMatrix = function (v, ...) .objtag ("SM", .dimstr (v) )
objtag.PartMatrix = function (v, ...) .objtag ("PM", .dimstr (v) )
objtag.ImageArray = function (im, ...) .objtag ("vIm", .dimstr (im) )
objtag.GeomArray = function (v, ...) .objtag ("vG", .dimstr (v) )
objtag.MatrixArray = function (v, ...) .objtag ("vM", .dimstr (v) )

objtag.function = function (v, ...) "<f>"
objtag.list = function (v, ...) .objtag ("l",, v)
objtag.matrix = function (v, ...) .objtag ("m", .dimstr (v) )
objtag.data.frame = function (v, ...) .objtag ("df", .dimstr (v) )
