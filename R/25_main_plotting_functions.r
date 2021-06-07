#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.init.graphics = function (gsys, g, add, xlim, ylim, ..., axs="r")
{	if (! add)
	{	if (missing (xlim) || missing (ylim) )
		{	rngs = range (g)

			if (missing (xlim) )
				xlim = rngs [1,]
			if (missing (ylim) )
				ylim = rngs [2,]
		}

		if (gsys == "graphics")
			.base.graphics.init (xlim, ylim, axs)
		else if (gsys == "grid")
			.grid.graphics.init (xlim, ylim)
		else
			stop ("unsupported graphics system")
	}
}

.init.RImage = function (gsys, x, y, add, vflip.yaxis=FALSE)
{	if (vflip.yaxis)
		y = rev (y)
	.init.graphics (gsys, NULL, add, x, y)
}

.projectif = function (g, proj, sort)
{	if (g@D == 2)
		g
	else
		vt3.proj (g, proj, sort=sort)
}

.get.write.function = function (gsys, base.bf, grid.bf)
{	if (gsys == "graphics")
		base.bf
	else if (gsys == "grid")
		grid.bf
	else
		stop ("unsupported graphics system")
}

plotv.GeomArray = function (v, ..., gsys="graphics", add=FALSE, proj="xy", sort=TRUE)
{	vg = .projectif (v, proj, sort)

	.init.graphics (gsys, vg, add)
	for (i in seq_len (length (vg) ) )
	{	if (is.null (vg [[i]]) )
			warning ("GeomArray contains null values.")
		else
			plot (vg [[i]], add=TRUE, gsys=gsys)
	}
}

plotv.Points = function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	.get.write.function (gsys, .base.bf.points, .grid.bf.points)(p@data [,1], p@data [,2], glist=p@glist)
}

plotv.Line = function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	.get.write.function (gsys, .base.bf.line, .grid.bf.line)(p@data [,1], p@data [,2], glist=p@glist)
}

plotv.Polygon = function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	.get.write.function (gsys, .base.bf.polygon, .grid.bf.polygon)(p@data [,1], p@data [,2], glist=p@glist)
}

plotv.Text = function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	.get.write.function (gsys, .base.bf.text, .grid.bf.text)(p@data [,1], p@data [,2], p@text, glist=p@glist)
}

plotv.Grid = function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	wf = .get.write.function (gsys, .base.bf.polygon, .grid.bf.polygon)

	for (i in 1:(p@nr - 1) )
	{	for (j in 1:(p@nc - 1) )
		{	xsub = p@x [c (i, i + 1), c (j, j + 1)][c (1, 3, 4, 2)]
			ysub = p@y [c (i, i + 1), c (j, j + 1)][c (1, 3, 4, 2)]

			if (is.null (p@vlist) )
				wf (xsub, ysub, glist=p@glist)
			else
				wf (xsub, ysub, glist = list (p@glist, .sub.vlist (p@vlist, i, j) ) )
		}
	}
}

plotv.VImage= function (v, ..., gsys="graphics", add=FALSE, proj="xy")
{	p = .projectif (v, proj)

	.init.graphics (gsys, p, add)
	wf = .get.write.function (gsys, .base.bf.cpanel, .grid.bf.cpanel)

	for (i in 1:(p@nr - 1) )
	{	for (j in 1:(p@nc - 1) )
		{	xsub = p@x [c (i, i + 1), c (j, j + 1)][c (1, 3, 4, 2)]
			ysub = p@y [c (i, i + 1), c (j, j + 1)][c (1, 3, 4, 2)]
			wf (xsub, ysub, p@opaque, p@colm [i, j])
		}
	}
}

.sub.vlist = function (u, i, j)
{	n = length (u)
	v = vector ("list", n)
	names (v) = names (u)
	for (k in seq_len (n) )
		v [[k]] = u [[k]][i, j]
	v
}

plotv.RImage = function (im,  ...)
	plot_RImage (im, ...)

plotv.ImageArray = function (im, ..., rby=TRUE, add=FALSE)
{	if (! add)
	{	if (im@N == 1)
			p0 = .init.image.board1 (im@n, rby=rby)
		else if (im@N == 2)
			p0 = init.image.board (im@n [1], im@n [2])
		else
			stop ("need to initialize plot first, for high-dim ImageArray")
	}
	for (im1 in im@data)
	{	if (is.null (im1) )
			plot.new ()
		else
			plot_RImage (im1, ...)

	}
	if (! add)
		par (p0)
}

plot_RImage = function (im, ..., gsys="graphics",
	xlim = c (0, 1), ylim = c (0, 1),
	orient="r",
	add=FALSE, interpolate=FALSE)
{	im = as.raster (im, ...)
	if (orient == "v")
	{	.init.RImage (gsys, xlim, ylim, add, FALSE)
		im = t (im)
		vflip.image = TRUE
	}
	else if (orient == "r")
	{	.init.RImage (gsys, xlim, ylim, add, TRUE)
		vflip.image = TRUE
	}
	else if (orient == "r.flip")
	{	.init.RImage (gsys, xlim, ylim, add, FALSE)
		vflip.image = FALSE
	}
	else
		stop ('orientation (orient) required, "v", "r" or "r.flip"')

	wf = .get.write.function (gsys, .base.bf.raster, .grid.bf.raster)
	wf (xlim, ylim, im, vflip.image, interpolate)
}
