/**
 * Copyright � 2017 Lhasa Limited
 * File created: 17 Oct 2017 by ThomasB
 * Creator : ThomasB
 * Version : $Id$
 */
package generation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javafx.geometry.Bounds;
import javafx.geometry.Point2D;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.effect.BoxBlur;
import javafx.scene.paint.Color;

/**
 * @author ThomasB
 * @since 17 Oct 2017
 */
public class SudokuGrid
{
	private Point2D upperLeftcorner;

	private final double sideLength;

	private final double outerLineWidth, sectionLineWidth, gridLineWidth;

	private final double outerLineColor, sectionLineColor, gridLineColor;

	private int nEntries = 5;

	private String fontFamily = "Arial";

	public SudokuGrid(
			final Point2D upperLeftcorner,
			final double sideLength,
			final double outerLineWidth,
			final double sectionLineWidth,
			final double gridLineWidth,
			final double outerLineColor,
			final double sectionLineColor,
			final double gridLineColor)
	{
		this.upperLeftcorner = upperLeftcorner;
		this.sideLength = sideLength;
		this.outerLineWidth = outerLineWidth;
		this.sectionLineWidth = sectionLineWidth;
		this.gridLineWidth = gridLineWidth;
		this.outerLineColor = outerLineColor;
		this.sectionLineColor = sectionLineColor;
		this.gridLineColor = gridLineColor;
	}

	private List<Point2D> getCorners()
	{
		final Point2D ul = upperLeftcorner;
		return Arrays.asList(
				new Point2D(ul.getX() + sideLength, ul.getY()),
				new Point2D(ul.getX() + sideLength, ul.getY() + sideLength),
				new Point2D(ul.getX(), ul.getY() + sideLength),
				ul);
	}

	private List<Line> getSectionLines()
	{
		final int widthShift = (int) (sectionLineWidth - outerLineWidth);

		final Point2D ul = upperLeftcorner;
		final double thirdSL = sideLength/3;
		final List<Point2D> upper = Arrays.asList(ul.add(thirdSL, 0), ul.add(2*thirdSL, 0));
		final List<Point2D> left = Arrays.asList(ul.add(0, thirdSL), ul.add(0, 2*thirdSL));

		final List<Line> sLines = new ArrayList<>();
		sLines.addAll(upper.stream().map(p -> new Line(p.add(0, widthShift), p.add(0, sideLength - widthShift))).collect(Collectors.toList()));
		sLines.addAll(left.stream().map(p -> new Line(p.add(widthShift, 0), p.add(sideLength - widthShift, 0))).collect(Collectors.toList()));
		return sLines;
	}

	public boolean isPositive(final Bounds canvasBounds)
	{
		final double shift = sideLength + outerLineWidth/2;
		return canvasBounds.contains(upperLeftcorner.add(shift, shift));
	}

	private List<Line> getGridLines()
	{
		final Point2D ul = upperLeftcorner;
		final double nineSL = sideLength/9;

		final List<Point2D> upper = Arrays.asList(
				ul.add(nineSL, 0),
				ul.add(2*nineSL, 0),
				ul.add(4*nineSL, 0),
				ul.add(5*nineSL, 0),
				ul.add(7*nineSL, 0),
				ul.add(8*nineSL, 0));

		final List<Point2D> left = Arrays.asList(
				ul.add(0, nineSL),
				ul.add(0, 2*nineSL),
				ul.add(0, 4*nineSL),
				ul.add(0, 5*nineSL),
				ul.add(0, 7*nineSL),
				ul.add(0, 8*nineSL));

		final List<Line> gLines = new ArrayList<>();
		gLines.addAll(upper.stream().map(p -> new Line(p, p.add(0, sideLength))).collect(Collectors.toList()));
		gLines.addAll(left.stream().map(p -> new Line(p, p.add(sideLength, 0))).collect(Collectors.toList()));
		return gLines;
	}

	public void translate(final double dx, final double dy)
	{
		upperLeftcorner = upperLeftcorner.add(dx, dy);
	}

	public void draw(final GraphicsContext gc)
	{
		// Draw the gridLines
		gc.setStroke(Color.gray(gridLineColor));
		gc.setLineWidth(gridLineWidth);
		final List<Line> gLines = getGridLines();

		gLines.stream().forEach(x ->
		{
			x.draw(gc, false);
		});

		// Draw the sectionLines
		gc.setStroke(Color.gray(sectionLineColor));
		gc.setLineWidth(sectionLineWidth);
		final List<Line> sLines = getSectionLines();

		sLines.stream().forEach(x ->
		{
			x.draw(gc, false);
		});

		// Draw the outline
		final List<Point2D> corners = getCorners();
		gc.setStroke(Color.gray(outerLineColor));
		gc.setLineWidth(outerLineWidth);
		gc.beginPath();
		gc.moveTo(upperLeftcorner.getX(), upperLeftcorner.getY());
		corners.stream().forEach(p -> gc.lineTo(p.getX(), p.getY()));
		gc.stroke();
		gc.closePath();

		gc.setFill(Color.RED);
		gc.fillOval(40, 40, 10, 10);

	}


	private void drawLine(final GraphicsContext gc, final Line line, final boolean blurred)
	{

	}
}

class Line
{
	private static final double TOL = 0.00001;

	private static final int PX_SHIFT = 4;


	final Point2D from, to;

	Line(final Point2D from, final Point2D to)
	{
		this.from = from;
		this.to = to;
	}

	void draw(final GraphicsContext gc, final boolean blurred)
	{
		//		int xshift1 = isVertical() ? PX_SHIFT : 0;
		//		int yshift = isVertical() ? 0 : PX_SHIFT;
		//
		//		Color prevFill = (Color) gc.getStroke();
		//		gc.setStroke(prevFill.interpolate(Color.WHITE, 0.75));
		//		gc.beginPath();
		//		gc.moveTo(from.getX() + xshift1, from.getY() + yshift);
		//		gc.lineTo(to.getX() + xshift1, to.getY() + yshift);
		//		gc.stroke();
		//		gc.closePath();
		//		gc.setStroke(prevFill);

		if (blurred)
		{
			final BoxBlur blurEffect = new BoxBlur(PX_SHIFT, PX_SHIFT, 5);
			gc.setEffect(blurEffect);
		}

		gc.beginPath();
		gc.moveTo(from.getX(), from.getY());
		gc.lineTo(to.getX(), to.getY());
		gc.stroke();
		gc.closePath();

		gc.setEffect(null);
	}

	boolean isVertical()
	{
		return Math.abs(to.getX() - from.getX()) < TOL;
	}
}

/* ---------------------------------------------------------------------*
 * This software is the confidential and proprietary
 * information of Lhasa Limited
 * Granary Wharf House, 2 Canal Wharf, Leeds, LS11 5PS
 * ---
 * No part of this confidential information shall be disclosed
 * and it shall be used only in accordance with the terms of a
 * written license agreement entered into by holder of the information
 * with LHASA Ltd.
 * --------------------------------------------------------------------- */