/**
 *
 */
package generation;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import javax.imageio.ImageIO;

import javafx.application.Application;
import javafx.embed.swing.SwingFXUtils;
import javafx.geometry.Point2D;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.Region;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

/**
 * @author t
 *
 */
public class Play01 extends Application
{
	static final Path OUTFOLDER_PATH = Paths.get("generatedpics");
	static final String PNG_EXT = ".png";

	private static final int SQUARE_PX_LEN = 512;

	private static final Color MAX_GREYSCALE = Color.gray(1);

	private SRegion canvas;
	private GraphicsContext gc;


	/* (non-Javadoc)
	 * @see javafx.application.Application#start(javafx.stage.Stage)
	 */
	@Override
	public void start(final Stage primaryStage) throws Exception
	{
		canvas = new SRegion();


		final Group g = new Group(Arrays.asList(canvas));
		primaryStage.setScene(new Scene(g, SRegion.SIZE, SRegion.SIZE));
		//		primaryStage.show();
		//		gc.fillRect(34, 34, 32, 32);
		//		drawPositive(new Point2D(250, 240), 9, 0.5);
		saveSnapshot("rottest1");
	}

	private void drawPositive(final Point2D centre, final double lw, final double color)
	{
		gc.clearRect(0, 0, SQUARE_PX_LEN, SQUARE_PX_LEN);
		gc.setLineWidth(lw);
		gc.setStroke(Color.gray(color));
		gc.beginPath();
		gc.moveTo(0, centre.getY());
		gc.lineTo(centre.getX(), centre.getY());
		gc.lineTo(centre.getX(), 0);
		gc.stroke();
		gc.closePath();
	}

	private void saveSnapshot(final String fileName)
	{
		final WritableImage wi = new WritableImage(SRegion.SIZE, SRegion.SIZE);
		canvas.snapshot(null, wi);

		final File f = Paths.get(OUTFOLDER_PATH.toString(), fileName + PNG_EXT).toFile();
		try
		{
			ImageIO.write(SwingFXUtils.fromFXImage(wi, null), "png", f);
		}
		catch (final Exception s) {
		}
	}


	/**
	 * @param args
	 */
	public static void main(final String[] args)
	{
		launch(args);
	}
}

class SRegion extends Region
{

	static final int SIZE = 1500, GRID_SIZE = 800;
	private static final int XSHIFT = 500, YSHIFT = 500;
	private static final double ROT = 20;

	private static final Color BACKING = Color.gray(0.95);

	private Canvas backing = new Canvas(SIZE, SIZE);
	private Canvas grid = new Canvas(GRID_SIZE, GRID_SIZE);
	private SudokuGrid sgrid = new SudokuGrid(new Point2D(0, 0), GRID_SIZE, 8, 4, 1, 0.4, 0.2, 0.6);

	public SRegion()
	{
		setPrefSize(SIZE, SIZE);
		getChildren().addAll(Arrays.asList(backing, grid));
		backing.getGraphicsContext2D().setFill(BACKING);
		backing.getGraphicsContext2D().fillRect(0, 0, SIZE, SIZE);
		sgrid.draw(grid.getGraphicsContext2D());

	}

	@Override
	protected void layoutChildren()
	{
		autosize();
		backing.relocate(0, 0);
		grid.relocate(XSHIFT, YSHIFT);
		grid.setRotate(ROT);
	}
}
