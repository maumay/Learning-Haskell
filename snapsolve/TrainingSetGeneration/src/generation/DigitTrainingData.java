/**
 *
 */
package generation;

import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.imageio.ImageIO;

import org.apache.commons.lang3.SystemUtils;

import javafx.application.Application;
import javafx.embed.swing.SwingFXUtils;
import javafx.geometry.Bounds;
import javafx.geometry.Point2D;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.effect.BoxBlur;
import javafx.scene.effect.Effect;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.Region;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.text.TextBoundsType;
import javafx.stage.Stage;

/**
 * @author t
 *
 */
public class DigitTrainingData extends Application
{
	private static final Random R = new Random();
	private static final int PIC_SIZE_LEN = 32, FONT_SIZE = 25;
	private static final Path OUTPATH = Paths.get("..", "DigitTrainingData");
	private static final String PNG_EXT = ".png";

	private static final PicRegion DIGIT_DISPLAYER = new PicRegion(PIC_SIZE_LEN);
	private static final List<String> FONT_FAMILIES = getFontFamilies();//Font.getFamilies();

	private static final int[] BLUR_LEVELS = {1, 2};
	private static final int[] THICKNESS_LEVELS = {2, 3};

	private static final double[] BACK_COLORS = {0.94, 0.96, 0.98, 1};
	private static final double[] TEXT_COLORS = { 0.1, 0.15,  0.2, 0.25};

	/* (non-Javadoc)
	 * @see javafx.application.Application#start(javafx.stage.Stage)
	 */
	@Override
	public void start(final Stage primaryStage) throws Exception
	{
		final Scene scene = new Scene(DIGIT_DISPLAYER, PIC_SIZE_LEN, PIC_SIZE_LEN);
		primaryStage.setScene(scene);
		primaryStage.show();
//		System.out.println(OUTPATH.toFile().getCanonicalPath());
//		//		assert Files.exists(OUTPATH);
//
//		System.out.println(new Text().isResizable());
		generateSamples(8000);
////				generateFontSamples();
//		System.exit(0);
	}

	private static void generateSamples(final int nsamples) throws IOException
	{
		if (!Files.exists(OUTPATH))
		{
			OUTPATH.toFile().mkdir();
		}
		
		for (final DigitClass dclass : DigitClass.values())
		{
			final Path folderPath = Paths.get(OUTPATH.toString(), dclass.strRep.toLowerCase());
			if (Files.exists(folderPath))
			{
				for (final File f : folderPath.toFile().listFiles())
				{
					Files.delete(f.toPath());
				}
			}
			else
			{
				Files.createDirectory(folderPath);
			}

			for (int i = 0; i < nsamples; i++)
			{
				DIGIT_DISPLAYER.assignRandom(dclass);
				final String fname = "" + i + PNG_EXT;
				final File newFile = Paths.get(folderPath.toString(), fname).toFile();
				WritableImage wi = new WritableImage(PIC_SIZE_LEN, PIC_SIZE_LEN);
				wi = DIGIT_DISPLAYER.snapshot(null, wi);
				final BufferedImage bImage = SwingFXUtils.fromFXImage(wi, null);
				ImageIO.write(bImage, "png", newFile);
			}
		}

	}

	private void generateFontSamples() throws IOException
	{
		if (!Files.exists(OUTPATH))
		{
			OUTPATH.toFile().mkdir();
		}
		
		writeAllFontFamilies();
		final int nFonts = Font.getFamilies().size();
		for (final DigitClass dclass : DigitClass.values())
		{
			final Path folderPath = Paths.get(OUTPATH.toString(), dclass.name().toLowerCase());
			if (Files.exists(folderPath))
			{
				for (final File f : folderPath.toFile().listFiles())
				{
					Files.delete(f.toPath());
				}
			}
			else
			{
				Files.createDirectory(folderPath);
			}

			for (int i = 0; i < nFonts; i++)
			{
//				final String fname = "" + i + PNG_EXT;
//				final File newFile = Paths.get(folderPath.toString(), fname).toFile();
//				WritableImage wi = new WritableImage(PIC_SIZE_LEN, PIC_SIZE_LEN);
//				drawSpecifiedFont(dclass, Font.font(Font.getFamilies().get(i), FONT_SIZE));
//				wi = CANVAS.snapshot(null, wi);
//				final BufferedImage bImage = SwingFXUtils.fromFXImage(wi, null);
//				ImageIO.write(bImage, "png", newFile);
			}
		}
	}

	private void writeAllFontFamilies()
	{
		final List<String> fontFamilies = Font.getFamilies();
		for (int i = 0; i < fontFamilies.size(); i++)
		{
			System.out.println("Index: " + i + "\nFamily: " + fontFamilies.get(i));
		}
	}

	private static List<String> getFontFamilies()
	{
		String fontResourceLoc = "/fonts/";
		if (SystemUtils.IS_OS_WINDOWS_7)
		{
			fontResourceLoc += "win7fonts.txt";
		}
		else if (SystemUtils.IS_OS_LINUX)
		{
			fontResourceLoc += "linuxfonts.txt";
		}
		else
		{
			throw new AssertionError("Not yet impl");
		}
		
		final List<String> allFamilies = Font.getFamilies();
		final List<String> fams = new ArrayList<>();
		try
		{
			final BufferedReader fontIndexReader = new BufferedReader(new InputStreamReader(DigitTrainingData.class.getResourceAsStream(fontResourceLoc)));
			String line;
			while ((line = fontIndexReader.readLine()) != null)
			{
				fams.add(allFamilies.get(Integer.parseInt(line)));
			}
		}
		catch (final IOException e)
		{
			e.printStackTrace();
			throw new AssertionError();
		}
		return allFamilies;
	}

//	private static void drawRandom(final DigitClass d)
//	{
//		GC.clearRect(0, 0, PIC_SIZE_LEN, PIC_SIZE_LEN);
//		randomiseGcSettings();
//		GC.fillRect(0, 0, PIC_SIZE_LEN, PIC_SIZE_LEN);
//		if (!d.isBlank())
//		{
//			final Text t = new Text(d.strRep);
//			t.setFont(GC.getFont());
//			final Bounds dim = t.getLayoutBounds();
//			final double hdiff = PIC_SIZE_LEN - FONT_SIZE;
//			final double wdiff = PIC_SIZE_LEN - dim.getWidth();
//			GC.setFill(GC.getStroke());
//			GC.fillText(d.strRep, wdiff/2, PIC_SIZE_LEN - hdiff/2);
//			//			GC.fillOval(wdiff/2, PIC_SIZE_LEN - hdiff/2, 5, 5);
//			//			GC.strokeText(d.strRep, hdiff/2, PIC_SIZE_LEN - hdiff/2);
//		}
//	}
//
//	private static void drawSpecifiedFont(final DigitClass d, final Font f)
//	{
//		GC.clearRect(0, 0, PIC_SIZE_LEN, PIC_SIZE_LEN);
//		randomiseGcSettings();
//		GC.fillRect(0, 0, PIC_SIZE_LEN, PIC_SIZE_LEN);
//		if (!d.isBlank())
//		{
//			GC.setFont(f);
//			final Text t = new Text(d.strRep);
//			t.setFont(GC.getFont());
//			final Bounds dim = t.getLayoutBounds();
//			final double hdiff = PIC_SIZE_LEN - FONT_SIZE;
//			final double wdiff = PIC_SIZE_LEN - dim.getWidth();
//			GC.setFill(GC.getStroke());
//			GC.fillText(d.strRep, wdiff/2, PIC_SIZE_LEN - hdiff/2);
//			//			GC.fillOval(wdiff/2, PIC_SIZE_LEN - hdiff/2, 5, 5);
//			//			GC.strokeText(d.strRep, hdiff/2, PIC_SIZE_LEN - hdiff/2);
//		}
//	}

	private static void randomiseGcSettings()
	{
//		GC.setFont(getRandomFont());
//		GC.setEffect(getRandomBlurEffect());
//		GC.setLineWidth(THICKNESS_LEVELS[R.nextInt(THICKNESS_LEVELS.length)]);
//		GC.setFill(Color.gray(BACK_COLORS[R.nextInt(BACK_COLORS.length)]));
//		GC.setStroke(Color.gray(TEXT_COLORS[R.nextInt(TEXT_COLORS.length)]));
	}

	private static Font getRandomFont()
	{
		return Font.font(FONT_FAMILIES.get(R.nextInt(FONT_FAMILIES.size())), FONT_SIZE);//, )
	}

	private static Effect getRandomBlurEffect()
	{
		final double width = BLUR_LEVELS[R.nextInt(BLUR_LEVELS.length)];
		final double height = BLUR_LEVELS[R.nextInt(BLUR_LEVELS.length)];
		final double iterations = BLUR_LEVELS[R.nextInt(BLUR_LEVELS.length)];
		return new BoxBlur(width, height, (int) iterations);
	}

	/**
	 * @param args
	 */
	public static void main(final String[] args)
	{
		launch(args);
	}
	
	static class PicRegion extends Region
	{
		private static final String STYLE = "-fx-background-color: white;", T_STYLE = "-fx-background-color: yellow;";
		private static final double MIN_FONT_SIZE = 15, MAX_FONT_SIZE = 25, MAX_ROT = 5;
		private static final List<String> FONT_FAMILIES = getFontFamilies();
		private static final int EDGEPAD = 3;
		
		private final Text t;
		private final int length;
		
		public PicRegion(int length) 
		{
			this.length = length;
			setStyle(STYLE);
			setPrefSize(length, length);
			this.t = new Text();
			t.setStyle(T_STYLE);
			t.setBoundsType(TextBoundsType.VISUAL);
			getChildren().addAll(t);
		}
		
		@Override
		protected void layoutChildren()
		{
//			t.relocate(t.getLayoutX(), t.getLayoutY());
		}
		
		private void assignRandom(DigitClass d)
		{
			if (d.isBlank())
			{
				t.setText("");
				t.relocate(0, 0);
			}
			else
			{
				t.setText(d.strRep);
				t.setFont(getRandomFont());
				t.setRotate(R.nextInt((int) (2*MAX_ROT)) - MAX_ROT);
//				System.out.println(t.getFont().getSize());
//				System.out.println(t.getLayoutBounds());
//				System.out.println(t.getBoundsInLocal());
				Point2D tPos = getRandomPosition(t.getLayoutBounds(),t.getFont().getSize());
				t.relocate(tPos.getX(), tPos.getY());
			}
		}
		
		private Point2D getRandomPosition(Bounds boundsInLocal, double fontSize) 
		{
			int minX = EDGEPAD, minY = EDGEPAD;
			int maxX = (int) (length - EDGEPAD - boundsInLocal.getWidth());
			int maxY = (int) (length - EDGEPAD - boundsInLocal.getHeight());
			double x = minX + R.nextInt(Math.max(maxX - minX, 1));
			double y = minY + R.nextInt(Math.max(maxY - minY, 1));
//			System.out.println(x);
//			System.out.println(y);
			return new Point2D(x, y );//- heightDiff);
		}

		private static Font getRandomFont()
		{
			int sze = (int) (MIN_FONT_SIZE + R.nextInt((int) (MAX_FONT_SIZE - MIN_FONT_SIZE + 1)));
			return Font.font(FONT_FAMILIES.get(R.nextInt(FONT_FAMILIES.size())), sze);//, )
		}
	}

}
