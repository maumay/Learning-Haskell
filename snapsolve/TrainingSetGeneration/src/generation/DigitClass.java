/**
 *
 */
package generation;

/**
 * @author t
 *
 */
public enum DigitClass
{
	ONE("1"), TWO("2"), THREE("3"), FOUR("4"), FIVE("5"), SIX("6"), SEVEN("7"), EIGHT("8"), NINE("9"), NONE("0");

	public final String strRep;

	private DigitClass(final String strRep)
	{
		this.strRep = strRep;
	}

	public boolean isBlank()
	{
		return this == NONE;
	}
}
