package model;

import java.util.List;

/**
 * class that defines the characteristics of a continent.
 * The continent contains a list of countries resides in it.
 * 
 * @author Rohit
 * @version build 2
 */
public class Continent {
	private int d_uniqueContinetId;
	private String d_continentId;
	private String d_continentName;
	private String d_contientValue;
	private List<Country> d_continentCountries;

	/**
	 * Default constructor
	 */
	public Continent() {
	}
	
	/**
	 * Parameterized constructor to set unique Continent ID, Continent name, and continent value
	 * @param p_uniqueContinetId - Unique int ID to be assigned to the continent
	 * @param p_continentId - Name of continent
	 * @param p_contientValue - value of bonus army assigned to player if they capture every country in that continent
	 */
	public Continent(int p_uniqueContinetId, String p_continentId, String p_contientValue) {
		super();
		this.d_uniqueContinetId = p_uniqueContinetId;
		this.d_continentId = p_continentId;
		this.d_contientValue= p_contientValue;
	}
	
	/**
	 * Parameterized constructor to set unique Continent ID and Continent name
	 * @param p_uniqueContinetId - Unique int ID to be assigned to the continent
	 * @param p_continentId - Name of continent
	 */
	public Continent(int p_uniqueContinetId, String p_continentId) {
		super();
		this.d_uniqueContinetId = p_uniqueContinetId;
		this.d_continentId = p_continentId;
	}

	/**
	 * Parameterized constructor to set Continent name
	 * @param p_continentId - Name of continent
	 */
	public Continent(String p_continentId) {
		super();
		this.d_continentId = p_continentId;
	}

	/**
	 * Parameterized constructor to set Continent name and continent value
	 * @param p_continentId - Name of continent
	 * @param p_contientValue - value of bonus army assigned to player if they capture every country in that continent
	 */
	public Continent(String p_continentId, String p_contientValue) {
		super();
		this.d_continentId = p_continentId;
		this.d_contientValue = p_contientValue;
	}
		
	/**
	 * Parameterized constructor to set Continent name and initialize list of countries that belong in that continent
	 * @param p_continentId - Name of continent
	 * @param p_continentCountries - List of countries belonging to continent
	 */
	public Continent(String p_continentId, List<Country> p_continentCountries) {
		super();
		this.d_continentId = p_continentId;
		this.d_continentCountries = p_continentCountries;
	}
	
	/**
	 * getter method that returns the continent's ID
	 * @return d_uniqueContinetId - continent ID
	 */
	public int getUniqueContinetId() {
		return d_uniqueContinetId;
	}
	
	/**
	 * setter method to set or reset a continent's ID
	 * @param p_uniqueContinetId - the specific ID we want for the continent
	 */
	public void setUniqueContinetId(int p_uniqueContinetId) {
		this.d_uniqueContinetId = p_uniqueContinetId;
	}
	
	/**
	 * getter method to retrieve the list on countries that reside in that particular continent
	 * @return d_continentCountries - list of countries
	 */
	public List<Country> getContinentCountries() {
		return d_continentCountries;
	}
	
	/**
	 * getter method to retrive the name of the continent
	 * @return d_continentName - name of the continent
	 */
	public String getD_continentName() {
		return d_continentName;
	}

	/**
	 * setter method to modify the name of the continent
	 * @param d_continentName - updated continent name
	 */
	public void setD_continentName(String d_continentName) {
		this.d_continentName = d_continentName;
	}
	
	/**
	 * setter method to modify the countries in the continent
	 * @param p_continentCountries - a list of countries
	 */
	public void setContinentCountries(List<Country> p_continentCountries) {
		this.d_continentCountries = p_continentCountries;
	}
	
	/**
	 * getter method for the name of the continent. Ex: Canada, USA, Mexico
	 * @return d_continentId - continent name
	 */
	public String getContinentId() {
		return d_continentId;
	}
	
	/**
	 * Modifies continent name
	 * @param p_continentId - new name we want to set for the continent
	 */
	public void setContinentId(String p_continentId) {
		this.d_continentId = p_continentId;
	}
	
	/**
	 * getter method to retrieve the continent's army bonus value
	 * @return d_contientValue - continent's army value once a player is in possession of every country in that continent
	 */
	public String getContientValue() {
		return d_contientValue;
	}
	
	/**
	 * setter method to modify the value of the continent's bonus army
	 * @param p_contientValue - continent's army value once a player is in possession of every country in that continent
	 */
	public void setContientValue(String p_contientValue) {
		this.d_contientValue = p_contientValue;
	}

	
	public void setContinentOwner(Player player) {
		Player d_continentOwner = player;
	}
	

}
