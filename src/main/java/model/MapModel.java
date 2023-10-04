package model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;


/**
 * class that defines all entities in the map (continents, countries, borders) and their operations
 * @author Rohit
 * @version build 1
 *
 */
public class MapModel {
	
	private String mapName;
	private List<Continent> continents;
	private List<Country> countries;
	private Map<Continent,List<Country>> continentCountries;
	private Map<Country,List<Country>> borders;

	private static MapModel d_MapModel;

	/**
	 * If an instance for MapModel has not been created, create it and return it
	 * @return d_MapModel
	 */
	public static MapModel getInstance() {
		if (Objects.isNull(d_MapModel)) {
			d_MapModel = new MapModel();
		}
		return d_MapModel;
	}
	
	/**
	 * getter method that returns the list of all countries on the map
	 * @return countries
	 */
	public List<Country> getCountries() {
		return countries;
	}
	
	/**
	 * setter method to modify the countries on the map
	 * @param countries
	 */
	public void setCountries(List<Country> countries) {
		this.countries = countries;
	}
	
	/**
	 * getter method for the name of the map
	 * @return mapName
	 */
	public String getMapName() {
		return mapName;
	}
	
	/**
	 * setter method to modify the map's name
	 * @param mapName
	 */
	public void setMapName(String mapName) {
		this.mapName = mapName;
	}
	
	/**
	 * getter method that returns the list of all continents
	 * @return continents
	 */
	public List<Continent> getContinents() {
		return continents;
	}
	
	/**
	 * setter method that modifies the list of continents on the map
	 * @param continents
	 */
	public void setContinents(List<Continent> continents) {
		this.continents = continents;
	}
	
	/**
	 * getter method that returns a map of all continents and their its associated countries
	 * @return continentCountries
	 */
	public Map<Continent, List<Country>> getContinentCountries() {
		return continentCountries;
	}
	
	/**
	 * setter method to modify the map's continents and their associated countries
	 * @param continentCountries
	 */
	public void setContinentCountries(Map<Continent, List<Country>> continentCountries) {
		this.continentCountries = continentCountries;
	}
	
	/**
	 * getter method that returns the map of each country the a list of countries that borders them
	 * @return borders
	 */
	public Map<Country, List<Country>> getBorders() {
		return borders;
	}
	
	/**
	 * setter method to modify the countries' borders
	 * @param borders
	 */
	public void setBorders(Map<Country, List<Country>> borders) {
		this.borders = borders;
	}
	
	/**
	 * method for adding a new continent on the map.
	 * If the map does not contain any continents, initialize map continents with an array list
	 * If the map does not contain a mapping of continents with associated countries, initialize the continent countries with a HashMap
	 * @param continent
	 */
	public void addContinent(Continent continent) {
		
		if(this.continents == null) {
			this.continents = new ArrayList<Continent>();
		}
		if(this.continentCountries == null) {
			this.continentCountries = new HashMap<Continent, List<Country>>();
		}
		this.continents.add(continent);
		this.continentCountries.put(continent, new ArrayList<Country>());
	}
	
	/**
	 * method to input a new country into its associated continent
	 * If that particular continent does not exist in the mapping of continentCountries, nothing happens.
	 * If it does, add that country into the map (continentCountries)
	 * If the map does not have any borders set, initialize the border with a linked hash-map and add that country
	 * 
	 * @param continent
	 * @param country
	 */
	public void addContinentCountries(Continent continent, Country country) {
		if(continent != null) {
			country.setContinent(continent);
			if (this.continentCountries.containsKey(continent)) {
				System.out.println(country.getCountryId() + "inside...............");
				this.continentCountries.get(continent).add(country);	
			}
		}
				
		if(this.borders == null) {
			this.borders = new LinkedHashMap<Country, List<Country>>();
		}
		this.borders.put(country, new ArrayList<Country>());
		if(this.countries == null) {
			this.countries = new ArrayList<Country>();
		}
		this.countries.add(country);
		this.borders.put(country, new ArrayList<Country>());
		
		System.out.println(country.getCountryId() + "outside...............");
		
	}
	
	/**
	 * method to add a neighboring country to the map of borders using mainCountry as search key 
	 * @param mainCountry
	 * @param neighbourCountry
	 */
	public void addBorders(Country mainCountry,Country neighbourCountry) {
		if(this.borders.containsKey(mainCountry)) {
			this.borders.get(mainCountry).add(neighbourCountry);
		}
	}
}
