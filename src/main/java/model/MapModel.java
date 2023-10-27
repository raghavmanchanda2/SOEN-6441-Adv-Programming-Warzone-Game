package model;

import logger.ConsoleWriter;
import logger.Logger;
import model.player.PlayerStrategy;
import persistence.MapFileAlteration;

import java.util.*;
import java.util.stream.Collectors;


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

	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;
	HashMap<String, Country> countryHashMap = new HashMap<>();

	private HashMap<String, Player> d_Players = new HashMap<>();

	private Player d_CurrentPlayer;
	private Boolean d_GameLoaded = false;

	public MapModel() {
		d_logger = new Logger();
		d_consoleWriter = new ConsoleWriter();
		d_logger.addObserver(d_consoleWriter);
	}

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
	 * getter method that returns the map of each country  a list of countries that borders them
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
//				System.out.println(country.getCountryId() + "inside...............");
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
		
//		System.out.println(country.getCountryId() + "outside...............");
		
	}
	
	/**
	 * method to add a neighboring country to the map of borders using mainCountry as search key 
	 * @param mainCountry
	 * @param neighbourCountry
	 */
	public void addBorders(Country mainCountry,Country neighbourCountry) {
		
		
		if(this.borders.containsKey(mainCountry)) {
			this.borders.get(mainCountry).add(neighbourCountry);
			mainCountry.getNeighbors().add(neighbourCountry);
		}else {
			this.borders.put(mainCountry,  new ArrayList<Country>());
//			System.out.println("ROHIT");
		}
	}

	/**
	 * method to retrieve player
	 * @param p_Id
	 * @return the specific player from the map value
	 */
	public Player getPlayer(String p_Id) {
		return d_Players.get(p_Id);
	}
	
	/**
	 * returns the mapping of players
	 * @return d_Players
	 */
	public HashMap<String, Player> getPlayers() {
		return d_Players;
	}

	
	/**
	 * Method to return the specific country
	 * @param p_Id
	 * @return country object
	 */
	public Country getCountry(String p_Id) {

		for (Country country : countries) {
			String uniqueId = country.getCountryId();
			countryHashMap.put(uniqueId, country);
		}
		return countryHashMap.get(p_Id);
	}


	
	/**
	 * method to add player
	 * @param p_PlayerName
	 * @throws Exception
	 */
	public void addPlayer(String p_PlayerName) throws Exception {
		if (this.getPlayers().containsKey(p_PlayerName)) {
			throw new Exception(p_PlayerName  + "exists already.");
		}
		Player l_Player = new Player(PlayerStrategy.getStrategy("human"));
		l_Player.setName(p_PlayerName);
		this.getPlayers().put(p_PlayerName, l_Player);
		d_logger.setLogMessage(p_PlayerName + " added successfully");
	}

	public void removePlayer(String p_PlayerName) throws Exception {
		Player l_Player = this.getPlayer(p_PlayerName);
		if (Objects.isNull(l_Player)) {
			throw new Exception("No player with name: " + p_PlayerName);
		}
		this.getPlayers().remove(l_Player.getName());
		d_logger.setLogMessage(p_PlayerName + " removed successfully");
	}

	public void allot() {
		int l_Index = 0;
		List<Player> l_Players = d_MapModel.getPlayers().values().stream().collect(Collectors.toList());

		List<Country> l_ListOfCountries =
				d_MapModel.getCountries();

		Collections.shuffle(l_ListOfCountries);
		for (Country l_Country : l_ListOfCountries) {
			Player l_Player = l_Players.get(l_Index);
			l_Player.getCapturedCountries().add(l_Country);
			l_Country.setPlayer(l_Player);
			d_logger.setLogMessage(l_Country.getCountryId() + " Allotted to " + l_Player.getName());

			if (l_Index < d_MapModel.getPlayers().size() - 1) {
				l_Index++;
			} else {
				l_Index = 0;
			}
		}
	}
	
	/**
	 * method to return the player
	 * @return
	 */
	public Player getD_CurrentPlayer() {
		return d_CurrentPlayer;
	}

	/**
	 * Set the current Player
	 *
	 * @param d_CurrentPlayer player
	 */
	public void setD_CurrentPlayer(Player d_CurrentPlayer) {
		this.d_CurrentPlayer = d_CurrentPlayer;
	}
	
	/**
	 * method to determine if game is loaded or not
	 * @return d_GameLoaded
	 */
	public Boolean getD_GameLoaded() {
		return d_GameLoaded;
	}
	
	/**
	 * method to load that specific game
	 * @param d_GameLoaded
	 */
	public void setD_GameLoaded(Boolean d_GameLoaded) {
		this.d_GameLoaded = d_GameLoaded;
	}

	/**
	 * method for clearing the map and all its parameters
	 */
	public void clearMap() {

		MapModel.getInstance().getPlayers().clear();
		
		if( MapModel.getInstance().getContinents() != null) {
			MapModel.getInstance().getContinents().clear();
		}
		if( MapModel.getInstance().getCountries() != null) {
			MapModel.getInstance().getCountries().clear();
		}
		
		if( MapModel.getInstance().getContinentCountries() != null) {
			MapModel.getInstance().getContinentCountries().clear();
		}
		
		if( MapModel.getInstance().getBorders() != null) {
			MapModel.getInstance().getBorders().clear();
		}
		
		
		
		
	}
}
