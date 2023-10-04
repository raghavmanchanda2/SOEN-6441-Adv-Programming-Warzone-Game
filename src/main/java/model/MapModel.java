package model;

import logger.ConsoleWriter;
import logger.Logger;
import model.player.PlayerStrategy;

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

	private HashMap<String, Continent> d_Continents = new HashMap<>();
	private HashMap<String, Country> d_Countries = new HashMap<>();

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
		}else {
			this.borders.put(mainCountry,  new ArrayList<Country>());
			System.out.println("ROHIT");
		}
	}

	public Continent getContinent(String p_Id) {
		return d_Continents.get(p_Id);
	}
	public HashMap<String, Continent> getD_Continents() {
		return d_Continents;
	}

	public Country getCountry(String p_Id) {
		return d_Countries.get(p_Id);
	}

	public HashMap<String, Country> getD_Countries() {
		return d_Countries;
	}

	public Player getPlayer(String p_Id) {
		return d_Players.get(p_Id);
	}

	public HashMap<String, Player> getPlayers() {
		return d_Players;
	}

	public void showMap() {

		d_logger.setLogMessage("Continents:");
		Iterator<Map.Entry<String, Continent>> l_IteratorForContinents = d_MapModel.getD_Continents().entrySet()
				.iterator();

		String l_Table = "- %-18s%n";

		System.out.format("********************%n");
		System.out.format("     Continents  %n");
		System.out.format("********************%n");

		while (l_IteratorForContinents.hasNext()) {
			Map.Entry<String, Continent> continentMap = l_IteratorForContinents.next();
			String l_ContinentId = (String) continentMap.getKey();
			Continent l_Continent = d_MapModel.getD_Continents().get(l_ContinentId); //Get the particular continent by its ID(Name)

			System.out.format(l_Table, l_Continent.getD_continentName());
		}
		System.out.format("********************%n");


		// Showing Countries in the Continent and their details
		d_logger.setLogMessage("\nCountries:");

		Iterator<Map.Entry<String, Continent>> l_IteratorForContinent = d_MapModel.getD_Continents().entrySet()
				.iterator();

		l_Table = "- %-23s- %-18s- %-60s%n";

		System.out.format(
				"****************************************************************************************************%n");
		System.out.format(
				"        Country   !     Continent    !     Neighbours                                  %n");
		System.out.format(
				"****************************************************************************************************%n");


		while (l_IteratorForContinent.hasNext()) {
			Map.Entry<String, Continent> l_ContinentMap = (Map.Entry<String, Continent>) l_IteratorForContinent.next();
			String l_ContinentId = (String) l_ContinentMap.getKey();
			Continent l_Continent = d_MapModel.getD_Continents().get(l_ContinentId);
			Iterator<Country> l_ListIterator = l_Continent.getCountries().iterator();

			while (l_ListIterator.hasNext()) {

				Country l_Country = (Country) l_ListIterator.next();
				System.out.format(l_Table, l_Country.getD_countryName(), l_Continent.getD_continentName(), l_Country.createNeighboursList(l_Country.getNeighbors()), l_Country.getArmies());
			}
		}

		System.out.format(
				"****************************************************************************************************%n");


		HashMap<String, Player> l_Players = d_MapModel.getPlayers();
		d_logger.setLogMessage("\nPlayers : ");
		if (l_Players != null) {
			l_Players.forEach((key, value) -> d_logger.setLogMessage(key));
			d_logger.setLogMessage("");
		}

		//Showing the Ownership of the players
		d_logger.setLogMessage("Continents allotted to players: ");


		System.out.format("**************************************************%n");
		System.out.format("     Players     !     Allotted Continents       %n");
		System.out.format("**************************************************%n");

		String l_Table1 = "- %-15s- %-30s- %n";


		for (Player l_Player : d_MapModel.getPlayers().values()) {
			System.out.format(l_Table1, l_Player.getName(), l_Player.createACaptureList(l_Player.getCapturedCountries()), l_Player.getReinforcementArmies());
		}

		System.out.format("**************************************************%n");

	}

	public void p_addContinent(String p_ContinentName, String p_ControlValue) throws Exception {
		if (this.getD_Continents().containsKey(p_ContinentName)) {
			throw new Exception("Continent already exists");
		}
		Continent l_Continent = new Continent();
		l_Continent.setD_continentName(p_ContinentName);
		l_Continent.setD_AwardArmies(Integer.parseInt(p_ControlValue));
		this.getD_Continents().put(p_ContinentName, l_Continent);
		d_logger.setLogMessage("Continent " + p_ContinentName + " added.");
	}

	public void p_addCountry(String p_CountryName, String p_ContinentName) throws Exception{

		if (this.getD_Countries().containsKey(p_CountryName)) {
			throw new Exception("Country already exist");
		}
		Country l_Country = new Country();
		l_Country.setD_countryName(p_CountryName);
		l_Country.setD_Continent(p_ContinentName);
		this.getD_Countries().put(p_CountryName, l_Country);
		this.getContinent(p_ContinentName).getCountries().add(l_Country);
		d_logger.setLogMessage("Country " + p_CountryName + " added.");
	}

	public void p_addNeighbor(String p_CountryName, String p_NeighborCountryName) throws Exception {
		Country l_Country1 = this.getCountry(p_CountryName);
		Country l_Country2 = this.getCountry(p_NeighborCountryName);
		if (Objects.isNull(l_Country1) || Objects.isNull(l_Country2)) {
			throw new Exception("At least one of the mentioned Countries does not exist");
		}
		l_Country1.getNeighbors().add(l_Country2);
		d_logger.setLogMessage(String.format("Route added between : %s - %s", p_CountryName, p_NeighborCountryName));
	}


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

		List<Country> l_ListOfCountries = d_MapModel.getD_Countries().values().stream().collect(Collectors.toList());
		Collections.shuffle(l_ListOfCountries);
		for (Country l_Country : l_ListOfCountries) {
			Player l_Player = l_Players.get(l_Index);
			l_Player.getCapturedCountries().add(l_Country);
			l_Country.setPlayer(l_Player);
			d_logger.setLogMessage(l_Country.getD_countryName() + " Allotted to " + l_Player.getName());

			if (l_Index < d_MapModel.getPlayers().size() - 1) {
				l_Index++;
			} else {
				l_Index = 0;
			}
		}
	}

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

	public Boolean getD_GameLoaded() {
		return d_GameLoaded;
	}

	public void setD_GameLoaded(Boolean d_GameLoaded) {
		this.d_GameLoaded = d_GameLoaded;
	}

	public void clearMap() {
		MapModel.getInstance().getD_Continents().clear();
		MapModel.getInstance().getD_Countries().clear();
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
