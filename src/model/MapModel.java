package model;

import logger.ConsoleWriter;
import logger.Logger;
import model.player.PlayerStrategy;

import javax.xml.bind.ValidationException;
import java.util.*;
import java.util.stream.Collectors;

public class MapModel {
	
	private String mapName;
	private List<Continent> continents;
	private List<Country> countries;
	private Map<Continent,List<Country>> continentCountries;
	private Map<Country,List<Country>> borders;

	private HashMap<String, Continent> d_Continents = new HashMap<>();
	private HashMap<String, Country> d_Countries = new HashMap<>();

	private HashMap<String, Player> d_Players = new HashMap<>();

	private Boolean d_GameLoaded = false;

	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;

	private static MapModel d_MapModel;

	public static MapModel getInstance() {
		if (Objects.isNull(d_MapModel)) {
			d_MapModel = new MapModel();
		}
		return d_MapModel;
	}

	public MapModel() {
		d_logger = new Logger();
		d_consoleWriter = new ConsoleWriter();
		d_logger.addObserver(d_consoleWriter);
	}

	public List<Country> getCountries() {
		return countries;
	}

	public Boolean getD_GameLoaded() {
		return d_GameLoaded;
	}

	public void setD_GameLoaded(Boolean d_GameLoaded) {
		this.d_GameLoaded = d_GameLoaded;
	}

	public Country getCountry(String p_Id) {
		return d_Countries.get(p_Id);
	}

	public HashMap<String, Country> getD_Countries() {
		return d_Countries;
	}
	public void setCountries(List<Country> countries) {
		this.countries = countries;
	}
	public String getMapName() {
		return mapName;
	}
	public void setMapName(String mapName) {
		this.mapName = mapName;
	}

	private Player d_CurrentPlayer;

	public Continent getContinent(String p_Id) {
		return d_Continents.get(p_Id);
	}
	public HashMap<String, Continent> getD_Continents() {
		return d_Continents;
	}

	
	public List<Continent> getContinents() {
		return continents;
	}
	public void setContinents(List<Continent> continents) {
		this.continents = continents;
	}
	public Map<Continent, List<Country>> getContinentCountries() {
		return continentCountries;
	}
	public void setContinentCountries(Map<Continent, List<Country>> continentCountries) {
		this.continentCountries = continentCountries;
	}
	public Map<Country, List<Country>> getBorders() {
		return borders;
	}
	public void setBorders(Map<Country, List<Country>> borders) {
		this.borders = borders;
	}
	
	public void addContinent(Continent continent) {
		System.out.println("add continent");
		if(this.continents == null) {
			this.continents = new ArrayList<>();
		}
		if(this.continentCountries == null) {
			this.continentCountries = new HashMap<>();
		}
		this.continents.add(continent);
		this.continentCountries.put(continent, new ArrayList<>());
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

	public void p_addNeighbor(String p_CountryName, String p_NeighborCountryName) throws ValidationException {
		Country l_Country1 = this.getCountry(p_CountryName);
		Country l_Country2 = this.getCountry(p_NeighborCountryName);
		if (Objects.isNull(l_Country1) || Objects.isNull(l_Country2)) {
			throw new ValidationException("At least one of the mentioned Countries does not exist");
		}
		l_Country1.getNeighbors().add(l_Country2);
		d_logger.setLogMessage(String.format("Route added between : %s - %s", p_CountryName, p_NeighborCountryName));
	}

	/**
	 * Get the current Player
	 *
	 * @return player
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
	
	public void addContinentCountries(Continent continent, Country country) {
		country.setContinent(continent);
		if (this.continentCountries.containsKey(continent)) {
			this.continentCountries.get(continent).add(country);
			if(this.borders == null) {
				this.borders = new LinkedHashMap<>();
			}
			this.borders.put(country, new ArrayList<>());
			if(this.countries == null) {
				this.countries = new ArrayList<>();
			}
			this.countries.add(country);
			this.borders.put(country, new ArrayList<>());
		}
		
	}
	
	public void addBorders(Country mainCountry,Country neighbourCountry) {
		if(this.borders.containsKey(mainCountry)) {
			this.borders.get(mainCountry).add(neighbourCountry);
		}
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

	public void flushGameMap() {
		MapModel.getInstance().getD_Continents().clear();
		MapModel.getInstance().getD_Countries().clear();
		MapModel.getInstance().getPlayers().clear();
	}
}
