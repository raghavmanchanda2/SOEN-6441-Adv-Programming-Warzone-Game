/**
 * The `ExecuteMapsCommands` class is responsible for executing map-related commands in the Risk game.
 * It provides methods for adding and removing continents, countries, neighbors, as well as map validation
 * and file operations.
 *
 * <p>
 * This class utilizes the {@link MapFileAlteration} class to perform map operations.
 * </p>
 *
 * @author Rohit
 * @version 1.0
 */
package business;

import java.io.File;
import java.io.IOException;
import Constants.ProjectConfig;
import GamePhase.MapPhaseState;
import logger.GeneralException;
import model.Continent;
import model.Country;
import model.Player;
import model.ResponseWrapper;
import persistence.MapFileAlteration;

/**
 * The `ExecuteMapsCommands` class is responsible for executing map-related commands in the Risk game.
 * It provides methods for adding and removing continents, countries, neighbors, as well as map validation
 * and file operations.
 *
 * <p>
 * This class utilizes the {@link MapFileAlteration} class to perform map operations.
 * </p>
 *
 * @author Rohit
 * @version build 2
 */
public class ExecuteMapsCommands extends Phase {
	
	private MapFileAlteration d_mapFileAlteration;
	private static final String COMMAND_ADD = "-add";
	private static final String COMMAND_REMOVE = "-remove";
	private static final String INCORRECT_COMMAND = "Incorrect Command";

	/**
     * Constructs an `ExecuteMapsCommands` object.
     */
	public ExecuteMapsCommands() {
		d_mapFileAlteration = new MapFileAlteration();
	}

	/**
     * Adds a continent to the map.
     *
     * @param p_continent - The continent to be added.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
	public ResponseWrapper addContinent(Continent p_continent) {
		return this.d_mapFileAlteration.addContinent(p_continent);
		 
	}

	/**
	 * Removes a continent from the map.
	 *
	 * @param p_continent - The continent to be removed.
	 * @return A {@link ResponseWrapper} object indicating the result of the operation.
	 */
	public ResponseWrapper removeContinent(Continent p_continent) {

		return this.d_mapFileAlteration.removeContinent(p_continent);
	}
	
	/**
	 * Method to add country from the map
	 * @param p_country - country to be added
	 * @return alert message that country has been properly added
	 */
	public ResponseWrapper addCountry(Country p_country) {
		return this.d_mapFileAlteration.addCountry(p_country);
		
	}
	
	/**
	 * Method to remove country from the map
	 * @param p_country - country to be added
	 * @return alert message that country has been properly removed
	 */
	public ResponseWrapper removeCountry(Country p_country) {

		return this.d_mapFileAlteration.removeCountry(p_country);
	}

	/**
	 * Adds neighboring to a specific country 
	 * @param p_mainCountry - specific  country
	 * @param p_neighbourCountry - neighbor country to be added
	 * @return alert message that a neighbor has successfully been added to a country
	 */
	public ResponseWrapper addNeighbour(Country p_mainCountry, Country p_neighbourCountry) {
		return this.d_mapFileAlteration.addNeighbour(p_mainCountry, p_neighbourCountry);
	}

	/**
	 * Removes neighbor from a specific country
	 * @param p_country - specific country 
	 * @param p_neighbourCountry - country to be removed
	 * @return alert message that a neighbor has successfully been removed from a country
	 */
	public ResponseWrapper removeNeighbour(Country p_country, Country p_neighbourCountry) {

		return this.d_mapFileAlteration.removeNeighbour(p_country,p_neighbourCountry);
	}
	
	/**
	 * method to show current state of map
	 * @return alert message that map is properly showing
	 */
	@Override
	public ResponseWrapper showMap() {
		return this.d_mapFileAlteration.showmap();
		
	}
	
	/**
	 * method to save current state of map
	 * @param p_mapFileName - name of map file
	 * @return alert message that map has successfully been saved
	 */
	@Override
	public ResponseWrapper saveMap(String p_mapFileName) {
		return this.d_mapFileAlteration.saveMap(p_mapFileName);
	}
	
	/**
	 * method to check if the map is valid
	 * @return alert message that map is valid or not
	 */
	@Override
	public ResponseWrapper validateMap() {
		
		return this.d_mapFileAlteration.validateMap();
	}

	/**
	 * It will open the map if the map is already in the map folder or else it will create a new map in the
	 * map folder
	 * @param p_mapFileName - name of map file
	 * @return alert message if the map has been successfully created or edited
	 */
	@Override
	public ResponseWrapper editOrCreateMap(String p_mapFileName) throws GeneralException {

		if (new File(ProjectConfig.D_MAP_FILES_PATH + p_mapFileName).exists()) {
			// load map to the game and show as well for better understanding
			MapPhaseState.D_CURRENT_MAP = p_mapFileName;
			
			this.d_mapFileAlteration.readMapFile();
			return new ResponseWrapper(200, "Map exists");

		} else {
			// create new map in map folder
			System.out.println("creating new map");
			try {
				if (new File(ProjectConfig.D_MAP_FILES_PATH + p_mapFileName).createNewFile()) {
					System.out.println("map created successfully");
					MapPhaseState.D_CURRENT_MAP = p_mapFileName;
					this.d_mapFileAlteration.readMapFile();
					return new ResponseWrapper(200, "Map created successfully ");
					// map file created successfully
				}
			} catch (IOException p_exception) {
				// error occured in creating new map and please check with adminstrator
				System.out.println(p_exception);
				return new ResponseWrapper(500, "Internal ERROR OCCURED .... PLEASE CONNECT WITH ADMIN");
			}

		}
		return new ResponseWrapper(500, "Internal ERROR OCCURED .... PLEASE CONNECT WITH ADMIN");

	}

	@Override
	public ResponseWrapper advance(Player currentPlayer, String countryNameFrom, String countryNameTo,
			int numerOfarmies) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper deploy(Player currentPlayer, String country, int numerOfarmies) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper assignCountries() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editContinent(Continent p_continent, String command) throws GeneralException {
		if(command.equals(COMMAND_ADD))
		{
			return this.addContinent(p_continent);
		}
		else if (command.equals(COMMAND_REMOVE))
		{
			return this.removeContinent(p_continent);
		}
		else 
		{
			return new ResponseWrapper(404, INCORRECT_COMMAND);
		}
	}

	@Override
	public ResponseWrapper addPlayerInGame(String playerName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper removeplayerFromGame(String playerName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper afterCommitReinforcement() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editNeighbour(Country p_mainCountry, Country p_neighbourCountry, String command)
			throws GeneralException {
		if(command.equals(COMMAND_ADD))
		{
			 return this.addNeighbour(p_mainCountry, p_neighbourCountry);
		}
		else if (command.equals(COMMAND_REMOVE))
		{
			return this.removeNeighbour(p_mainCountry, p_neighbourCountry);
		}
		else 
		{
			return new ResponseWrapper(404, INCORRECT_COMMAND);
		}
	}

	@Override
	public ResponseWrapper editCountry(Country p_country, String command) throws GeneralException {
		
		if(command.equals(COMMAND_ADD))
		{
			return this.addCountry(p_country);
		}
		else if (command.equals(COMMAND_REMOVE))
		{
			return this.removeCountry(p_country);
		}
		else 
		{
			return new ResponseWrapper(404, INCORRECT_COMMAND);
		}
		
	}

	@Override
	public ResponseWrapper loadMap(String map) throws GeneralException {
		return printInvalidCommandInState();
	}


	@Override
	public ResponseWrapper bomb(Player currentPlayer, String targetCountryName) throws GeneralException {
		return printInvalidCommandInState();
	}
	
	@Override
	public ResponseWrapper blockade(Player currentPlayer, String targetCountryName) throws GeneralException {
		return printInvalidCommandInState();
	}
	
	@Override
	public ResponseWrapper airlift(Player currentPlayer, String countryNameFrom, String countryNameTo, int numArmies)  throws GeneralException {
		return printInvalidCommandInState();
	}
	
	@Override
	public ResponseWrapper diplomacy(Player currentPlayer, String peaceWith) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper doReinforcements() throws GeneralException {
		return printInvalidCommandInState();
	}

	
}
