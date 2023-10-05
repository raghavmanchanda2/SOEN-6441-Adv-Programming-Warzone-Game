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
import model.Continent;
import model.Country;
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
 * @version 1.0
 */
public class ExecuteMapsCommands {
	
	private MapFileAlteration d_mapFileAlteration;
	
	/**
     * Constructs an `ExecuteMapsCommands` object.
     */
	public ExecuteMapsCommands() {
		d_mapFileAlteration = new MapFileAlteration();
	}

	 /**
     * Adds a continent to the map.
     *
     * @param p_continent The continent to be added.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
	public ResponseWrapper addContinent(Continent p_continent) {
		return this.d_mapFileAlteration.addContinent(p_continent);
		 
	}

	/**
    * Removes a continent from the map.
    *
    * @param p_continent The continent to be removed.
    * @return A {@link ResponseWrapper} object indicating the result of the operation.
    */
	public ResponseWrapper removeContinent(Continent p_continent) {

		return this.d_mapFileAlteration.removeContinent(p_continent);
	}
	
	/**
	 * Method to add country from the map
	 * @param p_country
	 * @return alert message that country has been properly added
	 */
	public ResponseWrapper addCountry(Country p_country) {
		return this.d_mapFileAlteration.addCountry(p_country);
		
	}
	
	/**
	 * Method to remove country from the map
	 * @param p_country
	 * @return alert message that country has been properly removed
	 */
	public ResponseWrapper removeCountry(Country p_country) {

		return this.d_mapFileAlteration.removeCountry(p_country);
	}

	/**
	 * Adds neighboring to a specific country 
	 * @param p_mainCountry
	 * @param p_neighbourCountry
	 * @return alert message that a neighbor has successfully been added to a country
	 */
	public ResponseWrapper addNeighbour(Country p_mainCountry, Country p_neighbourCountry) {
		return this.d_mapFileAlteration.addNeighbour(p_mainCountry, p_neighbourCountry);
	}

	/**
	 * Removes neighbor to a specific country
	 * @param p_country
	 * @param p_neighbourCountry
	 * @return alert message that a neighbor has successfully been removed from a country
	 */
	public ResponseWrapper removeNeighbour(Country p_country, Country p_neighbourCountry) {

		return this.d_mapFileAlteration.removeNeighbour(p_country,p_neighbourCountry);
	}
	
	/**
	 * method to show current state of map
	 * @return alert message that map is properly showing
	 */
	public ResponseWrapper showMap() {
		return this.d_mapFileAlteration.showmap();
		
	}
	
	/**
	 * method to save current state of map
	 * @param p_mapFileName
	 * @return alert message that map has successfully been saved
	 */
	public ResponseWrapper saveMap(String p_mapFileName) {
		return this.d_mapFileAlteration.saveMap(p_mapFileName);
	}
	
	/**
	 * method to check if the map is valid
	 * @return alert message that map is valid or not
	 */
	public ResponseWrapper validateMap() {
		
		return this.d_mapFileAlteration.validateMap();
	}

	/**
	 * It will open the map if the map is already in the map folder or else it will create a new map in the
	 * map folder
	 * @param p_mapFileName
	 * @return alert message if the map has been successfully created or edited
	 */
	public ResponseWrapper editOrCreateMap(String p_mapFileName) {

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

	
	/**
	 * method for validating map to be implemented in the future
	 * @param mapFileName
	 * @return null
	 */
	public ResponseWrapper validateMap(String mapFileName) {
		return null;

	}
}
