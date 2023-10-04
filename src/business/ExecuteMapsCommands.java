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
     * Adds a country to the map.
     *
     * @param p_country The country to be added.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper addCountry(Country p_country) {
        return this.d_mapFileAlteration.addCountry(p_country);
    }

    /**
     * Removes a country from the map.
     *
     * @param p_country The country to be removed.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper removeCountry(Country p_country) {
        return this.d_mapFileAlteration.removeCountry(p_country);
    }

    /**
     * Adds a neighbor to a country on the map.
     *
     * @param p_mainCountry     The main country.
     * @param p_neighbourCountry The neighboring country to be added.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper addNeighbour(Country p_mainCountry, Country p_neighbourCountry) {
        return this.d_mapFileAlteration.addNeighbour(p_mainCountry, p_neighbourCountry);
    }

    /**
     * Removes a neighbor from a country on the map.
     *
     * @param p_country          The main country.
     * @param p_neighbourCountry The neighboring country to be removed.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper removeNeighbour(Country p_country, Country p_neighbourCountry) {
        return this.d_mapFileAlteration.removeNeighbour(p_country, p_neighbourCountry);
    }

    /**
     * Shows the current state of the map.
     *
     * @return A {@link ResponseWrapper} object containing the map state.
     */
    public ResponseWrapper showMap() {
        return this.d_mapFileAlteration.showmap();
    }

    /**
     * Saves the current map to a file.
     *
     * @param p_mapFileName The name of the map file.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper saveMap(String p_mapFileName) {
        return this.d_mapFileAlteration.saveMap(p_mapFileName);
    }

    /**
     * Validates the current map.
     *
     * @return A {@link ResponseWrapper} object indicating the result of the validation.
     */
    public ResponseWrapper validateMap() {
        return this.d_mapFileAlteration.validateMap();
    }

    /**
     * Edits an existing map or creates a new one with the given filename.
     *
     * @param p_mapFileName The name of the map file.
     * @return A {@link ResponseWrapper} object indicating the result of the operation.
     */
    public ResponseWrapper editOrCreateMap(String p_mapFileName) {
        if (new File(ProjectConfig.D_MAP_FILES_PATH + p_mapFileName).exists()) {
            // Load map to the game and show as well for better understanding
            MapPhaseState.D_CURRENT_MAP = p_mapFileName;
            this.d_mapFileAlteration.readMapFile();
            return new ResponseWrapper(200, "Map exists");
        } else {
            // Create a new map in the map folder
            System.out.println("Creating new map");
            try {
                if (new File(ProjectConfig.D_MAP_FILES_PATH + p_mapFileName).createNewFile()) {
                    System.out.println("Map created successfully");
                    MapPhaseState.D_CURRENT_MAP = p_mapFileName;
                    this.d_mapFileAlteration.readMapFile();
                    return new ResponseWrapper(200, "Map created successfully");
                    // Map file created successfully
                }
            } catch (IOException p_exception) {
                // Error occurred in creating a new map, please check with the administrator
                System.out.println(p_exception);
                return new ResponseWrapper(500, "Internal ERROR OCCURRED .... PLEASE CONNECT WITH ADMIN");
            }
        }
        return new ResponseWrapper(500, "Internal ERROR OCCURRED .... PLEASE CONNECT WITH ADMIN");
    }

    /**
     * Validates a map with the given filename.
     *
     * @param mapFileName The name of the map file to validate.
     * @return A {@link ResponseWrapper} object indicating the result of the validation.
     */
    public ResponseWrapper validateMap(String mapFileName) {
        // Implementation not provided in this method
        return null;
    }
}
