package business;

import java.io.File;
import java.io.IOException;
import Constants.ProjectConfig;
import GamePhase.MapPhaseState;
import model.Continent;
import model.Country;
import model.ResponseWrapper;
import persistence.MapFileAlteration;

public class ExecuteMapsCommands {
	
	private MapFileAlteration d_mapFileAlteration;

	public ExecuteMapsCommands() {
		d_mapFileAlteration = new MapFileAlteration();
	}

	public ResponseWrapper addContinent(Continent p_continent) {
		return this.d_mapFileAlteration.addContinent(p_continent);
		 
	}

	public ResponseWrapper removeContinent(Continent p_continent) {

		return this.d_mapFileAlteration.removeContinent(p_continent);
	}

	public ResponseWrapper addCountry(Country p_country) {
		return this.d_mapFileAlteration.addCountry(p_country);
		
	}

	public ResponseWrapper removeCountry(Country p_country) {

		return this.d_mapFileAlteration.removeCountry(p_country);
	}

	public ResponseWrapper addNeighbour(Country p_mainCountry, Country p_neighbourCountry) {
		return this.d_mapFileAlteration.addNeighbour(p_mainCountry, p_neighbourCountry);
	}

	public ResponseWrapper removeNeighbour(Country p_country, Country p_neighbourCountry) {

		return this.d_mapFileAlteration.removeNeighbour(p_country,p_neighbourCountry);
	}

	public ResponseWrapper showMap() {
		return this.d_mapFileAlteration.showmap();
		
	}
	
	public ResponseWrapper saveMap(String p_mapFileName) {
		return this.d_mapFileAlteration.saveMap(p_mapFileName);
	}
	public ResponseWrapper validateMap() {
		
		return this.d_mapFileAlteration.validateMap();
	}

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

	

	public ResponseWrapper validateMap(String mapFileName) {
		return null;

	}
}
