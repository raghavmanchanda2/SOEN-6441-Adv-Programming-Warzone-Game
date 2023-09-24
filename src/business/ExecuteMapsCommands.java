package business;

import java.io.File;
import java.io.IOException;
import Constants.ProjectConfig;
import GamePhase.MapPhaseState;
import model.Continent;
import model.Country;
import model.MapModel;
import model.ResponseWrapper;
import persistence.MapFileAlteration;

public class ExecuteMapsCommands {
	
	private MapFileAlteration mapFileAlteration;

	public ExecuteMapsCommands() {
		mapFileAlteration = new MapFileAlteration();
	}

	public ResponseWrapper addContinent(Continent continent) {
		this.mapFileAlteration.addContinent(continent);
		return null;
	}

	public ResponseWrapper removeContinent(Continent continent) {

		return this.mapFileAlteration.removeContinent(continent);
	}

	public ResponseWrapper addCountry(Country country) {
		this.mapFileAlteration.addCountry(country);
		return null;
	}

	public ResponseWrapper removeCountry(Country country) {

		return this.removeCountry(country);
	}

	public ResponseWrapper addNeighbour(Country mainCountry, Country neighbourCountry) {
		this.mapFileAlteration.addNeighbour(mainCountry, neighbourCountry);
		return null;
	}

	public ResponseWrapper removeNeighbour(Country country, Country neighbourCountry) {

		return null;
	}

	public ResponseWrapper showMap() {
		return this.mapFileAlteration.showmap();
		
	}
	
	public ResponseWrapper saveMap(String mapFileName) {
		return this.mapFileAlteration.saveMap(mapFileName);
	}
	public ResponseWrapper validateMap() {
		
		return this.mapFileAlteration.validateMap();
	}

	public ResponseWrapper editOrCreateMap(String mapFileName) {

		if (new File(ProjectConfig.MAP_FILES_PATH + mapFileName).exists()) {
			// load map to the game and show as well for better understanding
			MapPhaseState.CURRENT_MAP = mapFileName;
			System.out.println("map exists");
			this.mapFileAlteration.readMapFile();
			return new ResponseWrapper(200, "Map exists");

		} else {
			// create new map in map folder
			System.out.println("creating new map");
			try {
				if (new File(ProjectConfig.MAP_FILES_PATH + mapFileName).createNewFile()) {
					System.out.println("map created successfully");
					MapPhaseState.CURRENT_MAP = mapFileName;
					this.mapFileAlteration.readMapFile();
					return new ResponseWrapper(200, "Map created successfully ");
					// map file created successfully
				}
			} catch (IOException exception) {
				// error occured in creating new map and please check with adminstrator
				System.out.println(exception);
				return new ResponseWrapper(500, "Internal ERROR OCCURED .... PLEASE CONNECT WITH ADMIN");
			}

		}
		return new ResponseWrapper(500, "Internal ERROR OCCURED .... PLEASE CONNECT WITH ADMIN");

	}

	

	public ResponseWrapper validateMap(String mapFileName) {
		return null;

	}
}
