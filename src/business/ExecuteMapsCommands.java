package business;

import java.io.File;
import java.io.IOException;
import Constants.ProjectConfig;
import GamePhase.MapPhaseState;
import model.Continent;
import model.Country;
import model.MapModel;
import model.ResponseWrapper;

public class ExecuteMapsCommands {

	public ExecuteMapsCommands() {

	}

	public ResponseWrapper addContinent(Continent continent) {

		return null;
	}

	public ResponseWrapper removeContinent(Continent continent) {

		return null;
	}

	public ResponseWrapper addCountry(Country country) {

		return null;
	}

	public ResponseWrapper removeCountry(Country country) {

		return null;
	}

	public ResponseWrapper addNeighbour(Country country, Country neighbourCountry) {

		return null;
	}

	public ResponseWrapper removeNeighbour(Country country, Country neighbourCountry) {

		return null;
	}

	public ResponseWrapper showMap() {

		return null;
	}
	
	public ResponseWrapper saveMap(String mapFileName) {
		
		return null;
	}
	public ResponseWrapper validateMap() {
		
		return null;
	}

	public ResponseWrapper editOrCreateMap(String mapFileName) {

		if (new File(ProjectConfig.MAP_FILES_PATH + mapFileName).exists()) {
			// load map to the game and show as well for better understanding
			MapPhaseState.CURRENT_MAP = mapFileName;
			System.out.println("map exists");
			return new ResponseWrapper(200, "Map exists");

		} else {
			// create new map in map folder
			System.out.println("creating new map");
			try {
				if (new File(ProjectConfig.MAP_FILES_PATH + mapFileName).createNewFile()) {
					System.out.println("map created successfully");
					MapPhaseState.CURRENT_MAP = mapFileName;

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

	public void showMap(String mapFileName) {

	}

	public ResponseWrapper validateMap(String mapFileName) {
		return null;

	}
}
