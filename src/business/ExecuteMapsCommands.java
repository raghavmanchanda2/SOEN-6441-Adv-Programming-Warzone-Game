package business;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

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

	public ResponseWrapper validateMap(MapModel mapObject) {

		Set<Continent> continents = new HashSet<Continent>(mapObject.getContinents());
		Set<Country> countries = new HashSet<Country>(mapObject.getCountries());

		Boolean countryBorderRelevantData = countries.stream()
				.filter((country) -> !mapObject.getBorders().containsKey(country)).collect(Collectors.toList())
				.size() == 0 ? true : false;
		Boolean countryContinentNotExists = mapObject.getCountries().stream()
				.anyMatch((country) -> country.getContinent().getContinentId() == null
						|| country.getContinent().getContientValue() == null
						|| "".equals(country.getContinent().getContinentId())
						|| "".equals(country.getContinent().getContientValue()));
		Boolean countryContinentExistsInContinentsList = mapObject.getCountries().stream()
				.allMatch((country) -> continents.contains(country.getContinent().getContientValue()));
		Boolean countryBorderNotExists = mapObject.getBorders().entrySet().stream()
				.anyMatch(borderMap -> borderMap.getValue().size() == 0) ? true : false;

		if ("".equals(mapObject.getMapName()) || mapObject.getMapName() == null || mapObject.getContinents().size() == 0
				|| mapObject.getCountries().size() == 0 || mapObject.getContinentCountries().size() == 0
				|| mapObject.getBorders().size() == 0) {

			return new ResponseWrapper(404, "Map is not created Properly");

		} else if (continents.size() != mapObject.getContinents().size()
				|| countries.size() != mapObject.getContinents().size()) {
			return new ResponseWrapper(404, "Duplicate Continent or Country Found in map");

		} else if (Boolean.TRUE.equals(countryContinentNotExists)) {
			return new ResponseWrapper(404, "Country Continent is missing");

		} else if (countries.size() < 2) {
			return new ResponseWrapper(404, " Countries Should be Atleast 2 in map ");

		} else if (Boolean.FALSE.equals(countryBorderRelevantData)) {
			return new ResponseWrapper(404,
					" Border Data for Countries is not consistent with Countries that are added ");

		} else if (Boolean.FALSE.equals(countryContinentExistsInContinentsList)) {
			return new ResponseWrapper(404, " Countries Continents Mismatch ");

		} else if (Boolean.FALSE.equals(countryContinentExistsInContinentsList)) {
			return new ResponseWrapper(404, " Countries Continents Mismatch ");

		} else if (Boolean.TRUE.equals(countryBorderNotExists)) {
			return new ResponseWrapper(404, " Countries Border Missing ");
		}

		return new ResponseWrapper(200, " VALIDATION SUCCESSFUL ");	
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
}
