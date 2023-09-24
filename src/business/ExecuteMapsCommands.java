package business;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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

	public void showMap(MapModel mapObject) {

		System.out.format("\n Map Details : \n");
		System.out.format("\n Continents of Map are : \n");
		System.out.format("+------------------+%n");
		System.out.format("| Continent's name |%n");
		System.out.format("+------------------+%n");

		mapObject.getContinents().stream().forEach((continent) -> {
			String table = "|%-18s|%n";
			System.out.format(table, continent.getContientValue());
		});

		System.out.format("+------------------+%n");

		// Showing Countries in the Continent and their details
		System.out.format("\nThe countries in this Map and their details are : \n");

		System.out.format(
				"+--------------+-----------------------+------------------+----------------------------+---------------+-%n");
		System.out.format(
				"     Country name     | Continent Name |   Bordering Countries                                      |%n");
		System.out.format(
				"+--------------+-----------------------+------------------+----------------------------+----------------+%n");

		mapObject.getBorders().entrySet().stream().forEach((object) -> {
			String tablePattern = "|%-23s|%-18s|%-60s|%n";
			System.out.format(tablePattern, object.getKey(), object.getValue());
		});

		for (Map.Entry<Country, List<Country>> entry : mapObject.getBorders().entrySet()) {
			String tablePattern = "|%-23s|%-18s|%-60s|%n";
			for (Country country : entry.getValue()) {
				System.out.format(tablePattern, entry.getKey(), country.getContinent(),
						getCountriesList(entry.getValue()));
			}
		}

		System.out.format(
				"+--------------+-----------------------+------------------+----------------------------+----------------+%n");

		System.out.format("\nPlayers in this game are : ");
//		if (l_Players != null) {
//			l_Players.forEach((key, value) -> d_Logger.log(key));
//			d_Logger.log("");
//		}

		// Showing the Ownership of the players
		System.out.format("The Map ownership of the players are : ");

		System.out.format("+---------------+-------------------------------+%n");
		System.out.format("| Player's name |    Continent's Controlled    |%n");
		System.out.format("+---------------+-------------------------------+%n");

		String table = "|%-15s|%-30s|%n";

//		for (Player l_Player : d_GameMap.getPlayers().values()) {
//			System.out.format(l_Table1, l_Player.getName(),
//					l_Player.createACaptureList(l_Player.getCapturedCountries()), l_Player.getReinforcementArmies());
//		}

		System.out.format("+---------------+-------------------------------+%n");
	}

	public String getCountriesList(List<Country> countriesList) {
		String countList = "";
		for (Country country : countriesList) {
			countList += country.getCountryId() + "-";
		}
		return countList.length() > 0 ? countList.substring(0, countList.length() - 1) : "";
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
