package persistence;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
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

public class MapFileAlteration {
	
	private FileReader mapFileReader;
	private BufferedReader bufferReader;
	
	private FileWriter mapFileWriter;
	private BufferedWriter bufferWriter;
	
	private  MapModel mapModel;
	
	public MapFileAlteration() {
		mapModel = new MapModel();
	}
	
	public void readMapFile() {
		
		try {
			System.out.println(ProjectConfig.MAP_FILES_PATH+MapPhaseState.CURRENT_MAP);
			mapFileReader = new FileReader(ProjectConfig.MAP_FILES_PATH+MapPhaseState.CURRENT_MAP);
			bufferReader = new BufferedReader(mapFileReader);
			
		} catch (FileNotFoundException e) {
			// Log
			e.printStackTrace();
		}
		
		String mapFileLine ;
		boolean isMapContent , isCountriesTableContent,isContinentTableContent,isBorderTableContent;
		isMapContent = isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;
		
		try {
			while ((mapFileLine = bufferReader.readLine()) != null) {
				if(mapFileLine.equals("MAP")) {
					isMapContent = true;
					isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;
					continue;
				}else if(mapFileLine.equals("CONTINENTS_TABLE")) {
					isContinentTableContent = true;
					isMapContent = isCountriesTableContent = isBorderTableContent = false;
					continue;
				}else if(mapFileLine.equals("COUNTRIES_TABLE")) {
					isCountriesTableContent = true;
					isMapContent = isContinentTableContent = isBorderTableContent = false;
					continue;
				}else if(mapFileLine.equals("BORDERS_TABLE")) {
					isBorderTableContent = true;
					isMapContent = isCountriesTableContent = isContinentTableContent = false;
					continue;
				}
				
				if(isMapContent) {
					this.mapModel.setMapName(mapFileLine);
				}else if(isContinentTableContent) {
					String[] continentRow = mapFileLine.trim().split("\\s+");
					System.out.println("M"+continentRow[0]);
					this.mapModel.addContinent(new Continent(Integer.parseInt(continentRow[0]),continentRow[1]));
				}else if(isCountriesTableContent) {
					String[] countryRow = mapFileLine.trim().split("\\s+");
					Country country = new Country(Integer.parseInt(countryRow[0]), countryRow[1]);
					System.out.println("R"+countryRow[2]);
					this.mapModel.addContinentCountries(this.mapModel.getContinents().get(Integer.parseInt(countryRow[2])), country);
				}else if(isBorderTableContent) {
					String[] borderRow = mapFileLine.trim().split("\\s+");
					System.out.println("qwe" + borderRow[0] + borderRow[1]);
					Country mainCountry = this.mapModel.getCountries().get(Integer.parseInt(borderRow[0]));
					System.out.println("T"+mainCountry.getCountryId());
					for(int counter = 1; counter<borderRow.length; counter++) {
						System.out.println("N1"+counter);
						System.out.println(this.mapModel.getCountries().get(counter).getCountryId());
						this.mapModel.addBorders(mainCountry, this.mapModel.getCountries().get(Integer.parseInt(borderRow[counter])));	
					}
				}
				
			}
		} catch (IOException e) {
			
			e.printStackTrace();
		}
	}
	
	private void writeMapFile() {
		try {
			System.out.println(ProjectConfig.MAP_FILES_PATH+MapPhaseState.CURRENT_MAP);
			System.out.println("Write Map File");
			mapFileWriter = new FileWriter(ProjectConfig.MAP_FILES_PATH+MapPhaseState.CURRENT_MAP);
			bufferWriter = new BufferedWriter(mapFileWriter);
			
			String mapFileData = "MAP\n" + MapPhaseState.CURRENT_MAP+"\nCONTINENTS_TABLE\n";
			System.out.println(this.mapModel.getContinents().size());
			for(Continent continent : this.mapModel.getContinents()) {
				mapFileData += continent.getUniqueContinetId() + " " + continent.getContinentId() + "\n";
			}
			
			if(this.mapModel.getCountries() != null){
				mapFileData += "COUNTRIES_TABLE\n";
				for(Country country : this.mapModel.getCountries()) {
					
					mapFileData += country.getUniqueCountryId() + " " +country.getCountryId() + " " + country.getContinent().getUniqueContinetId()+"\n";
				}
			}
			if(this.mapModel.getBorders() != null) {
				mapFileData += "BORDERS_TABLE";
				for(Map.Entry<Country, List<Country>> border: this.mapModel.getBorders().entrySet()) {
					System.out.println(border.getKey().getCountryId() + " Q ");
					mapFileData += "\n"+border.getKey().getUniqueCountryId();
					for(Country country : border.getValue()) {
						System.out.println(country.getCountryId() + " W ");
						mapFileData += " " + country.getUniqueCountryId();
					
					}
				}
			}
			bufferWriter.write(mapFileData);
			bufferWriter.flush();
		} catch (  IOException e) {
			// Log
			e.printStackTrace();
		}
		
		
	}
	
	public ResponseWrapper addContinent(Continent continent) {
		System.out.println("read map file");
		if(this.mapModel.getContinents() == null) {
			continent.setUniqueContinetId(0);
		}else {
			continent.setUniqueContinetId(this.mapModel.getContinents().size());
		}
		
		this.mapModel.addContinent(continent);
		
		return new ResponseWrapper(200, "Add continent successfully ");
	}
	
	public ResponseWrapper addCountry(Country country) {
		
		for(Continent continent : this.mapModel.getContinents()) {
			if(continent.getContinentId().equals(country.getContinent().getContinentId())) {
				country.setContinent(continent);
				break;
			}
		}
		if(this.mapModel.getCountries() == null) {
			country.setUniqueCountryId(0);
		}else {
			country.setUniqueCountryId(this.mapModel.getCountries().size());
		}
		
		this.mapModel.addContinentCountries(country.getContinent(), country);
		
		return new ResponseWrapper(200, "add country successfully ");
	}
	
	public ResponseWrapper addNeighbour(Country mainCountry, Country neighbourCountry) {
		
		for(Country country : this.mapModel.getCountries()) {
			if(country.getCountryId().equals(mainCountry.getCountryId())) {
				mainCountry = country;
				System.out.println(mainCountry.getUniqueCountryId()+mainCountry.getCountryId());
			}
			if(country.getCountryId().equals(neighbourCountry.getCountryId())) {
				neighbourCountry = country;
				System.out.println(neighbourCountry.getUniqueCountryId()+neighbourCountry.getCountryId());
			}
		}
		this.mapModel.addBorders(mainCountry, neighbourCountry);
		this.mapModel.addBorders(neighbourCountry, mainCountry);
		return new ResponseWrapper(200, "add neighbour successfully ");
	}
	
	public ResponseWrapper saveMap(String mapFileName) {
		System.out.println("savemap");
		this.writeMapFile();
		return new ResponseWrapper(200, "Save Map successfully ");
	}
	
	public ResponseWrapper showmap() {

		System.out.format("\n Map Details : \n");
		System.out.format("\n Continents of Map are : \n");
		System.out.format("+------------------+%n");
		System.out.format("| Continent's name |%n");
		System.out.format("+------------------+%n");

		this.mapModel.getContinents().stream().forEach((continent) -> {
			String table = "|%-18s|%n";
			System.out.format(table, continent.getContinentId());
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

		for (Map.Entry<Country, List<Country>> entry : this.mapModel.getBorders().entrySet()) {
			String tablePattern = "|%-23s|%-18s|%-60s|%n";
			for (Country country : entry.getValue()) {
				System.out.format(tablePattern, entry.getKey().getCountryId(), country.getContinent().getContinentId(),
						this.getCountriesList(entry.getValue()));
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
		
		return new ResponseWrapper(200,"show map done successfully");

	}
	
	private String getCountriesList(List<Country> countriesList) {
		String countList = "";
		for (Country country : countriesList) {
			countList += country.getCountryId() + "-";
		}
		return countList.length() > 0 ? countList.substring(0, countList.length() - 1) : "";
	}

	
	public ResponseWrapper removeContinent(Continent continent) {

		// Deleting Continents
		// Check if this works
		mapModel.getContinents().remove(continent);

		for (int contIndex = 0; contIndex < mapModel.getContinents().size(); contIndex++) {
			if (mapModel.getContinents().get(contIndex).getContinentId().equals(continent.getContinentId())) {
				mapModel.getContinents().remove(contIndex);
			}
		}
		// Deleting Countries
		List<Country> deletedCountriesList = new ArrayList<Country>();
		for (int countryIndex = 0; countryIndex < mapModel.getCountries().size(); countryIndex++) {
			if (mapModel.getCountries().get(countryIndex).getContinent().equals(continent.getContinentId())) {
				deletedCountriesList.add(mapModel.getCountries().get(countryIndex));
				mapModel.getCountries().remove(countryIndex);
			}
		}
		// Removing Continents from Map
		for (Map.Entry<Continent, List<Country>> mapEntry : mapModel.getContinentCountries().entrySet()) {
			if (mapEntry.getKey().getContinentId().equals(continent.getContinentId())) {
				mapModel.getContinentCountries().entrySet().remove(continent);
			}
		}
		// removing countries from Borders
		for (Map.Entry<Country, List<Country>> mapEntry : mapModel.getBorders().entrySet()) {
			if (deletedCountriesList.contains(mapEntry.getKey())) {
				mapModel.getBorders().entrySet().remove(mapEntry.getKey());
			}
		}

		// removing neighboring countries from borders
		List<Country> countriesUpdatedBorderList = new ArrayList<Country>();
		for (Map.Entry<Country, List<Country>> mapEntry : mapModel.getBorders().entrySet()) {
			for (Country country : deletedCountriesList) {
				if (mapEntry.getValue().contains(country)) {
					countriesUpdatedBorderList = mapModel.getBorders().get(mapEntry.getKey());
					countriesUpdatedBorderList.remove(country);
				}
			}
			mapModel.getBorders().put(mapEntry.getKey(), countriesUpdatedBorderList);
			countriesUpdatedBorderList.clear();
		}
		
		return new ResponseWrapper(200,"Remove Continent Successfully");

	}

	public ResponseWrapper removeCountry(Country country) {
		// Deleting Countries
		List<Country> deletedCountriesList = new ArrayList<Country>();
		for (int countryIndex = 0; countryIndex < mapModel.getCountries().size(); countryIndex++) {
			if (mapModel.getCountries().get(countryIndex).getCountryId().equals(country.getCountryId())) {
				deletedCountriesList.add(mapModel.getCountries().get(countryIndex));
				mapModel.getCountries().remove(countryIndex);
			}
		}

		// Removing countries from ContinentCountries map
		List<Country> countriesUpdatedContinentsList = new ArrayList<Country>();
		for (Map.Entry<Continent, List<Country>> mapEntry : mapModel.getContinentCountries().entrySet()) {
			for (Country contr : deletedCountriesList) {
				if (mapEntry.getValue().contains(contr)) {
					countriesUpdatedContinentsList = mapModel.getContinentCountries().get(mapEntry.getKey());
					countriesUpdatedContinentsList.remove(contr);
				}
			}
			mapModel.getContinentCountries().put(mapEntry.getKey(), countriesUpdatedContinentsList);
			countriesUpdatedContinentsList.clear();
		}

		// Removing countries from Borders
		for (Map.Entry<Country, List<Country>> mapEntry : mapModel.getBorders().entrySet()) {
			if (deletedCountriesList.contains(mapEntry.getKey())) {
				mapModel.getBorders().entrySet().remove(mapEntry.getKey());
			}
		}

		// Removing neighboring countries from borders
		List<Country> countriesUpdatedBorderList = new ArrayList<Country>();
		for (Map.Entry<Country, List<Country>> mapEntry : mapModel.getBorders().entrySet()) {
			for (Country contr : deletedCountriesList) {
				if (mapEntry.getValue().contains(contr)) {
					countriesUpdatedBorderList = mapModel.getBorders().get(mapEntry.getKey());
					countriesUpdatedBorderList.remove(contr);
				}
			}
			mapModel.getBorders().put(mapEntry.getKey(), countriesUpdatedBorderList);
			countriesUpdatedBorderList.clear();
		}
		return new ResponseWrapper(200, "Remove country successfully");
	}
	public ResponseWrapper validateMap() {
		
		Set<Continent> continents = new HashSet<Continent>(this.mapModel.getContinents());
		Set<Country> countries = new HashSet<Country>(this.mapModel.getCountries());

		Boolean countryBorderRelevantData = countries.stream()
				.filter((country) -> !this.mapModel.getBorders().containsKey(country)).collect(Collectors.toList())
				.size() == 0 ? true : false;
		Boolean countryContinentNotExists = this.mapModel.getCountries().stream()
				.anyMatch((country) -> country.getContinent().getContinentId() == null
						|| country.getContinent().getContientValue() == null
						|| "".equals(country.getContinent().getContinentId())
						|| "".equals(country.getContinent().getContientValue()));
		// to be removed
		this.mapModel.getCountries().stream().forEach((country)-> {System.out.println(country.getCountryId() + " - " + country.getContinent().getContientValue());});
		
		Boolean countryContinentExistsInContinentsList = this.mapModel.getCountries().stream()
				.allMatch((country) -> continents.contains(country.getContinent().getContientValue()));
		Boolean countryBorderNotExists = this.mapModel.getBorders().entrySet().stream()
				.anyMatch(borderMap -> borderMap.getValue().size() == 0) ? true : false;

		if ("".equals(this.mapModel.getMapName()) || this.mapModel.getMapName() == null || this.mapModel.getContinents().size() == 0
				|| this.mapModel.getCountries().size() == 0 || this.mapModel.getContinentCountries().size() == 0
				|| this.mapModel.getBorders().size() == 0) {

			return new ResponseWrapper(404, "Map is not created Properly");

		} else if (continents.size() != this.mapModel.getContinents().size()
				|| countries.size() != this.mapModel.getCountries().size()) {
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

}
