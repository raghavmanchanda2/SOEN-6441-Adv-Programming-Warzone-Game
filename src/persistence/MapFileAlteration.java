package persistence;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
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
	
	private FileReader d_mapFileReader;
	private BufferedReader d_bufferReader;
	
	private FileWriter d_mapFileWriter;
	private BufferedWriter d_bufferWriter;
	
	private MapModel d_mapModel;
	
	public MapFileAlteration() {
		d_mapModel = new MapModel();
	}
	
	public void readMapFile() {
		
		try {
			System.out.println(ProjectConfig.D_MAP_FILES_PATH+MapPhaseState.D_CURRENT_MAP);
			d_mapFileReader = new FileReader(ProjectConfig.D_MAP_FILES_PATH+MapPhaseState.D_CURRENT_MAP);
			d_bufferReader = new BufferedReader(d_mapFileReader);
			
		} catch (FileNotFoundException p_e) {
			// Log
			p_e.printStackTrace();
		}
		
		String l_mapFileLine ;
		boolean l_isMapContent , l_isCountriesTableContent,l_isContinentTableContent,l_isBorderTableContent;
		l_isMapContent = l_isCountriesTableContent = l_isContinentTableContent = l_isBorderTableContent = false;
		
		try {
			while ((l_mapFileLine = d_bufferReader.readLine()) != null) {
				if(l_mapFileLine.equals("MAP")) {
					l_isMapContent = true;
					l_isCountriesTableContent = l_isContinentTableContent = l_isBorderTableContent = false;
					continue;
				}else if(l_mapFileLine.equals("CONTINENTS_TABLE")) {
					l_isContinentTableContent = true;
					l_isMapContent = l_isCountriesTableContent = l_isBorderTableContent = false;
					continue;
				}else if(l_mapFileLine.equals("COUNTRIES_TABLE")) {
					l_isCountriesTableContent = true;
					l_isMapContent = l_isContinentTableContent = l_isBorderTableContent = false;
					continue;
				}else if(l_mapFileLine.equals("BORDERS_TABLE")) {
					l_isBorderTableContent = true;
					l_isMapContent = l_isCountriesTableContent = l_isContinentTableContent = false;
					continue;
				}
				
				if(l_isMapContent) {
					this.d_mapModel.setMapName(l_mapFileLine);
				}else if(l_isContinentTableContent) {
					String[] l_continentRow = l_mapFileLine.trim().split("\\s+");
					System.out.println("M"+l_continentRow[0]);
					this.d_mapModel.addContinent(new Continent(Integer.parseInt(l_continentRow[0]),l_continentRow[1],l_continentRow[2]));
				}else if(l_isCountriesTableContent) {
					String[] l_countryRow = l_mapFileLine.trim().split("\\s+");
					Country l_country = new Country(Integer.parseInt(l_countryRow[0]), l_countryRow[1]);
					System.out.println("R"+l_countryRow[2]);
					this.d_mapModel.addContinentCountries(this.d_mapModel.getContinents().get(Integer.parseInt(l_countryRow[2])), l_country);
				}else if(l_isBorderTableContent) {
					String[] l_borderRow = l_mapFileLine.trim().split("\\s+");
					System.out.println("qwe" + l_borderRow[0] + l_borderRow[1]);
					Country l_mainCountry = this.d_mapModel.getCountries().get(Integer.parseInt(l_borderRow[0]));
					System.out.println("T"+l_mainCountry.getCountryId());
					for(int counter = 1; counter<l_borderRow.length; counter++) {
						System.out.println("N1"+counter);
						System.out.println(this.d_mapModel.getCountries().get(counter).getCountryId());
						this.d_mapModel.addBorders(l_mainCountry, this.d_mapModel.getCountries().get(Integer.parseInt(l_borderRow[counter])));	
					}
				}
				
			}
		} catch (IOException p_e) {
			
			p_e.printStackTrace();
		}
	}
	
	private void writeMapFile() {
		try {
			System.out.println(ProjectConfig.D_MAP_FILES_PATH+MapPhaseState.D_CURRENT_MAP);
			System.out.println("Write Map File");
			d_mapFileWriter = new FileWriter(ProjectConfig.D_MAP_FILES_PATH+MapPhaseState.D_CURRENT_MAP);
			d_bufferWriter = new BufferedWriter(d_mapFileWriter);
			
			String l_mapFileData = "MAP\n" + MapPhaseState.D_CURRENT_MAP+"\nCONTINENTS_TABLE\n";
			System.out.println(this.d_mapModel.getContinents().size());
			for(Continent continent : this.d_mapModel.getContinents()) {
				l_mapFileData += continent.getUniqueContinetId() + " " + continent.getContinentId() + "\n";
			}
			
			if(this.d_mapModel.getCountries() != null){
				l_mapFileData += "COUNTRIES_TABLE\n";
				for(Country country : this.d_mapModel.getCountries()) {
					
					l_mapFileData += country.getUniqueCountryId() + " " +country.getCountryId() + " " + country.getContinent().getUniqueContinetId()+"\n";
				}
			}
			if(this.d_mapModel.getBorders() != null) {
				l_mapFileData += "BORDERS_TABLE";
				for(Map.Entry<Country, List<Country>> border: this.d_mapModel.getBorders().entrySet()) {
					System.out.println(border.getKey().getCountryId() + " Q ");
					l_mapFileData += "\n"+border.getKey().getUniqueCountryId();
					for(Country country : border.getValue()) {
						System.out.println(country.getCountryId() + " W ");
						l_mapFileData += " " + country.getUniqueCountryId();
					
					}
				}
			}
			d_bufferWriter.write(l_mapFileData);
			d_bufferWriter.flush();
		} catch (  IOException p_e) {
			// Log
			p_e.printStackTrace();
		}
		
		
	}
	
	public ResponseWrapper addContinent(Continent p_continent) {
		System.out.println("read map file");
		if(this.d_mapModel.getContinents() == null) {
			p_continent.setUniqueContinetId(0);
		}else {
			p_continent.setUniqueContinetId(this.d_mapModel.getContinents().size());
		}
		
		this.d_mapModel.addContinent(p_continent);
		
		return new ResponseWrapper(200, "Add continent successfully ");
	}
	
	public ResponseWrapper addCountry(Country p_country) {
		
		for(Continent continent : this.d_mapModel.getContinents()) {
			if(continent.getContinentId().equals(p_country.getContinent().getContinentId())) {
				p_country.setContinent(continent);
				break;
			}
		}
		if(this.d_mapModel.getCountries() == null) {
			p_country.setUniqueCountryId(0);
		}else {
			p_country.setUniqueCountryId(this.d_mapModel.getCountries().size());
		}
		
		this.d_mapModel.addContinentCountries(p_country.getContinent(), p_country);
		
		return new ResponseWrapper(200, "add country successfully ");
	}
	
	public ResponseWrapper addNeighbour(Country p_mainCountry, Country p_neighbourCountry) {
		
		for(Country country : this.d_mapModel.getCountries()) {
			if(country.getCountryId().equals(p_mainCountry.getCountryId())) {
				p_mainCountry = country;
				System.out.println(p_mainCountry.getUniqueCountryId()+p_mainCountry.getCountryId());
			}
			if(country.getCountryId().equals(p_neighbourCountry.getCountryId())) {
				p_neighbourCountry = country;
				System.out.println(p_neighbourCountry.getUniqueCountryId()+p_neighbourCountry.getCountryId());
			}
		}
		this.d_mapModel.addBorders(p_mainCountry, p_neighbourCountry);
		this.d_mapModel.addBorders(p_neighbourCountry, p_mainCountry);
		return new ResponseWrapper(200, "add neighbour successfully ");
	}
	
	public ResponseWrapper saveMap(String p_mapFileName) {
		System.out.println("savemap");
		this.writeMapFile();
		return new ResponseWrapper(200, "Save Map successfully ");
	}
	
	public ResponseWrapper showmap() {

		System.out.format("\n Map Details are : \n");
		System.out.format("\n Continents of Map are : \n");
		System.out.format("+------------------+%n");
		System.out.format("| Continent's Name |%n");
		System.out.format("+------------------+%n");

		this.d_mapModel.getContinents().stream().forEach((continent) -> {
			String l_table = "|%-20s|%n";
			System.out.format(l_table, continent.getContinentId());
		});

		System.out.format("+------------------+%n");

		// Showing Countries in the Continent and their details
		System.out.format("\n Countries in this Map and their details are : \n");

		System.out.format("+--------------+-----------------------+------------------+----------------------------+---------------+-%n");
		System.out.format("     Country Name     |    Continent Name    |   Bordering Countries                                      |%n");
		System.out.format("+--------------+-----------------------+------------------+----------------------------+----------------+%n");

		for (Map.Entry<Country, List<Country>> entry : this.d_mapModel.getBorders().entrySet()) {
			String l_tablePattern = "|%-25s|%-20s|%-70s|%n";
			System.out.format(l_tablePattern, entry.getKey().getCountryId(), entry.getKey().getContinent().getContinentId(),
					this.getCountriesList(entry.getValue()));
		}

		System.out.format("+--------------+-----------------------+------------------+----------------------------+----------------+%n");

		System.out.format("\nPlayers in this game are : ");
//		if (l_Players != null) {
//			l_Players.forEach((key, value) -> d_Logger.log(key));
//			d_Logger.log("");
//		}

		// Showing the Ownership of the players
		System.out.format("The Map ownership of the players are : ");

		System.out.format("+---------------+-------------------------------+%n");
		System.out.format("| Player's Name |    Continent's Controlled    |%n");
		System.out.format("+---------------+-------------------------------+%n");

		//String table = "|%-15s|%-30s|%n";

//		for (Player l_Player : d_GameMap.getPlayers().values()) {
//			System.out.format(l_Table1, l_Player.getName(),
//					l_Player.createACaptureList(l_Player.getCapturedCountries()), l_Player.getReinforcementArmies());
//		}

		System.out.format("+---------------+-------------------------------+%n");
		
		return new ResponseWrapper(200," Show Map Done Successfully");

	}
	
	private String getCountriesList(List<Country> countriesList) {
		String l_countList = "";
		for (Country country : countriesList) {
			l_countList += country.getCountryId() + "-";
		}
		return l_countList.length() > 0 ? l_countList.substring(0, l_countList.length() - 1) : "";
	}

	
	public ResponseWrapper removeContinent(Continent p_continent) {

		// Deleting Continents

		for (int contIndex = 0; contIndex < d_mapModel.getContinents().size(); contIndex++) {
			if (d_mapModel.getContinents().get(contIndex).getContinentId().equals(p_continent.getContinentId())) {
				d_mapModel.getContinents().remove(contIndex);
			}
		}
		
		// Deleting Countries
		List<Country> l_deletedCountriesList = d_mapModel.getCountries();
		
		d_mapModel.getCountries().stream().filter((conti)-> ! conti.getContinent().getContinentId().equals(p_continent.getContinentId()));
		l_deletedCountriesList.removeIf((cont)-> !cont.getContinent().getContinentId().equals(p_continent.getContinentId()));
		
		// Removing Continents from Map
		
		d_mapModel.getContinentCountries().entrySet().removeIf((map)-> map.getKey().getContinentId().equals(p_continent.getContinentId()));
	
		// Removing countries from Borders
		Map<Country, List<Country>> l_tempMap = new HashMap<Country, List<Country>>(d_mapModel.getBorders());
		
		for (Map.Entry<Country, List<Country>> mapEntry : l_tempMap.entrySet()) {
			Boolean l_countExists= l_deletedCountriesList.stream().anyMatch((cont)-> cont.getCountryId().equals(mapEntry.getKey().getCountryId()));
			if (Boolean.TRUE.equals(l_countExists)) {
				for (Country country : l_deletedCountriesList) {
					d_mapModel.getBorders().entrySet().removeIf((map)-> map.getKey().getCountryId().equals(country.getCountryId()));
				}				
			}
		}	

		// Removing neighboring countries from Borders
		List<Country> l_countriesUpdatedBorderList;
		for (Map.Entry<Country, List<Country>> mapEntry : d_mapModel.getBorders().entrySet()) {
			l_countriesUpdatedBorderList = mapEntry.getValue();
			for (Country country : l_deletedCountriesList) {
				l_countriesUpdatedBorderList = l_countriesUpdatedBorderList.stream()
						.filter((cont) -> !cont.getCountryId().equals(country.getCountryId()))
						.collect(Collectors.toList());
			}
			d_mapModel.getBorders().put(mapEntry.getKey(), l_countriesUpdatedBorderList);
		}
		
		return new ResponseWrapper(200,"Removed Continent Successfully");

	}

	public ResponseWrapper removeCountry(Country country) {
		// Deleting Countries
		List<Country> l_deletedCountriesList = d_mapModel.getCountries();
		
		d_mapModel.getCountries().stream().filter((conti)-> ! conti.getCountryId().equals(country.getCountryId()));
		l_deletedCountriesList.removeIf((conti)-> ! conti.getCountryId().equals(country.getCountryId()));	
		
		// Removing countries from ContinentCountries map
		
		List<Country> l_continentCountriesList;
		for (Map.Entry<Continent, List<Country>> mapEntry : d_mapModel.getContinentCountries().entrySet()) {
			l_continentCountriesList = mapEntry.getValue();
			for (Country conti : l_deletedCountriesList) {
				l_continentCountriesList = l_continentCountriesList.stream()
						.filter((cont) -> !cont.getCountryId().equals(conti.getCountryId()))
						.collect(Collectors.toList());
			}
			d_mapModel.getContinentCountries().put(mapEntry.getKey(), l_continentCountriesList);
		}
		
		// Removing countries from Borders
		Map<Country, List<Country>> l_tempMap = new HashMap<Country, List<Country>>(d_mapModel.getBorders());
		
		for (Map.Entry<Country, List<Country>> mapEntry : l_tempMap.entrySet()) {
			Boolean l_countExists= l_deletedCountriesList.stream().anyMatch((cont)-> cont.getCountryId().equals(mapEntry.getKey().getCountryId()));
			if (Boolean.TRUE.equals(l_countExists)) {
				for (Country cont : l_deletedCountriesList) {
					d_mapModel.getBorders().entrySet().removeIf((map)-> map.getKey().getCountryId().equals(cont.getCountryId()));
				}				
			}
		}	
		
		// Removing neighboring countries from borders
				
		List<Country> l_countriesUpdatedBorderList;
		for (Map.Entry<Country, List<Country>> mapEntry : d_mapModel.getBorders().entrySet()) {
			l_countriesUpdatedBorderList = mapEntry.getValue();
			for (Country conti : l_deletedCountriesList) {
				l_countriesUpdatedBorderList = l_countriesUpdatedBorderList.stream()
						.filter((cont) -> !cont.getCountryId().equals(conti.getCountryId()))
						.collect(Collectors.toList());
			}
			d_mapModel.getBorders().put(mapEntry.getKey(), l_countriesUpdatedBorderList);
		}
	
		return new ResponseWrapper(200, "Removed Country Successfully");
	}
	
	public ResponseWrapper removeNeighbour(Country mainCountry,Country neighbourCountry) {	
		
		List<Country> l_neighbouringCountriesList;
		for (Map.Entry<Country, List<Country>> mapEntry : d_mapModel.getBorders().entrySet()) {
			if(!mapEntry.getKey().getCountryId().equals(mainCountry.getCountryId()))
			{
				continue;
			}
			l_neighbouringCountriesList = mapEntry.getValue();			
			l_neighbouringCountriesList = l_neighbouringCountriesList.stream()
						.filter((cont) -> !cont.getCountryId().equals(neighbourCountry.getCountryId()))
						.collect(Collectors.toList());			
			d_mapModel.getBorders().put(mapEntry.getKey(), l_neighbouringCountriesList);
		}
		
		return new ResponseWrapper(200, "Removed Neighbouring Country Successfully");
		
	}
	
	public ResponseWrapper validateMap() {
		
		Set<Continent> l_continents = new HashSet<Continent>(this.d_mapModel.getContinents());
		Set<Country> l_countries = new HashSet<Country>(this.d_mapModel.getCountries());

		Boolean l_countryBorderRelevantData = l_countries.stream()
				.filter((country) -> !this.d_mapModel.getBorders().containsKey(country)).collect(Collectors.toList())
				.size() == 0 ? true : false;
		Boolean l_countryContinentNotExists = this.d_mapModel.getCountries().stream()
				.anyMatch((country) -> country.getContinent().getContinentId() == null
						|| country.getContinent().getContientValue() == null
						|| "".equals(country.getContinent().getContinentId())
						|| "".equals(country.getContinent().getContientValue()));
	
		Boolean l_countryContinentExistsInContinentsList=false;
		for(Country count : this.d_mapModel.getCountries()) 
		{
			for(Continent conti : l_continents)
			{
				if(count.getContinent().getContinentId().contains(conti.getContinentId()))
				{
					l_countryContinentExistsInContinentsList=true;
					break;
				}				
			}
			if(Boolean.FALSE.equals(l_countryContinentExistsInContinentsList))
			{
				break;
			}
			
		}
				
		Boolean l_countryBorderNotExists = this.d_mapModel.getBorders().entrySet().stream()
				.anyMatch(borderMap -> borderMap.getValue().size() == 0) ? true : false;

		if ("".equals(this.d_mapModel.getMapName()) || this.d_mapModel.getMapName() == null || this.d_mapModel.getContinents().size() == 0
				|| this.d_mapModel.getCountries().size() == 0 || this.d_mapModel.getContinentCountries().size() == 0
				|| this.d_mapModel.getBorders().size() == 0) {

			return new ResponseWrapper(404, "Map is not created Properly");

		} else if (l_continents.size() != this.d_mapModel.getContinents().size()
				|| l_countries.size() != this.d_mapModel.getCountries().size()) {
			return new ResponseWrapper(404, "Duplicate Continent or Country Found in map");

		} else if (Boolean.TRUE.equals(l_countryContinentNotExists)) {
			return new ResponseWrapper(404, "Country Continent Data is inappropriate");

		} else if (l_countries.size() < 2) {
			return new ResponseWrapper(404, " Countries Should be Atleast 2 in map ");

		} else if (Boolean.FALSE.equals(l_countryBorderRelevantData)) {
			return new ResponseWrapper(404,
					" Border Data for Countries is not consistent with Countries that are added ");

		} else if (Boolean.FALSE.equals(l_countryContinentExistsInContinentsList)) {
			return new ResponseWrapper(404, " Countries Continents Mismatch ");

		}  else if (Boolean.TRUE.equals(l_countryBorderNotExists)) {
			return new ResponseWrapper(404, " Countries Border Missing ");
		}

		return new ResponseWrapper(200, " VALIDATION SUCCESSFUL ");
	}

}
