package persistence;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import Constants.ProjectConfig;
import GamePhase.MapPhaseState;
import model.Continent;
import model.Country;
import model.MapModel;
import model.ResponseWrapper;

public class MapFileAlteration {

	private FileReader mapFileReader;
	private BufferedReader bufferReader;

	private MapModel mapModel;

	public MapFileAlteration() {
		mapModel = new MapModel();

		try {
			mapFileReader = new FileReader(ProjectConfig.MAP_FILES_PATH + MapPhaseState.CURRENT_MAP);
			bufferReader = new BufferedReader(mapFileReader);

		} catch (FileNotFoundException e) {
			// Log
			e.printStackTrace();
		}

	}

	private void readMapFile() {

		String mapFileLine;
		boolean isMapContent, isCountriesTableContent, isContinentTableContent, isBorderTableContent;
		isMapContent = isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;

		try {
			while ((mapFileLine = bufferReader.readLine()) != null) {
				if (mapFileLine.equals("MAP")) {
					isMapContent = true;
					isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;
				} else if (mapFileLine.equals("CONTINENTS_TABLE")) {
					isContinentTableContent = true;
					isMapContent = isCountriesTableContent = isBorderTableContent = false;
				} else if (mapFileLine.equals("COUNTRIES_TABLE")) {
					isCountriesTableContent = true;
					isMapContent = isContinentTableContent = isBorderTableContent = false;
				} else if (mapFileLine.equals("BORDERS_TABLE")) {
					isBorderTableContent = true;
					isMapContent = isCountriesTableContent = isContinentTableContent = false;
				}

				if (isMapContent) {
					this.mapModel.setMapName(mapFileLine);
				} else if (isContinentTableContent) {
					String[] continentRow = mapFileLine.trim().split("\\\\s+");
					this.mapModel.addContinent(new Continent(Integer.parseInt(continentRow[0]), continentRow[1]));
				} else if (isCountriesTableContent) {
					String[] countryRow = mapFileLine.trim().split("\\\\s+");
					Country country = new Country(Integer.parseInt(countryRow[0]), countryRow[1]);
					this.mapModel.addContinentCountries(
							this.mapModel.getContinents().get(Integer.parseInt(countryRow[3])), country);
				} else if (isBorderTableContent) {
					String[] borderRow = mapFileLine.trim().split("\\\\s+");
					Country mainCountry = this.mapModel.getCountries().get(0);
					for (int counter = 1; counter < borderRow.length; counter++) {
						this.mapModel.addBorders(mainCountry, this.mapModel.getCountries().get(counter));
					}
				}

			}
		} catch (IOException e) {

			e.printStackTrace();
		}
	}

	private String getMapName() {

		return "";
	}

	private List<Object[][]> getContinentTableContent() {

		return null;

	}

	private List<Object[][][]> getCountryTableContent() {

		return null;
	}

	private List<Object[][][]> getBorderTableContent() {

		return null;
	}

	public ResponseWrapper addContinent(Continent continent) {

		return null;
	}

	public void removeContinent(Continent continent) {

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

	}

	public void removeCountry(Country country) {
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
	}

}
