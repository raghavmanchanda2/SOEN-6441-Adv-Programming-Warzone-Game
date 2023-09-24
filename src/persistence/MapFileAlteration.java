package persistence;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

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
			mapFileReader = new FileReader(ProjectConfig.MAP_FILES_PATH+MapPhaseState.CURRENT_MAP);
			bufferReader = new BufferedReader(mapFileReader);
			
		} catch (FileNotFoundException e) {
			// Log
			e.printStackTrace();
		}
	
	}
	
	private void readMapFile() {
		
		String mapFileLine ;
		boolean isMapContent , isCountriesTableContent,isContinentTableContent,isBorderTableContent;
		isMapContent = isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;
		
		try {
			while ((mapFileLine = bufferReader.readLine()) != null) {
				if(mapFileLine.equals("MAP")) {
					isMapContent = true;
					isCountriesTableContent = isContinentTableContent = isBorderTableContent = false;
				}else if(mapFileLine.equals("CONTINENTS_TABLE")) {
					isContinentTableContent = true;
					isMapContent = isCountriesTableContent = isBorderTableContent = false;
				}else if(mapFileLine.equals("COUNTRIES_TABLE")) {
					isCountriesTableContent = true;
					isMapContent = isContinentTableContent = isBorderTableContent = false;
				}else if(mapFileLine.equals("BORDERS_TABLE")) {
					isBorderTableContent = true;
					isMapContent = isCountriesTableContent = isContinentTableContent = false;
				}
				
				if(isMapContent) {
					this.mapModel.setMapName(mapFileLine);
				}else if(isContinentTableContent) {
					String[] continentRow = mapFileLine.trim().split("\\\\s+");
					this.mapModel.addContinent(new Continent(Integer.parseInt(continentRow[0]),continentRow[1]));
				}else if(isCountriesTableContent) {
					String[] countryRow = mapFileLine.trim().split("\\\\s+");
					Country country = new Country(Integer.parseInt(countryRow[0]), countryRow[1]);
					this.mapModel.addContinentCountries(this.mapModel.getContinents().get(Integer.parseInt(countryRow[3])), country);
				}else if(isBorderTableContent) {
					String[] borderRow = mapFileLine.trim().split("\\\\s+");
					Country mainCountry = this.mapModel.getCountries().get(0);
					for(int counter = 1; counter<borderRow.length; counter++) {
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
	
	private List<Object[][]> getContinentTableContent(){
		
		return null;
		
	}
	
	private List<Object[][][]> getCountryTableContent(){
		
		return null;
	}
	
	private List<Object[][][]> getBorderTableContent(){
		
		return null;
	}
	
	public ResponseWrapper addContinent(Continent continent) {
		
		return null;
	}
	

}
