package model;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class MapModel {
	
	private String mapName;
	private List<Continent> continents;
	private List<Country> countries;
	private Map<Continent,List<Country>> continentCountries;
	private Map<Country,List<Country>> borders;
	
	
	public List<Country> getCountries() {
		return countries;
	}
	public void setCountries(List<Country> countries) {
		this.countries = countries;
	}
	public String getMapName() {
		return mapName;
	}
	public void setMapName(String mapName) {
		this.mapName = mapName;
	}
	
	public List<Continent> getContinents() {
		return continents;
	}
	public void setContinents(List<Continent> continents) {
		this.continents = continents;
	}
	public Map<Continent, List<Country>> getContinentCountries() {
		return continentCountries;
	}
	public void setContinentCountries(Map<Continent, List<Country>> continentCountries) {
		this.continentCountries = continentCountries;
	}
	public Map<Country, List<Country>> getBorders() {
		return borders;
	}
	public void setBorders(Map<Country, List<Country>> borders) {
		this.borders = borders;
	}
	
	public void addContinent(Continent continent) {
		this.continents.add(continent);
		this.continentCountries.put(continent, new ArrayList<>());
	}
	
	public void addContinentCountries(Continent continent, Country country) {
		if (this.continentCountries.containsKey(continent)) {
			this.continentCountries.get(continent).add(country);
			this.borders.put(country, new ArrayList<>());
			this.countries.add(country);
		}
	}
	
	public void addBorders(Country mainCountry,Country neighbourCountry) {
		if(this.borders.containsKey(mainCountry)) {
			this.borders.get(mainCountry).add(neighbourCountry);
		}
	}

}
