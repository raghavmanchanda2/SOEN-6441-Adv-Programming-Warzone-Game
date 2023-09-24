package model;

import java.util.List;
import java.util.Map;

public class Country {
	private int uniqueCountryId;
	
	private String countryId;
	private Continent continent;
	private Map<Country,List<Country>> connectedCountries;
	
	
	public Country(int uniqueCountryId, String countryId) {
		super();
		this.uniqueCountryId = uniqueCountryId;
		this.countryId = countryId;
	}

	public Country(String countryId) {
		super();
		this.countryId = countryId;
	}

	public Country(String countryId, Continent continent) {
		super();
		this.countryId = countryId;
		this.continent = continent;
	}
	
	public Country(String countryId, Map<Country, List<Country>> connectedCountries) {
		super();
		this.countryId = countryId;
		this.connectedCountries = connectedCountries;
	}


	public int getUniqueCountryId() {
		return uniqueCountryId;
	}

	public void setUniqueCountryId(int uniqueCountryId) {
		this.uniqueCountryId = uniqueCountryId;
	}

	public Map<Country, List<Country>> getConnectedCountries() {
		return connectedCountries;
	}

	public void setConnectedCountries(Map<Country, List<Country>> connectedCountries) {
		this.connectedCountries = connectedCountries;
	}

	public String getCountryId() {
		return countryId;
	}
	public void setCountryId(String countryId) {
		this.countryId = countryId;
	}
	public Continent getContinent() {
		return continent;
	}
	public void setContinent(Continent continent) {
		this.continent = continent;
	}
	
	
}
