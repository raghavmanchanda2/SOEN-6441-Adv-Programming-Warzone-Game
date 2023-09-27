package model;

import java.util.List;
import java.util.Map;

public class Country {
	private int d_uniqueCountryId;
	
	private String d_countryId;
	private Continent d_continent;
	private Map<Country,List<Country>> d_connectedCountries;
	
	
	public Country(int p_uniqueCountryId, String p_countryId) {
		super();
		this.d_uniqueCountryId = p_uniqueCountryId;
		this.d_countryId = p_countryId;
	}

	public Country(String p_countryId) {
		super();
		this.d_countryId = p_countryId;
	}

	public Country(String p_countryId, Continent p_continent) {
		super();
		this.d_countryId = p_countryId;
		this.d_continent = p_continent;
	}
	
	public Country(String p_countryId, Map<Country, List<Country>> p_connectedCountries) {
		super();
		this.d_countryId = p_countryId;
		this.d_connectedCountries = p_connectedCountries;
	}


	public int getUniqueCountryId() {
		return d_uniqueCountryId;
	}

	public void setUniqueCountryId(int p_uniqueCountryId) {
		this.d_uniqueCountryId = p_uniqueCountryId;
	}

	public Map<Country, List<Country>> getConnectedCountries() {
		return d_connectedCountries;
	}

	public void setConnectedCountries(Map<Country, List<Country>> p_connectedCountries) {
		this.d_connectedCountries = p_connectedCountries;
	}

	public String getCountryId() {
		return d_countryId;
	}
	public void setCountryId(String p_countryId) {
		this.d_countryId = p_countryId;
	}
	public Continent getContinent() {
		return d_continent;
	}
	public void setContinent(Continent p_continent) {
		this.d_continent = p_continent;
	}
	
	
}
