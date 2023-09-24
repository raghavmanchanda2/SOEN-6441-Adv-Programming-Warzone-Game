package model;

import java.util.List;

public class Continent {
	private int uniqueContinetId;
	private String continentId;
	private String contientValue;
	private List<Country> continentCountries;
	
	
	
	public Continent(int uniqueContinetId, String continentId) {
		super();
		this.uniqueContinetId = uniqueContinetId;
		this.continentId = continentId;
	}

	public Continent(String continentId) {
		super();
		this.continentId = continentId;
	}

	public Continent(String continentId, String contientValue) {
		super();
		this.continentId = continentId;
		this.contientValue = contientValue;
	}
		
	public Continent(String continentId, List<Country> continentCountries) {
		super();
		this.continentId = continentId;
		this.continentCountries = continentCountries;
	}

	public int getUniqueContinetId() {
		return uniqueContinetId;
	}

	public void setUniqueContinetId(int uniqueContinetId) {
		this.uniqueContinetId = uniqueContinetId;
	}

	public List<Country> getContinentCountries() {
		return continentCountries;
	}

	public void setContinentCountries(List<Country> continentCountries) {
		this.continentCountries = continentCountries;
	}

	public String getContinentId() {
		return continentId;
	}
	public void setContinentId(String continentId) {
		this.continentId = continentId;
	}
	public String getContientValue() {
		return contientValue;
	}
	public void setContientValue(String contientValue) {
		this.contientValue = contientValue;
	}
	

}
