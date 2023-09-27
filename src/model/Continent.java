package model;

import java.util.List;

public class Continent {
	private int d_uniqueContinetId;
	private String d_continentId;
	private String d_contientValue;
	private List<Country> d_continentCountries;
	
	
	
	public Continent(int p_uniqueContinetId, String p_continentId) {
		super();
		this.d_uniqueContinetId = p_uniqueContinetId;
		this.d_continentId = p_continentId;
	}

	public Continent(String p_continentId) {
		super();
		this.d_continentId = p_continentId;
	}

	public Continent(String p_continentId, String p_contientValue) {
		super();
		this.d_continentId = p_continentId;
		this.d_contientValue = p_contientValue;
	}
		
	public Continent(String p_continentId, List<Country> p_continentCountries) {
		super();
		this.d_continentId = p_continentId;
		this.d_continentCountries = p_continentCountries;
	}

	public int getUniqueContinetId() {
		return d_uniqueContinetId;
	}

	public void setUniqueContinetId(int p_uniqueContinetId) {
		this.d_uniqueContinetId = p_uniqueContinetId;
	}

	public List<Country> getContinentCountries() {
		return d_continentCountries;
	}

	public void setContinentCountries(List<Country> p_continentCountries) {
		this.d_continentCountries = p_continentCountries;
	}

	public String getContinentId() {
		return d_continentId;
	}
	public void setContinentId(String p_continentId) {
		this.d_continentId = p_continentId;
	}
	public String getContientValue() {
		return d_contientValue;
	}
	public void setContientValue(String p_contientValue) {
		this.d_contientValue = p_contientValue;
	}
	

}
