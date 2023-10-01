package model;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Continent {
	private int d_uniqueContinetId;

	private String d_continentName;
	private String d_continentId;
	private String d_contientValue;
	private List<Country> d_continentCountries;

	private Set<Country> d_Countries;

	private int d_AwardArmies;

	public Continent() {
	}

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

	public String getD_continentName() {
		return d_continentName;
	}

	public void setD_continentName(String d_continentName) {
		this.d_continentName = d_continentName;
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

	public Set<Country> getCountries() {
		if (d_Countries == null) {
			d_Countries = new HashSet<>();
		}
		return d_Countries;
	}

	public int getAwardArmies() {
		return d_AwardArmies;
	}

	public void setD_AwardArmies(int d_AwardArmies) {
		this.d_AwardArmies = d_AwardArmies;
	}
}
