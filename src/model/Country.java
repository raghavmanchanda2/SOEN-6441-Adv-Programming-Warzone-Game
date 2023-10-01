package model;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Country {
	private int d_uniqueCountryId;
	
	private String d_countryId;

	private String d_countryName;
	private Continent d_continent;

	private String d_Continent;

	private Player d_Player;
	private Map<Country,List<Country>> d_connectedCountries;

	private Set<Country> d_Neighbors;

	private int d_Armies;

	public Country() {
	}

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

	public String getD_Continent() {
		return d_Continent;
	}

	public void setD_Continent(String d_Continent) {
		this.d_Continent = d_Continent;
	}

	public String getD_countryName() {
		return d_countryName;
	}

	public void setD_countryName(String d_countryName) {
		this.d_countryName = d_countryName;
	}

	public Set<Country> getNeighbors() {
		if (d_Neighbors == null) {
			d_Neighbors = new HashSet<>();
		}
		return d_Neighbors;
	}

	public int getArmies() {

		return d_Armies;
	}

	public Player getPlayer() {
		return d_Player;
	}

	public void setPlayer(Player p_Player) {
		this.d_Player = p_Player;
	}

	public String createNeighboursList(Set<Country> p_Neighbors) {
		String l_result = "";
		for (Country l_Neighbor : p_Neighbors) {
			l_result += l_Neighbor.getD_countryName() + "-";
		}
		return l_result.length() > 0 ? l_result.substring(0, l_result.length() - 1) : "";
	}

	public void armiesDeploy(int p_NumberOfArmies){
		d_Armies += p_NumberOfArmies;
	}
}
