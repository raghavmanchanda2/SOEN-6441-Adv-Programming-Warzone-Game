package model;

public class Country {
	private String countryId;
	private Continent continent;
	
	
	public Country(String countryId, Continent continent) {
		super();
		this.countryId = countryId;
		this.continent = continent;
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
