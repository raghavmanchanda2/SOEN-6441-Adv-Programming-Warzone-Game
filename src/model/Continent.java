package model;

public class Continent {
	
	private String continentId;
	private String contientValue;
	
	
	
	public Continent(String continentId, String contientValue) {
		super();
		this.continentId = continentId;
		this.contientValue = contientValue;
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
