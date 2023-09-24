package model;

public class ResponseWrapper {
	private int statusValue;
	private String description;
	public int getStatusValue() {
		return statusValue;
	}
	public ResponseWrapper(int statusValue, String description) {
		this.statusValue = statusValue;
		this.description = description;
	}
	
	public void setStatusValue(int statusValue) {
		this.statusValue = statusValue;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	
	
	

}
