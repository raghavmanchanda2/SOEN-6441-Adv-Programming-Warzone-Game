package model;

public class ResponseWrapper {
	private int d_statusValue;
	private String d_description;
	public int getStatusValue() {
		return d_statusValue;
	}
	public ResponseWrapper(int p_statusValue, String p_description) {
		this.d_statusValue = p_statusValue;
		this.d_description = p_description;
	}
	
	public void setStatusValue(int p_statusValue) {
		this.d_statusValue = p_statusValue;
	}
	public String getDescription() {
		return d_description;
	}
	public void setDescription(String description) {
		this.d_description = description;
	}
	
	
	

}
