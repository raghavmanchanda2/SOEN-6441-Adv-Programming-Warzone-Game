package com.concordia.warzone;

public class GeneralException extends Exception {

	
	private static final long serialVersionUID = 1L;

	public GeneralException(String message) {
		super(message);
	}
	
	public String validateCommand(String message) throws GeneralException {
		String specialCharacters = " !#$%&'()*+,-./:;<=>?@[]^_`{|}~";
		if (message == null || "".equals(message) || specialCharacters.contains(message)) {
			throw new GeneralException("Incorrect Command, Please write correct command");
		} else {
			return message;
		}
	}
}
