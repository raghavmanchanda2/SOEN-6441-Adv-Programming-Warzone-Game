package logger;

public class GeneralException extends Exception {

	
	private static final long serialVersionUID = 1L;

	public GeneralException(String p_message) {
		super(p_message);
	}
	
	public String validateCommand(String p_message) throws GeneralException {
		String specialCharacters = " !#$%&'()*+,-./:;<=>?@[]^_`{|}~";
		if (p_message == null || "".equals(p_message) || specialCharacters.contains(p_message)) {
			throw new GeneralException("Incorrect Command, Please write correct command");
		} else {
			return p_message;
		}
	}
}
