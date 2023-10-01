package logger;

import java.util.ArrayList;
import java.util.List;

public class GeneralException extends Exception {

	public GeneralException() {
	}

	private static final long serialVersionUID = 1L;
	public GeneralException(String p_message) {
		super(p_message);
	}
	
	public String validateCommand(String p_message) throws GeneralException {
		
		if (p_message == null || "".equals(p_message)) {
			throw new GeneralException(" Command shouldn't be null or empty ");
		}
		
		List<Character> specialCharacters = new ArrayList<Character>();
		String chars = "!#$%&'()*+,/:;<=>?@[]^_`{|}~";
        for(int i=0;i<chars.length();i++)
        {
        	specialCharacters.add(chars.charAt(i));
             
        }
		for(int i=0; i<p_message.length();i++)
		{
			if(specialCharacters.contains(p_message.charAt(i)))
			{
				throw new GeneralException(" Command shouldn't have special character other than dash");

			}
		}
		return p_message;
		
	}
}
