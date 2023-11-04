package logger;

import java.util.Observable;


/**
 * class for handling log messages
 * @author Raghav
 * @version build 1
 */
public class LogEntryBuffer extends Observable {
	
	public String d_logMessage;
	
	/**
	 * Set log message and notify observers
	 * @param p_updateMsg log message
	 */
	
	public void setLogMessage(String ...p_updateMsg) {
		
		if(p_updateMsg.length==1 || p_updateMsg[1]==null)
		{
			d_logMessage=p_updateMsg[0];		
		}
		else 
		{
			switch(p_updateMsg[1].toLowerCase()){
			
	        case "command":
	            d_logMessage = System.lineSeparator()+ " Command Entered: "+ p_updateMsg[0] + System.lineSeparator();
	            break;
	        case "order":
	            d_logMessage = System.lineSeparator()+ " Order Issued: "+p_updateMsg[0]+System.lineSeparator();
	            break;
	        case "phase":
	            d_logMessage = System.lineSeparator() + "----" + p_updateMsg[0] + "----"  + System.lineSeparator()+System.lineSeparator();
	            break;
	        case "effect":
	            d_logMessage = "LogMsg: "+ p_updateMsg[0] + System.lineSeparator();
	            break;
	        case "start":
	        case "end":
	            d_logMessage = p_updateMsg[0] + System.lineSeparator();
	            break;
				default: break;
		}
		
    }
		
		setChanged();
		notifyObservers(d_logMessage);
	}

	public String getD_logMessage() {
		return d_logMessage;
	}

	public void setD_logMessage(String d_logMessage) {
		this.d_logMessage = d_logMessage;
	}

}
