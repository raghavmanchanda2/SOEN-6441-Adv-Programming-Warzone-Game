package logger;

import java.util.Observable;


/**
 * class for handling log messages
 * @author Raghav
 * @version build 1
 */
public class Logger extends Observable {
	
	/**
	 * Set log message and notify observers
	 * @param p_logMsg log message
	 */
	public void setLogMessage(String p_logMsg) {
		setChanged();
		notifyObservers(p_logMsg);
	}

}
