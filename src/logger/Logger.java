package logger;

import java.util.Observable;

public class Logger extends Observable {

	public void setLogMessage(String p_logMsg) {
		setChanged();
		notifyObservers(p_logMsg);
	}

}
