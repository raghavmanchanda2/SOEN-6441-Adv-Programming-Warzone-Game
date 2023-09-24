package com.concordia.warzone;

import java.util.Observable;

public class Logger extends Observable {

	public void setLogMessage(String logMsg) {
		setChanged();
		notifyObservers(logMsg);
	}

}
