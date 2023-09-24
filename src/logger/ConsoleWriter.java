package com.concordia.warzone;

import java.util.Observable;
import java.util.Observer;

public class ConsoleWriter implements Observer {

	private String logMsg;

	@Override
	public void update(Observable observable, Object logObject) {
		this.setLogMsg((String) logObject);
		System.out.println(logMsg);
	}
	
	public String getLogMsg() {
		return logMsg;
	}

	public void setLogMsg(String logMsg) {
		this.logMsg = logMsg;
	}
	
}