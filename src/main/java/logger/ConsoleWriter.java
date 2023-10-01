package logger;

import java.util.Observable;
import java.util.Observer;

public class ConsoleWriter implements Observer {

	private String d_logMsg;

	@Override
	public void update(Observable p_observable, Object p_logObject) {
		this.setLogMsg((String) p_logObject);
		System.out.println(d_logMsg);
	}
	
	public String getLogMsg() {
		return d_logMsg;
	}

	public void setLogMsg(String logMsg) {
		this.d_logMsg = logMsg;
	}
	
}