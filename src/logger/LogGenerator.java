package logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class LogGenerator {

	private static final Logger SYSTEM_LOG = Logger.getLogger("systemLogFile");
	private static final String LOG_FILE_PATH = Paths.get("").toAbsolutePath() + "/src/logger/" + "/systemLog.log";
	private static FileHandler sytsemLogFileHandler;
	
	public void createFile() {
		
		try {
			sytsemLogFileHandler = new FileHandler(LOG_FILE_PATH, true);
			SYSTEM_LOG.addHandler(sytsemLogFileHandler);
			SimpleFormatter formatter = new SimpleFormatter();
			sytsemLogFileHandler.setFormatter(formatter);
			clearLogs();
		} catch (SecurityException | IOException exception) {
			exception.printStackTrace();
		}
	}

	public void writeLogOnConsole(Logger systemLog, Boolean logOnCnsole) {
		systemLog.setUseParentHandlers(logOnCnsole);
	}

	public void logInfoMsg(String msg, char logType) {
		switch (logType) {
		case 'I':
			SYSTEM_LOG.info(msg);
			break;
		case 'W':
			SYSTEM_LOG.warning(msg);
			break;
		case 'S':
			SYSTEM_LOG.severe(msg);
			break;
		default:
			SYSTEM_LOG.fine(msg);
			break;

		}
		SYSTEM_LOG.info(msg);
	}

	public void clearLogs() {

		sytsemLogFileHandler.close();
		File logFile = new File(LOG_FILE_PATH);
		if (logFile.exists()) {
			if (logFile.delete()) {
				System.out.println("Log file deleted successfully.");
			} else {
				System.err.println("Failed to delete the log file.");
			}
		}
	}

}
