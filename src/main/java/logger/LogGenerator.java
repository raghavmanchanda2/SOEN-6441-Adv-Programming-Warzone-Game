package logger;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;


/**
 * class for log generation, file creation and log manipulation
 * @author Raghav
 * @version build 1
 */
public class LogGenerator {

	private static final Logger D_SYSTEM_LOG = Logger.getLogger("systemLogFile");
	private static final String D_LOG_FILE_PATH = Paths.get("").toAbsolutePath() + "/src/main/java/logger/" + "/systemLog.log";
	private static FileHandler d_sytsemLogFileHandler;
	
	/**
	 * Method for setting up file creation using class Logger and FileHandler
	 */
	public void createFile() {
		
		try {
			d_sytsemLogFileHandler = new FileHandler(D_LOG_FILE_PATH, true);
			D_SYSTEM_LOG.addHandler(d_sytsemLogFileHandler);
			SimpleFormatter l_formatter = new SimpleFormatter();
			d_sytsemLogFileHandler.setFormatter(l_formatter);
		} catch (SecurityException | IOException l_exception) {
			l_exception.printStackTrace();
		}
	}
	
	/**
	 * Method to allow display of log messages on the console with use of parent handlers method
	 * @param p_systemLog Logger variable 
	 * @param p_logOnCnsole boolean variable that specifies whether log messages should be displayed on the console
	 */
	public void writeLogOnConsole(Logger p_systemLog, Boolean p_logOnCnsole) {
		p_systemLog.setUseParentHandlers(p_logOnCnsole);
	}
	
	
	/**
	 * Method for handling multiple message types with the use of a switch statement and initializes D_SYSTEM_LOG
	 * @param p_msg log message
	 * @param p_logType type of message
	 */
	public void logInfoMsg(String p_msg, char p_logType) {
		switch (p_logType) {
		case 'I':
			D_SYSTEM_LOG.info(p_msg);
			break;
		case 'W':
			D_SYSTEM_LOG.warning(p_msg);
			break;
		case 'S':
			D_SYSTEM_LOG.severe(p_msg);
			break;
		default:
			D_SYSTEM_LOG.fine(p_msg);
			break;

		}
		D_SYSTEM_LOG.info(p_msg);
	}
	
	/**
	 * Method for deleting log file.
	 */
	public void clearLogs() {

		d_sytsemLogFileHandler.close();
		File l_logFile = new File(D_LOG_FILE_PATH);
		if (l_logFile.exists()) {
			if (l_logFile.delete()) {
				System.out.println("Log file deleted successfully.");
			} else {
				System.err.println("Failed to delete the log file.");
			}
		}
	}

}
