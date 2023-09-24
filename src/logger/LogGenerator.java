package logger;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class LogGenerator {

	private static final Logger SYSTEM_LOG = Logger.getLogger("systemLogFile");
	private static final String LOG_FILE_PATH = System.getProperty("user.dir") + "\\src\\logFiles\\" + "systemLog.log";

	
	public  void createFile() {
		FileHandler sytsemLogFileHandler;

		try {
			
			sytsemLogFileHandler = new FileHandler(LOG_FILE_PATH, true);
			SYSTEM_LOG.addHandler(sytsemLogFileHandler);
			SimpleFormatter formatter = new SimpleFormatter();
			sytsemLogFileHandler.setFormatter(formatter);
		}
		catch (SecurityException | IOException exception) {
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

	public void clearLogs(Logger systemLog, String filePath) {

		try (RandomAccessFile raf = new RandomAccessFile(filePath, "rw")) {
			raf.setLength(0);

		} catch (IOException e) {
			logInfoMsg( "Failed to truncate file: " + e.getMessage(), 'W');
		}
	}

}
