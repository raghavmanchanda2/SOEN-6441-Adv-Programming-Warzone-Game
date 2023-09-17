package com.concordia.warzone.log;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class LogGenerator {

	public static final Logger SYSTEM_LOG = Logger.getLogger("systemLogFile");
	public static final String LOG_FILE_PATH = System.getProperty("user.dir") + "\\src\\logFiles\\" + "systemLog.log";

	
	public  void createFile(Logger systemLog, String filePath) {
		FileHandler sytsemLogFileHandler;

		try {
			
			sytsemLogFileHandler = new FileHandler(filePath, true);
			systemLog.addHandler(sytsemLogFileHandler);
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

	public void logInfoMsg(Logger systemLog, String msg, char logType) {
		switch (logType) {
		case 'I':
			systemLog.info(msg);
			break;
		case 'W':
			systemLog.warning(msg);
			break;
		case 'S':
			systemLog.severe(msg);
			break;
		default:
			systemLog.fine(msg);
			break;

		}
		systemLog.info(msg);
	}

	public void clearLogs(Logger systemLog, String filePath) {

		try (RandomAccessFile raf = new RandomAccessFile(filePath, "rw")) {
			raf.setLength(0);

		} catch (IOException e) {
			logInfoMsg(systemLog, "Failed to truncate file: " + e.getMessage(), 'W');
		}
	}

}
