import GamePhase.MapPhaseState;
import controller.CurrentGamePlay;
import controller.WarzoneEngineController;
import logger.ConsoleWriter;
import logger.LogGenerator;
import logger.Logger;
import model.GamePhaseEnum;

public class WarzoneEngine {

	private WarzoneEngineController d_warzoneEngineController;
	private LogGenerator d_logGenrator;
	private MapEngine d_mapEngine;
	private CurrentGamePlay d_currentGamePlay;

	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;

	public WarzoneEngine() {
		d_warzoneEngineController = new WarzoneEngineController();
		d_mapEngine = new MapEngine();
		d_currentGamePlay = new CurrentGamePlay();
		d_logGenrator = new LogGenerator();
		d_logGenrator.createFile();
		d_consoleWriter = new ConsoleWriter();
		d_logger = new Logger();
		d_logger.addObserver(d_consoleWriter);

	}

	/**
	 * method to print all the available commands at the start New Game, Load Game,
	 * Single Game Mode, Tournament Mode, Exit Game
	 */

	private void printAvailableWarZoneCommands() {

		// Initialising war zone game comments
		// printout different features
		// 1. Map Features
		// 2. Single game mode between multiple players
		// 3. tournament mode between multiple players

		d_logger.setLogMessage("");
		d_logger.setLogMessage("*******************************");
		d_logger.setLogMessage("     Warzone 2023 by W13");
		d_logger.setLogMessage("*******************************");
		d_logger.setLogMessage("");
		d_logger.setLogMessage("M M    1. New Game");
		d_logger.setLogMessage("A E    2. Single Game Mode");
		d_logger.setLogMessage("I N    3. Tournament Mode");
		d_logger.setLogMessage("N U    3. Quit Game");
		d_logger.setLogMessage("");
		d_logger.setLogMessage("Input one option to get started");
		d_logger.setLogMessage("(Getting input from user...)");

	}

	public synchronized void gameStarts() throws Exception {
		d_logGenrator.logInfoMsg("WAR ZONE GAME STARTS", 'I');
		this.printAvailableWarZoneCommands();

		// get commands input from warZone controller file

		switch (this.d_warzoneEngineController.getGameFeatureInputs()) {
		case 1:
			d_logGenrator.logInfoMsg("MAP ENGINE STARTS", 'I');
			// sends to map engine
			d_mapEngine.startMapEngine();

			// clear Map states
			MapPhaseState.clearMapPhaseStates();

			//d_logGenrator.logInfoMsg("MAP ENGINE ENDS", 'I');
			//d_logGenrator.logInfoMsg("STARTUP PHASE STARTS", 'I');
			d_currentGamePlay.startStartup();



			break;
		case 2:
			// single game mode engines
			break;
		case 3:
			// tournament game mode engine
			break;
		case -1:
			// restart the game

		}

	}

}
