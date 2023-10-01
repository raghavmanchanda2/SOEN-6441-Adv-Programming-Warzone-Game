import GamePhase.MapPhaseState;
import controller.WarzoneEngineController;
import logger.ConsoleWriter;
import logger.LogGenerator;
import logger.Logger;

public class WarzoneEngine {

	private WarzoneEngineController d_warzoneEngineController;
	private LogGenerator d_logGenrator;
	private MapEngine d_mapEngine;

	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;

	public WarzoneEngine() {
		d_warzoneEngineController = new WarzoneEngineController();
		d_mapEngine = new MapEngine();
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
		d_logger.setLogMessage("N U    4. Quit Game");
		d_logger.setLogMessage("");
		d_logger.setLogMessage("Input one option to get started");
		d_logger.setLogMessage("(Getting input from user...)");

	}

	public synchronized void gameStarts() {
		while (true) {
			d_logGenrator.logInfoMsg("WAR ZONE GAME STARTS", 'I');
			this.printAvailableWarZoneCommands();

			// get commands input from warZone controller file
			int  userInput = this.d_warzoneEngineController.getGameFeatureInputs();
			if(userInput == 1) {
				d_logGenrator.logInfoMsg("MAP ENGINE STARTS", 'I');
				// sends to map engine
				d_mapEngine.startMapEngine();
				// clear Map states
				MapPhaseState.clearMapPhaseStates();
				d_logGenrator.logInfoMsg("MAP ENGINE ENDS", 'I');
			}else if(userInput == 2) {
				
			}else if(userInput == 3) {
				
			}else if(userInput == 4) {
				System.out.println("Game ENDED GRACEFULLY....");
				break;
			}else if(userInput == 404) {
				continue;
			}
			
			break;
		}

	}

}
