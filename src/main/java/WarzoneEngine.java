import GamePhase.MapPhaseState;

import controller.WarzoneEngineController;
import logger.ConsoleWriter;
import logger.LogGenerator;
import logger.Logger;
import model.ResponseWrapper;

/**
 * Class to start the game with multiple options to the user.
 * @author Rohit
 * @author Ishaan Bajaj
 * @author Raghav Manchanda
 * @author Bhavya Bugude
 * @author Sumit Kumar
 * @author Kevin
 * @version build 1
 */
public class WarzoneEngine {


	/**
	 * Data member of WarZoneEngineController
	 */
	private WarzoneEngineController d_warzoneEngineController;
	/**
	 * LogGenerator to generate engine logs
	 */
	private LogGenerator d_logGenrator;
	/**
	 * MapEngine object to offer map options
	 */
	private MapEngine d_mapEngine;
	
	/**
	 * to print the statements in the console
	 */
	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;

	private SingleGameModePlayEngine singleGameModePlayEngine;

	/**
	 * constructor for WarZoneEngine
	 */
	public WarzoneEngine() {
		d_warzoneEngineController = new WarzoneEngineController();
		d_mapEngine = new MapEngine();
		d_logGenrator = new LogGenerator();
		d_logGenrator.createFile();
		singleGameModePlayEngine = new SingleGameModePlayEngine();
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
		d_logger.setLogMessage("****************************************");
		d_logger.setLogMessage("*********** RISK GAME by W13 ***********");
		d_logger.setLogMessage("****************************************");
		d_logger.setLogMessage("************** MAIN MENU ***************");
		d_logger.setLogMessage("****************************************");
		d_logger.setLogMessage("");
		d_logger.setLogMessage("-> To start the game: gamestart -new");
		d_logger.setLogMessage("-> To end the game: gameend -end");
		d_logger.setLogMessage("");
		d_logger.setLogMessage("***** Input any command to proceed *****");
		d_logger.setLogMessage("****(Getting input from the user...)****");


	}

	/**
	 * method to start the game
	 * @throws Exception - input command is incorrect
	 */

	public synchronized void gameStarts() throws Exception{
		while (true) {
			d_logGenrator.logInfoMsg("WAR ZONE GAME STARTS", 'I');
			this.printAvailableWarZoneCommands();

			// get commands input from warZone controller file
			String userInput = this.d_warzoneEngineController.getGameFeatureInputs();
			if(userInput.equals("gamestart -new")) {
				d_logGenrator.logInfoMsg("MAP ENGINE STARTS", 'I');
				// sends to map engine
				ResponseWrapper responseWrapper = d_mapEngine.startMapEngine();
				
				// clear Map states
				MapPhaseState.clearMapPhaseStates();
				
				if(responseWrapper.getStatusValue() == 201) {
					//start game
					singleGameModePlayEngine.startGamePlayMode();
				}
				
				break;
			}
			else if(userInput.equals("gameend -end")) {
				System.exit(0);
			} else if (userInput.equals("404")){
				continue;
			}
			
			break;
		}

	}

}
