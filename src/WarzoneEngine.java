import GamePhase.MapPhaseState;
import controller.WarzoneEngineController;
import logger.LogGenerator;

public class WarzoneEngine {

	  private WarzoneEngineController warzoneEngineController;
	  private LogGenerator logGenrator ;
	  private MapEngine mapEngine;
	  
	
	  
	  public WarzoneEngine() {
		  warzoneEngineController = new WarzoneEngineController();
		  mapEngine = new MapEngine();
		  logGenrator = new LogGenerator();
		  logGenrator.createFile();
		  
	  }
	  
	  private void printAvailableWarZoneCommands() {
			System.out.println("Print Available Commands OF WAR ZONE");
			// intializing war zone game comments
			// printout different features
			// 1. Map Features
			// 2. Single game mode between multiple players
			// 3. tournament mode between multiple players  
		}
	  
	  
	  public synchronized void gameStarts() {
		  logGenrator.logInfoMsg("WAR ZONE GAME STARTS", 'I');
		  this.printAvailableWarZoneCommands();
		  
		  
		  // get commands input from warZone controller file 
		  
		  switch(this.warzoneEngineController.getGameFeatureInputs()) {
		  case 1:
			  logGenrator.logInfoMsg("MAP ENGINE STARTS", 'I');
			  //  sends to map engine 
			  mapEngine.startMapEngine();
			  
			  // clear Map states
			  MapPhaseState.clearMapPhaseStates();
			  
			  logGenrator.logInfoMsg("MAP ENGINE ENDS", 'I');
			  
			  break;
		  case 2: 
			  // single game mode engines
			  break;
		  case 3: 
			 // tournament game mode engine
			  break;
		  case -1 :
			 // restart the game
		  
		  
		  
		  
		  }
 
	}
	  
	  
}
