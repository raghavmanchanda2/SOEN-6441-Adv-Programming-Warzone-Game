
import java.util.List;

import controller.MainPlayPhaseController;
import controller.MapEngineController;
import controller.SingleGameModePlayEngineController;
import logger.ConsoleWriter;
import logger.GeneralException;
import logger.Logger;
import model.Player;
import model.ResponseWrapper;

/**
 * Single game mode class to be implemented in the future
 * @author Kevin
 * @version build 1
 */
public class SingleGameModePlayEngine {
	
	
	private SingleGameModePlayEngineController singleGameModePlayEngineController;
	private MainPlayPhaseController  mainPlayPhaseController;
	
	public SingleGameModePlayEngine() {
		singleGameModePlayEngineController = new SingleGameModePlayEngineController();
		mainPlayPhaseController = new MainPlayPhaseController();
		
	}
	
	private void printPlaySetupCommands() {
		// loadmap
		// showmap
		
		// addplayers
		// assigncountries
		
		
		
		
		
		System.out.println("showmap");
		System.out.println("loadmap");
		System.out.println("gameplayer -add or remove");
		System.out.println("assigncountries");
		
		// reinforcement
		
		
		// issue order
		System.out.println("Orders commands");
		
		
		
		System.out.println("commit commands");
		
	}
	private void printMainPlaySetupCommands() {
		//Reinforcement
		
		
		// attack
		System.out.println("deploy");
		System.out.println("Advance");
		System.out.println("Reinforcement");
		System.out.println("Bomb");
		System.out.println("Airlift");
		System.out.println("Blockade");
		System.out.println("Diplomacy");
		
		//commit
		
		// who win
	}
	
	
	
	public ResponseWrapper startGamePlayMode() throws GeneralException {
		
		ResponseWrapper initialSetupResponse ; 
		
		// initial setup phase
		while(true) {
			this.printPlaySetupCommands();
			initialSetupResponse = singleGameModePlayEngineController.getPlaySetupCommandsFromUser();
			System.out.println(initialSetupResponse.getDescription());
			if(initialSetupResponse.getStatusValue() == 201) {
				System.out.println("Moving to GamePlay Phase and can't be go back in initial set up phase");
				break;
			}
		}
		
		// Reinforcement attack and fortification
		ResponseWrapper mainPlaySetUpResponse;
		while(true) {
			
			// do Reinforcements 
			
			// get player's turn 
						
			
			this.printMainPlaySetupCommands();
			
			
			mainPlaySetUpResponse = mainPlayPhaseController.getMainPlaySetUpCommandsFromUser();
			System.out.println(mainPlaySetUpResponse.getDescription());
			if(mainPlaySetUpResponse.getStatusValue() == 201) {
				System.out.println("Game Ends");
				break;
			}
		}
		
		
		
		
		
		
		return null;
			
	}

}
