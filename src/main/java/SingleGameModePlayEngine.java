
import java.util.List;

import business.MainPlayPhaseBusinessCommands;
import controller.MainPlayPhaseController;
import controller.MapEngineController;
import controller.SingleGameModePlayEngineController;
import logger.ConsoleWriter;
import logger.GeneralException;
import logger.Logger;
import model.GameModel;
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
	private MainPlayPhaseBusinessCommands mainPlayPhaseBusinessCommands;
	private GameModel gameModel;
	
	public SingleGameModePlayEngine() {
		singleGameModePlayEngineController = new SingleGameModePlayEngineController();
		mainPlayPhaseController = new MainPlayPhaseController();
		mainPlayPhaseBusinessCommands = new MainPlayPhaseBusinessCommands();
		gameModel = GameModel.getInstance();
		
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
		
		
		System.out.println("continue");
		
		
		
		
		
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
		System.out.print("Commit");
		
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
			mainPlayPhaseBusinessCommands.doReinforcements();
			
			while(true) {
				// get player's turn 
				this.printMainPlaySetupCommands();
				Player currentPlayer = gameModel.getCurrentPlayer();
				System.out.println(currentPlayer.getPlayerName() + "turn");
				// ask for attack commands phase  with player
				mainPlaySetUpResponse = mainPlayPhaseController.getMainPlaySetUpCommandsFromUser(currentPlayer);
				System.out.println(mainPlaySetUpResponse.getDescription());
				
				
				if (gameModel.doNextPlayer()) {
					// no next player do commit state
					System.out.println("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
					break;
				}
				 
				
			}
			// in execution if player capture country he will get card
			// in execution if player goin to win
			
			
			System.out.println(mainPlaySetUpResponse.getDescription());
			if(mainPlaySetUpResponse.getStatusValue() == 201) {
				System.out.println("Game Ends");
				break;
			}
			
			
		}
		
		return null;
			
	}

}
